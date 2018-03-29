function [act_val act_seq ltm d] = CLEF_model(nS, nC, stim_seq, ctxt_seq, ...
    act_seq, rewards, params, flags, ltm_ini)
%% Implementation of the CLEF model
%
% The implementation is based on
%
% Collins, Koechlin (2012). Reasoning, Learning, and Creativity: Frontal
% Lobe Function and Human Decision-Making. PLoS Biology 10(3): e1001293.
% doi:10.1371/journal.pbio.1001293
%
% with an initial implementation by Vincent Adam, and later
% refinements/modifications by Jan Drugowitsch and Muriel Ekovich.
%
% The function takes the following inputs:
%
% - nS: scalar, number of stimuli
% - nC: scalar, number of different contextual cues
% - stim_seq: vector, sequence of stimuli, from 1 to nS
% - ctxt_seq: vector, sequence of contextual cues, from 1 to nC
% - act_seq: vector, sequence of actions, from 1 to nA (implicit in rewards)
% - rewards: trials x nA reward matrix, contains 0/1,
%   rewards(t,a) specifies reward returned for performing action a in trial t
% - params: vector, contains the model parameters (more below)
% - flags (optional): specifies the model flags (more below)
% - ltm_ini (optimal): specifies the initial long-term memory (more below)
%
% If no contextual cues are present, the function expects nC = 0 and
% ctxt_seq = []. If act_seq = [], the function performs its own actions
% stochastically by eps-softmax action selection.
%
% The function returns:
%
% - act_val: trials x nA matrix of action-values, one per action/trial,
%   before applying eps-softmax
% - act_seq: vector, sequence of performed actions
% - ltm: structure, containing long-term memory parameters
% - d: structure, containing all data accumulated throughout simulation,
%   for details, see end of script
%
% The params vector specifies the model parameters in the following order:
%
%  1 - mon_size: size of the monitoring buffer, >= 1
%  2 - rl_alpha: learning rate of RL model, 0 <= rl_alpha <= 1
%  3 - rl_decay: decay rate of RL model, 0 (no decay) <= rl_decay <= 1
%  4 - bay_prior: prior strength of Baysian model, >= 1
%  5 - bay_decay: decay rate of Bayesian model, 0 (no decay) <= bay_decay  <= 1
%  6 - softmax_beta: inv. temperatur of softmax, >= 0
%  7 - softmax_eps: 'lapse' rate of softmax, 0 <= softmax_eps <= 0.5
%  8 - volatility: assumed volatility of task, 0 <= volatility <= 1
%  9 - bias_conf: initial confidence bias on probe, 0 <= bias_conf <= 1
% 10 - bias_ini: ltm initialisation bias on probe, 0 <= bias_ini <= 1
% 11 - ctxt_prior: prior strength of context model, >= 1
% 12 - ctxt_decay: decay rate of context model, 0 <= ctxt_decay <= 1
% 13 - bias_ctxt: contxt-switch influence on vol., 0 <= bias_ctxt <= 1
%
% The softmax parameters are only used if the given act_seq = [], such that
% the model chooses actions according to eps-softmax. The
% context-parameters are only used if contextual cues are present. In the
% case of the non-Bayesian context model, ctxt_prior becomes ctxt_alpha,
% which is the learning rate of this model.
%
% The flags argument is a sum of the following constants:
%
%   1 - rl_a_norm, uses action-normalised RL model
%   2 - rl_no_act_decay, does not decay actor in RL model
%   4 - rl_prediction, uses RL (rather than Bayesian) model for prediction
%  16 - bay_norm, uses normalised Bayesian model
%  32 - bay_no_act_decay, does not decay actor in Bayesian model
%  64 - bay_selection, uses Baysian (rather than RL) model for selection
% 128 - rl_s_norm, uses stimulus-normalised RL model
% 256 - ctxt_nonbay, uses non-Bayesian context model
%
% Changelog:
% 2011 Jun-Aug  Vincent Adam
%               initial implementation
%
% 2011/2012     Jan Drugowitsch & Muriel Ekovich
%               Bug fixes & modifications to normalisation & context model,
%               allowing initialisation with previous long-term memory
%
% 2012 Aug      Jan Drugowitsch
%               almost complete re-implementation to make equivalent to 
%               version implemented in C. Addition of flags
%
% 2013 Feb      Jan Drugowitsch
%               addition of stimulus-normalised RL model / rl_s_norm flag
%
% 2013 Feb      Jan Drugowitsch
%               implementation of normalized Bayesian model / bay_norm flag
%
% 2013 Apr      Jan Drugowitsch
%               added non-bayesian context update model / ctxt_nonbay flag

%% process parameters
if nargin < 9
    ltm_ini = [];
    if nargin < 8
        flags = 0;
    end
end
if isempty(ctxt_seq), ctxt_seq = zeros(size(stim_seq)); end
if isempty(act_seq), act_seq = zeros(size(stim_seq)); end
if isempty(nC), nC = 0; end
[nTrials, nA] = size(rewards);
% process flags
flags = uint32(flags);
f_rl_a_norm = bitand(flags,uint32(1))==uint32(1);
f_rl_no_act_decay = bitand(flags,uint32(2))==uint32(2);
f_rl_prediction = bitand(flags,uint32(4))==uint32(4);
f_bay_norm = bitand(flags,uint32(16))==uint32(16);
f_bay_no_act_decay = bitand(flags,uint32(32))==uint32(32);
f_bay_selection = bitand(flags,uint32(64))==uint32(64);
f_rl_s_norm = bitand(flags,uint32(128))==uint32(128);
f_ctxt_nonbay = bitand(flags,uint32(256))==uint32(256);


%% minimum allows probability (used in some limiting cases)
p_min = 1e-60;


%% model parameters
mon_size = params(1);
rl_alpha = params(2);
rl_decay = params(3);
bay_prior = params(4);
bay_decay = params(5);
softmax_beta = params(6);
softmax_eps = params(7);
volatility = params(8);
bias_conf = params(9);
bias_ini = params(10);
ctxt_prior = params(11);
ctxt_decay = params(12);
bias_ctxt = params(13);

% update parameters and prior for non-Bayesian context model
if f_ctxt_nonbay
    ctxt_alpha = ctxt_prior;
    if nC > 0, ctxt_prior = 1/nC;
    else ctxt_prior = 0; end
end


%% 'previous' context, needed for LTM initialisation
prev_ctxt = ctxt_seq(1);
% priors for rl & bayes
if f_bay_norm, bay_p_prior = repmat(bay_prior/nA, [nA nS]);
else
    bay_p_prior = repmat(cat(3, bay_prior/nA, bay_prior*(nA-1)/nA), [nA nS 1]);
end
rl_p_prior = ones(nA, nS) / nA;


%% ---------- initialisation of model variables ---------
if isempty(ltm_ini)
    % default initialisation
    nTS = 2;   % initial task sets: dummy + initial actor
    
    % reserve space for task-set parameters (Q-values, predictive model,
    % context/TS associations). Last dimension is always number of task sets
    rl_p = zeros(nA, nS, nTrials+1, nTS);
    if f_bay_norm, bay_p = zeros(nA, nS, nTrials+1, nTS);
    else bay_p = zeros(nA, nS, 2, nTrials+1, nTS); end
    ctxt_p = zeros(nC, nTrials+1, nTS);
    
    % initialise first actor set to uniform distribution
    rl_p(:,:,1,nTS) = rl_p_prior;
    if f_bay_norm, bay_p(:,:,1,nTS) = bay_p_prior; 
    else bay_p(:,:,:,1,nTS) = bay_p_prior; end
    ctxt_p(:,1,nTS) = ctxt_prior;
    
else
    % long-term memory available -> use for initialisation
    ltm_nTS = size(ltm_ini.rl_p, 3);
    nTS = ltm_nTS + 2;  % ltm + dummy + initial actor
    ltm_nC = size(ltm_ini.ctxt_p, 1);
    assert(((f_bay_norm & ltm_nTS == size(ltm_ini.bay_p, 3)) | ...
            (~f_bay_norm & ltm_nTS == size(ltm_ini.bay_p, 4))) & ...
           ltm_nTS == size(ltm_ini.ctxt_p, 2));
       
    % reserve space for task-set parameters (Q-values, predictive model,
    % context/TS associations). Last dimension is always number of task sets
    rl_p = zeros(nA, nS, nTrials+1, nTS);
    if f_bay_norm, bay_p = zeros(nA, nS, nTrials+1, nTS);
    else bay_p = zeros(nA, nS, 2, nTrials+1, nTS); end
    ctxt_p = zeros(nC, nTrials+1, nTS);
    
    % copy long-term memory
    rl_p(:,:,1,2:(ltm_nTS+1)) = reshape(ltm_ini.rl_p, [nA nS 1 ltm_nTS]);
    if f_bay_norm
        bay_p(:,:,1,2:(ltm_nTS+1)) = reshape(ltm_ini.bay_p, [nA nS 1 ltm_nTS]);
    else
        bay_p(:,:,:,1,2:(ltm_nTS+1)) = reshape(ltm_ini.bay_p, [nA nS 2 1 ltm_nTS]);
    end
    % only the previously seen contexts
    ctxt_p(1:ltm_nC,1,2:(ltm_nTS+1)) = reshape(ltm_ini.ctxt_p, [ltm_nC 1 ltm_nTS]);

    % initialise first actor task set based on long-term memory, bias eta
    if f_bay_norm
        [avg_rl, avg_bay] = ltm_avg_norm(rl_p(:,:,1,2:(ltm_nTS+1)), ...
                                         bay_p(:,:,1,2:(ltm_nTS+1)), ...
                                         ctxt_p(:,1,2:(ltm_nTS+1)), ...
                                         prev_ctxt, p_min);
        bay_p(:,:,1,nTS) = bias_ini*bay_prior*avg_bay + ...
                           (1-bias_ini)*bay_p_prior;
    else
        [avg_rl, avg_bay] = ltm_avg(rl_p(:,:,1,2:(ltm_nTS+1)), ...
                                    bay_p(:,:,:,1,2:(ltm_nTS+1)), ...
                                    ctxt_p(:,1,2:(ltm_nTS+1)), prev_ctxt, p_min);
        bay_p(:,:,:,1,nTS) = bias_ini*cat(3, avg_bay*bay_prior, ...
                                             (1-avg_bay)*bay_prior) + ...
                             (1-bias_ini)*bay_p_prior;
    end
    rl_p(:,:,1,nTS) = bias_ini * avg_rl + (1 - bias_ini) * rl_p_prior;
    ctxt_p(:,1,nTS) = ctxt_prior;
end

% initialise dummy to all-uniform distributions
rl_p(:,:,1,1) = rl_p_prior;
if f_bay_norm, bay_p(:,:,1,1) = bay_p_prior;
else bay_p(:,:,:,1,1) = bay_p_prior; end
ctxt_p(:,:,1,1) = ctxt_prior;

% task set store properties
actorTS = nTS;
monTS = [1 actorTS];  % currently monitored: dummy + actor
ltmTS = 2:nTS;        % list of task sets in long-term memory

% current state, 1 = exploitation, 2 = exploration
state = zeros(1, nTrials+1);
state(1) = 1;  % start with exploitation

% ex-ante confidence for each task set at all times
l = zeros(nTrials, nTS);
l(1,1) = 0.3;
l(1,nTS) = 0.7;  % first actor task set is dominant

% ex-post confidence for each task set at all times
% (initialised to get above initial ex-ante confidences)
mu = zeros(nTrials+1, nTS);
mu_ini = (1/(1-2*volatility)) * ...
         ([(1-volatility) -volatility; -volatility (1-volatility)] * ...
          [l(1,1); l(1,nTS)]);
mu(1,1) = mu_ini(1);
mu(1,nTS) = mu_ini(2);


%% variables storing model activity
TS_seq = zeros(nTrials, 1);      % task set sequence
monTS_seq = zeros(nTrials, mon_size+2);  % history of monitored sets
act_val = zeros(nTrials, nA);    % 'value' of each action for actor task set


%% ---------- main loop ----------
for t = 1:nTrials
    cur_ctxt = ctxt_seq(t);
    cur_stim = stim_seq(t);
    cur_act = act_seq(t);
    
    %% --------- (1) update confidence: transition & context ---------
    % context change? -> bias transition probability
    if (cur_ctxt ~= 0) && (cur_ctxt ~= prev_ctxt)
        eff_vol = 1 - bias_ctxt*(1-volatility);
    else eff_vol = volatility;
    end
    % compite ex-ante confidences after transition
    l(t, monTS) = (1-eff_vol) * mu(t,monTS) + ...
                  eff_vol / (length(monTS)-1) * (1-mu(t,monTS));
    % add context likelihood
    if cur_ctxt ~= 0
        l(t,monTS) = squeeze(ctxt_p(cur_ctxt,t,monTS))' .* l(t, monTS);
        l(t,:) = l(t,:) / sum(l(t,:));
    end
    
    
    %% ---------- (2) control: stay/switch ----------
    if state(t) == 1
        %% currently exploiting
        if l(t, actorTS) >= 0.5
            % current task set has confidence >= 0.5 - keep using it
            state(t+1) = 1;
        else
            % actor task set no longer fits - check currently best task set
            [lmax, imax] = max(l(t,monTS));
            
            if (lmax > 0.5) && (imax ~= 1)
                % task set other than dummy has confidence > 0.5 - switch
                actorTS = monTS(imax);
                state(t+1) = 1;
                % move new actor task set to end of monitoring buffer
                monTS = [monTS(1:(imax-1)) monTS((imax+1):end) actorTS];
            else
                % no task set has confidence > 0.5 - build probe
                nTS = nTS + 1;    % explore with newly created task set
                actorTS = nTS;
                state(t+1) = 2;
                
                % reserve space
                l = cat(2, l, zeros(nTrials,1));
                rl_p = cat(4, rl_p, zeros(nA,nS,nTrials+1));
                if f_bay_norm, bay_p = cat(4, bay_p, zeros(nA, nS, nTrials+1));
                else bay_p = cat(5, bay_p, zeros(nA, nS, 2, nTrials+1)); end
                ctxt_p = cat(3, ctxt_p, zeros(nC, nTrials + 1));
                
                % new ex-ante confidence, based on entropy
                H = -sum(log(max(p_min, l(t, monTS))) .* l(t, monTS)); % entropy
                lopt = 1 / (1+exp(H));  % entropy-maximising confidence
                lprob = min(1, max(0, bias_conf*0.5 + (1-bias_conf)*lopt));
                l(t,monTS) = l(t,monTS)*(1-lprob);
                l(t,actorTS) = lprob;
                
                % avg Q-values and predictive model from long-term memory
                if f_bay_norm
                    [avg_rl, avg_bay] = ltm_avg_norm(rl_p(:,:,t,ltmTS), ...
                                                     bay_p(:,:,t,ltmTS), ...
                                                     ctxt_p(:,t,ltmTS), ...
                                                     cur_ctxt, p_min);
                    bay_p(:,:,t,actorTS) = bias_ini*bay_prior*avg_bay + ...
                                           (1-bias_ini)*bay_p_prior;

                else
                    [avg_rl, avg_bay] = ltm_avg(rl_p(:,:,t,ltmTS), ...
                                                bay_p(:,:,:,t,ltmTS), ...
                                                ctxt_p(:,t,ltmTS), cur_ctxt, p_min);
                    bay_p(:,:,:,t,actorTS) = bias_ini*cat(3, avg_bay*bay_prior, ...
                                                             (1-avg_bay)*bay_prior) + ...
                                             (1-bias_ini)*bay_p_prior;
                end
                rl_p(:,:,t,actorTS) = bias_ini*avg_rl + (1-bias_ini)*rl_p_prior;
                
                % context likelihood
                ctxt_p(:,t,actorTS) = ctxt_prior;
                
                % add probe to monitoring buffer
                monTS = cat(2, monTS, actorTS);
            end
        end
    else
        %% currently exploring
        [lmax, imax] = max(l(t,monTS));
        emergeTS = monTS(imax);
        
        if (lmax > 0.5) && (imax ~= 1)
            % some task set has confidence > 0.5 - establish and exploit
            if emergeTS ~= actorTS
                % new task set is not probe set - remove probe
                monTS = monTS(1:(end-1));  % remove probe from buffer
                l(t,nTS) = 0;             % probe confidence = 0
                l(t,:) = l(t,:) ./ sum(l(t,:));  % re-normalise
                % move active TS to end
                monTS = [monTS(1:(imax-1)) monTS((imax+1):end) emergeTS];
            else
                % probe set becomes active task set
                ltmTS = cat(2, ltmTS, actorTS);  % register in long-term mem
                if length(monTS) >= mon_size + 2
                    % out of memory - remove oldest task set
                    l(t,monTS(2)) = 0;
                    l(t,:) = l(t,:) ./ sum(l(t,:));
                    monTS = [monTS(1) monTS(3:end)];
                end
            end
            state(t+1) = 1;
            actorTS = emergeTS;
        else
            % no task set has confidence > 0.5 - keep exploring
            state(t+1) = 2;
        end
    end
    % store monitoring buffer and tak set
    monTS_seq(t,1:length(monTS)) = monTS;
    TS_seq(t) = actorTS;
    
    
    %% ---------- (3) action selection & feedback ----------
    if f_bay_selection
        if f_bay_norm
            act_val(t,:) = bay_p(:,cur_stim,t,actorTS) / ...
                           sum(bay_p(:,cur_stim,t,actorTS));
        else
            act_val(t,:) = squeeze(bay_p(:,cur_stim,1,t,actorTS) ./ ...
                                   sum(bay_p(:,cur_stim,:,t,actorTS),3));
        end
    else
        act_val(t,:) = rl_p(:,cur_stim,t,actorTS);
    end
    if cur_act == 0
        % compute action probability by softmax
        act_prob = exp(softmax_beta*(act_val(t,:)- max(act_val(t,:))));
        act_prob = softmax_eps/nA + (1-softmax_eps)*act_prob/sum(act_prob);
        % choose action according to this probability
        cur_act = find(cumsum(act_prob) > rand(1,1), 1, 'first');
        act_seq(t) = cur_act;
    end

    cur_reward = rewards(t,cur_act);
    
    
    %% ---------- (4) updated confidence based on reward ----------
    % reward likelihood for each monitored task set
    if f_rl_prediction
        reward_lh = squeeze(rl_p(cur_act,cur_stim,t,monTS));
    else
        if f_bay_norm
            reward_lh = squeeze(...
                bsxfun(@rdivide, bay_p(cur_act,cur_stim,t,monTS),...
                                 sum(bay_p(:,cur_stim,t,monTS),1)));
        else
            reward_lh = squeeze(...
                bsxfun(@rdivide, bay_p(cur_act,cur_stim,1,t,monTS), ...
                                 sum(bay_p(cur_act,cur_stim,:,t,monTS),3)));
        end
    end
    if cur_reward ~= 1, reward_lh = 1 - reward_lh; end
    % ex-post confidence for t+1, based on reward likelihood
    mu(t+1,monTS) = reward_lh' .* l(t,monTS);
    mu(t+1,:) = mu(t+1,:) / sum(mu(t+1,:));
    
    
    %% ---------- (5) update of internal models ----------
    allTS = union(ltmTS, monTS);
    rl_p(:,:,t+1,allTS) = rl_p(:,:,t,allTS);
    if f_bay_norm, bay_p(:,:,t+1,allTS) = bay_p(:,:,t,allTS);
    else bay_p(:,:,:,t+1,allTS) = bay_p(:,:,:,t,allTS); end
    ctxt_p(:,t+1,allTS) = ctxt_p(:,t,allTS);
    
    % update RL model by standard TD learning + ev. normalization
    rl_p(cur_act,cur_stim,t+1,actorTS) = (1-rl_alpha)*rl_p(cur_act,cur_stim,t,actorTS) + ...
                                      rl_alpha*cur_reward;
    if f_rl_a_norm
        % if requested, action-normalise values
        rl_p(:,cur_stim,t+1,actorTS) = rl_p(:,cur_stim,t+1,actorTS) / ...
                                       sum(rl_p(:,cur_stim,t+1,actorTS));
    end
    if f_rl_s_norm
        % if requested, stimulus-normalise values
        non_cur_stim = 1:nS ~= cur_stim;
        rl_p(cur_act,non_cur_stim,t+1,actorTS) = ...
            (1-rl_alpha)*rl_p(cur_act,non_cur_stim,t+1,actorTS) + ...
            rl_alpha*(1-cur_reward)/(nS-1);
    end
    if rl_decay > 0
        % eventually exclude actor task set from decay
        if f_rl_no_act_decay, decayTS = setdiff(monTS(2:end), actorTS);
        else decayTS = monTS(2:end); end
        % decay towards prior
        rl_p(:,:,t+1,decayTS) = ...
            bsxfun(@plus, (1-rl_decay)*rl_p(:,:,t+1,decayTS), ...
                          rl_decay*rl_p_prior);
    end
    
    % update Bayesian model
    if f_bay_norm
        bay_p_update = repmat((1-cur_reward)/(nA-1),[nA 1]);        % r=0
        bay_p_update(cur_act) = bay_p_update(cur_act) + ...
                                cur_reward*nA/(nA-1) - 1/(nA-1);    % r=1
        bay_p(:,cur_stim,t+1,actorTS) = ...
            bay_p(:,cur_stim,t+1,actorTS) + bay_p_update;
        if bay_decay > 0
            % eventually exclude actor task set from decay
            if f_bay_no_act_decay, decayTS = setdiff(monTS(2:end), actorTS);
            else decayTS = monTS(2:end); end
            % decay towards prior
            bay_p(:,:,t+1,decayTS) = ...
                bsxfun(@plus, (1-bay_decay)*bay_p(:,:,t+1,decayTS), ...
                              bay_decay*bay_p_prior);
        end
    else
        bay_p(cur_act,cur_stim,2-cur_reward,t+1,actorTS) = ...
            bay_p(cur_act,cur_stim,2-cur_reward,t+1,actorTS) + 1;
        if bay_decay > 0
            % eventually exclude actor task set from decay
            if f_bay_no_act_decay, decayTS = setdiff(monTS(2:end), actorTS);
            else decayTS = monTS(2:end); end
            % decay towards prior
            bay_p(:,:,:,t+1,decayTS) = ...
                bsxfun(@plus, (1-bay_decay)*bay_p(:,:,:,t+1,decayTS), ...
                              bay_decay*bay_p_prior);
        end
    end
    
    % update context / task set associations
    if cur_ctxt ~= 0
        if f_ctxt_nonbay
            ctxt_p(cur_ctxt,t+1,monTS(2:end)) = ctxt_decay*ctxt_prior + ...
                (1-ctxt_decay)*(ctxt_alpha*reshape(mu(t+1,monTS(2:end)), ...
                                                   [1 size(mu(t+1,monTS(2:end)))]) + ...
                                (1-ctxt_alpha)*ctxt_p(cur_ctxt,t+1,monTS(2:end)));
        else
            ctxt_p(cur_ctxt,t+1,monTS(2:end)) = ...
                (1-ctxt_decay)*(ctxt_p(cur_ctxt,t+1,monTS(2:end))-ctxt_prior) + ...
                ctxt_prior + reshape(mu(t+1,monTS(2:end)), [1 size(mu(t+1,monTS(2:end)))]);
        end
    end
    
    prev_ctxt = cur_ctxt;
end


%% extract long-term memory from last step
ltmTS_num = length(ltmTS);
% using reshape instead of squeeze, in case regTS_num = 1
if f_bay_norm
    ltm = struct(...
        'rl_p', reshape(rl_p(:,:,nTrials+1,ltmTS), [nA nS ltmTS_num]), ...
        'bay_p', reshape(bay_p(:,:,nTrials+1,ltmTS), [nA nS ltmTS_num]), ...
        'ctxt_p', reshape(ctxt_p(:,nTrials+1,ltmTS), [nC ltmTS_num]));
else
    ltm = struct(...
        'rl_p', reshape(rl_p(:,:,nTrials+1,ltmTS), [nA nS ltmTS_num]), ...
        'bay_p', reshape(bay_p(:,:,:,nTrials+1,ltmTS), [nA nS 2 ltmTS_num]), ...
        'ctxt_p', reshape(ctxt_p(:,nTrials+1,ltmTS), [nC ltmTS_num]));
end


%% build returned data structure
if nargout >= 4
    d = struct('TS_seq', TS_seq, 'monTS_seq', monTS_seq, 'ltmTS', ltmTS, ...
               'nTS', nTS, 'act_seq', act_seq, 'rl_p', rl_p, 'l', l, ...
               'ctxt_p', ctxt_p, 'bay_p', bay_p, 'ltm', ltm, 'state', state, ...
               'Mu', mu);
end

end
