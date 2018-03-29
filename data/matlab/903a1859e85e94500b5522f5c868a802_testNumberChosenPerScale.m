% Science Articles
% clear all;
% baseName = 'X20_042709b';
% 
% cd('/Users/emonson/Programming/Matlab/EMonson/Fodava/DocumentAnalysis/Analysis');
% fprintf(1,'Loading data set\n');
%     load([baseName '.mat']);

% 20 newsgroups TFIDF nltk tokenize
clear all;
% save_dir = '/Volumes/SciVis_LargeData/Fodava';
save_dir = '/Users/emonson/Data/Fodava/EMoDocMatlabData';
save_name = 'n20_set2train_TFcorr_121009.mat';
% n20_sub1train_TFcorr_111809
% n20_sub2train_TFcorr_111809
% n20_sub3train_TFcorr_111809
% n20_sub4train_TFcorr_111809
% save_name = 'n20_set5train_TFcorr_112409.mat';

cd(save_dir);
fprintf(1,'Loading data set\n');
load(save_name);

% NOTE: Only for further trials after fitting
% G = G2;

% 'difference', 'norm difference', 'filled', 'entropy', 'random',
% 'correlation'
uncertMeasure = 'entropy';
filledThreshold = 0.8;
maxProps = 20;

overlap = true;
numReplicates = 30; % Not used if overlap=false

leaveOutFracOfTotal = false;    
numLeaveOut = 20;   % Not used if leaveOutFracOfTotal = true;
leaveOutFrac = 0.2;

Wdeg = sum(G.W,1);

numPts = length(classes(:,1));
scaleIdx = 1;
for scale = 2:4,
    extIdxs = G.Tree{scale,1}.ExtIdxs(2:end);
    % TEST: Remove very high degree ExtIdxs points
%     extDeg = Wdeg(extIdxs);
%     extQ = quantile(full(Wdeg),0.75);
%     extIdxs = extIdxs(extDeg < extQ);
%     fprintf(1,'Leaving out %d extIdxs points becuase of high degree\n',sum(full(extDeg) < extQ));
    % TEST: randomizing extIdxs order
    %  extIdxs = extIdxs(randperm(length(extIdxs)));
    % TEST: Pick random points from a randomized index list rather than random
    %  set each time...
    % rextIdxs = randperm(numPts);
    numIdx = 1;
    for num = 40:10:500,
        if (num > length(extIdxs)), break; end;
        numLog(numIdx,scaleIdx) = num;

        fprintf(1,'Choosing scale %d points (%d dups) -- ', scale, num);
        [scaleIdxs,scaleCats] = docChooseScalePoints(classes(:,1),extIdxs,'number',num);
        fprintf(1,'Propagating scale labels\n');
        if (leaveOutFracOfTotal), numLeaveOut = floor(num*leaveOutFrac); end;
        [scaleLabelArray, optCorrect] = propagateLabels3(scaleIdxs,scaleCats,G.P,maxProps,overlap,numLeaveOut,numReplicates);
        [junk,scaleCatOut] = max(scaleLabelArray,[],2);
        fprintf(1,'Number all zero propagated results: %d\n', length(find(junk<1e-20)) );

        unlabeledIdxs = setdiff(1:length(classes(:,1)),scaleIdxs);
        scaleCorrect = (scaleCatOut == classes(:,1));
        % Count all zeros as wrong
        scaleCorrect(sum(scaleLabelArray,2)<1e-20) = 0;
        % For accuracy, only count unlabeled vertices, and record as
        % fraction since that number will be constantly changing
        fracCorrect = sum(scaleCorrect(unlabeledIdxs))/length(unlabeledIdxs);
        sc(numIdx,scaleIdx) = fracCorrect;
        
        if strcmpi(uncertMeasure,'norm difference'),
            labelsSorted = sort(scaleLabelArray,2,'descend');
            labelsSorted = labelsSorted./repmat(sum(labelsSorted,2),[1 size(labelsSorted,2)]);
            uncertainty = 1+diff(labelsSorted(:,1:2),1,2);
        elseif strcmpi(uncertMeasure,'difference'),
            labelsSorted = sort(scaleLabelArray,2,'descend');
            uncertainty = 1+diff(labelsSorted(:,1:2),1,2);
        elseif strcmpi(uncertMeasure,'entropy'),
            normLabels = scaleLabelArray./repmat(sum(scaleLabelArray,2),[1 size(scaleLabelArray,2)]); 
            normLabels(isnan(normLabels)) = 0;
            tmpLog = log10(normLabels);
            tmpLog(isinf(tmpLog)) = 0;
            uncertainty = sum( -1.*normLabels.*tmpLog ,2);
        elseif strcmpi(uncertMeasure,'filled'),
            labelsSorted = sort(scaleLabelArray,2,'descend');
            labelsSorted = labelsSorted./repmat(sum(labelsSorted,2),[1 size(labelsSorted,2)]);
            uncertainty = zeros([size(labelsSorted,1) 1]);
            for jj = 1:length(uncertainty),
                % interp1 needs unique values, so adding a little noise before cumsum...
                tmpRow = cumsum( labelsSorted(jj,:) + 0.00001.*rand([1 size(labelsSorted,2)]), 2 );
                uncertainty(jj) = interp1( [0 tmpRow], [0:size(labelsSorted,2)], filledThreshold );
            end;
        elseif strcmpi(uncertMeasure,'correlation'),
            normLabels = scaleLabelArray./repmat(sum(scaleLabelArray,2),[1 size(scaleLabelArray,2)]);
            uncertainty = zeros([size(labelsSorted,1) 1]);
            for jj = 1:length(uncertainty),
                inNeighborCorrs = corr(normLabelArray(jj,:)',normLabelArray(setdiff(find(G.P(:,jj)),jj),:)');
                uncertainty(jj) = 1-mean(inNeighborCorrs);
            end;
        elseif strcmpi(uncertMeasure,'random'),
            uncertainty = rand([size(scaleLabelArray,1) 1]);
        else
            uncertainty = zeros([size(scaleLabelArray,1) 1]);
        end;
        
        un(numIdx,scaleIdx) = mean(uncertainty);
        pc(numIdx,scaleIdx) = optCorrect; % *numPts;
        fprintf(1,'scale correct: %d / %d (%4.3f)\n', sum(scaleCorrect(unlabeledIdxs)),length(unlabeledIdxs), fracCorrect);

        fprintf(1,'Choosing random points (%d dups) -- ', num);
        % Standard: Choose new random set each time
        [randIdxs,randCats] = docChooseRandomPoints(classes(:,1),'number',num);
        
        % TEST: Pick random points from a randomized index list rather than random
        %  set each time...
        % [randIdxs,randCats] = docChooseScalePoints(classes(:,1),rextIdxs,'number',num);

        fprintf(1,'Propagating random labels\n');
        [randLabelArray, roptCorrect] = propagateLabels3(randIdxs,randCats,G.P,maxProps,overlap,numLeaveOut,numReplicates);
        [junk,randCatOut] = max(randLabelArray,[],2);
        fprintf(1,'Number all zero propagated results: %d\n', length(find(junk==0.0)) );

        randCorrect = (randCatOut == classes(:,1));
        % For accuracy, only count unlabeled vertices, and record as
        % fraction since that number will be constantly changing
        runlabeledIdxs = setdiff(1:length(classes(:,1)),randIdxs);
        randFracCorrect = sum(randCorrect(runlabeledIdxs))/length(runlabeledIdxs);
        rc(numIdx,scaleIdx) = randFracCorrect;
        rpc(numIdx,scaleIdx) = roptCorrect; %*numPts;
        fprintf(1,'random correct: %d / %d (%4.3f)\n\n', sum(randCorrect(runlabeledIdxs)), length(runlabeledIdxs), randFracCorrect);
        
        numIdx = numIdx + 1;
    end;
    scaleIdx = scaleIdx + 1;
end;

figure; 
% subplot(2,1,1);
plot(numLog,sc,'.-','Color',[0.2 0.2 0.5],'LineWidth',2,'MarkerSize',20);
% title('scale');
% subplot(2,2,2);
hold on;
plot(numLog,rc,'.-','Color',[0.5 0.2 0.2],'LineWidth',2,'MarkerSize',20);
% title('random');
% subplot(2,1,2);
plot(numLog,pc,'-','Color',[0.7 0.7 0.9]);
if (overlap),
    overlapString = sprintf('Overlapping leaveOuts, numReps=%d',numReplicates);
else
    overlapString = 'Not overlapping leaveOuts';
end;
if (leaveOutFracOfTotal),
    pertotalString = sprintf('Leave out frac=%3.2f',leaveOutFrac);
else
    pertotalString = sprintf('Num left out=%d',numLeaveOut);
end;
titleString = sprintf('%s\n%s\n%s\n%s', regexprep(save_name, '_', '\\_'), 'Propagation cross-valid correct (mean)', overlapString, pertotalString);
title(titleString);
% hold on;
% subplot(2,2,4);
plot(numLog,rpc,'-','Color',[0.9 0.7 0.7]);
% title('Cross-validation on random picks');
plot(numLog,un,'o-','Color',[0.7 0.9 0.7]);
% title([uncertMeasure ' uncertainty measure (mean)']);