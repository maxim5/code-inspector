% Science News Articles dataset

% This version building term graph
% For now just transposing original (already TF normalized) term-document
% matrix and doing same normalization along document direction...

clear all;
% data_dir = '/Users/emonson/Data/Fodava/EMoDocMatlabData';
% data_name = 'tdm_emo1nvj_112509.mat';
data_dir = '/Users/emonson/Data/Fodava/MauroDocAnalysis/Data';
data_name = 'X20.mat';
cd(data_dir);
tic;

corrQuantile = 0.90;    % Global cutoff for correlations
NN = 5;                 % number of nn required in corr mtx 
                        % -- includes one self-neighbor

fprintf(1,'Reading training data file\n');
load(data_name);

save_dir = '/Users/emonson/Data/Fodava/EMoDocMatlabData';
% save_dir = '/Volumes/SciVis_LargeData/Fodava';
save_name = 'SN_mauro_DFcorr_121709.mat';

labels = dlmread('pure.classes');
fid = fopen('LabelsMeaning.txt');
articlegroups = textscan(fid,'%d = %s');
fclose(fid);

% Get rid of any "non-pure" classes (assigned 0 in X20)
%   by using filename integer converted to array index
%   Note: this takes care of missing 10976 file...
% tdm = tdm(:,classes(:,2)-10000+1);
X(classes(:,1)==0,:) = [];
tdm = X;    % NOTE: Here is where not transposing, so get term graph
classes = labels(:,[2 1]);

clear('X','labels');

% calculate TFIDF (std) normalization for word counts
nkj = sum(tdm,1)';      % how many terms in each document

[ii,jj,vv] = find(tdm);
vv_norm = vv./nkj(jj);

tdm_norm = sparse(ii,jj,vv_norm);

clear('ii','jj','vv','nkj','D','df','idf','vv_norm');

XX = mat_corr(tdm_norm);

fprintf(1,'Calculating %f quantile of correlation values',corrQuantile); toc;
qq = quantile(XX(XX>0),corrQuantile);

fprintf(1,'Filtering out low corr values :: '); toc;
YY = sparse(XX.*(XX>qq));

% Check if any rows/columns of YY are too sparse
fprintf(1,'Adjusting neighbors :: '); toc;
for ii = find(sum(YY>0,1) < NN),
    % Add elements from XX back into YY to reach required NN count
    [sC,sI] = sort(XX(:,ii),'descend');
    YY(sI(1:NN),ii) = sC(1:NN);
    YY(ii,sI(1:NN)) = sC(1:NN);
end;

numConnComp = graphconncomp(YY);
fprintf(1,'Number of connected components = %d\n', numConnComp);

% For now, break if graph not completely connected...
if (numConnComp > 1)
    break;
end;

clear('XX');

G.W = YY;

clear('YY');

% Compute operators on the graph
fprintf(1,'Constructing graph operators :: '); toc;
G = ConstructGraphOperators ( G );

% Compute the eigenvalues and eigenfunctions
fprintf(1,'Computing eigenvectors/values :: '); toc;
[G.EigenVecs,G.EigenVals] = eigs(G.T,min([10,size(G.W,1)]),'LM',struct('verbose',0));
G.EigenVals = diag(G.EigenVals);

% Compute the multiscale analysis
fprintf(1,'Performing diffusion wavelet analysis :: '); toc;
WaveletOpts = struct('Symm',true,'Wavelets',false); % ,'GS','gsqr_emo','GSOptions',struct('StopN',4800));
G.Tree = DWPTree(G.T, 20, 1e-5, WaveletOpts);

% figure; 
% plot(sum(G.Tree{1,1}.ExtBasis,1));
% hold on;
% plot(sum(G.Tree{2,1}.ExtBasis,1),'r');
% plot(sum(G.Tree{3,1}.ExtBasis,1),'g');
% plot(sum(G.Tree{4,1}.ExtBasis,1),'m');
% plot(sum(G.Tree{5,1}.ExtBasis,1),'k');
% plot(sum(G.Tree{6,1}.ExtBasis,1),'c');
% 
% figure; 
% plot(diag(G.Tree{1,1}.T{1}));
% hold on;
% plot(diag(G.Tree{2,1}.T{1}),'r');
% plot(diag(G.Tree{3,1}.T{1}),'g');
% plot(diag(G.Tree{4,1}.T{1}),'m');

fprintf(1,'Saving file :: '); toc;
cd(save_dir);
if (size(G.T,1) > 10000)
    save('-v7.3',save_name);
else
    save(save_name);
end;
