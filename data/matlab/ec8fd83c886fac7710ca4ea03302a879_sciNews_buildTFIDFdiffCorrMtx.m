% Science News Articles dataset
% This version is for data that I re-tokenized with nltk

clear all;
cd('/Users/emonson/Programming/Matlab/EMonson/Fodava/DataSets');
tic;

corrQuantile = 0.90;    % Global cutoff for correlations
NN = 5;                 % number of nn required in corr mtx 
                        % -- includes one self-neighbor

fprintf(1,'Reading training data file\n');
load('tdm_SN_singNouns.mat');

labels = dlmread('pure.classes');
classes = labels(:,[2 1]);
fid = fopen('LabelsMeaning.txt');
articlegroups = textscan(fid,'%d = %s');
fclose(fid);

% Get rid of any "non-pure" classes (assigned 0 in X20)
%   by using filename integer converted to array index
%   Note: this takes care of missing 10976 file...
tdm = tdm(:,classes(:,2)-10000+1);

% calculate TFIDF (std) normalization for word counts
nkj = sum(tdm,1)';      % how many terms in each document
D = size(tdm,2);        % number of documents
df = sum(tdm>0,2);      % number of documents each term shows up in
idf = log(D./(1+df));   % the 1+ is common to avoid divide-by-zero

[ii,jj,vv] = find(tdm);
vv_norm = (vv./nkj(jj)).*idf(ii);

tdm_norm = sparse(ii,jj,vv_norm);

clear('ii','jj','vv','nkj','D','df','idf','vv_norm');

tdm_colmean = mean(tdm_norm,1);
[ii,jj,vv] = find(tdm_norm);
vv = vv-tdm_colmean(jj)';
tdm_norm_sub = sparse(ii,jj,vv);

clear('ii','jj','vv');
tdm_colsqrtsumofsq = sqrt(sum(tdm_norm.^2,1));

fprintf(1,'Calculating cov matrix :: '); toc;
XXcov = tdm_norm_sub'*tdm_norm_sub;

fprintf(1,'Calculating product of standard deviations :: '); toc;
XXstdprod = tdm_colsqrtsumofsq'*tdm_colsqrtsumofsq;

fprintf(1,'Calculating correlation matrix :: '); toc;
XX = XXcov./XXstdprod;

% Getting a bunch of NaNs in XX...
XX(isnan(XX)) = 0;

clear('XXcov','XXstdprod','tdm_colmean','tdm_norm_sub','tdm_colsqrtsumofsq');

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

clear('XX');

G.W = YY;

clear('YY');

% Compute operators on the graph
fprintf(1,'Constructing graph operators :: '); toc;
G = ConstructGraphOperators ( G );

% Compute the eigenvalues and eigenfunctions
% fprintf(1,'Computing eigenvectors/values :: '); toc;
% [G.EigenVecs,G.EigenVals] = eigs(G.T,min([100,size(G.W,1)]),'LM',struct('verbose',0));
% G.EigenVals = diag(G.EigenVals);

% Compute the multiscale analysis
fprintf(1,'Performing diffusion wavelet analysis :: '); toc;
WaveletOpts = struct('Symm',true,'Wavelets',false); % ,'GS','gsqr_emo','GSOptions',struct('StopN',4800));
G.Tree = DWPTree(G.T, 20, 1e-5, WaveletOpts);

figure; 
plot(sum(G.Tree{1,1}.ExtBasis,1));
hold on;
plot(sum(G.Tree{2,1}.ExtBasis,1),'r');
plot(sum(G.Tree{3,1}.ExtBasis,1),'g');
plot(sum(G.Tree{4,1}.ExtBasis,1),'m');
plot(sum(G.Tree{5,1}.ExtBasis,1),'k');
plot(sum(G.Tree{6,1}.ExtBasis,1),'c');

figure; 
plot(diag(G.Tree{1,1}.T{1}));
hold on;
plot(diag(G.Tree{2,1}.T{1}),'r');
plot(diag(G.Tree{3,1}.T{1}),'g');
plot(diag(G.Tree{4,1}.T{1}),'m');

fprintf(1,'Saving file :: '); toc;
cd('/Users/emonson/Programming/Matlab/EMonson/Fodava/DataSets/20news-bydate_matlab/matlab/LargeData');
save('SN_111009_singNouns_TFIDF_WF3.mat');

