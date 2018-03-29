% Science News Articles dataset
% This version is for data that I re-tokenized with nltk

clear all;
data_dir = '/Users/emonson/Data/Fodava/EMoDocMatlabData';
data_name = 'tdm_emo1nvj_112509.mat';

cd(data_dir);
tic;

fprintf(1,'Reading training data file\n');
load(data_name);

save_dir = '/Users/emonson/Data/Fodava/EMoDocMatlabData';
% save_dir = '/Volumes/SciVis_LargeData/Fodava';
save_name = 'SN_emo1nvj_TFdiff2515_121509.mat';

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

[ii,jj,vv] = find(tdm);
vv_norm = vv./nkj(jj);

tdm_norm = sparse(ii,jj,vv_norm);

diffOpts = struct('kNN',25,'kNNAutotune',15,'Normalization','sbeltrami','Symmetrization','W+Wt');
[T,W,P,DI] = MakeDiffusion2(full(tdm_norm),0, diffOpts);

G.W = sparse(W);
G.DI = DI;

clear('T','W','P','DI');

% Compute operators on the graph
fprintf(1,'Constructing graph operators :: '); toc;
G = ConstructGraphOperators ( G );

% Compute the eigenvalues and eigenfunctions
fprintf(1,'Computing eigenvectors/values :: '); toc;
[G.EigenVecs,G.EigenVals] = eigs(G.T,min([10,size(G.W,1)]),'LM',struct('verbose',0));
G.EigenVals = diag(G.EigenVals);

% Compute the multiscale analysis
fprintf(1,'Performing diffusion wavelet analysis :: '); toc;
WaveletOpts = struct('Symm',true,'Wavelets',false,'GS','gsqr_emo','GSOptions',struct('StopN',4800));
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
save('-v7.3',save_name);

