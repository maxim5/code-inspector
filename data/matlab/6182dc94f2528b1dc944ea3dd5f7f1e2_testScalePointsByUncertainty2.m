% function [sc, un, ptCount] = testScalePointsByUncertainty2(P,extIdxs,classes,initNum,incNum,maxRuns)
% clear all;
% baseName = 'X20_042709b';
% 
% cd('/Users/emonson/Programming/Matlab/EMonson/Fodava/DocumentAnalysis/Analysis');
% fprintf(1,'Loading data set\n');
% load([baseName '.mat']);

% 20 newsgroups TFIDF nltk tokenize
clear all;
save_dir = '/Users/emonson/Data/Fodava/EMoDocMatlabData';
save_name = 'n20_sub2train_TFcorr_111809.mat';

cd(save_dir);
fprintf(1,'Loading data set\n');
load(save_name);

scale = 4;
initNum = 20;       % Original number of points picked
hillClimbingStart = 21;
incNum = 1 ;     % How many each time when picking uncertain points
maxRuns = 150;
numPts = length(classes(:,1));
% 'difference', 'norm difference', 'filled', 'entropy', 'random',
% 'correlation'
uncertMeasure = 'entropy';
filledThreshold = 0.8;

stats_plot_on = false;

overlap = true;
numReplicates = 30; % Not used if overlap=false

leaveOutFracOfTotal = false;    
numLeaveOut = 10;   % Not used if leaveOutFracOfTotal = true;
leaveOutFrac = 0.1;

% Use real ExtIdxs points for labeling
extIdxs = G.Tree{scale,1}.ExtIdxs;

% Try using random points for labeling
% tmp = randperm(length(classes(:,1)));
% extIdxs = tmp(1:length(G.Tree{scale,1}.ExtIdxs));

cats = classes(:,1);
P = G.P;

runs = floor((length(extIdxs)-initNum)/incNum);
if (runs > maxRuns), runs = maxRuns; end;

scaleIdxs = [];
scaleCats = [];
sc = zeros([runs 1]);
ptCount = zeros([runs 1]);
un = zeros([runs 1]);
pun = zeros([runs 1]);
gc = zeros([runs 1]);
prevGuesses = zeros(size(classes(:,1)));
prevUncert = zeros(size(classes(:,1)));
prevCorrect = zeros(size(classes(:,1)));

for ii = 1:runs,
    
    if (ii==1), num = initNum;
    else num = incNum;
    end;
    
    % Pick points from list of multiscale indices
    % fprintf(1,'Choosing scale %d points (num: %d) -- ', scale, num);
    [pickedIdxs,pickedCats] = docChooseScalePoints(classes(:,1),extIdxs,'number',num);
    
    % Append newly picked points onto old list
    scaleIdxs = cat(2,scaleIdxs,pickedIdxs);
    scaleCats = cat(2,scaleCats,pickedCats);
    
    % Propagate points
    % fprintf(1,'Propagating scale labels\n');
    if (leaveOutFracOfTotal), numLeaveOut = floor(length(scaleIdxs)*leaveOutFrac); end;
    [scaleLabelArray, optCorrect] = propagateLabels3(scaleIdxs,scaleCats,P,20,overlap,numLeaveOut,numReplicates);
    
    % Find which are really correct
    [junk,scaleCatOut] = max(scaleLabelArray,[],2);
    scaleCorrect = (scaleCatOut == classes(:,1));
    % Count all zeros as wrong
    scaleCorrect(sum(scaleLabelArray,2)<1e-20) = 0;
    % Record original number correct
    sc(ii) = sum(scaleCorrect);
    if (ii==1), ptCount(ii) = initNum;
    else ptCount(ii) = ptCount(ii-1) + incNum;
    end;
     fprintf(1,'\nPts: %d || scale correct: %d / %d (%4.3f)\n\n', ptCount(ii), sc(ii), length(scaleCorrect), sum(scaleCorrect)/length(scaleCorrect));

    % Take original points out of extIdxs, but make sure they're in
    % original order by sorting indices and applying after setdiff()
    [newExtIdxs,II] = setdiff(extIdxs,pickedIdxs);
    extIdxs = extIdxs(sort(II));

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
        normLabelsArray = scaleLabelArray./repmat(sum(scaleLabelArray,2),[1 size(scaleLabelArray,2)]);
        uncertainty = zeros([size(normLabelsArray,1) 1]);
        for jj = 1:length(uncertainty),
            % Calculate (incoming) neighbor correlations
            neighborIdxs = setdiff(find(P(:,jj)),jj);
            inNeighborCorrs = corr(normLabelsArray(jj,:)',normLabelsArray(neighborIdxs,:)');
            % Weight them by the incoming edge weights
            inNeighborWeights = P(neighborIdxs,jj);
            % But first normalize incoming edge weights
            inNeighborWeights = inNeighborWeights./sum(inNeighborWeights);
            % Because of weighting, mean now calculated by sum()
            uncertainty(jj) = 1-sum(inNeighborCorrs'.*inNeighborWeights);
        end;
    elseif strcmpi(uncertMeasure,'random'),
        uncertainty = rand([size(scaleLabelArray,1) 1]);
    else
        uncertainty = zeros([size(scaleLabelArray,1) 1]);
    end;
    
    un(ii) = mean(uncertainty);
    pun(ii) = optCorrect*numPts;
    % Keep only uncertainty values for remaining extIdxs points
    extUncert = uncertainty(extIdxs);
    % Sort these
    [junk,II] = sort(extUncert,1,'descend');
    
    % Reorder remaining extIdxs according to uncertainty if past threshold
    if (length(scaleIdxs) < hillClimbingStart),
        extIdxs = extIdxs;
    else
        extIdxs = extIdxs(II);
    end;
    
    if (ii == 1),
        prevGuesses = scaleCatOut;
        prevUncert = uncertainty;
        prevCorrect = scaleCorrect;
    end;
    
    gc(ii) = sum(prevGuesses ~= scaleCatOut);
    
%     % Plot changes in both uncertainty and color
%     figure(2001);
%     subplot(1,2,1);
%     scatter(pts(:,1),pts(:,2),150,((prevUncert >= 0.6)-(prevUncert < 0.6)),'filled');
%     colormap(map2);
%     axis image;
%     hold on;
%     scatter(pts(:,1),pts(:,2),90,((uncertainty-prevUncert >= 0.1)-(uncertainty-prevUncert <= -0.1)),'filled');
%     scatter(pts(:,1),pts(:,2),20,(prevGuesses ~= scaleCatOut),'filled');
%     plot(pts(scaleIdxs,1),pts(scaleIdxs,2),'ko','Color',[0.3 0.3 0.3],'MarkerSize',12,'LineWidth',1.5);
%     plot(pts(pickedIdxs,1),pts(pickedIdxs,2),'ko','Color',[0 0 0],'MarkerSize',12,'LineWidth',1.5);
%     hold off;
%     title('Largest: Prev uncert :: Large = Uncertainty change :: Small = Color changed');
%     subplot(1,2,2);
%     scatter(pts(:,1),pts(:,2),90,(scaleCorrect-prevCorrect),'filled');    
%     colormap(map2);
%     axis image;
%     hold on;
%     scatter(pts(:,1),pts(:,2),20,(scaleCorrect),'filled');
%     plot(pts(:,1),pts(:,2),'ko','Color',[0.6 0.6 0.6],'MarkerSize',10,'LineWidth',1);
%     plot(pts(scaleIdxs,1),pts(scaleIdxs,2),'ko','Color',[0.3 0.3 0.3],'MarkerSize',12,'LineWidth',1.5);
%     plot(pts(pickedIdxs,1),pts(pickedIdxs,2),'ko','Color',[0 0 0],'MarkerSize',12,'LineWidth',1.5);
%     hold off;
%     title('Large = Becoming correct or incorrect :: Small = correct or incorrect');
    
    % 2D stats plot
    if (stats_plot_on),
        figure(2002);
        stayedSame = ((scaleCorrect-prevCorrect) == 0);
        wentRight = ((scaleCorrect-prevCorrect) > 0);
        wentWrong = ((scaleCorrect-prevCorrect) < 0);
        deltaUncert = uncertainty-prevUncert;
        colorChanged = (prevGuesses ~= scaleCatOut);
        plot(prevUncert(scaleCorrect), uncertainty(scaleCorrect)-prevUncert(scaleCorrect),'k.','Color',[0.5 0.5 0.5],'MarkerSize',6);
        hold on;
        line([0 1],[0 0],'LineStyle',':','Color',[0 0 0]);
        line([mean(prevUncert) mean(prevUncert)],[min(deltaUncert) max(deltaUncert)],'LineStyle','--','Color',[0.8 0.8 0.8]);
        line([mean(uncertainty) mean(uncertainty)],[min(deltaUncert) max(deltaUncert)],'LineStyle','-','Color',[0.8 0.8 0.8]);
        line([min(prevUncert) max(prevUncert)],[mean(deltaUncert) mean(deltaUncert)],'LineStyle','-','Color',[0.8 0.8 0.8]);
        plot(prevUncert(colorChanged), uncertainty(colorChanged)-prevUncert(colorChanged),'ko','MarkerSize',10);
        plot(prevUncert(stayedSame), uncertainty(stayedSame)-prevUncert(stayedSame),'k.','MarkerSize',4);
        plot(prevUncert(wentRight), uncertainty(wentRight)-prevUncert(wentRight),'r.','MarkerSize',20);
        plot(prevUncert(wentWrong), uncertainty(wentWrong)-prevUncert(wentWrong),'b.','MarkerSize',20);
        hold off;
        maxDelta = max(abs(deltaUncert));
        if(maxDelta < 1e-6), maxDelta = 0.1; end;
        axis([min(prevUncert) max(prevUncert) -maxDelta maxDelta]);
        xlabel(strcat('Previous uncertainty (',uncertMeasure,' measure)'));
        ylabel(strcat('Change in uncertainty (',uncertMeasure,' measure)'));
        legend('correct','zero delta uncert','prev mean uncert','mean uncert','mean delta uncert','color changed','no change acc','became right','became wrong');
        changeStats = sprintf('delta acc = %d\nbecoming right = %d\nbecoming wrong = %d\ncolor changes = %d\nmean delta uncert = %4.3f',(sum(scaleCorrect)-sum(prevCorrect)),sum(wentRight),sum(wentWrong),sum(colorChanged),mean(deltaUncert));
        text(0.25,0.8,changeStats,'HorizontalAlignment','right');
        title([int2str(length(scaleIdxs)) ' picked']);
        drawnow;
    end;
    
    prevGuesses = scaleCatOut;
    prevUncert = uncertainty;
    prevCorrect = scaleCorrect;

end;

figure; 
subplot(2,2,1);
plot(ptCount,sc,'.-');
line([20 100],[700 780],'Color','k');     % science articles
% line([150 450],[1500 1800],'Color','k');  % 20 newsgroups
% xlim([ptCount(1) ptCount(end)]);                    % science articles
titleStr = sprintf('scale %d, initNum %d, inc %d, %s uncert',scale,initNum,incNum,uncertMeasure);
title(titleStr);

subplot(2,2,2);
plot(ptCount,pun,'k.-');
% xlim([ptCount(1) ptCount(end)]);                    % science articles
% axis([20 90 0.2 0.8]);
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
titleString = sprintf('%s\n%s\n%s','Propagation cross-valid correct (mean)', overlapString, pertotalString);
title(titleString);

subplot(2,2,3);
plot(ptCount,un,'r.-');
hold on;
plot(ptCount(2:end),gc(2:end)./(2*max(gc(2:end))),'.-','Color',[0.5 0.5 0.5]);
plot(ptCount,sc./numPts,'b.-');
% xlim([ptCount(1) ptCount(end)]);                    % science articles

subplot(2,2,4);
gcTmp = gc;
gcTmp(1) = 0;
diffTmp = diff(un);
gcTmp(2:end) = gcTmp(2:end).*(-1.*(diffTmp>0.01)+(diffTmp<=0.01)); % put negatives back in
% gcTmp(2:end) = gcTmp(2:end).*(-1.*(diffTmp<-0.01)+(diffTmp>=-0.01)); % put negatives back in
gcTmp = gcTmp./4.0;   % Scaling factor
% gcTmp = gcTmp./(5.0*exp(-ptCount/180));   % Scaling factor
% gcTmp = gcTmp./(0.05*ptCount + 2.0);   % Scaling factor
plot(ptCount,gcTmp,'.-','Color',[0.5 0.5 0.5]);
hold on;
plot(ptCount(2:end),diff(sc),'b.-');
plot(ptCount,un.*50,'r.-');
maxVal = 2.0*max(abs(cat(1,diff(sc),gcTmp)));
% xlim([ptCount(1) ptCount(end)]);                    % science articles
% ylim([-maxVal maxVal]);

subplot(2,2,1);
hold on;
plot(ptCount,sc(1)+cumsum(gcTmp),'.-','Color',[0.5 0.5 0.5]);
plot(ptCount,pun,'k.-');
% xlim([ptCount(1) ptCount(end)]);                    % science articles
% end
