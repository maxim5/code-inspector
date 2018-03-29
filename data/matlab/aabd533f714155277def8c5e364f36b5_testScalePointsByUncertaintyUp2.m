% clear all;
% baseName = 'X20_042709b';
% 
% cd('/Users/emonson/Programming/Matlab/EMonson/Fodava/DocumentAnalysis/Analysis');
% fprintf(1,'Loading data set\n');
% load([baseName '.mat']);

scale = 4;
initNum = 150;       % Original number of points picked
incNum = 5;     % How many each time when picking uncertain points
allowedDrop = 2;    % How far overall accuracy allowed to drop before points are thrown out.
removeAmt = 'one';  % 'set' or 'one' for removing if see large drop
maxRuns = 60;
numPts = length(classes(:,1));
reorderByUncertainty = true;   % If ~= 1, then using natural order
% 'difference', 'norm difference', 'filled', 'entropy', 'random',
% 'correlation'
uncertMeasure = 'entropy';
filledThreshold = 0.8;

overlap = true;
numReplicates = 50; % Not used if overlap=false

leaveOutFracOfTotal = false;    
numLeaveOut = 20;   % Not used if leaveOutFracOfTotal = true;
leaveOutFrac = 0.1;

extIdxs = G.Tree{scale,1}.ExtIdxs;

runs = floor((length(extIdxs)-initNum)/incNum);
if (runs > maxRuns), runs = maxRuns; end;

scaleIdxs = [];
scaleCats = [];
labeledIdxs = [];
sc = zeros([runs 1]);
ptCount = zeros([runs 1]);
un = zeros([runs 1]);
pun = zeros([runs 1]);
gc = zeros([runs 1]);
exitAll = 0;
prevGuesses = zeros(size(classes(:,1)));

for ii = 1:runs,
    
    if (ii==1), num = initNum;
    else num = incNum;
    end;
    
    done = 0;
    
    extIdxsTmp = extIdxs;
    deleteCount = 0;
    while (done == 0),
        % Pick points from list of multiscale indices
        % fprintf(1,'Choosing scale %d points (num: %d) -- ', scale, num);
        try
            [pickedIdxs,pickedCats] = docChooseScalePoints(classes(:,1),extIdxsTmp,'number',num);
        catch ME1
            fprintf(1,'picking error');
            exitAll = 1;
            break;
        end;

        % Append newly picked points onto old list
        scaleIdxs = cat(2,scaleIdxs,pickedIdxs);
        scaleCats = cat(2,scaleCats,pickedCats);
        if(ii==1),
            labeledIdxsTmp = scaleIdxs;
        else
            % Want to keep track of which idxs have been labeled in earlier
            % rounds, so if they get reused, we don't count them again
            labeledIdxsTmp = union(labeledIdxsTmp,pickedIdxs);
        end;

        % fprintf(1,'Propagating scale labels\n');
        if (leaveOutFracOfTotal), numLeaveOut = floor(length(scaleIdxs)*leaveOutFrac); end;
        [scaleLabelArray, optCorrect] = propagateLabels3(scaleIdxs,scaleCats,G.P,20,overlap,numLeaveOut,numReplicates);
    
        % Find which are really correct
        [junk,scaleCatOut] = max(scaleLabelArray,[],2);
        scaleCorrect = (scaleCatOut == classes(:,1));
        if((ii > 1) && (sum(scaleCorrect) > sc(ii-1)-allowedDrop)),
            fprintf(1,'%d used but deleted\n', deleteCount);
            done = 1;
        elseif (ii == 1),
            done = 1;
        else
            % If recent answer not better, remove those points from lists
            % and redo propagation
            if(strcmpi(removeAmt,'set')), delNum = num;
            else delNum = 1;
            end; 
            deleteCount = deleteCount + delNum;
            scaleIdxs = scaleIdxs(1:end-delNum);
            scaleCats = scaleCats(1:end-delNum);
            extIdxsTmp = extIdxsTmp(delNum+1:end);
        end;
    end;
    if(exitAll), break; end;
    newIdxs = setdiff(labeledIdxsTmp,labeledIdxs);
    labeledIdxs = labeledIdxsTmp;
    fprintf(1,'\nscale correct: %d / %d (%4.3f)\n', sum(scaleCorrect), length(scaleCorrect), sum(scaleCorrect)/length(scaleCorrect));
    % Record original number correct
    sc(ii) = sum(scaleCorrect);
    if (ii==1), ptCount(ii) = initNum;
    else ptCount(ii) = ptCount(ii-1) + length(newIdxs);
    end;

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
            neighborIdxs = setdiff(find(G.P(:,jj)),jj);
            inNeighborCorrs = corr(normLabelsArray(jj,:)',normLabelsArray(neighborIdxs,:)');
            % Weight them by the incoming edge weights
            inNeighborWeights = G.P(neighborIdxs,jj);
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
    % Reorder remaining extIdxs and cats according to uncertainty
    if(reorderByUncertainty),
        extIdxs = extIdxs(II);
    end;
    
    gc(ii) = sum(prevGuesses ~= scaleCatOut);
    prevGuesses = scaleCatOut;

end;

figure; 
subplot(2,2,1);
plot(ptCount,sc,'.-');
line([20 100],[700 780],'Color','k');     % science articles
% line([150 450],[1500 1800],'Color','k');  % 20 newsgroups
% xlim([ptCount(1) ptCount(end)]);                    % science articles
titleStr = sprintf('scale %d, initNum %d, inc %d, %s uncert',4,initNum,incNum,uncertMeasure);
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
gcTmp = gcTmp./20.0;   % Scaling factor
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
plot(ptCount,1500+cumsum(gcTmp),'.-','Color',[0.5 0.5 0.5]);
plot(ptCount,pun,'k.-');
% xlim([ptCount(1) ptCount(end)]);                    % science articles
% end

