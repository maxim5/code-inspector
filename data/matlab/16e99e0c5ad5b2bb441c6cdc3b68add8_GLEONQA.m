% program to test wavelets on discontinuities
clear all;
warning off MATLAB:javaclasspath:duplicateEntry;

% config
StartDate = '2007-01-01'; EndDate = '2008-01-01'; % date range to clean... > 1yr for best results
    s = strtok(StartDate, '-'); e = strtok(EndDate, '-');
    span = str2num(e) - str2num(s);
IterationLimit = 50 * span; % number of allowable iterations to remove outliers before moving on
Threshold = 0.75; % Required bins
MinGapLength = 0.08; % Min year frac to be considered a gap
TimeFormat = 2; % Year fraction
Interactive = 1; % Graph data and use interactive interface
PutResults = 0; % Store the results to the clean data db

Interpolate = 0; %
Filter = 1; % 0=off, 1=wavelet, 2=MA,
TargetPeriod = 60; % minutes, for interpolation and filtering

% user chooses site, stream
%Sites = GetGLEONSites();
%[nSites c] = size(Sites);
%SiteID = input(['Choose a site (1-' num2str(nSites) ') ']);
%Streams = GetGLEONStreams(SiteID);
%[nStreams c] = size(Streams);
%StreamID = input(['Choose a value (1-' num2str(nStreams) ') ']);

ticmajor = tic; % time whole process
records = 0; % and count total # records

flag = 0;%USED FOR TESTING

% iterate from here
% all sites, all streams
% ---------------------------------------
Sites = GetGLEONSites();
[nSites c] = size(Sites);
for i1=1:nSites
    SiteID = Sites(i1, 1);
    SiteName = Sites(i1, 2);
    disp('::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::');
    disp(['::: Cleaning streams for ' SiteName{1}]);
    Streams = GetGLEONStreams(SiteID{1});
    [nStreams c] = size(Streams);
    for i2=1:nStreams
        ticminor = tic; % time this stream
        Stream = Streams(i2,1);
      if Stream{1} == 15 % FOR TESTING
        flag = 1; % FOR TESTING
        disp(':::::::::::::::::::::::::::::::::::');
        disp([':: Cleaning stream ID ' num2str(Stream{1})]);
        D = {}; % our working copy of the data
        
        % get the data
        [D.YearFrac D.Data D.QResult Removable TS] = GetGLEONData(Stream{1},StartDate,EndDate);
        if strcmp(D.QResult, 'No Data')
            disp([':: No data for stream of ID ' num2str(Stream{1}) ' in (' StartDate ' - ' EndDate ')']);
            break;
        end
        records = records + size(D.Data.OutputData); % count the data

        N = datenum(D.QResult(:,3),'yyyy-mm-dd HH:MM:SS');
        % scope locally
        YearFrac = D.YearFrac;
        Data = D.Data;

        % Plot original data
%         if Interactive
%             figure(1); clf;
%             plot(N, cell2mat(D.QResult(:,2)));
%             title('Original Data');
%         end
        
        % find gaps
        % get valid indices & segmented data
        [iValid YFs Ds] = FindGaps(YearFrac, Data, MinGapLength);
        disp([': Segmented data into ' num2str(size(Ds, 2)) ' chunk(s)']);

        % transform each segment & concat back onto single vector
        TFData = {};
        TFData.OutputData = [];
        TFYearFrac = [];
        for e=1:size(Ds,2)
            [segYF segD] = TransformData(YFs{e}, Ds{e}, Interactive, e);
            TFData.OutputData = [TFData.OutputData; segD.OutputData];
            TFYearFrac = [TFYearFrac; segYF];
        end

        
        % run range checks
        disp(': Running range checks...');
        [ newYearFrac newData ] = RangeChecks(D.YearFrac, Removable, Stream{1}, TS);
        YearFrac = newYearFrac;
        Data = newData;

        % find gaps
        % get valid indices & segmented data
        [iValid YFs Ds] = FindGaps(YearFrac, Data, MinGapLength);
        disp([': Segmented data into ' num2str(size(Ds, 2)) ' chunk(s)']);

        % transform each segment & concat back onto single vector
        TFData = {};
        TFData.OutputData = [];
        TFYearFrac = [];
        for e=1:size(Ds,2)
            [segYF segD] = TransformData(YFs{e}, Ds{e}, Interactive, e);
            TFData.OutputData = [TFData.OutputData; segD.OutputData];
            TFYearFrac = [TFYearFrac; segYF];
        end

        % clean data of outliers
        Done = 0;
        Counter = 0;
        CData = TFData;
        CYearFrac = TFYearFrac;
        CData.OutputData = CData.OutputData(diff(CData.OutputData) < 50); % remove ridiculous data immediately
%        while ~Done
%            [iBad iGood] = QAWavelet(CYearFrac,CData,Interactive,Threshold);
%            CYearFrac = CYearFrac(iGood);
%            CData.OutputData = CData.OutputData(iGood);
%            for i=1:numel(iBad) % remove the iBad from our iValid indices
%                iValid(iValid == iBad(i)) = [];
%            end
%            if isempty(iBad) % if no bad data, we're done
%                Done = 1;
%            end
%            Counter = Counter + 1;
%            if Counter > IterationLimit  % cleaning took too many iterations
%                Done = 1;
%                disp([': Cleaning hit iteration limit of ' num2str(IterationLimit)]);
%            end
%        end

        % display record count
        disp([': Time to process ' num2str(size(D.QResult, 1)) ' records: ' num2str(toc(ticminor)) ' seconds']);
        
 

        % finally, filter the original SQL result set against our clean
        % data
        Final = D.QResult(iValid, :);
        disp([': Removed ' num2str(size(D.QResult,1) - size(Final,1)) ' data points']);
        
               % graph our clean data
        N = datenum(Final(:,3),'yyyy-mm-dd HH:MM:SS');
%         if Interactive
%             figure(300); clf;
%             plot(N, cell2mat(Final(:,2)));
%             title('Filtered Data');
%         end

        % if we're storing the results to db...
        if PutResults
            disp([': Putting cleaned data to database (' num2str(size(Final,1)) ' records)']);
            PutGLEONData(Final);
        end
        
        disp([':: Records processed so far: ' num2str(records(1))]);
        disp([':: Run time so far: ' num2str(toc(ticmajor))]);
      end %FOR TESTING
    end
clearvars -except nSites c Sites records ticmajor EndDate ...
StartDate e s span IterationLimit Threshold MinGapLength ...
TimeFormat Interactive PutResults Interpolate Filter TargetPeriod
end