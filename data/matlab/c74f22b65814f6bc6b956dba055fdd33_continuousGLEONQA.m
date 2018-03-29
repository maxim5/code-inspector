% program to test wavelets on discontinuities
clear all;
warning off MATLAB:javaclasspath:duplicateEntry;

% config
currentValID = GetInitialValueID();
% Begin the QA
PutResults = 1; % Store the results to the clean data db
timeToWait = 30; % time before clean occurs again, in seconds
moveOn = 1;
disp(':::::::::::::::::::::::::::::::::::');
disp(':: Initializing All Stream Data ');
% Contains metadata about a stream as follows:
% Each row is a different stream.
% Col1 = streamID, Col2 = VariableID, Col3 = UnitID, Col4 = Max, Col5 =
% Min
streamDataArray = InitialStreamDataArray();

% iterate from here
% ---------------------------------------
while moveOn == 1 
    ticminor = tic; % time this stream
    disp([':: Cleaning from ID ' num2str(currentValID)]);
    
    D = {}; % our working copy of the data
    
    % get the data
    try 
        [D.DateNum D.QResult D.Data D.Streams] = GetGLEONData(currentValID);
    catch exception
        break;
    end
    
    if size(D.QResult,1) > 0
        if strcmp(D.QResult, 'No Data')
            disp([':: No data from ID ' num2str(currentValID)]);
        else
            disp([':: Cleaning ' num2str(size(D.QResult,1)) ' records']);
            
            % run range checks, updating the new array containing metadata
            % about a certain stream
            disp(': Running range checks...');
            [ newQResult newStreamDataArray] = ...
                RangeChecks(D.QResult, streamDataArray);
            disp([': Removed ' num2str(size(D.QResult,1) - size(newQResult,1)) ' data points']);
            streamDataArray = newStreamDataArray;
            D.newQResult = newQResult;
            
            % display record count
            disp([': Time to process ' num2str(size(D.QResult, 1)) ' records: ' num2str(toc(ticminor)) ' seconds']);
            
            % if we're storing the results to db...
            if PutResults
                disp([': Putting cleaned data to database (' num2str(size(D.newQResult,1)) ' records)']);
                PutGLEONData(D);
            end
            currentValID = D.QResult{size(D.QResult,1),1};
        end
    end
    clearvars -except records PutResults timeToWait moveOn streamDataArray currentValID
    pause(timeToWait); % Wait, clean again.
end