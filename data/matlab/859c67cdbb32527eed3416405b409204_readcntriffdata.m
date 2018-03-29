% Data chunk

% $Id$

function Data = readcntriffdata(fid, Data, nChans, nPnts, epochLength)

if nargin < 3 % Epoch chunk must be read before data
    fseek(fid, Data.size, 0);
else

    % Set file pointer to chunk offset
    fseek(fid, Data.offset, -1);
    
    % Epoch information
    epochArray = [0:epochLength:nPnts - 1 nPnts];
    epochLengthArray = diff(epochArray);

    % Preallocate memory
    Data.data = zeros(nChans, nPnts, 'single');

    for iEpoch = 1:length(epochArray) - 1
        for iChan = 1:nChans

            % Compression method
            comprMeth = fread(fid, 1, 'ubit4');

            % Read data
            switch comprMeth

                case 12 % 32 bit float, no compression
                    dummy = fread(fid, 1, 'ubit4');
                    Data.data(iChan, epochArray(iEpoch) + 1:epochArray(iEpoch + 1)) = ...
                        fread(fid, epochLengthArray(iEpoch), 'float32');

                otherwise
                    error('Unknown or not yet implemented compression method.')

            end

        end
    end

end
