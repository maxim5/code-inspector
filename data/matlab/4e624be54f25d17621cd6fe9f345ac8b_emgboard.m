classdef emgboard < handle
    %EMGBOARD interface for signal acquisition
    %
    %   This class handles the serial communication with the ERACLE device,
    %   offering an method which pools the board and parses the output,
    %   thus returning the signal as a matrix of integers.    
    %
    %   See also CLOSE, GETEMG, GETRAW, OPEN, PARSER, PLOTEMG
    
    %   By Luca Cavazzana for Politecnico di Milano
    %   luca.cavazzana@gmail.com
    
    properties
        port;       	% port name
        ser;            % serial handler
        
        chunk;          % incomplete sample from last acquisition
        
        dump;           % dump filename
        dumpH = -1;     % dump file handler (-1: no dump)
    end     % properties
    
    properties (Constant)
        sRate = 236;    % serial sample rate.
    end
    
    
    methods
        
        % constructor
        function EB = emgboard(port, dump)
            %EMGBOARD class constructor
            %   EB = EMGBOARD(PORT, DUMP) returns an object to handle the 
            %   emg board. PORT is the serial port name, while DUMP
            %   (optional) is the name of the file where raw data will be
            %   stored.
            
            if(nargin == 0 || isempty(port))
                if(ispc())
                    EB.port = 'COM13';  % my default port
                else
                    EB.port = '/dev/ttyUSB0';
                end
            else
                EB.port = port;
            end
            
            if(nargin > 1)
                EB.dump = dump;
            end
            
        end
        
        
    end     % methods
    
    
    
    methods (Static)
        
        function [ch, chunk] = parser(raw, chunk)
            %PARSER parses emg board output
            %
            %   [CH, CHUNK] = emgboard.PARSER(RAW, CHUNK) parses the EMG
            %   board output RAW, concatenating it with CHUNK if provided.
            %   Returns the NxC matrix (with N number of samples, C number
            %   of channels) and the tail of the last incomplete sample.
            
            ds = find(raw == 'D'); % Ds indices
            
            if( nargin>1 && ~isempty(chunk) ) % if chunk is not empty
                
                if(isempty(ds)) % not even a single complete set
                    ch = zeros(0,3);
                    chunk = [chunk, raw]; % concat new incomplete samples
                    return;
                    
                else
                    ch = zeros(size(ds,2),3);	% preallocate #Ds
                    chunk = [chunk, raw(1:ds(1))];
                    ch(1,:) = sscanf(chunk(3:end), '%d')';
                    nSets = 1;
                end
                
            else
                ch = zeros(size(ds,2)-1,3);    % preallocate #Ds
                nSets = 0;
            end
            
            
            if(length(ds)>1)    % at least a complete sample
                for ii = 2:length(ds)
                    
                    nSets = nSets+1;
                    out = sscanf(raw(ds(ii-1)+2:ds(ii)), '%d')';
                    
                    if (size(out,2)==3)
                        ch(nSets,:) = out;
                        
                    % FIXME: THIS ELSE WAS INTENDED TO MANAGE SOME FORMAT
                    % BUG IN THE BOARD STREAM... BUT HAS BEEN A WHILE SINCE
                    % THE LAST TIME IT SHOWED, SO...
                    else
                        
                        fprintf(['--------\n' ...
                            'Bad serial output format [%d,%d]\n' ...
                            '%s\n--------\n'], ...
                            ds(ii-1), ds(ii), ... boundaries of the error
                            raw(ds(ii-1):ds(ii))); % bugged substring
                        
                        ch(nSets,:) = ch(nSets-1,:);  % dunno how to manage here... info is lost anyway...
                        warning('Bad serial output format');
                        
                    end
                end     % end for
                
                chunk = raw(ds(end):end);
                
                
            elseif(length(ds) == 1)
                chunk = raw(ds(end):end);
                
            end
            
        end     % parser
        
        
    end     % static methods
end     % classdef