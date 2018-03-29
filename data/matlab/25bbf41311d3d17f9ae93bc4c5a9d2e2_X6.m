% MATLAB wrapper for the X6 driver.
%
% Usage notes: The channelizer creates multiple data streams per physical
% input channel. These are indexed by a 3-parameter label (a,b,c), where a
% is the 1-indexed physical channel, b is the 0-indexed virtual channel
% (b=0 is the raw stream, b>1 are demodulated streams, and c indicates
% demodulated (c=0) or demodulated and integrated (c = 1).

% Original authors: Blake Johnson and Colm Ryan
% Date: August 25, 2014

% Copyright 2014 Raytheon BBN Technologies
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.

classdef X6 < hgsetget

    properties
        samplingRate = 1e9;
        triggerSource
        reference
        deviceID = 0;
        enabledStreams = {} %keep track of enabled streams so we can transfer them all
        dataTimer
        nbrSegments
        recordLength
        nbrWaveforms
        nbrRoundRobins
    end

    properties(Constant)
        LIBRARY_PATH = '../../build/';
        DECIM_FACTOR = 4;
        DSP_WB_OFFSET = [hex2dec('2000'), hex2dec('2100')];
        SPI_ADDRS = containers.Map({'adc0', 'adc1', 'dac0', 'dac1'}, {16, 18, 144, 146});
    end

    events
        DataReady
    end

    methods
        function obj = X6()
            X6.load_library();
            obj.set_debug_level(4);
        end

        function x6_call(obj, func, varargin)
            % Make void call to the library
            status = calllib('libx6adc', func, obj.deviceID, varargin{:});
            X6.check_status(status);
        end

        function val = x6_getter(obj, func, varargin)
            % Make a getter call to the library passing and returning a pointer
            [status, val] = calllib('libx6adc', func, obj.deviceID, varargin{:}, 0);
            X6.check_status(status);
        end

        function val = x6_channel_getter(obj, func, varargin)
            % Specialized getter for API's that also take a ChannelTuple pointer
            [status, ~, val] = calllib('libx6adc', func, obj.deviceID, varargin{:}, 0);
            X6.check_status(status);
        end

        function connect(obj, id)
            if ischar(id)
                id = str2double(id);
            end
            obj.deviceID = id;
            x6_call(obj, 'connect_x6');
            % temporary fix for stream enable register
            obj.write_register(X6.DSP_WB_OFFSET(1), 15, 0);
            obj.write_register(X6.DSP_WB_OFFSET(2), 15, 0);
        end

        function disconnect(obj)
            x6_call(obj, 'disconnect_x6');
        end

        function delete(obj)
            try
                disconnect(obj);
            catch
            end
            if ~isempty(obj.dataTimer)
                delete(obj.dataTimer);
            end
        end

        function init(obj)
            x6_call(obj, 'initX6');
        end

        function val = get.samplingRate(obj)
            val = x6_getter(obj, 'get_sampleRate');
        end

        function set.samplingRate(obj, rate)
            x6_call(obj, 'set_sampleRate', rate);
        end

        function val = get.triggerSource(obj)
            val = x6_getter(obj, 'get_trigger_source');
        end

        function set.triggerSource(obj, source)
            x6_call(obj, 'set_trigger_source', source)
        end

        function set.reference(obj, ref)
            x6_call(obj, 'set_reference', ref);
        end

        function val = get.reference(obj)
            val = x6_getter(obj, 'get_reference');
        end

        function enable_stream(obj, a, b, c)
            x6_call(obj, 'enable_stream', a, b, c)
            obj.enabledStreams{end+1} = [a,b,c];
        end

        function disable_stream(obj, a, b, c)
            x6_call(obj, 'disable_stream', a, b, c);
            % remove the stream from the enabledStreams list
            idx = find(cellfun(@(x) isequal(x, [a,b,c])));
            if ~isempty(idx)
                obj.enabledStreams(idx) = [];
            end
        end

        function set_averager_settings(obj, recordLength, nbrSegments, waveforms, roundRobins)
            x6_call(obj, 'set_averager_settings', recordLength, nbrSegments, waveforms, roundRobins);
            obj.recordLength = recordLength;
            obj.nbrSegments = nbrSegments;
            obj.nbrWaveforms = waveforms;
            obj.nbrRoundRobins = roundRobins;
        end

        function acquire(obj)
            x6_call(obj, 'acquire');
            %Since we cannot easily pass callbacks to the C library to fire
            %on new data arriving we resort to polling on a timer
            %We also fire on stopping to catch any last data
            function do_poll(~,~)
                if (x6_getter(obj, 'get_has_new_data'))
                    notify(obj, 'DataReady');
                end
            end
            obj.dataTimer = timer('TimerFcn', @do_poll, 'StopFcn', @(~,~) notify(obj, 'DataReady'), 'Period', 0.1, 'ExecutionMode', 'fixedSpacing');
            start(obj.dataTimer);
        end

        function val = wait_for_acquisition(obj, timeout)
            t = tic;
            val = -1;
            while toc(t) < timeout
                if ~x6_getter(obj, 'get_is_running')
                    val = 0;
                    break
                end
                pause(0.1)
            end
            stop(obj);
        end

        function stop(obj)
            x6_call(obj, 'stop');
            if ~isempty(obj.dataTimer)
                stop(obj.dataTimer);
                delete(obj.dataTimer);
                obj.dataTimer = [];
            end
        end

        function data = transfer_waveform(obj, channel)
            % returns a structure of streams associated with the given
            % channel
            data = struct();
            for stream = obj.enabledStreams
                if stream{1}(1) == channel
                    s = struct('a', stream{1}(1), 'b', stream{1}(2), 'c', stream{1}(3));
                    data.(['s' sprintf('%d',stream{1})]) = obj.transfer_stream(s);
                end
            end
        end

        function wf = transfer_stream(obj, channels)
            % expects channels to be a vector of structs of the form:
            % struct('a', X, 'b', Y, 'c', Z)
            % when passed a single channel struct, returns the corresponding waveform
            % when passed multiple channels, returns the correlation of the channels
            bufSize = x6_channel_getter(obj, 'get_buffer_size', channels, length(channels));
            wfPtr = libpointer('doublePtr', zeros(bufSize, 1, 'double'));
            x6_call(obj, 'transfer_waveform', channels, length(channels), wfPtr, bufSize);

            if channels(1).b == 0 % physical channel
                wf = wfPtr.Value;
            else
                wf = wfPtr.Value(1:2:end) + 1i*wfPtr.Value(2:2:end);
            end
            if channels(1).c == 0 % non-results streams should be reshaped
                wf = reshape(wf, length(wf)/obj.nbrSegments, obj.nbrSegments);
            end
        end

        function wf = transfer_stream_variance(obj, channels)
            % expects channels to be a vector of structs of the form:
            % struct('a', X, 'b', Y, 'c', Z)
            bufSize = x6_channel_getter(obj, 'get_variance_buffer_size', channels, length(channels));
            wfPtr = libpointer('doublePtr', zeros(bufSize, 1, 'double'));
            x6_call(obj, 'transfer_variance', channels, length(channels), wfPtr, bufSize);

            wf = struct('real', [], 'imag', [], 'prod', []);
            if channels(1).b == 0 % physical channel
                wf.real = wfPtr.Value;
                wf.imag = zeros(length(wfPtr.Value), 1);
                wf.prod = zeros(length(wfPtr.Value), 1);
            else
                wf.real = wfPtr.Value(1:3:end);
                wf.imag = wfPtr.Value(2:3:end);
                wf.prod = wfPtr.Value(3:3:end);
            end
            if channels(1).c == 0 % non-results streams should be reshaped
                wf.real = reshape(wf.real, length(wf.real)/obj.nbrSegments, obj.nbrSegments);
                wf.imag = reshape(wf.imag, length(wf.imag)/obj.nbrSegments, obj.nbrSegments);
                wf.prod = reshape(wf.prod, length(wf.prod)/obj.nbrSegments, obj.nbrSegments);
            end
        end

        function write_register(obj, addr, offset, data)
            x6_call(obj, 'write_register', addr, offset, data);
        end

        function val = read_register(obj, addr, offset)
            val = x6_getter(obj, 'read_register', addr, offset);
        end

        function write_spi(obj, chip, addr, data)
           %read flag is low so just address
           val = bitshift(addr, 16) + data;
           obj.write_register(hex2dec('0800'), obj.SPI_ADDRS(chip), val);
        end

        function val = read_spi(obj, chip, addr)
           %read flag is high
           val = bitshift(1, 28) + bitshift(addr, 16);
           obj.write_register(hex2dec('0800'), obj.SPI_ADDRS(chip), val);
           val = int32(obj.read_register(hex2dec('0800'), obj.SPI_ADDRS(chip)+1));
           assert(bitget(val, 32) == 1, 'Oops! Read valid flag was not set!');
        end

        function val = getLogicTemperature(obj)
            % get temprature using method one based on Malibu Objects
            val = x6_getter(obj, 'get_logic_temperature', 0);
        end

        function set_nco_frequency(obj, a, b, freq)
            x6_call(obj, 'set_nco_frequency', a, b, freq);
        end

        function write_kernel(obj, a, b, kernel)
            packedkernel = zeros(2*length(kernel), 1);
            for ct = 1:length(kernel)
                packedkernel(2*ct - 1) = real(kernel(ct));
                packedkernel(2*ct) = imag(kernel(ct));
            end
            x6_call(obj, 'write_kernel', a, b, packedkernel, length(packedkernel));
        end

        function set_threshold(obj, a, b, threshold)
            x6_call(obj, 'set_threshold', a, b, threshold);
        end

        %Instrument meta-setter that sets all parameters
        function setAll(obj, settings)
            fields = fieldnames(settings);
            for tmpName = fields'
                switch tmpName{1}
                    case 'horizontal'
                        % Skip for now. Eventually this is where you'd want
                        % to pass thru trigger delay
                    case 'averager'
                        obj.set_averager_settings( ...
                            settings.averager.recordLength, ...
                            settings.averager.nbrSegments, ...
                            settings.averager.nbrWaveforms, ...
                            settings.averager.nbrRoundRobins);
                    case 'channels'
                        for channel = fieldnames(settings.channels)'
                            obj.set_channel_settings( channel{1}, settings.channels.(channel{1}) );
                        end
                    case 'enableRawStreams'
                        if settings.enableRawStreams
                            obj.enable_stream(1, 0, 0);
                            obj.enable_stream(2, 0, 0);
                        end
                    otherwise
                        if ismember(tmpName{1}, methods(obj))
                            feval(['obj.' tmpName{1}], settings.(tmpName{1}));
                        elseif ismember(tmpName{1}, properties(obj))
                            obj.(tmpName{1}) = settings.(tmpName{1});
                        end
                end
            end
        end

        function set_channel_settings(obj, label, settings)
            % channel labels are of the form 'sAB'
            a = str2double(label(2));
            b = str2double(label(3));
            if settings.enableDemodStream
                obj.enable_stream(a, b, 0);
            else
                obj.disable_stream(a, b, 0);
            end
            if settings.enableResultStream
                obj.enable_stream(a, b, 1);
            else
                obj.disable_stream(a, b, 1);
            end
            obj.set_nco_frequency(a, b, settings.IFfreq);
            if ~isempty(settings.kernel)
                %Try to decode base64 encoded kernels
                if (ischar(settings.kernel))
                   tmp = typecast(org.apache.commons.codec.binary.Base64.decodeBase64(uint8(settings.kernel)), 'uint8');
                   tmp = typecast(tmp, 'double');
                   settings.kernel = tmp(1:2:end) + 1j*tmp(2:2:end);
                end
                obj.write_kernel(a, b, settings.kernel);
            end
            obj.set_threshold(a, b, settings.threshold);
        end
    end

    methods (Static)

        function load_library()
            %Helper functtion to load the platform dependent library
            switch computer()
                case 'PCWIN64'
                    libfname = 'libx6adc.dll';
                    libheader = 'libx6adc.matlab.h';
                    %protoFile = @obj.libx6;
                otherwise
                    error('Unsupported platform.');
            end
            % build library path and load it if necessary
            if ~libisloaded('libx6adc')
                myPath = fileparts(mfilename('fullpath'));
                loadlibrary(fullfile(myPath, X6.LIBRARY_PATH, libfname), fullfile(myPath, libheader));
            end
        end

        function check_status(status)
          X6.load_library();
          assert(strcmp(status, 'X6_OK'),...
            'X6 library call failed with status: %s', calllib('libx6adc', 'get_error_msg', status));
          %TODO: implement error message lookup in library and call here
        end

        function val = num_devices()
            X6.load_library();
            [status, val]  = calllib('libx6adc', 'get_num_devices', 0);
            X6.check_status(status);
        end


        function set_debug_level(level)
            % sets logging level in libx6.log
            % level = {logERROR=0, logWARNING, logINFO, logDEBUG, logDEBUG1, logDEBUG2, logDEBUG3, logDEBUG4}
            calllib('libx6adc', 'set_logging_level', level);
        end

        function UnitTest()

            fprintf('BBN X6-1000 Test Executable\n')

            x6 = X6();

            x6.set_debug_level(6);

            x6.connect(0);

            fprintf('current logic temperature = %.1f\n', x6.getLogicTemperature());

            fprintf('current PLL frequency = %.2f GHz\n', x6.samplingRate/1e9);
            fprintf('Setting clock reference to external\n');
            x6.reference = 'EXTERNAL_REFERENCE';

            fprintf('Enabling streams\n');
            numDemodChan = 1;
            numMatchFilters = 2; % 4
            for phys = 1:2
                x6.enable_stream(phys, 0, 0); % the raw stream
                x6.enable_stream(phys, 1, 0); % the demod stream
                for demod = 1:numMatchFilters
                    x6.enable_stream(phys, demod, 1);
                end
            end

            fprintf('Setting NCO phase increments\n');
            x6.set_nco_frequency(1, 1, 10e6);
            x6.set_nco_frequency(2, 1, 20e6);

            fprintf('Writing integration kernels\n');
            x6.write_kernel(1, 1, ones(100,1));
            x6.write_kernel(1, 2, ones(100,1));
            x6.write_kernel(1, 3, ones(100,1));
            x6.write_kernel(1, 4, ones(100,1));
            x6.write_kernel(2, 1, ones(100,1));
            x6.write_kernel(2, 2, ones(100,1));
            x6.write_kernel(2, 3, ones(100,1));

            fprintf('Writing decision engine thresholds\n');
            x6.set_threshold(1, 1, 0.5);
            x6.set_threshold(1, 2, 0.5);
            x6.set_threshold(2, 1, 0.5);
            x6.set_threshold(2, 2, 0.5);

            fprintf('setting averager parameters to record 16 segments of 2048 samples\n');
            x6.set_averager_settings(2048, 16, 1, 1);

            % write a waveform into transmitter memory
            for ct = 1:2048
                x6.write_register(hex2dec('2200'), 9, ct-1);
                x6.write_register(hex2dec('2200'), 10, bitshift(int32(2*ct), 16) + bitand(int32(2*ct+1), hex2dec('FFFF')));
            end

            % write waveform length
            x6.write_register(hex2dec('2200'), 8, 1024);

            %DAC trigger window
            fprintf('DAC trigger window: 0x%08x\n', x6.read_register(hex2dec('0800'), 129))
            fprintf('Acquiring\n');
            x6.acquire();

            success = x6.wait_for_acquisition(1);
            fprintf('Wait for acquisition returned %d\n', success);

            fprintf('Stopping\n');
            x6.stop();

            fprintf('DAC trigger window: 0x%08x\n', x6.read_register(hex2dec('0800'), 129))
            fprintf('Transferring waveforms\n');
            wfs = cell(numDemodChan+1,1);
            for ct = 0:numDemodChan
                wfs{ct+1} = x6.transfer_stream(struct('a', 1, 'b', ct, 'c', 0));
            end
            figure();
            subplot(numDemodChan+1,1,1);
            plot(wfs{1}(:));
            title('Raw Channel 1');

            for ct = 1:numDemodChan
                subplot(numDemodChan+1,1,ct+1);
                plot(real(wfs{ct+1}(:)), 'b');
                hold on
                plot(imag(wfs{ct+1}(:)), 'r');
                title(sprintf('Virtual Channel %d',ct));
            end


            for ct = 0:numDemodChan
                wfs{ct+1} = x6.transfer_stream(struct('a', 2, 'b', ct, 'c', 0));
            end
            figure();
            subplot(numDemodChan+1,1,1);
            plot(wfs{1}(:));
            title('Raw Channel 2');

            for ct = 1:numDemodChan
                subplot(numDemodChan+1,1,ct+1);
                plot(real(wfs{ct+1}(:)), 'b');
                hold on
                plot(imag(wfs{ct+1}(:)), 'r');
                title(sprintf('Virtual Channel %d',ct));
            end

            fprintf('Result vectors:\n');
            fprintf('Ch 1.1:\n'); disp(real(x6.transfer_stream(struct('a', 1, 'b', 1, 'c', 1))));
            fprintf('Ch 1.2:\n'); disp(real(x6.transfer_stream(struct('a', 1, 'b', 2, 'c', 1))));
            fprintf('Ch 2.1:\n'); disp(real(x6.transfer_stream(struct('a', 2, 'b', 1, 'c', 1))));
            fprintf('Ch 2.2:\n'); disp(real(x6.transfer_stream(struct('a', 2, 'b', 2, 'c', 1))));

            x6.disconnect();
            unloadlibrary('libx6adc')
        end

    end

end
