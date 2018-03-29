%% Data Acq tool for USB BCI Board (July 2009)	
%% reset serial
clear
delete(instrfindall);

%% load configuraton
configBoard;

%% Open Port and Connect
s=openPort(portNum);
s.RequestToSend = 'off';
s.RequestToSend = 'on';
	
%% Set Sample Rate RATE
[setSamp, sampRate]=setSamp(s, sampRate);
s.RequestToSend = 'off';
s.RequestToSend = 'on';

%% Set Channels for Data Acq
[setCh, setChValue]=setCh(s, channels)
s.RequestToSend = 'off';
s.RequestToSend = 'on';

%% TEST SETUP
fwrite(s, 22);%hex-0x16
test1 = fread(s);
flushoutput(s);
status = char(test1)'
s.RequestToSend = 'off';
s.RequestToSend = 'on';
figure1 = figure;

%% RUN ACQ
stopStamp=0;
makeSignal1=[];
makeSignal2=[];
fwrite(s, 21);%hex-0x15
i=1;
k=1;
while (stopStamp==0)
    if i<=setChValue*2
        runAcq = fread(s, 4);%read the signals 2-byte every serial read
        flushoutput(s);
    else
        runAcq = fread(s, 4);%read the signals 2-byte every serial read
        flushoutput(s);
        
        rawS1=makeAcq_samp(runAcq(1),runAcq(2));%manage acq samples and generate signal
        rawS2=makeAcq_samp(runAcq(3),runAcq(4));%manage acq samples and generate signal

        makeSignal1=vertcat(makeSignal1, rawS1);
        makeSignal2=vertcat(makeSignal2, rawS2);
       
        if i==acqDuration*sampRate%duration of Acq
            [killAcq]=stopAcq(s)%stop Acq
            stopStamp=1;
        end
    end
    i=i+1;
end

subplot(2,1,1), plot(makeSignal1);drawnow;
subplot(2,1,2), plot(makeSignal2);drawnow;
save makeSignal1;
save makeSignal2;

%Disconnect and Close Port
[x]=closePort(s)
