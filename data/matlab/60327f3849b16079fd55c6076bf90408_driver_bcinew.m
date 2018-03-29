%%driver for BCI board using USB (July 2009)	
%reset serial
delete(instrfindall);
 	
%Create serial port object
clear
s=serial('COM4');
set(s,'BaudRate',921600,'DataBits',8,'Parity','none','StopBits',1);
s.FlowControl = 'none';
InputBufferSize=2048;
 	
%open port
fopen(s);
message = s.status

s.RequestToSend = 'off';
s.RequestToSend = 'on';
	
%QUERYDEV
fwrite(s, 19); %hex-0x13
querydev1 = fread(s);
querydev=char(querydev1)'

s.RequestToSend = 'off';
s.RequestToSend = 'on';

%SETSRATE
fwrite(s, 20);%hex-0x14
setsamp1 = fread(s);
setsamp=setsamp1

s.RequestToSend = 'off';
s.RequestToSend = 'on';

%SETSRATE value
msg1=char(bitshift(1000,-8));
msg2=char(1000);
msg=[msg1 msg2];

fwrite(s, msg, 'char');
samprate1 = fread(s);
samprate= samprate1(1)*256 + samprate1(2)

s.RequestToSend = 'off';
s.RequestToSend = 'on';

%SETCHANNELS
fwrite(s,18);%hex-0x12
setCh1 = fread(s);
setCh=setCh1

s.RequestToSend = 'off';
s.RequestToSend = 'on';

%SETCHANNELS Value
fwrite(s,  bin2dec('00000000 00000000 00000000 00000011'), 'uint32');
% fwrite(s,   chMatrix);
setChValue1 = fread(s);
setChValue = setChValue1

s.RequestToSend = 'off';
s.RequestToSend = 'on';

%TESTSETUP
fwrite(s, 22);%hex-0x16
test1 = fread(s);
status = char(test1)'

figure1 = figure;

%RUN ACQ
stopStamp=0;
makeSignal1=[];
makeSignal2=[];
fwrite(s, 21);%hex-0x15
i=1;
k=1;
while (stopStamp==0)
    runAcq = fread(s, 4);%read the signals 2-byte every serial read

    rawS1=makeAcq_samp(runAcq(1),runAcq(2));%manage acq samples and generate signal
    rawS2=makeAcq_samp(runAcq(3),runAcq(4));%manage acq samples and generate signal
    
    makeSignal1=vertcat(makeSignal1, rawS1);
    makeSignal2=vertcat(makeSignal2, rawS2);
    
    if i==2000
        %KILLACQ
        fwrite(s, 17);%hex-0x15
        killAcq = fread(s)
        stopStamp=1;
    end
    i=i+1;
end

subplot(2,1,1), plot(makeSignal1);drawnow;
subplot(2,1,2), plot(makeSignal2);drawnow;
save makeSignal1;
save makeSignal2;

fclose(s);
delete(s);
clear s;