
data  = load('../logs/25_01_39_25_log.log');
thr    = load('../logs/25_01_39_25_speed.log');

% [0.3, 0.0, 0.5]; 10; 15
%data  = load('../logs/25_01_03_07_log.log');
%thr    = load('../logs/25_01_03_07_speed.log');
%[0.3, 0.5, 0.5]; 20; 5
%data  = load('../logs/25_00_48_49_log.log');
%thr    = load('../logs/25_00_48_49_speed.log');
angle = data(data(:,4) == 0);

subplot(2,1,1);
plot(angle);
subplot(2,1,2);
plot(thr(1:length(angle)));