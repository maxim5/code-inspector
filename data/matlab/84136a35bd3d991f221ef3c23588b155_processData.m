% data-wrangling script that takes the train.csv and test.csv files as given
% and leaves them in the form they are used in.
% author: David Thaler

% First process the training data:
DATAPATH = 'data/';
data = csvread([DATAPATH 'train.csv']);
% the data comes in with a row of zeros
data = data(2:end,:);

% The labels are in the first column
y = data(:,1);

% The rest is training data
x = data(:, 2:end);

% Binaries load faster.
save('-binary', [DATAPATH 'xtrain.mat'], 'x');
save ('-binary', [DATAPATH 'ytrain.mat'], 'y');

% Next process the test set:
data = csvread([DATAPATH 'test.csv']);
% the data comes in with a row of zeros
x = data(2:end, :);
save('-binary', [DATAPATH 'xtest.mat'], 'x');