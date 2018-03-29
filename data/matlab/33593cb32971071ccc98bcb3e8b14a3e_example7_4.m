data = load('T7-1.DAT');
[n,r] = size(data);

% use statistics toolbox
X = data(:,1:2);
y = data(:,end);
stats = regstats(y,X);
f = anova(stats);  

fprintf('\n');
fprintf('%15s %10.4f %15s %10.4f \n','Root MSE', sqrt(stats.mse), 'R-square', stats.rsquare);
fprintf('%42s %10.4f','Adj R-sq', stats.adjrsquare);
fprintf('\n');

t = estimate_parameters(stats);

X = [ones(n,1), X];

% [b,bint,r,rint] returns an n-by-2 matrix rint of intervals that can be
% Used to diagnose outliers. 
% If the interval rint(i,:) for observation i does not contain zero, 
% the corresponding residual is larger than expected in 95% 
% of new observations, suggesting an outlier.

[b,bint] = regress(y,X);
fprintf('95 percent confidence intervals for coefficient estimates \n');
disp(bint);
