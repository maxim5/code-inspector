data = load('T7-1.DAT');
[n,r] = size(data);
r = r-1;

% compute statistics manually 
Z = [ones(n,1), data(:,1:2)];
Y = data(:,end);
beta_hat = (Z'*Z)\Z'*Y;
pred = Z*beta_hat;
model_df = r;
error_df = n-r-1;
total_df = n-1;
e_hat = Y - pred;
s_sqr = e_hat'*e_hat/error_df;

s = sqrt(s_sqr);
beta_hat_var = diag(s_sqr*inv(Z'*Z));
beta_std_err = sqrt(beta_hat_var); 

ssr = (pred - mean(Y))'*(pred - mean(Y));   %regression sum of squares
sse = e_hat'*e_hat;                         %residual (error) sum of squares
sst = (Y-mean(Y))'*(Y-mean(Y));
R_squared = 1 - sse/sst;
adj_R_squared = 1- (sse/error_df)/(sst/total_df);
