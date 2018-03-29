data = load('T7-2.DAT');
[n,r] = size(data);

% use statistics toolbox
X = zeros(n,12);
X(:,1) = ones(n,1);
X(:,2) = [ones(7,1); zeros(n-7,1)];
X(:,3) = [zeros(7,1); ones(7,1); zeros(4,1)];
X(:,4) = [zeros(n-4,1); ones(4,1)];
X(:,5) = [ones(5,1); zeros(2,1); ones(5,1);zeros(2,1);ones(2,1);zeros(2,1)];
X(:,5) = ~X(:,5);
X(:,7) = [ones(5,1); zeros(n-5,1)];
X(:,8) = [zeros(5,1); ones(2,1); zeros(n-7,1)];
X(:,9) = [zeros(7,1); ones(5,1); zeros(6,1)];
X(:,10) = [zeros(12,1); ones(2,1); zeros(4,1)];
X(:,11) = [zeros(14,1); ones(2,1); zeros(2,1)];
X(:,12) = [zeros(16,1); ones(2,1)];
y = data(:,3);

rank1 = rank(X);
[b,bint,error,err_int] = regress(y,X);
SSres_complete = error'*error;
fprintf('SS error for complete model %10.4f \n', SSres_complete);

rank2 = rank(X(:,1:6));
[b2,bint2,error2,err_int2] = regress(y,X(:,1:6));
SSres_nointeration = error2'*error2;
fprintf('SS error for model w/o interation terms %10.4f \n', SSres_nointeration);

f_val  = ((SSres_nointeration - SSres_complete)/(rank1-rank2)) / (SSres_complete/(n-rank1));
crit_val = finv(0.95, rank1-rank2, n-rank1);
p_val = fcdf(f_val, rank1-rank2, n-rank1);

if f_val < crit_val
    fprintf('f-statistic: %10.4f, crit_value: %10.4f, p-value: %10.4f \n', f_val, crit_val, p_val);    
    fprintf('the service index does not depend upon location-gender ineration \n');    
else 
    fprintf('f-statistic: %10.4f, crit_value: %10.4f, p-value: %10.4f \n', f_val, crit_val, p_val);    
    fprintf('location-gender interation factor does contribute to the service index \n');    
end

%stepwise(X,y);


