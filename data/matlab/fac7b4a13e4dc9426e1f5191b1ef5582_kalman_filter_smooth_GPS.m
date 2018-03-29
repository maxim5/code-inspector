data = load("-ascii","esmatrix") ;
tick = 0.25 ;

n = length(data(:,4))
finish = input('enter finish, no greater than n  ')

if ( finish > length(data(:,4)) )
   finish = 0 % i.e. all available data is used
end

open = data(:,4) ;
high = data(:,5) ;
low = data(:,6) ;
close = data(:,7) ;
market_type = data(:,230) ;

clear data

vwap = round( ( ( open .+ close .+ ( (high .+ low) ./ 2 ) ) ./ 3 ) ./ tick) .* tick ;
vwap_process_noise = ( vwap .- shift(vwap,1) ) ./ 2.0 ;
median_vwap_process_noise = median(vwap_process_noise(2:end-finish,1)) ;
vwap_process_noise_deviations = vwap_process_noise(2:end-finish,1) .- median_vwap_process_noise ;
MAD_process_noise = median( abs( vwap_process_noise_deviations ) ) ;

% convert this to variance under the assumption of a normal distribution
std_vwap_noise = 1.4826 * MAD_process_noise ;
process_noise_variance = std_vwap_noise * std_vwap_noise 

measurement_noise = 0.666 .* ( high .- low ) ;
median_measurement_noise = median( measurement_noise(1:end-finish,1) ) ;
measurement_noise_deviations = measurement_noise(1:end-finish,1) .- median_measurement_noise ;
MAD_measurement_noise = median( abs( measurement_noise_deviations ) ) ;

% convert this to variance under the assumption of a normal distribution
std_measurement_noise = 1.4826 * MAD_measurement_noise ;
measurement_noise_variance = std_measurement_noise * std_measurement_noise

% Transition matrix for the continous-time system.
F = [0 0 1 0 0 0;
     0 0 0 1 0 0;
     0 0 0 0 1 0;
     0 0 0 0 0 1;
     0 0 0 0 0 0;
     0 0 0 0 0 0];

% Noise effect matrix for the continous-time system.
L = [0 0;
     0 0;
     0 0;
     0 0;
     1 0;
     0 1];

% Process noise variance
q = process_noise_variance ;
Qc = diag([q q]);

% Discretisation of the continuous-time system.
[A,Q] = lti_disc(F,L,Qc,1); % last item is dt stepsize set to 1

% Measurement model.
H = [1 0 0 0 0 0;
     0 1 0 0 0 0];

% Variance in the measurements.
r1 = measurement_noise_variance ;
R = diag([r1 r1]);

% Initial guesses for the state mean and covariance.
m = [0 vwap(1,1) 0 0 0 0]';
P = diag([0.1 0.1 0.1 0.1 0.5 0.5]) ;

% Space for the estimates.
MM = zeros(size(m,1), length(vwap));

% create vectors for eventual plotting
predict_plot = zeros(length(vwap),1) ;
MM_plot = zeros(length(vwap),1) ;
sigmaP_plus = zeros(length(vwap),1) ;
sigmaP_minus = zeros(length(vwap),1) ;

% Filtering steps.
for ii = 1:length(vwap)
   [m,P] = kf_predict(m,P,A,Q);

   predict_plot(ii,1) = m(2,1) ;

   [m,P] = kf_update(m,P,vwap(ii,:),H,R);
   MM(:,ii) = m;

   MM_plot(ii,1) = m(2,1) ;

   % sigmaP is for storing the current error covariance for plotting purposes
   sigmaP = sqrt(diag(P)) ; 
   sigmaP_plus(ii,1) = MM_plot(ii,1) + 2 * sigmaP(1) ;
   sigmaP_minus(ii,1) = MM_plot(ii,1) - 2 * sigmaP(1) ;
end

% output in terminal for checking purposes
kalman_last_50 = [kalman_last_50,MM_plot(end-50:end,1)] 

% output for plotting in Gnuplot
x_axis = ( 1:length(vwap) )' ;
A = [x_axis,open,high,low,close,vwap,MM_plot,sigmaP_plus,sigmaP_minus,predict_plot,market_type] ;
dlmwrite('my_cosy_kalman_plot',A)