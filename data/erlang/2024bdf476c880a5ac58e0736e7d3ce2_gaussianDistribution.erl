% The function integral calculates the value of the cumulative
% distribution function of the Gaussian distribution. Due to
% rounding errors, we abort the computation for extreme values.
%
% Frank Recker, 2012

-module(gaussianDistribution).
-export([integral/2,error_term/2]).
% x: Argument for the Gaussian integral
% n: Number of steps for the approximation
% return value: approximation of Phi(x)
integral(X,N) when N>=1, X>(-6.2), X<6.2 ->
    integral_(X,N,0).

integral_(X,1,D) ->
    0.5 + X*(D+1)/ math:sqrt(2 * math:pi());
integral_(X,J,D) ->
    integral_(X,J-1,-X*X*(D+1/(2*J-1))/(2*(J-1))).


% x: Argument for the Gaussian integral
% n: Number of steps for the approximation
% An upper bound for the absolute difference of the result and Phi(x)
error_term(X,N) when 2 * N >= X * X ->
    {bound,X * product([X * X / (2*J) || J<-lists:seq(1,N)]) / (2*N+1) / math:sqrt(2 * math:pi())};
error_term(_,_) -> no_bound.

product(Xs) ->
    lists:foldl(fun(A,B) -> A*B end,1,Xs).
