% algorithm for solving element forces using force control method
%
% written by T.Y. Yang on 2009/08/22

% clean start
clear all; close all; clc;

% Problem parameter definition
P1 = 0.5;
P2 = 0.2;
% K1 = 30;
% K2 = 40;
% K3 = 50;

% formulation
Pf = [P1; P2];
Bi = [1 1; 0 1; 0 0];
Bx = [1 1 1]';
M = [0.04 0; 0 0.02];
C = 1.01*[0.04 0; 0 0.02];
%Fs = diag([1/K1,1/K2,1/K3]);
%Fxx = Bx'*Fs'*Bx;
% % calculate the element forces
% Qx = -Fxx\(Bx'*Fs*Bi*Pf)
% Q = Bi*Pf+Bx*Qx
% 
% [Qx,Z] = NLForceEquations(Pf,Bi,Bx)
% Q = Bi*Pf+Bx*Qx

Uf = zeros(size(Pf,1),4);
[Q,U] = NLForceNewtonRaphson(Pf,Bi,Bx,M,C,Uf)
