% algorithm for solving element forces using force control method
%
% written by T.Y. Yang on 2009/08/22

% clean start
clear all; close all; clc;

% Problem parameter definition
P1 = 10;
P2 = 20;
K1 = 30;
K2 = 40;
K3 = 50;

% formulation
Pf = [P1; P2];
Bi = [1 1; 0 1; 0 0];
Fs = diag([1/K1,1/K2,1/K3]);
Bx = [1 1 1]';
Fxx = Bx'*Fs'*Bx;
% calculate the element forces
Qx = -Fxx\(Bx'*Fs*Bi*Pf);
Q = Bi*Pf+Bx*Qx
