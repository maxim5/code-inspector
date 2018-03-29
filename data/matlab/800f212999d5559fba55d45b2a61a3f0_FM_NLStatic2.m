% algorithm for solving element forces using force control method
%
% written by T.Y. Yang on 2009/08/22

% clean start
clear all; close all; clc;

% Problem parameter definition
GMDIR = 'C:\Documents and Settings\Tony\Desktop\OneBayFrame\';
Acc = load([GMDIR 'elcentro.txt'])*386.1; 
dt = 0.02;
Time = [0:(length(Acc)-1)]*dt;

% formulation
B = [1; 1];
M = [0.04 0; 0 0.02];
C = 1.01*M;
Bi = [1 1; 0 1; 0 0];
Bx = [1 1 1]';

Uf = zeros(size(B,1),length(Acc)+3);
Q = zeros(size(Bx,1),length(Acc)+3);
for nn = 1:length(Acc)
    P = -M*B*Acc(nn);
    Uflast3 = Uf(:,end-3:end);
    dt = 1;
    [Q_0,Uf_0] = NLForceNewtonRaphson(P,Bi,Bx,M,C,Uflast3,dt,Q(nn+2));
    Q(:,nn+3) = Q_0;
    Uf(:,nn+3) = Uf_0;
end

figure;
subplot(3,1,1);
plot(Uf(1,4:end),Q(1,4:end))
grid
subplot(3,1,2);
plot(Uf(2,4:end)-Uf(1,4:end),Q(2,4:end))
grid
subplot(3,1,3);
plot(-Uf(2,4:end),Q(3,4:end))
grid

figure;
subplot(2,1,1)
plot(Time,Uf(1,4:end))
grid
subplot(2,1,2)
plot(Time,Uf(2,4:end))
grid