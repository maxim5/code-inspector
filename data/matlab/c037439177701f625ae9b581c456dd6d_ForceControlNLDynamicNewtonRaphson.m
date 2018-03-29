% algorithm for solving element forces using force control method
%
% written by T.Y. Yang on 2009/08/22

% clean start
clear all; close all; clc;

% Problem parameter definition
GMDIR = 'C:\Documents and Settings\Tony\Desktop\OneBayFrame\';
Acc = load([GMDIR 'elcentro.txt'])*386.1; 
dt = 0.02;
Time = (0:(length(Acc)-1))*dt;
% % change the sampling time to 0.002
% dt = 0.002;
% for pp = 1:floor(Time(end)/dt)
%     AccN(pp) = interp1(Time,Acc,dt*pp);
% end
% Time = [0 (1:pp)*dt];
% Acc = [0; Acc];

% load the opensees results
path = 'C:\Documents and Settings\Tony\Desktop\OneBayFrame\';
data = load([path,'\Elmt_Frc.out']);
fE = data(:,[4,8,12]);
Q3O = -fE(:,2); % tension positive

% formulation
m1 = 0.04;
m2 = 0.02;
c1 = 1.01*m1;
c2 = 1.01*m2;
Bi = [1 1; 0 1; 0 0];
Bx = [1 1 1]';
Tol = 1e-2;
iterMax = 1000;

Uf = zeros(size(Bi,2),length(Acc));
Ufdot = zeros(size(Bi,2),length(Acc));
Ufdotdot = zeros(size(Bi,2),length(Acc));
Q = zeros(size(Bi,1),length(Acc));

for nn = 2:length(Acc)
    clc; disp(nn)
    %[Q_0,Uf_0,Ufdot_0,Ufdotdot_0] = NLForceEquationsNewtonRaphson(Uf(:,nn-1),Ufdot(:,nn-1),Ufdotdot(:,nn-1),m1,m2,c1,c2,dt,Acc(nn),Q(3,nn-1),Tol,iterMax);
    [Q_0,Uf_0,Ufdot_0,Ufdotdot_0] = NLForceEquationsNewtonRaphson(Uf(:,nn-1),Ufdot(:,nn-1),Ufdotdot(:,nn-1),m1,m2,c1,c2,dt,Acc(nn),Q3O(nn),Tol,iterMax);
    Q(:,nn) = Q_0;
    Uf(:,nn) = Uf_0;
    Ufdot(:,nn) = Ufdot_0;
    Ufdotdot(:,nn) = Ufdotdot_0;    
end

figure;
subplot(3,1,1);
plot(Uf(1,1:nn),Q(1,1:nn))
grid
subplot(3,1,2);
plot(Uf(2,1:nn)-Uf(1,1:nn),Q(2,1:nn))
grid
subplot(3,1,3);
plot(-Uf(2,1:nn),Q(3,1:nn))
grid

figure;
subplot(2,1,1)
plot(Time(1:nn),Uf(1,1:nn))
grid
subplot(2,1,2)
plot(Time(1:nn),Uf(2,1:nn))
grid

figure;
subplot(2,1,1)
plot(Time(1:nn),Ufdot(1,1:nn))
grid
subplot(2,1,2)
plot(Time(1:nn),Ufdot(2,1:nn))
grid

figure;
subplot(2,1,1)
plot(Time(1:nn),Ufdotdot(1,1:nn))
grid
subplot(2,1,2)
plot(Time(1:nn),Ufdotdot(2,1:nn))
grid


% check for Equilibrium EQs
ZZ1 = m1*Ufdotdot(1,1:nn)+c1*Ufdot(1,1:nn)-Q(2,1:nn)+Q(1,1:nn)+m1*Acc(1:nn)';
ZZ2 = m2*Ufdotdot(2,1:nn)+c2*Ufdot(2,1:nn)+Q(3,1:nn)+Q(2,1:nn)+m2*Acc(1:nn)';
figure;
plot(ZZ1)
figure;
plot(ZZ2)

% check U,V,A
v1cal = diff(Uf(1,1:nn))./dt;
a1cal = diff(Ufdot(1,1:nn))./dt;
figure;
plot(Ufdot(1,1:nn))
hold
plot([0, v1cal],'r--')
grid
figure;
plot(Ufdotdot(1,1:nn))
hold
plot([0, a1cal],'r--')
grid

gamma = 1/2;
beta = 1/4;
dt = 0.02;
U1dotCal = zeros(size(Uf,2),1);
U1dotdotCal = zeros(size(Uf,2),1);
U2dotCal = zeros(size(Uf,2),1);
U2dotdotCal = zeros(size(Uf,2),1);
for pp = 2:size(Uf,2)
    [U1dotCal(pp), U1dotdotCal(pp)] = NemarkIntegration(Ufdotdot(1,pp-1),Ufdot(1,pp-1),Uf(1,pp-1),Uf(1,pp),gamma,beta,dt);
    [U2dotCal(pp), U2dotdotCal(pp)] = NemarkIntegration(Ufdotdot(2,pp-1),Ufdot(2,pp-1),Uf(2,pp-1),Uf(2,pp),gamma,beta,dt);
end
    
figure;
plot(Ufdot(1,1:nn))
hold
plot(U1dotCal(1:nn),'r--')
grid
figure;
plot(Ufdotdot(1,1:nn))
hold
plot(U1dotdotCal(1:nn),'r--')
grid
