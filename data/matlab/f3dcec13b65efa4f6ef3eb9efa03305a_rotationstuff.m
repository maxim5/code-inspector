%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Diagnostic Medical Image Processing (DMIP) 
% WS 2010/11
% Author: Attila Budai & Jan Paulus
% Exercise: Quaternions, Axis-Angle representation, ...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function rotationstuff()
    clear all;
    close all;
    clc;
   

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Axis-Angle representation %
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    disp('Axis-Angle Representation');

    % Rotation angle
    u = [1; 2 ; 3]
    
    % u must be a single unit norm axis
    disp('u must be unit length');
    u = u/norm(u)

    % Angle theta given in radians [0, 6.2832] = [0, 360] Grad
    theta = 1.2; 

    % Compute the degrees out of radians
    degrees = theta/(180/pi)
            
    % Calculate the rotation matrix componentwise  %slide (23/85)
%    disp('Rotation calculated componentwise');

%     R1 = [u(1)^2+(1-u(1)^2)*cos(theta), u(1)*u(2)*(1-cos(theta)-u(3)*sin(theta), u(1)*u(3)*(1-cos(theta))+u(2)*sin(theta); ...
%           u(1)*u(2)*(1-cos(theta))+u(3)*sin(theta), u(2)^2+(1-u(2)^2)*cos(theta), u(2)*u(3)*(1-cos(theta)) - u(1)*sin(theta); ...
%           u(1)*u(3)*(1-cos(theta))u(2)*sin(theta)].. blablabla. siehe folien
%    
%    

    %calculate the skew matrix
    disp('skew matrix:');
    skew_matrix = [0 -u(3) u(2); u(3) 0 -u(1); -u(2) u(1) 0]
  
     
    % Calculate the rotation matrix using the formula of Rodrigues
    disp('Rotation calculated using Rodigues Formula');
    R2 = u*u' + (eye(3) - u*u')*cos(theta) + skew_matrix*sin(theta)
    
    n = norm(u); 
    % Calculate magnitude of the rotation axis
    % (Equal to the rotation angle)
    u = (u/n)*theta;
    
    % Solution of Peter Kovesi, http://www.csse.uwa.edu.au/
    disp('Rotation matrix calculated by Kovesis code');
    T = angleaxis2matrix2(u)
    
    %Calculate u and theta from the rotation matrix
    [V,D] = eigs(R2);
    disp('calculated rotation axes:');    
    u2 = V(:,1)
    disp('calculated rotation angles:');
    theta2 = acos(real(D(2,2)))
    theta2 = acos(real(D(3,3)))
    theta2 = - asin(imag(D(2,2)))
    theta2 = asin(imag(D(3,3)))
    
    % Generate homogeneous matrix
    T = zeros(4,4);
    T(1:3,1:3) = R2;
    T(4,4) = 1;
    T
    
    % Solution of Peter Kovesi, http://www.csse.uwa.edu.au/
    disp('axis and angle calculated by Kovesis code');
    t = matrix2angleaxis(T)
    
    theta = norm(t)
    disp('normalized axis calculated by Kovesis code');
    t = t/norm(t)
    
    %%%%%%%%%%%%%%
    % Quaternion %
    %%%%%%%%%%%%%%
    disp('Quaternion Representation');
    
    %Calculate Quaternion Representation using "theta" and "t"
    r = [cos(theta/2), sin(theta/2)*t']

    % Solution of Peter Kovesi, http://www.csse.uwa.edu.au/
    disp('Quaternions calculated by Kovesis code');
    q = matrix2quaternion(T)
    
    
    phi_x = atan((2*(r(1)*r(2)+r(3)*r(4)))/(1-2*(r(2)^2+r(3)^2)))
    phi_y = asin(2*(r(1)*r(3)-r(4)*r(2)))
    phi_z = atan((2*(r(1)*r(4)+r(2)*r(3)))/(1-2*(r(3)^2+r(4)^2)))
    
    degrees_x = phi_x/(pi/180)
    degrees_y = phi_y/(pi/180)
    degrees_z = phi_z/(pi/180)
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Euler Angle Representation %
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    disp('Euler Angle Representation');

    % Rotation angles around axes x,y,z given in radian
    phi_x = 0.5947; % roll
    phi_y = 0.3699; % pitch
    phi_z = 1.1180; % yaw

    % Calculate the angles in degrees
    degrees_x = phi_x/(pi/180)
    degrees_y = phi_y/(pi/180)
    degrees_z = phi_z/(pi/180)
    
    % Calculate Rotation matrices around the axes 
    R_x = [1 0 0; 0 cos(phi_x) -sin(phi_x); 0 sin(phi_x) cos(phi_x)]
    R_y = [cos(phi_y) 0 sin(phi_y); 0 1 0; -sin(phi_y) 0 cos(phi_y)]
    R_z = [cos(phi_z) -sin(phi_z) 0; sin(phi_z) cos(phi_z) 0; 0 0 1]       
    
    % Rotation matrix calculated from R_x, R_y, R_z
    disp('Rotation calculated using R_x, R_y, R_z');
    R3 = R_x*R_y*R_z
    det(R3)

    % Calculate the rotation matrix using the phi values
    disp('Rotation componentwise');
%    R4 =
%        
%        

    % This is the correct rotation matrix!
    disp('Rotation calculated using R_z, R_y, R_x');
    R5 = R_z*R_y*R_x
    det(R5)
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% function [T] = angleaxis2matrix2(t)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2008 Peter Kovesi
% School of Computer Science & Software Engineering
% The University of Western Australia
% pk at csse uwa edu au
% http://www.csse.uwa.edu.au/
function T = angleaxis2matrix2(t)
% converts angle-axis descriptor to 4x4 homogeneous
% transformation matrix
% t - 3-vector giving rotation axis with magnitude equal to the
%     rotation angle in radians.

    % Magnitude of rotation axis is rotation angle
    theta = norm(t);
    
    % Is rotation very small?
    if(theta < eps)   
        T = [ 1   -t(3) t(2) 0
              t(3) 1   -t(1) 0
             -t(2) t(1) 1    0
              0    0    0    1];
        return;
    end
    
    t = t/theta;  
    x = t(1); 
    y = t(2); 
    z = t(3);
    
    c = cos(theta); 
    s = sin(theta); 
    C = 1-c;
    xs = x*s;   ys = y*s;   zs = z*s;
    xC = x*C;   yC = y*C;   zC = z*C;
    xyC = x*yC; yzC = y*zC; zxC = z*xC;

    T = [ x*xC+c   xyC-zs   zxC+ys  0
          xyC+zs   y*yC+c   yzC-xs  0
          zxC-ys   yzC+xs   z*zC+c  0
            0         0       0     1];     
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% function [t] = matrix2angleaxis(T)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2008 Peter Kovesi
% School of Computer Science & Software Engineering
% The University of Western Australia
% pk at csse uwa edu au
% http://www.csse.uwa.edu.au/
function t = matrix2angleaxis(T)
% converts homogeneous matrix to angle-axis descriptor
% T - 4x4 Homogeneous transformation matrix
% t - 3-vector giving rotation axis with magnitude equal to the
%     rotation angle in radians.

    % This code follows the implementation suggested by Hartley and Zisserman    
    R = T(1:3,1:3);   % Extract rotation part of T
    
    % Find rotation axis as the eigenvector having unit eigenvalue
    % Solve (R-I)v = 0;
    [v,d] = eig(R-eye(3));
    
    % The following code assumes the eigenvalues returned are not necessarily
    % sorted by size. This may be overcautious on my part.
    d = diag(abs(d));   % Extract eigenvalues
    [s, ind] = sort(d); % Find index of smallest one
    if d(ind(1)) > 0.001   % Hopefully it is close to 0
        warning('Rotation matrix is dubious');
    end
    
    axis = v(:,ind(1)); % Extract appropriate eigenvector
    
    if abs(norm(axis) - 1) > .0001     % Debug
        warning('non unit rotation axis');
    end
    
    % Now determine the rotation angle
    twocostheta = trace(R)-1;
    twosinthetav = [R(3,2)-R(2,3), R(1,3)-R(3,1), R(2,1)-R(1,2)]';
    twosintheta = axis'*twosinthetav;
    
    theta = atan2(twosintheta, twocostheta);
    
    t = theta*axis;
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% function [Q] = matrix2quaternion(T)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2008 Peter Kovesi
% School of Computer Science & Software Engineering
% The University of Western Australia
% pk at csse uwa edu au
% http://www.csse.uwa.edu.au/
function Q = matrix2quaternion(T)
% Homogeneous matrix to quaternion
% T - 4x4 Homogeneous transformation matrix
% Q - a quaternion in the form [w, xi, yj, zk]

    % This code follows the implementation suggested by Hartley and Zisserman    
    R = T(1:3, 1:3);   % Extract rotation part of T
    
    % Find rotation axis as the eigenvector having unit eigenvalue
    % Solve (R-I)v = 0;
    [v,d] = eig(R-eye(3));
    
    % The following code assumes the eigenvalues returned are not necessarily
    % sorted by size. This may be overcautious on my part.
    d = diag(abs(d));   % Extract eigenvalues
    [s, ind] = sort(d); % Find index of smallest one
    if d(ind(1)) > 0.001   % Hopefully it is close to 0
        warning('Rotation matrix is dubious');
    end
    
    axis = v(:,ind(1)); % Extract appropriate eigenvector
    
    if abs(norm(axis) - 1) > .0001     % Debug
        warning('non unit rotation axis');
    end
    
    % Now determine the rotation angle
    twocostheta = trace(R)-1;
    twosinthetav = [R(3,2)-R(2,3), R(1,3)-R(3,1), R(2,1)-R(1,2)]';
    twosintheta = axis'*twosinthetav;
    
    theta = atan2(twosintheta, twocostheta);
    
    Q = [cos(theta/2); axis*sin(theta/2)];
end