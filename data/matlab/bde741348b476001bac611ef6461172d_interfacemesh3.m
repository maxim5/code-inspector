function  [node,elem, interfaceData] = interfacemesh3(cube,phi,h)
%% INTERFACEMESH3 generates the mesh structure for elliptic interface problems
%
%   [node,elem,interfaceData] = INTERFACEADAPTIVEMESH(cube, phi, h), where 
%    - cube = [xmin, xmax, ymin, ymax, zmin, zmax],  represent an cube domain 
%       enclosing the interface; 
%    - phi is a level set function defining the interface as follows
%       {p | phi(p)=0} represents the interface
%       {p | phi(p) < 0} represents the interior domain enclosed by the interface
%       {p | phi(p) > 0} rpepresents the exterior domain of the interface. 
%    - h is the mesh size of the Cartesian grid on cube.
%
%   Example:
%       cube = [-1 1 -1 1 -1 1];
%       phi = @sphereM;
%       h = 0.125;
%       [node, elem, interfaceData] = interfacemesh3(cube,phi,h);
%
%   Author: Huayi Wei <weihuayi@xtu.edu.cn>, based on discussion with Long Chen.
%
%   See also: interfacemesh, interfaceadaptivemesh
%
%
%   Copyright (C) Long Chen. See COPYRIGHT.txt for details.


%% Generate the Cartesian mesh on a cube domain with size h
[node,elem, ni,nj, ~] = cubehexmesh(cube, h);
N = size(node, 1); % number of nodes
NC = size(elem,1); % number of small cubes

%% Evaluate sign of vertices and find interface elements
% find the sign function of all vertices
phiValue = phi(node);
phiValue(abs(phiValue) < h^2) = 0;
vSign = msign(phiValue);
e2vSign = vSign(elem);     % element to vertices sign

% find cut element
eta = max(e2vSign,[],2).*min(e2vSign,[],2);
isCutElem  = (eta < 0 | sum(abs(e2vSign),2) <= 5); % elements cross interface
cutElem = elem(isCutElem,:);

 
% index of element
elemIdx = zeros(NC,1,'int8');
elemIdx((eta >= 0) & (max(e2vSign,[],2) == 1)) = 1;   % 1: exterior element
elemIdx((eta >= 0) & (min(e2vSign,[],2) == -1)) = -1; % -1: interior element
elemIdx(isCutElem) = 0; % 0: interface element

%  find the cut edge
localEdge = [1 5; 2 6; 4 8; 3 7; 1 2; 3 4; 5 6; 7 8; 1 4; 2 3; 5 8; 6 7];
edgeMatrix = sparse(cutElem(:, localEdge), cutElem(:, localEdge(:, [2,1])), 1, N, N);
[I,J,~] = find(triu(edgeMatrix));
edge = [I, J];
isCutEdge = vSign(edge(:,1)).*vSign(edge(:, 2)) <  0; 
edge = edge(isCutEdge, :);

% find the cut points
cutPoint = findintersectbisect(phi,node(edge(:,1),:),node(edge(:, 2),:));
nCutPt = size(cutPoint,1);  % nCutPt: number of intersection points
N1 = N + nCutPt;
node(N+1:N1,:) = cutPoint; % add the coordinates of the intersection points
vSign(N+1:N1) = 0;


%% Get the boundary faces of all cutElems which are squares.
T = auxstructurehex(cutElem);
sface = double(T.bdFace);
sface2elem = double(T.bdFace2elem);
localidx2globalidx = find(isCutElem);
sface2elem = localidx2globalidx(sface2elem); 
eta = max(vSign(sface), [] ,2) == 1;
sface2elem(eta) = sface2elem(eta) + NC;

%% Get the special face whose have at least two vertices on the interface 
%     and they are opposites on this face.
face = T.face;
face2elem = double(T.face2elem);
clear T;
isSpecialFace = face2elem(:, 1) ~= face2elem(:, 2) & ...
    ((sum(vSign(face), 2) == 0 & sum(abs(vSign(face)), 2) == 2));
auxPoint = (node(face(isSpecialFace, 1), :)+ node(face(isSpecialFace, 2),:)...
    + node(face(isSpecialFace, 3), :) + node(face(isSpecialFace,4),:) )/4.0; % get the center of the special face
N2 = N1 + size(auxPoint,1);
node(N1+1:N2, :) = auxPoint; % add the coordinates of the aux points
vSign(N1+1:N2, :) = 0;

%% Construct Delaunay triangulation
isInterfaceNode = false(size(node,1), 1);
isInterfaceNode(cutElem(:)) = true; % include the nodes in cutElem and 
isInterfaceNode(N+1:end) = true; % the cut points and the aux points

interfaceNode = node(isInterfaceNode,:);
DT = DelaunayTri(interfaceNode);
tetElem = DT.Triangulation;
tetElem = fixorder3(interfaceNode, tetElem);
localidx2globalidx = find(isInterfaceNode);
tetElem = localidx2globalidx(tetElem);
bc = (node(tetElem(:, 1), :) + node(tetElem(:, 2), :) + node(tetElem(:, 3), :) + node(tetElem(:, 4), :))/4.0;
idx = getCubeIdx(bc, cube, h,ni,nj); % Get the indices of the small cubes which the every tet blongs to.
tetElem = tetElem(isCutElem(idx), :);% Just keep the tets in cutElem
idx = idx(isCutElem(idx)); 

% In some cases, there exist some bad tet elements whose vertices are in
% different cubes. But we just need to keep the tet in every cut cube. So
% here we get rid of them. 
NT = size(tetElem,1);
X = reshape(node(tetElem,1), NT, 4);
Y = reshape(node(tetElem,2), NT, 4);
Z = reshape(node(tetElem,3), NT, 4);

isBadTet = (max(X,[],2) - min(X,[],2)) > 2*h - eps | (max(Y,[],2) - min(Y,[],2)) > 2*h-eps ...
    | (max(Z,[],2) - min(Z,[],2)) > 2*h - eps;

tetElem = tetElem(~isBadTet,:);
idx = idx(~isBadTet);



%% Get all the exterior and interrior triangle faces between cutted small cubes
localFace = [2 3 4;1 4 3;1 2 4;1 3 2];
NT = size(tetElem,1);
tface = zeros(4*NT, 3);
tface2elem = zeros(4*NT, 1);
interface = zeros(4*NT, 3);

% find the interior tet elements
isIntTet = min(vSign(tetElem),[], 2) == -1;
intTet = tetElem(isIntTet,:);
% find the cube indices which the interior tet elements belong to.
intIdx = idx(isIntTet); 
NT = size(intTet, 1);
T = auxstructure3(intTet);
neighbor = T.neighbor;
clear T;
tmp = (1:NT)';
c1 = 0;
c3 = 0;
for i = 1:4
    face = intTet(:, localFace(i,:));
    % find the triangle face between small cubes.
    isPolyTriFace = neighbor(:, i) ~= tmp & intIdx ~= intIdx(neighbor(:,i));
    % find the triangle faces which are on interface, and their normal points to exterior. 
    isInterface = neighbor(:, i) == tmp & sum(abs(vSign(face)), 2) == 0;
    c2 = c1 + sum(isPolyTriFace);
    tface((c1+1):c2,:) = face(isPolyTriFace,:);
    tface2elem((c1+1):c2,:) = intIdx(isPolyTriFace); % the indices of the polyhedron 
                                                     % which the tri faces
                                                     % blong to.
    c1 = c2;
    c2 = c1 + sum(isInterface);
    tface((c1+1):c2,:) = face(isInterface,:); % the interface tri faces.
    tface2elem((c1+1):c2) = intIdx(isInterface); % the indices of the polyhedron
                                                 % which the interface tri
                                                 % faces blong to.
    c1 = c2;
    c4 = c3 + sum(isInterface);
    interface((c3+1):c4,:) = face(isInterface,:);% We just keep the interface 
                                                 % triangle which normal
                                                 % point outside of the
                                                 % interface.
    c3 = c4;
end
interface((c3+1):end,:) = [];

% find the exterior tet elements 
extTet = tetElem(~isIntTet, :);
% find the cube indices which the exterior tet elements belong to.
extIdx = idx(~isIntTet);
NT = size(extTet,1);
T = auxstructure3(extTet);
neighbor = T.neighbor;
clear T;
tmp = (1:NT)';

for i = 1:4
    face = extTet(:, localFace(i,:));
    % find the triangle face between small cubes.
    isPolyTriFace = neighbor(:, i) ~= tmp & extIdx ~= extIdx(neighbor(:,i));
    % find the triangle faces which are on interface, and their normal points to interior. 
    isInterface = neighbor(:, i) == tmp & sum(abs(vSign(face)), 2) == 0;
    
    c2 = c1 + sum(isPolyTriFace);
    tface((c1+1):c2,:) = face(isPolyTriFace,:);
    tface2elem((c1+1):c2,:) = extIdx(isPolyTriFace)+NC; % the indices of the polyhedron
                                                        % the tri faces
                                                        % blong to. Here we
                                                        % plus NC to make
                                                        % them unique, and NC
                                                        % is the total
                                                        % number of the
                                                        % cubes.
    c1 = c2;
    c2 = c1 + sum(isInterface);
    tface((c1+1):c2,:) = face(isInterface,:);
    tface2elem((c1+1):c2) = extIdx(isInterface) + NC;
    c1 = c2;
end

tface((c1+1):end,:) = [];
tface2elem((c1+1):end) = [];

face2elem = [tface2elem;sface2elem];


%% prepare data for return
interfaceData.elemIdx = elemIdx;
interfaceData.face = [tface,zeros(size(tface,1),1);sface];
interfaceData.face2elem = face2elem;
interfaceData.vSign = vSign;
interfaceData.interface = interface;
interfaceData.cutElem = cutElem;

end
