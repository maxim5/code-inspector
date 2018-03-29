% gc_rousson_deriche   Calculate Rousson-Deriche segmentation via graph cuts.
%
% SYNOPSIS:
%  [seg energy iter c1 v1 c2 v2] = gc_rousson_deriche(img, lambda, nb, flow, log, conv, maxiter)
%
% EXAMPLE:
%  a = imread('somefile.tif');
%  an = gc_normalize_image(single(a));
%  [seg energy iter c1 v1 c2 v2] = gc_rousson_deriche(an, 1);
%
% PARAMETERS:
%  img: Input image. It is recommended to normalize the image using
%   gc_normalize_image function to better balance the regularization and
%   data terms of the functional. Smoothing using a gaussian filter 
%   is recommended prior to normalization and segmentation.
%  lambda: Data term weight. Greater than 0.
%  nb: Neighbourhood type. One of:
%    - for 2D: 'N4', 'N8', 'N16' and 'N32'
%    - for 3D: 'N6', 'N18', 'N26' and 'N98' (equivalent of N16 in 3D)
%  flow: Maximum flow algorithm. One of:
%    - 'GRD-KO' - Dynamic Boykov-Kolmogorov algorithm for grid graphs 
%                 (Pushmeet Kohli, et al.) - RECOMMENDED in most
%                 situations. Warning: returns incorrect energy value.
%    - 'GRD-PRF' - Push-Relabel algorithm for grid graphs with FIFO
%                 selection rule.
%    - 'GRD-PRH' - Push-Relabel algorithm for grid graphs with
%                 highest-level selection rule.
%  log: Enable/disable logging.
%  conv: Convergence criterion. The minimization is stopped when the sum of
%   deltas of the mean values is less than this number.
%  maxiter: Maximum number of iterations.
%
%  seg: Final labeling.
%  energy: Energy of the final labeling.
%  iter: Number of iterations performed.
%  c1: Background mean value.
%  v1: Background intensity variance.
%  c2: Foreground mean value.
%  v2: Foreground intensity variance.
%
% PARAMETERS:
%  img        SINGLE[] or DOUBLE[]
%  lambda     DOUBLE
%  nb         STRING
%  flow       STRING
%  log        LOGICAL
%  conv       DOUBLE
%  maxiter    DOUBLE
%
%  seg        UINT8[]
%  energy     DOUBLE
%  iter       DOUBLE
%  c1         DOUBLE
%  v1         DOUBLE
%  c2         DOUBLE
%  v2         DOUBLE
%
% DEFAULTS:
%  img        compulsory
%  lambda     compulsory
%  nb         'N16' in 2D, 'N26' in 3D
%  flow       'GRD-KO'
%  log        false
%  conv       0.001
%  maxiter    30
%
% DESCRIPTION:
%  Computes the Rousson-Deriche segmentation of an image 
%  with a given data term weight via graph-cut based optimization.
%
% NOTES:
%  The data type of the input image determines also the precision used
%  during the minimization. Images of constant intensity are not supported. 
%  If method returns a 'Convergence error' than your parameters are probably 
%  too strong.
%
% LITERATURE:
%  [1] M. Rousson and R. Deriche. A variational framework for active and adaptative 
%      segmentation of vector valued images. Proceedings of IEEE Workshop on Motion 
%      and Video Computing, 2002, pp. 56-61
%
% SEE ALSO:
%  gc_normalize_image
%
% Copyright (c) 2010 Centre for Biomedical Image Analysis
% Copyright (c) 2010 Ondrej Danek

function [seg energy iter c1 v1 c2 v2] = gc_rousson_deriche(img, lambda, nb, flow, log, conv, maxiter)

% Check input data type
if (isfloat(img) == false)
    error('Use only with SINGLE[] or DOUBLE[] images.');
end

% Check parameter count
if (nargin < 2)
    error('At least 2 parameters required.');
end

% Supply default parameters if necessary
if (nargin < 3)
    % Default neighbourhood size
    if (ndims(img) == 2)
        nb = 'N16';
    else
        nb = 'N26';
    end
end

if (nargin < 4)
    % Default maximum flow algorithm
    flow = 'GRD-KO';
end

if (nargin < 5)
    % Disable logging by default
    log = false;    
end

if (nargin < 6)
    % Default convergence criterion
    conv = 0.001;    
end

if (nargin < 7)
    % Default maximum number of iterations
    maxiter = 30;
end

[seg energy iter c1 v1 c2 v2] = GcRoussonDeriche(img, lambda, conv, maxiter, nb, flow, log);

end
