% gc_chan_vese   Calculate Chan-Vese segmentation via graph cuts.
%
% SYNOPSIS:
%  [seg energy iter fc1 fc2] = gc_chan_vese(img, mu, lambda1, lambda2,
%       mask, nb, flow, log, conv, maxiter, c1, c2)
%
% EXAMPLE:
%  a = imread('somefile.tif');
%  an = gc_normalize_image(single(a));
%  [seg energy iter c1 c2] = gc_chan_vese(an, 1, 10, 10);
%
% PARAMETERS:
%  img: Input image. It is recommended to normalize the image using
%   gc_normalize_image function to better balance the regularization and
%   data terms of the functional. Smoothing using a gaussian filter 
%   is recommended prior to normalization and segmentation.
%  mu: Countour length weight. Regularization parameter. Greater than 0.
%  lambda1: Background uniformity weight. Greater than 0.
%  lambda2: Foreground uniformity weight. Greater than 0.
%  mask: Optional voxel mask. Can be used to precisely specify computational 
%   domain and hard constraints. It must have the same size as the input image 
%   and each element can have one of the following values:
%    - 0 - Masked voxel. These voxels are not taken into account during the
%    computation.
%    - 1 - Background hard constraint. This voxel is known to belong to the
%    background.
%    - 2 - Foreground hard constraint. This voxel is known to belong to the
%    foreground.
%    - 3 - Voxel to be classified.
%   If no mask is to be applied use [] as the parameter value.
%  nb: Neighbourhood type. One of:
%    - for 2D: 'N4', 'N8', 'N16' and 'N32'
%    - for 3D: 'N6', 'N18', 'N26' and 'N98' (equivalent of N16 in 3D)
%  flow: Maximum flow algorithm. One of:
%    - 'GEN-FF' - Ford-Fulkerson algorithm for general graphs. Warning
%                 EXTREMELY slow.
%    - 'GEN-EK' - Edmonds-Karp algorithm for general graphs. Warning 
%                 EXTREMELY slow.
%    - 'GEN-DI' - Dinitz algorithm for general graphs.
%    - 'GEN-BK' - Boykov-Kolmogorov algorithm for general graphs.
%    - 'GEN-KO' - Dynamic Boykov-Kolmogorov algorithm for general graphs 
%                 (Pushmeet Kohli, et al.). Warning: returns incorrect
%                 energy value.
%    - 'GEN-PRF' - Push-Relabel algorithm for general graphs with FIFO
%                 selection rule.
%    - 'GEN-PRH' - Push-Relabel algorithm for general graphs with
%                 highest-level selection rule.
%    - 'GRD-KO' - Dynamic Boykov-Kolmogorov algorithm for grid graphs 
%                 (Pushmeet Kohli, et al.) - RECOMMENDED in most
%                 situations. Warning: returns incorrect energy value.
%    - 'GRD-PRF' - Push-Relabel algorithm for grid graphs with FIFO
%                 selection rule.
%    - 'GRD-PRH' - Push-Relabel algorithm for grid graphs with
%                 highest-level selection rule.
%  log: Enable/disable logging.
%  conv: Convergence criterion. The minimization is stopped when the sum of
%   c1 and c2 deltas is less than this number.
%  maxiter: Maximum number of iterations.
%  c1: Initial background mean value.
%  c2: Initial foreground mean value. Greater than c1.
%
%  seg: Segmentation mask.
%  energy: Energy of the final segmentation.
%  iter: Number of iterations performed.
%  fc1: Final background mean intensity.
%  fc2: Final foreground mean intensity.
%
% PARAMETERS:
%  img        SINGLE[] or DOUBLE[]
%  mu         DOUBLE
%  lambda1    DOUBLE
%  lambda2    DOUBLE
%  mask       UINT8[]
%  nb         STRING
%  flow       STRING
%  log        LOGICAL
%  conv       DOUBLE
%  maxiter    DOUBLE
%  c1         DOUBLE
%  c2         DOUBLE
%
%  seg        LOGICAL[]
%  energy     DOUBLE
%  iter       DOUBLE
%  fc1        DOUBLE
%  fc2        DOUBLE
%
% DEFAULTS:
%  img        compulsory
%  mu         compulsory
%  lambda1    compulsory
%  lambda2    compulsory
%  mask       []
%  nb         'N16' in 2D, 'N26' in 3D
%  flow       'GRD-KO'
%  log        false
%  conv       0.001
%  maxiter    30
%  c1, c2     Initialized using gc_weighted_kmeans function
%
% DESCRIPTION:
%  Computes the Chan-Vese segmentation of input image with given weights
%  via graph-cut based optimization. The mask can be used to either reduce the 
%  computational domain (and reduce memory consumption and increase speed)
%  or to incorporate hard constraints (foreground or background) into the
%  computation.
%
% NOTES:
%  The data type of the input image determines also the precision used
%  during the minimization. Grid variants of the maximum flow algorithms
%  are recommended as they have much lower memory consumption and should be
%  also faster. 'GRD-KO' is possibly the best choice in most situations.
%  Only 'GRD-PRH' may be faster in special cases. To obtain a good initial
%  estimate of c1 and c2 use gc_weighted_kmeans function with the same
%  lambda1 and lambda2 parameters.
%
%  Images of constant intensity are not supported. If method returns a
%  'Convergence error' than your parameters are probably too strong causing
%  the minimization to end in a trivial minima (whole image either
%  foreground or background).
%
% LITERATURE:
%  [1] Y. Zeng, W. Chen, and Q. Peng. Efficiently solving the piecewise constant mumford-shah
%      model using graph cuts. Technical report, Dept. of Computer Science, Zhejiang University,
%      P.R. China, 2006.
%  [2] O. Danek and P. Matula: Graph Cuts ans Approximation of the
%      Euclidean Metric on Anisotropic Grids. In: VISAPP 2010 International
%      Conference on Computer Vision Theory and Applications, Volume 2,
%      pages 68-73, 2010
%  [3] T. F. Chan and L. A. Vese. Active contours without edges. IEEE Transactions on Image
%      Processing, 10(2):266-277, 2001.
%  [4] Y. Boykov and V. Kolmogorov. An experimental comparison of min-cut/max-flow 
%      algorithms for energy minimization in vision. IEEE Transactions on Pattern 
%      Analysis and Machine Intelligence, 26(9):1124-1137, 2004.
%  [5] P. Kohli and P. H. S. Torr. Efficiently solving dynamic markov random fields using graph
%      cuts. In ICCV '05: Proceedings of the Tenth IEEE International Conference on Computer
%      Vision, pages 922-929, 2005.
%  [6] A. V. Goldberg and R. E. Tarjan. A new approach to the maximum flow problem. In
%      STOC '86: Proceedings of the eighteenth annual ACM symposium on Theory of computing,
%      pages 136-146, 1986.
%
% SEE ALSO:
%  gc_normalize_image, gc_weighted_kmeans, gc_chan_vese_two_stage
%
% Copyright (c) 2010 Centre for Biomedical Image Analysis
% Copyright (c) 2010 Ondrej Danek

function [seg energy iter fc1 fc2] = gc_chan_vese(img, mu, lambda1, lambda2, mask, nb, flow, log, conv, maxiter, c1, c2)

% Check input data type
if (isfloat(img) == false)
    error('Use only with SINGLE[] or DOUBLE[] images.');
end

% Check parameter count
if (nargin < 4)
    error('At least 4 parameters required.');
end

if (nargin == 11)
    error('c1 and c2 have to be specified either both or none.');
end

% Supply default parameters if necessary
if (nargin < 6)
    % Default neighbourhood size
    if (ndims(img) == 2)
        nb = 'N16';
    else
        nb = 'N26';
    end
end

if (nargin < 7)
    % Default maximum flow algorithm
    flow = 'GRD-KO';
end

if (nargin < 8)
    % Disable logging by default
    log = false;    
end

if (nargin < 9)
    % Default convergence criterion
    conv = 0.001;    
end

if (nargin < 10)
    % Default maximum number of iterations
    maxiter = 30;
end

if (nargin < 12)
    % Init c1 and c2 using weighted kmeans algorithm
    [c1 c2] = gc_weighted_kmeans(img, lambda1, lambda2, 0.001, 30);
end

% Compute segmentation
if (nargin == 4 || numel(mask) == 0)
    % Compute unmasked segmentation
    [seg energy iter fc1 fc2] = GcChanVese(img, lambda1 / mu, lambda2 / mu, c1, c2, conv, maxiter, nb, flow, log);
else
    % Compute masked segmentation
    [seg energy iter fc1 fc2] = GcChanVese(img, lambda1 / mu, lambda2 / mu, c1, c2, conv, maxiter, nb, flow, log, mask);
end

end
