function block_SPIHT(soubor, level, bpp, bw, bh, steps)
% Block SPIHT using iterative linear SSIM-based bit allocation
% parameters:     soubor - input image file                
%                 level - transform depth
%                 bpp - bits per pixel quantifier
%                 bw - block width  
%                 bh - block height
%                 (steps) - number of iterations

if(nargin < 6)
    steps=1;
end

if(nargin < 5)
    disp('5 parameters required!');
    return;
end

% TICK THIS TO PROCESS ONLY PLANE Y
% "grayscales" color image
color = 0;

% init
image = floor(double(rgb2ycbcr(imread(soubor))));

% run for all channels
size_x = size(image,2);
size_y = size(image,1);
planes = size(image,3);

mode = 'B';

% blocking schema
disp(['image size ' num2str(size_x) ' x ' num2str(size_y)]);

tile=[];
image=image-128;

if color == 0
    image(:,:,2) = zeros(size_y, size_x);
    image(:,:,3) = zeros(size_y, size_x);
    planes=1;
end

% image -> DWT domain
for i=1:planes
    tile(:,:,i) = waveletcdf97(image(:,:,i), level);
end

% dSPIHT encoder
% bpp -> bits count
bpp_full = 8;
bytes = ceil((bpp/bpp_full)*(size_x*size_y*planes));

eff = bytes;
bits = 8*bytes;

% KEY POINTS IN ALGORITHM:

% 1. PREPARE BANDSIZE W / H
bandsizew = 2^(log2(size_x) - level);
bandsizeh = 2^(log2(size_y) - level);
block_bsw = 2^(log2(bw) - level);
block_bsh = 2^(log2(bh) - level);

% 1.b. check sizes
if(mod(size_x,bw) ~= 0 || mod(size_y,bh) ~= 0)
    disp('size not fully divisible by blocks');
    return
end
if(bandsizew < 2 || mod(bandsizew, 2) > 0 || bandsizeh < 2 || mod(bandsizeh, 2) > 0 )
    disp('error, bandsize is too small');
    return
end
if(block_bsw < 2 || mod(block_bsw, 2) > 0 || block_bsh < 2 || mod(block_bsh, 2) > 0 )
    disp('error, block is too small, every block must has at least 2x2 in LL band');
    return
end
blocks = (size_x * size_y) / (bw * bh);

img1 = double(image(:,:,1))+128.0;


% 2. FOR EVERY BLOCK FIND LIMITS IN BANDSIZE - X-Y-X2-Y2
b=0;
max_BB = bits;
% stream = {};
% for j=1:block_bsh:bandsizeh
%     for i=1:block_bsw:bandsizew
%         b = b + 1;
% %         disp(['block ' num2str(b) ' - roots: x:' num2str(i) ' y:' num2str(j) ' x2:' num2str(i+block_bsw-1) ' y2:' num2str(j+block_bsh-1) ])
% %         
% %         3. SPIHT-OVERCODE EVERY BLOCK WITH MAX BB.
% %              NEEDED PARAMS:  IMAGE, LEVEL, X, Y, X2, Y2 IN BANDSIZE, BB
% %              OUTPUTS:        BLOCK STREAM WITH NMAX AT BEGIN
%         stream{b} = encodeSPIHTBlock(tile(:,:,1), level, i, j, i+block_bsw-1, j+block_bsh-1, max_BB);
%     end
% end
% 
% % save stream
% save 'stream.mat' stream;
% return
load stream.mat

% --- ALGORITHM WILL BEGIN HERE
% 4. TRUNCATION: COMPUTE STOP BIT FOR EVERY BLOCK

% initial stop bit. compute from block band size square
x0 = (block_bsw * block_bsh) / 4;
percentEqual = 0.01;    % how much of equal distribution is performed
slopeGrade = 1;
xb_full = floor(((max_BB - x0*blocks) / blocks) / steps);
% create decoding canvas & init block iterator

xa = zeros(blocks,1)+x0;
xb = zeros(blocks,1)+x0+xb_full;

for l=1:blocks
    m{l} = [0; 0];
end

% for every step:
for s=1:steps+1
    tile2 = zeros(size_y, size_x);
    tile3 = zeros(size_y, size_x);
    b=0;
      
    disp(['step ' num2str(s)]);
    % 5. CREATE TWO SSIM MAPS OF DECODED XA, XB PARTS
    for j=1:block_bsh:bandsizeh
        for i=1:block_bsw:bandsizew
            b = b + 1;
            % 5a. DECODE BLOCKS INTO TILES WITH GIVEN PARTIAL TRUNCATION
            %       NEEDED PARAMS:  IMAGE, LEVEL, X, Y, X2, Y2, BLOCK STREAM, STOP BIT
            %       OUTPUTS:        UPDATED IMAGE
            tile2 = decodeSPIHTBlock(tile2, level, i, j, i+block_bsw-1, j+block_bsh-1, stream{b}, xa(b));
            tile3 = decodeSPIHTBlock(tile3, level, i, j, i+block_bsw-1, j+block_bsh-1, stream{b}, xb(b));
        end
    end    
    % 5b. RECONSTRUCT FINISHED TILES
    recon2 = waveletcdf97(tile2, -level);
    recon3 = waveletcdf97(tile3, -level);
    img2 = double(recon2)+128.0;
    img3 = double(recon3)+128.0;
    % 5c. COMPUTE SSIM MAPS OF TILES
    ssim2 = ssim(img1, img2);
    ssim3 = ssim(img1, img3);
    % 6. USE SA, SB FOR EVERY BLOCK TO DETERMINE NEXT XB
    k = zeros(blocks,1);
    Sas = zeros(blocks,1);
    Sbs = zeros(blocks,1);
    u = zeros(blocks,1);
    b=0;
    for j=1:block_bsh:bandsizeh
        for i=1:block_bsw:bandsizew
            b = b + 1;
            % 6a. COMPUTE SA, SB BY TAKING MEAN FROM BLOCK
            Sa = ssimMean(ssim2,i,j,i+block_bsw-1,j+block_bsh-1);
            Sas(b) = Sa;
            Sb = ssimMean(ssim3,i,j,i+block_bsw-1,j+block_bsh-1);
            Sbs(b) = Sb;
            % 6b. COMPUTE THE TANGENT
            k(b) = (Sb - Sa) / (xb(b) - xa(b));                            
            disp(['block ' num2str(b) ' has: Sa(' num2str(xa(b)) ')=' num2str(Sa) ', Sb(' num2str(xb(b)) ')=' num2str(Sb) ', k=' num2str(k(b)) ]);                    
        end
    end

    if s == steps+1    
        for b=1:blocks
            m{b} = [m{b} [Sas(b); xa(b)]]; 
        end
        break;
    end


    % 6c. DETERMINE INTERSECTION WITH S=1
    %       FOR ALL BLOCKS
    % (k(l) / max(k))
    for l=1:blocks        
        if k(l) > 0 
            mean = (Sas(l) + Sbs(l)) / 2;
            u(l) =  (1/mean-1.0) * (k(l)/max(k)) * ((1.0 - k(l)*xa(l) + Sas(l)) / k(l));
        else
            u(l) = 1;
        end
    end
    % 6d. DETERMINE SCORE TOTAL
    u_tot = sum(u);
    xb_new = zeros(b,1);
    % 6e. COMPUTE xb SHIFTS
    disp(':::');
    add_sum = 0;
    for l=1:blocks
        add = floor(xb_full*percentEqual + floor((u(l)/u_tot)*((xb_full-xb_full*percentEqual)*blocks)));
        add_sum = add_sum + add;
        xb_new(l) = xa(l) + add;        
        disp(['block ' num2str(l) ' has u=' num2str(u(l)) ', intersected in ' num2str((1.0 - Sas(l)) / k(l)) ', k_to_max=' num2str((k(l)/max(k))) ', add=' num2str(add)]);
    end
    disp(['---' num2str(add_sum) '/' num2str(xb_full*blocks)]);
    % 7.  INIT NEW xa, xb FOR NEXT CYCLE
    for b=1:blocks
        m{b} = [m{b} [Sas(b); xa(b)]]; 
    end

    xa = xb_new;
    xb = xb_new + xb_full;    
end
    
% --- ALGORITHM ENDS HERE
% 8. FINAL EVALUATION
% create equally compressed version
plotM(m,[1 2 3 4]);

b=0;
tile = zeros(size_y, size_x);
tile_eq = zeros(size_y, size_x);
for j=1:block_bsh:bandsizeh
    for i=1:block_bsw:bandsizew
        b=b+1;
        tile = decodeSPIHTBlock(tile, level, i, j, i+block_bsw-1, j+block_bsh-1, stream{b}, xb(b));
        tile_eq = decodeSPIHTBlock(tile_eq, level, i, j, i+block_bsw-1, j+block_bsh-1, stream{b}, round(max_BB / blocks));
    end
end

recon(:,:,1) = waveletcdf97(tile, -level);
recon(:,:,2) = image(:,:,2);
recon(:,:,3) = image(:,:,3);

recon_eq(:,:,1) = waveletcdf97(tile_eq, -level);
recon_eq(:,:,2) = image(:,:,2);
recon_eq(:,:,3) = image(:,:,3);

img1 = double(image(:,:,1))+128.0;
img2 = double(recon(:,:,1))+128.0;
img3 = double(recon_eq(:,:,1))+128.0;

z = ssim(img1, img2);
MSSIM = mean2(z)

z_eq = ssim(img1, img3);
MSSIM_eq = mean2(z_eq)


recon = recon + 128;
recon_eq = recon_eq + 128;
image = ycbcr2rgb(uint8(image+128));
recon = ycbcr2rgb(uint8(recon));
recon_eq = ycbcr2rgb(uint8(recon_eq));

% % PSNR + rounding
% sum_1 = 0;
% for i=1:size_y
%     for j=1:size_x
%         sum_1 = sum_1 + double((image(i,j,1) - recon(i,j,1))^2);
%     end
% end
% 
% MSE = sum_1/(size_y*size_x);
% RMSE = sqrt(double(MSE));
% PSNR = 20*log10(255/RMSE)*100;
% PSNR = round(PSNR);
% PSNR = PSNR/100
PSNR=0;
% show results
str_orig = ['Original, dimensions=' num2str(size_x) 'x' num2str(size_x) 'px, size=' num2str(size_x*size_y*planes) 'B'];
str_dec = ['DWT-SPIHT result, size=' num2str(bytes) 'B (1:' num2str(round((size_x*size_y*planes)/bytes)) ' / ' num2str(bpp) 'bpp), PSNR=' num2str(PSNR) 'dB'];

figure(1);
subplot(2,2,1);
imshow(uint8(image), 'InitialMagnification', 100);
title(str_orig);
    
subplot(2,2,3);
imshow(recon, 'InitialMagnification', 100);
title(str_dec);

subplot(2,2,4);
imshow(recon_eq, 'InitialMagnification', 100);
title(str_dec);



