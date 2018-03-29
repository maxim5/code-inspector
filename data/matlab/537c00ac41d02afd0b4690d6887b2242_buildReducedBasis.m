function [ Phi_approx, Phi_hat, Phi_interior, Phi_mfem_o ] = ...
  buildReducedBasis( Kfree, Mfree, ratio, numEle, c, quadSize, ...
  do_approx, do_hat, do_interior, do_mfem_o )
%
% buildReducedBasis Computes all the basis functions
% for the multiscale finite element methods studied
% in the M2AN paper.
%
%  UH
%  Last update: 2011/05/24
%

%-------------------------------------------
%
% Start building the reduced order models
%
%-------------------------------------------

[quadPoint, quadWeight] = GaussQuad(quadSize);

nEle_coarse = numEle / ratio;

%
%--- Compute the smallest interior modes
%
Phi_interior = [];
if (do_interior > 0)
  %------------
  tmpInterior = [];
  for jTmp = 1:(ratio - 1),
    tmpInterior = [tmpInterior; ( jTmp * (numEle + 1) + (2:ratio) )'];
  end
  %------------
  iPhi = [];
  jPhi = [];
  vPhi = [];
  counter = 0;
  nev = 10;
  %------------
  for jy = 1:nEle_coarse,
    for ix = 1:nEle_coarse,
      %---
      interior = tmpInterior + (ix - 1) * ratio + (jy - 1) * ratio * (numEle + 1);
      %---
      if (size(tmpInterior, 1) <= 1025)
        %fprintf(' Use eig \n');
        [Z, T] = eig(full(Kfree(interior, interior)), full(Mfree(interior, interior)));
      else
        %fprintf(' Use eigs \n');
        OPTS.disp = 0;
        OPTS.p = 36;
        OPTS.maxit = 500;
        [Z, T] = eigs(Kfree(interior, interior), Mfree(interior, interior), ...
          1, 'SM', OPTS);
        %diag(T)
      end
      k = 1;
      %---
      for ik = 1:k,
        counter = counter + 1;
        iPhi = [iPhi; interior];
        jPhi = [jPhi; kron([counter], ones(size(interior, 1), 1))];
        vPhi = [vPhi; Z(:, ik)];
      end
      %---
      clear Z;
      clear T;
      clear PhiTmp;
      %---
    end
  end
  %-------
  Phi_interior = sparse(iPhi, jPhi, vPhi, size(Kfree, 1), counter);
  %-------
  fprintf(' ... Done Computing Interior Modes ... \n');
  clear iPhi; clear jPhi; clear vPhi; clear counter;
  clear tmpInterior;
end

%
%--- Compute the nodal hat function on coarse nodes
%
Phi_hat = [];
if (do_hat > 0)
  %----------
  val = [1:ratio, (ratio-1):-1:1]'/ratio;
  %----------
  tmpInterior = [];
  for jTmp = (-ratio + 1):(ratio - 1),
    tmpInterior = [tmpInterior; ( jTmp * (numEle + 1) + ( (-ratio + 2):ratio ) )'];
  end
  %----------
  for jy = 2:nEle_coarse,
    for ix = 2:nEle_coarse,
      PhiTmp = zeros(size(Kfree, 1), 1);
      %--- Vertical interface
      interface_y =  (((jy-2) * ratio + 1):(jy * ratio - 1)) * (numEle + 1);
      interface_y = interface_y + (ix - 1) * ratio + 1;
      PhiTmp(interface_y) = val;
      %--- Horizontal interface
      interface_x = ((ix - 2) * ratio + 2):(ix * ratio);
      interface_x = interface_x + (jy - 1) * ratio * (numEle + 1);
      PhiTmp(interface_x) = val;
      %---
      interface = union(interface_x, interface_y);
      %--- Solve for the interior values
      interior = tmpInterior + (ix - 1) * ratio + (jy - 1) * ratio * (numEle + 1);
      freeInterior = setdiff(interior, interface);
      if (size(freeInterior, 1) > 0)
        PhiTmp(freeInterior)=-Kfree(freeInterior,interface)*PhiTmp(interface);
        PhiTmp(freeInterior)= Kfree(freeInterior, freeInterior)\PhiTmp(freeInterior);
      end
      %--- Increment the counter
      PhiTmp = real(sparse(PhiTmp));
      Phi_hat = real(sparse([Phi_hat, PhiTmp]));
      %----
    end
  end
  clear interface_y; clear interface_x; clear interface;
  clear val; clear tmpInterior;
  fprintf(' ... Done Computing Hat Functions ... \n');
end


%
%--- Compute the shape functions from MFEM-O
%


Phi_mfem_o = [];
if (do_mfem_o > 0)
  %----------
  tmpInterior = [];
  for jTmp = (-ratio + 1):(ratio - 1),
    tmpInterior = [tmpInterior; ( jTmp * (numEle + 1) + ( (-ratio + 2):ratio ) )'];
  end
  %----------
  for jy = 2:nEle_coarse,
    for ix = 2:nEle_coarse,
      PhiTmp = zeros(size(Kfree, 1), 1);
      interface = [];
      %--- Vertical interface
      interface_y =  (((jy-2) * ratio):((jy - 1) * ratio)) * (numEle + 1);
      interface_y = interface_y + (ix - 1) * ratio + 1;
      interface = union(interface, interface_y);
      %----------------
      x0 = (ix - 1) * ratio / numEle;
      y0 = (jy - 2) * ratio / numEle;
      val = [0.0];
      for iquad = 1:ratio,
        a = y0 + (iquad - 1) / numEle;
        b = a + 1.0 / numEle;
        vTmp = 0.0;
 %
 % 2011/05/24 UH
 % There was an error in the integral computation
 % The sum was not taken (i.e. using a point integration rule)
 % The composite rule is replaced with Gaussian quadrature.
 %
        for jquad = 1:quadSize,
          pa = a + (quadPoint(jquad) + 1)*(b - a)/2;
          ca = 1.0/c(x0,pa);
          vTmp = vTmp + ca * quadWeight(jquad) * (b-a)/2;
        end
%        for jquad = 0:63,
%          pa = a + jquad / (numEle * 64);
%          pb = a + (jquad + 1) / (numEle * 64);
%          ca = 1.0/c(x0,pa);
%          cb = 1.0/c(x0,pb);
%          vTmp = vTmp + 0.5 * (ca + cb) * (b - a) / 64;
%          %vTmp = 0.5 * (ca + cb) * (b - a) / 64;
%        end
        val = [val; vTmp];
      end
      val = cumsum(val);
      val = val / val(size(val, 1));
      %----------------
      PhiTmp(interface_y) = val;
      interface_y =  (((jy-1) * ratio):(jy * ratio)) * (numEle + 1);
      interface_y = interface_y + (ix - 1) * ratio + 1;
      %----------------
      x0 = (ix - 1) * ratio / numEle;
      y0 = (jy - 1) * ratio / numEle;
      val = [0.0];
      for iquad = 1:ratio,
        a = y0 + (iquad - 1) / numEle;
        b = a + 1.0 / numEle;
        vTmp = 0.0;
        for jquad = 1:quadSize,
          pa = a + (quadPoint(jquad) + 1)*(b - a)/2;
          ca = 1.0/c(x0,pa);
          vTmp = vTmp + ca * quadWeight(jquad) * (b-a)/2;
        end
%        for jquad = 0:63,
%          pa = a + jquad / (numEle * 64);
%          pb = a + (jquad + 1) / (numEle * 64);
%          ca = 1.0/c(x0,pa);
%          cb = 1.0/c(x0,pb);
%          vTmp = vTmp + 0.5 * (ca + cb) * (b - a) / 64;
%          %vTmp = 0.5 * (ca + cb) * (b - a) / 64;
%        end
        val = [val; vTmp];
      end
      val = cumsum(val);
      val = 1.0 - val / val(size(val, 1));
      %----------------
      PhiTmp(interface_y) = val;
      %----------------
      interface = union(interface, interface_y);
      %--- Horizontal interface
      interface_x = ((ix - 2) * ratio + 1):((ix - 1) * ratio + 1);
      interface_x = interface_x + (jy - 1) * ratio * (numEle + 1);
      %----------------
      x0 = (ix - 2) * ratio / numEle;
      y0 = (jy - 1) * ratio / numEle;
      val = [0.0];
      for iquad = 1:ratio,
        a = x0 + (iquad - 1) / numEle;
        b = a + 1.0 / numEle;
        vTmp = 0.0;
        for jquad = 1:quadSize,
          pa = a + (quadPoint(jquad) + 1)*(b - a)/2;
          ca = 1.0/c(pa,y0);
          vTmp = vTmp + ca * quadWeight(jquad) * (b-a)/2;
        end
%        for jquad = 0:63,
%          pa = a + jquad / (numEle * 64);
%          pb = a + (jquad + 1) / (numEle * 64);
%          ca = 1.0/c(pa,y0);
%          cb = 1.0/c(pb,y0);
%          vTmp = vTmp + 0.5 * (ca + cb) * (b - a) / 64;
%          %vTmp = 0.5 * (ca + cb) * (b - a) / 64;
%        end
        val = [val; vTmp];
      end
      val = cumsum(val);
      val = val / val(size(val, 1));
      %----------------
      PhiTmp(interface_x) = val;
      %----------------
      interface = union(interface, interface_x);
      %----------------
      interface_x = ((ix - 1) * ratio + 1):(ix * ratio + 1);
      interface_x = interface_x + (jy - 1) * ratio * (numEle + 1);
      x0 = (ix - 1) * ratio / numEle;
      y0 = (jy - 1) * ratio / numEle;
      val = [0.0];
      for iquad = 1:ratio,
        a = x0 + (iquad - 1) / numEle;
        b = a + 1.0 / numEle;
        vTmp = 0.0;
        for jquad = 1:quadSize,
          pa = a + (quadPoint(jquad) + 1)*(b - a)/2;
          ca = 1.0/c(pa,y0);
          vTmp = vTmp + ca * quadWeight(jquad) * (b-a)/2;
        end
%        for jquad = 0:63,
%          pa = a + jquad / (numEle * 64);
%          pb = a + (jquad + 1) / (numEle * 64);
%          ca = 1.0/c(pa,y0);
%          cb = 1.0/c(pb,y0);
%          vTmp = vTmp + 0.5 * (ca + cb) * (b - a) / 64;
%          %vTmp = 0.5 * (ca + cb) * (b - a) / 64;
%        end
        val = [val; vTmp];
      end
      val = cumsum(val);
      val = 1.0 - val / val(size(val, 1));
      %----------------
      PhiTmp(interface_x) = val;
      %----------------
      interface = union(interface, interface_x);
      %----------------
      %--- Solve for the interior values
      interior = tmpInterior + (ix - 1) * ratio + (jy - 1) * ratio * (numEle + 1);
      freeInterior = setdiff(interior, interface);
      if (size(freeInterior, 1) > 0)
        PhiTmp(freeInterior)=-Kfree(freeInterior,interface)*PhiTmp(interface);
        PhiTmp(freeInterior)=Kfree(freeInterior, freeInterior)\PhiTmp(freeInterior);
      end
      %--- Increment the counter
      PhiTmp = real(sparse(PhiTmp));
      Phi_mfem_o = real(sparse([Phi_mfem_o, PhiTmp]));
      %----
    end
  end
  clear interface_y; clear interface_x; clear interface;
  clear val; clear tmpInterior;
  fprintf(' ... Done Computing MFEM-O Functions ...\n');
end



%
%--- Compute approximate basis on the interface
%
Phi_approx = [];
if (do_approx > 0)
  %-------------
  tmpInterior = [];
  for jTmp = 1:(ratio - 1),
    tmpInterior = [tmpInterior; ( jTmp * (numEle + 1) + (2:ratio) )'];
  end
  %-------------
  for iPass = 1:2,
    %-------------
    if (iPass == 1)
      tmpInterface = (2:ratio)';
      ix_start = 1;
      jy_start = 2;
    else
      tmpInterface = ( 1 + (1:(ratio - 1)) * (numEle + 1) )';
      ix_start = 2;
      jy_start = 1;
    end
    %-------------
    for jy = jy_start:nEle_coarse,
      for ix = ix_start:nEle_coarse,
        if (iPass == 1)
          interior = tmpInterior + (ix - 1) * ratio + (jy - 2) * ratio * (numEle + 1);
        else
          interior = tmpInterior + (ix - 2) * ratio + (jy - 1) * ratio * (numEle + 1);
        end
        interior = [interior; tmpInterior + (ix - 1) * ratio + (jy - 1) * ratio * (numEle + 1)];
        interface = tmpInterface + (ix - 1) * ratio + (jy - 1) * ratio * (numEle + 1);
        %------
        MatTmp = Kfree(interior, interior) \ Kfree(interior, interface);
        %---
        SchurK = Kfree(interface, interface);
        SchurK = SchurK - Kfree(interface, interior) * MatTmp;
        SchurK = real(triu(SchurK) + triu(SchurK, 1)');
        %---
        SchurM = - Mfree(interface, interior) * MatTmp;
        SchurM = SchurM + SchurM';
        SchurM = SchurM + Mfree(interface, interface);
        SchurM = SchurM + MatTmp' * (Mfree(interior, interior) * MatTmp);
        SchurM = real(triu(SchurM) + triu(SchurM, 1)');
        %---
        if (size(SchurK, 1) <= 1025)
          [Z, T] = eig(full(SchurK), full(SchurM));
        else
          OPTS.disp = 0;
          OPTS.p = 36;
          OPTS.maxit = 500;
          [Z, T] = eigs(SchurK, SchurM, 1, 'SM', OPTS);
        end
        k = 1;
        %---
        PhiTmp = sparse(size(Kfree, 1), k);
        PhiTmp(interface, :) = Z(:, 1:k);
        PhiTmp(interior, :) = - MatTmp * Z(:, 1:k);
        %---
        PhiTmp = real(sparse(PhiTmp));
        Phi_approx = real(sparse([Phi_approx, PhiTmp]));
        clear PhiTmp;
        %---
      end
    end
  end
  fprintf(' ... Done Computing Approximate Basis ... \n');
end

end

