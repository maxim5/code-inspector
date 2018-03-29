% multiprofiles Draw several profiles on a same plot
%
% [] = multiprofiles(T,OPTIONS)
% 
% Draw several profiles on a same plot.
%
% OPTIONS:
%	ztyp: one of T.geo properties which can be used as a vertical axis
%		Default is: 'DEPH'
%	VARN: a cell of strings with the data names to plot
%		Default is: {'TEMP';'PSAL'}
%	iS: station index to plot (Default is 1)
%	zlab: a string to be used as the vertical axis label
%	zdir: the direction ('normal' or 'reverse') of the vertical axis
%	zlim: 2 values with the vertical axis limits
%	xlim: 2 values with the horizontal axis limits in the case of a single variables
%		plotted for several stations.
%
% If the number of stations is 1, all variables are plotted on the same figure.
% If the number of stations is larger than 1 (given by iS), all stations profiles are
% plotted on the same plot, with one figure per variable(s).
%
% Eg:
%	multiprofiles(T,'VARN',{'TEMP';'PSAL'},'iS',12)
%	multiprofiles(T,'VARN',{'TEMP'},'iS',[1 4 10],'xlim',[-2 20])
%
%	T.geo.SIG0 = T.data.SIG0.cont;
%	multiprofiles(T,'ztyp','SIG0','zlab','\sigma_0 (kg/m^3)','zdir','reverse')
%
% Created: 2010-05-25.
% http://copoda.googlecode.com
% Copyright 2010, COPODA

% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:
%
% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.
%
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
% THE SOFTWARE.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function varargout = multiprofiles(T,varargin)
	
% Default options:
ztyp = 'DEPH';  % Y axis
VARN = {'TEMP';'PSAL'}; % List of variables to plot in X axis
iS   = 1; % Which station ?
zlab = '?';
zdir = 'normal';
xlim = 'auto';
zlim = 'auto';
		
Tref  = NaN; % Reference database		
iSref = NaN; % Profile in the reference database
	
plotype = 1; % Type of plot by default	
		
% User options:
for in = 2 : 2 : nargin-1
	eval(sprintf('%s=varargin{%i};',varargin{in-1},in));
end
if ischar(VARN)
	VARN = {VARN};
end% if 

switch ztyp
	case 'DEPH'
		zlab = 'Depth (m)';
		zdir = 'normal';
	case 'PRES'
		zlab = 'Pression (hPa)';
		zdir = 'reverse';
end

switch class(Tref)
	case 'transect'
		% We do have a reference database to plot on behalf of all profiles
		addref = true;
	otherwise
		addref = false;
end

if plotype ~= 3
	if length(VARN) >= 1 && length(iS) == 1
		plotype = 1;
	elseif length(iS) >= 1
		plotype = 2;
	end
end% if 

if isinf(iS)
	plotype = 3;
end% if 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Several variables at one station
if plotype == 1	
	
	%-- Manage new or clean figure:
	% If we called this function from the COPODA figure toolbar, we need to know what to do,
	% otherwise we simply create a new figure.
	
	% If we called this function from the COPODA figure toolbar, the 'copoda_figtoolbar.m'
	% file name must appear in the dbstack.
	st = dbstack; st = struct2cell(st);
	if ~isempty(intersect(st(1,:),'copoda_figtoolbar.m'))
		switch get(gcf,'tag')
			case 'profile_plot'
	%			disp('called from a profile plot, add more variables to the same figure')
				clf;
			case 'track_map'
	%			disp('Called from a track map, create a new figure')
				% Create a new figure
				figure;figure_tall
			otherwise
				disp('You called me from the COPODA figure toolbar but I don''t know what''s plotted here !')
				stophere
		end% switch 
	else	
		% Create a new figure
		figure;figure_tall
	end% if 

	%-- Plot:

	cmap = [0 0 0;1 0 0;0 0 1;0.2 .7 0.2];
	if length(VARN) > 4
		cmap = [cmap ;jet(length(VARN))];
	end
	if ischar(zlim)		
		try % If z is defined for each stations:
			z = subsref(T,substruct('.','geo','.',ztyp,'()',{iS,':'}));
		catch % Otherwise, regular vertical grid:
			z = subsref(T,substruct('.','geo','.',ztyp,'()',{1,':'}));		
		end
		zmin = nanmin(z(:));
		zmax = nanmax(z(:));
	else
		if length(zlim) ~= 2
			error('Z axis limit must be 2 values')
		end
		zlim = sort(zlim);
		zmin = zlim(1);
		zmax = zlim(2);
	end
	for iv = 1 : length(VARN)
		od = subsref(T,substruct('.','data','.',VARN{iv}));
		try % If z is defined for each stations:
			z = subsref(T,substruct('.','geo','.',ztyp,'()',{iS,':'}));
		catch % Otherwise, regular vertical grid:
			z = subsref(T,substruct('.','geo','.',ztyp,'()',{1,':'}));		
		end
		
		if addref
			odref = subsref(Tref,substruct('.','data','.',VARN{iv}));
			try % If z is defined for each stations:
				if isnan(iSref(1)),iSref=iS;end
				zref = subsref(Tref,substruct('.','geo','.',ztyp,'()',{iSref,':'}));
			catch % Otherwise, regular vertical grid:
				zref = subsref(Tref,substruct('.','geo','.',ztyp,'()',{1,':'}));		
			end
		end

					
		if iv == 1
			if size(od,2) == 1 % This is a N_PROF x 1 variable (defined at one level)
				error('Please, flip the variables order so that it starts with a profile variable !')
			end% if 

			hold on
			if addref			 	
				pl(iv)    = plot(od.cont(iS,:),z);hold on
				plref(iv) = plot(odref.cont(iSref,:),zref); 
			else
				pl(iv) = plot(od.cont(iS,:),z);
				set(gca,'tag',VARN{iv});				
			end
			ax_ref(iv) = gca; 
			set(pl(iv),'color',cmap(iv,:));
			set(ax_ref(iv),'XMinorTick','on','box','on','xcolor',get(pl(iv),'color'),'ydir',zdir);		
			xlabel(getxlab(od),'fontsize',8);
			grid on,box on;
			title(sprintf('%s\nLAT=%0.1f, LON=%0.1f, TIME=%s\nSTATION NUMBER %i, STATION TRANSECT INDEX %i',stamp(T,5),T.geo.LATITUDE(iS),T.geo.LONGITUDE(iS),datestr(T.geo.STATION_DATE(iS)),T.geo.STATION_NUMBER(iS),iS),'fontweight','bold');
			set(gcf,'name',sprintf('%s, STATION ID %i, #%i',stamp(T,5),T.geo.STATION_NUMBER(iS),iS));
			set(ax_ref(iv),'ylim',[zmin zmax]);
			ylabel(sprintf('%s',zlab));
		
			if addref
				set(plref(iv),'color',get(pl(iv),'color'),'linestyle','--','tag','reference_profile');				
			end
		
		else
			if size(od,2) == 1 % This is a N_PROF x 1 variable (defined at one level)
				% We can plot this variable at this station as a horizontal line.
				% but we need to check if the vertical axis is of the correct unit !
				switch ztyp
					case 'DEPH'
						if ~strcmp(od.unit,'m')
							error(sprintf('I cannot plot %s with this vertical axis (%s)',od.long_name,zlab));
						end					
					case 'PRES'
						if ~strcmp(od.unit,'db')
							error(sprintf('I cannot plot %s with this vertical axis (%s)',od.long_name,zlab));
						end
				end% switch 
				
				if exist('ax_plot','var')
					axes(ax_plot(end));hold on
				end% if 
				switch VARN{iv}
					case 'THH'
						try		
%							stophere
%							pl(iv) = plot(get(gca,'xlim'), [1 1]*od.cont(iS)+T.data.THD(iS),'color',cmap(iv,:));
%							pl(iv) = plot(get(gca,'xlim'),-[1 1]*od.cont(iS)+T.data.THD(iS),'color',cmap(iv,:));
%							t = text(max(get(gca,'xlim')),od.cont(iS)+T.data.THD(iS),sprintf('%s [%s]',VARN{iv},od.name));
							hl(iv,1) = hline(T.data.THDTOP(iS),'color',cmap(iv,:));
							hl(iv,2) = hline(T.data.THDBTO(iS),'color',cmap(iv,:));
							t(1) = text(max(get(gca,'xlim')),T.data.THDTOP(iS),sprintf('%s [%s]',VARN{iv},T.data.THDTOP.name));
							t(2) = text(max(get(gca,'xlim')),T.data.THDBTO(iS),sprintf('%s [%s]',VARN{iv},T.data.THDBTO.name));
						catch
							warning('I can''t plot the thermocline thickness because I can''t get its depth !')
						end	
					otherwise
						hl(iv,1) = plot(get(gca,'xlim'),[1 1]*od.cont(iS));
						set(hl(iv,1),'color',cmap(iv,:));	
						t = text(max(get(gca,'xlim')),od.cont(iS),sprintf('%s [%s]',VARN{iv},od.name));											
				end% switch 
				
				set(t,'verticalAlignment','bottom','horizontalAlignment','right','interpreter','none');
				set(t,'tag','1Dlabel','color',cmap(iv,:),'fontweight','bold');
				
			else % This is a N_PROF x N_LEVELS variable
				[pl(iv),ax_plot(iv-1),ax_disp(iv-1)] = floatAxisX(od.cont(iS,:),z,'-',getxlab(od));		
				set(pl(iv),'color',cmap(iv,:));	
				set(ax_disp(iv-1),'xcolor',cmap(iv,:),'ydir',zdir);
				set(ax_plot(iv-1),'tag',VARN{iv});
				set(ax_disp(iv-1),'tag',VARN{iv});
				
				if addref
					ax0 = gca;
					axes(ax_plot(iv-1));hold on
					plref(iv) = plot(odref.cont(iSref,:),zref,'color',cmap(iv,:),'linestyle','--','tag','reference_profile');
					axes(ax0);
				end
			end% if
		

		end% if 
		
		
	end%for iv
	
	try
		set(pl(pl~=0),'marker','.');
		set(hl(hl~=0),'linewidth',2);
	catch
		stophere
	end
	if addref,
		set(plref,'marker','.');
%		
	end% if 
	
	%-- We made it to here, so update figure informations:
	set(gcf,'tag','profile_plot');
	setappdata(gcf,'id_station',iS);setappdata(gcf,'var_plotted',VARN);	
	copoda_figtoolbar(T);
end% if 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% One variable at several stations
if plotype == 2 
	for iv = 1 : length(VARN)
		
		figure;figure_tall
		set(gcf,'tag','profile_plot');
		setappdata(gcf,'id_station',iS);setappdata(gcf,'var_plotted',VARN(iv));		
		copoda_figtoolbar(T);	
		
		cmap = [0 0 0;1 0 0;0 0 1;0.2 .7 0.2];
		if length(iS) > 4
			cmap = [cmap ;jet(length(iS))];			
		end
	
		od = subsref(T,substruct('.','data','.',VARN{iv}));
		if addref
			odref = subsref(Tref,substruct('.','data','.',VARN{iv}));
		end
		if ischar(xlim)
			xmin = nanmin(nanmin(od.cont(iS,:)));
			xmax = nanmax(nanmax(od.cont(iS,:)));
			dx   = 0.1*abs(xmax-xmin);
		else
			if length(xlim) ~= 2
				error('X axis limit must be 2 values')
			end
			xlim = sort(xlim);
			xmin = xlim(1);
			xmax = xlim(2);
			dx = 0;
		end
		if ischar(zlim)
			try
				z = subsref(T,substruct('.','geo','.',ztyp,'()',{iS,':'}));
			catch
				z = subsref(T,substruct('.','geo','.',ztyp,'()',{1,':'}));			
			end
			zmin = nanmin(z(:));
			zmax = nanmax(z(:));
		else
			if length(zlim) ~= 2
				error('Z axis limit must be 2 values')
			end
			zlim = sort(zlim);
			zmin = zlim(1);
			zmax = zlim(2);
		end
		
		for is = 1 : length(iS)
			z  = subsref(T,substruct('.','geo','.',ztyp,'()',{iS(is),':'}));
			if addref				
				if isnan(iSref(1)),iSref=iS;end
				zref  = subsref(Tref,substruct('.','geo','.',ztyp,'()',{iSref(is),':'}));
			end
			if is == 1
				if addref			 	
					pl(is)    = plot(od.cont(iS(is),:),z);hold on
					plref(is) = plot(odref.cont(iSref(is),:),zref); 
				else
					pl(is) = plot(od.cont(iS(is),:),z);
				end
				ax_ref(is) = gca; 
				set(gca,'tag',VARN{iv});				
				set(ax_ref(is),'xlim',[xmin xmax]+[-1 1]*dx);
				set(ax_ref(is),'ylim',[zmin zmax]);

				set(pl(is),'color',cmap(is,:));
				set(ax_ref(is),'XMinorTick','on','box','on','xcolor',get(pl(is),'color'),'ydir',zdir);		
				xlabel(sprintf('LAT=%0.1f, LON=%0.1f, TIME=%s, STATION ID %i, # %i',...
					T.geo.LATITUDE(iS(is)),T.geo.LONGITUDE(iS(is)),datestr(T.geo.STATION_DATE(iS(is))),T.geo.STATION_NUMBER(iS(is)),iS(is)),'fontsize',8);
				grid on,box on;
				title(sprintf('%s\n%s',stamp(T,5),getxlab(od)),'fontweight','bold');				
				set(gcf,'name',sprintf('%s: %s',stamp(T,5),getxlab(od)));
				ylabel(sprintf('%s',zlab));
				if addref
					set(plref(is),'color',get(pl(is),'color'),'linestyle','--','tag','reference_profile');				
				end
			else
				[pl(is),ax_plot(is-1),ax_disp(is-1)] = floatAxisX(od.cont(iS(is),:),z,'-',...
						sprintf('LAT=%0.1f, LON=%0.1f, TIME=%s, STATION ID %i, # %i',...
						T.geo.LATITUDE(iS(is)),T.geo.LONGITUDE(iS(is)),datestr(T.geo.STATION_DATE(iS(is))),T.geo.STATION_NUMBER(iS(is)),iS(is)),...
						[[xmin xmax]+[-1 1]*dx zmin zmax]);
				set(pl(is),'color',cmap(is,:));	
				set(ax_plot(is-1),'ydir',zdir);
				set(ax_disp(is-1),'xcolor',cmap(is,:),'ydir',zdir);
				%set(ax_plot(is-1),'xlim',[xmin xmax]);
				%set(ax_disp(is-1),'xlim',[xmin xmax]);				
				set(ax_plot(is-1),'tag',VARN{iv});
				set(ax_disp(is-1),'tag',VARN{iv});
				if addref
					ax0 = gca;
					axes(ax_plot(is-1));hold on
					plref(is) = plot(odref.cont(iSref(is),:),zref,'color',cmap(is,:),'linestyle','--','tag','reference_profile');
					axes(ax0);
				end
			end			
		end%for is
		set(pl,'marker','.');
		if addref,
	%		set(plref,'marker','.');
			set(pl,'linewidth',2);
		end
	end%for iv

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
end%if

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% WaterFall plot
if plotype == 3 
	
	%-- Manage new or clean figure:
	% If we called this function from the COPODA figure toolbar, we need to know what to do,
	% otherwise we simply create a new figure.
	
	% If we called this function from the COPODA figure toolbar, the 'copoda_figtoolbar.m'
	% file name must appear in the dbstack.
	st = dbstack; st = struct2cell(st);
	if ~isempty(intersect(st(1,:),'copoda_figtoolbar.m'))
		switch get(gcf,'tag')
			case {'profile_plot','waterfall_plot'}
	%			disp('called from a profile plot, add more variables to the same figure')
				clf;
			case 'track_map'
	%			disp('Called from a track map, create a new figure')
				% Create a new figure
				figure;figure_land
			otherwise
				disp('You called me from the COPODA figure toolbar but I don''t know what''s plotted here !')
				stophere
		end% switch 
	else	
		% Create a new figure
		figure;figure_tall
	end% if
	
	%-- Plot
	% for a waterfall plot, we can only have one 2D variable (Nprof,Nlevel)
	% and then as many as we want of 1D variable (Nprof,1)

	iS = 1 : size(T,1);

	od = getfield(T,'data',VARN{1});
	if size(od,1) ~= size(T,1) & size(od,2) ~= size(T,2)
		error('For a waterfall plot, please specify the (NPROFs x NLEVELS) variable first in the list')
	end% if 
	
	iv = 1;
	set(gcf,'tag','waterfall_plot');
	setappdata(gcf,'id_station',iS);setappdata(gcf,'var_plotted',VARN(iv));		
	copoda_figtoolbar(T);	
	
	if ischar(zlim)		
		try % If z is defined for each stations:
			z = subsref(T,substruct('.','geo','.',ztyp,'()',{iS,':'}));
		catch % Otherwise, regular vertical grid:
			z = subsref(T,substruct('.','geo','.',ztyp,'()',{1,':'}));		
		end
		zmin = nanmin(z(:));
		zmax = nanmax(z(:));
	else
		if length(zlim) ~= 2
			error('Z axis limit must be 2 values')
		end
		zlim = sort(zlim);
		zmin = zlim(1);
		zmax = zlim(2);
	end
	
	od = getfield(T,'data',VARN{iv});
	dx = xtrm(abs(diff(od.cont)));

	% It may happen that the first level is full of NaN (BRV2 for instance)
	iz0 = 1;
	if length(find(isnan(od.cont(iS,1))==1)) == length(iS)
		iz0 = 2;
	end% if 

	for is = 1 : length(iS)
		c  = od.cont(iS(is),:);
		c0 = od.cont(iS(is),iz0);
		try
			z  = subsref(T,substruct('.','geo','.',ztyp,'()',{iS(is),':'}));			
		catch
			z  = subsref(T,substruct('.','geo','.',ztyp,'()',{1,':'}));
		end
%			p(is) = plot3(c - c0 + is*dx,z,c);
%			p(is) = patch(c-c0+is*dx,z,c);
		p(is) = patch([c-c0+is*dx fliplr(c-c0+is*dx)],[z fliplr(z)],[c fliplr(c)]);
	end% for ip
	grid on, box on
	set(p,'edgecolor','flat','facecolor','none','linewidth',2);
	set(gca,'xaxisLocation','top')
	if length(iS) > 10
		xtl  = fix(linspace(1,size(T,1),5)); xtl = unique(sort(xtl));
		set(gca,'xtick',xtl*dx,'xticklabel',xtl);		
	else
		xt = fix(dx*[1:size(T,1)]); xt = unique(sort(xt));
		set(gca,'xtick',xt,'xticklabel',xt);
	end% if 
	
	set(gca,'ylim',[zmin zmax]);
	cl=colorbar;ct=ctitle(cl,od.unit);
		
	xlabel('Profile(s) transect index','fontsize',8);
	title(sprintf('%s\n%s',stamp(T,5),getxlab(od)),'fontweight','bold','interpreter','none');
	ylabel(sprintf('%s',zlab));
	
	
	%-- Eventually add more variables:
	if length(VARN) > 1
		odC = od; hold on
		marker = ' xot*sph';
		
		for iv = 2 : length(VARN)
			od = getfield(T,'data',VARN{iv});
			if size(od,1) == size(T,1) & size(od,2) == 1
				clear p,ii=0;
				for is = 1 : length(iS)
					c  = odC.cont(iS(is),:);
					c0 = odC.cont(iS(is),iz0);
					c  = c-c0+is*dx;
					z  = subsref(T,substruct('.','geo','.',ztyp,'()',{iS(is),':'}));
					iz = find(z>=od.cont(iS(is),1),1,'last');
					if ~isempty(iz)
						ii = ii + 1;
						p(ii) = plot(c(iz),od.cont(iS(is),1),'s');
						x(is) = c(iz);
						y(is) = od.cont(iS(is),1);
					else
						x(is) = NaN;
						y(is) = NaN;						
					end% if 
				end% for is
				if exist('p','var')
					set(p,'marker',marker(iv),'color','k');
				end% if 
				plot(x,y,'color','k','linewidth',2);
			end% if 
		end% for iv
		
	end% if 
	
	%-- We made it to here, so update figure informations:
	set(gcf,'tag','waterfall_plot');
	setappdata(gcf,'id_station',iS);setappdata(gcf,'var_plotted',VARN);	
	copoda_figtoolbar(T);
	
end% if plotype



end %functionmultiprofiles
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function lab = getxlab(od)

% We prefer long strings

if ~isempty(od.long_name)
	nam = od.long_name;
else
	nam = od.name;
end

if ~isempty(od.long_unit)
	unit = od.long_unit;
else
	unit = od.unit;
end

lab = sprintf('%s [%s]',nam,unit);


end%function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [hl1,ax2,ax3] = floatAxisX(varargin)
% floatAxisX  create floating x-axis for multi-parameter plot
% =========================================================================
% floatAxisX  Version 1.2 6-Mar-2000
%
% Usage: 
%   [h1,ax2,ax3] = floatAxisX(varargin)
%
% Description:
%   This Matlab function creates a floating x-axis for mutli-parameter
%   plots with different units on the same figure. For example, in oceanography,
%   it is common to plot temperature, salinity, and density versus depth.
%
%
% Input:
%   A minimum of two parameters is required. The first and second parameters are
%   the x,y pairs to plot. The third parameter (optional) specifies the linestyle
%   (defaults to 'k-' solid black). The fourth parameter (optional) specifies the
%   x-axis label for the floating axis. The fifth parameter (optional) specifies 
%   the x and y limits for the axis(this should be of the form 
%   [xlower xupper ylower yupper]).
%
% Output:
%   n/a
%
% Called by:
%   CTDplotX.m - script to demo floatAxis function
%
% Calls:
%   n/a
%
% Author:
%   Blair Greenan
%   Bedford Institute of Oceanography
%   18-May-1999
%   Matlab 5.2.1
%   greenanb@mar.dfo-mpo.gc.ca
% =========================================================================
%

% History
% Version 1.0 18-May-1999
% Version 1.1 31-May-1999
%    Added the ability to pass an array containing the x and y limits for
%    the axis.
% Version 1.2 6-Mar-2000
%    Added code to handle data with different y-limits. Previous versions
%    assumed all data had the same y-limits. Oops! Thanks to Jan Even Nilsen
%    (even@gfi.uib.no) for pointing this out.

% strip the arguments passed in
if (nargin < 2)
   error('floatAxis requires a minimum of three parameters')
elseif (nargin == 2)
   x = varargin{1};
   y = varargin{2};
   % default lines style (solid black line) 
   lstyle = 'k-';   
elseif (nargin == 3)
   x = varargin{1};
   y = varargin{2};  
   lstyle = varargin{3};
elseif (nargin == 4)
   x = varargin{1};
   y = varargin{2};  
   lstyle = varargin{3};
   xlbl = varargin{4};
elseif (nargin == 5)
   x = varargin{1};
   y = varargin{2};
   lstyle = varargin{3};
   xlbl = varargin{4};
   limits = varargin{5};
else
   error('Too many arguments')
end

ddy = 0.1;
hdy = 0.01;

% get position of axes
%allAxes = get(gcf,'Children');
allAxes = findall(gcf,'type','axes');
allAxes = setdiff(allAxes,findobj(gcf,'tag','footnote'));
allAxes = setdiff(allAxes,findobj(gcf,'tag','suptitle'));
set(allAxes,'fontsize',8);
ax1Pos  = get(allAxes(1),'position');

% rescale and reposition all axes to handle additional axes
for ii = 2:length(allAxes)
   if (rem(ii,2)==0) 
      % even ones in array of axes handles represent axes on which lines are plotted (2,4,6 ...)
      set(allAxes(ii),'Position',[ax1Pos(1) ax1Pos(2)+ddy ax1Pos(3) ax1Pos(4)-ddy])
   else
      % odd ones in array of axes handles represent axes on which floating x-axis exist (1,3,5 ...)
      axPos = get(allAxes(ii),'Position');
      set(allAxes(ii),'Position',[axPos(1) axPos(2)+ddy axPos(3) axPos(4)])
   end
end
% first axis is a special case (doesn't fall into even/odd scenario of figure children)
set(allAxes(1),'Position',[ax1Pos(1) ax1Pos(2)+ddy ax1Pos(3) ax1Pos(4)-ddy])
ylimit1 = get(allAxes(1),'Ylim');

% get new position for plotting area of figure
ax1Pos = get(allAxes(1),'position');

% axis to which the floating axes will be referenced
ref_axis = allAxes(end);
refPosition = get(ref_axis,'position');

% overlay new axes on the existing one
ax2 = axes('Position',ax1Pos,'fontsize',8);
set(ax2,'tag','floataxis')

% plot data and return handle for the line
hl1 = plot(x,y,lstyle);
% make the new axes invisible, leaving only the line visible
set(ax2,'visible','off','ylim',ylimit1)

if (nargin < 5)
   % get the x limits for the 
   xlimit = get(ax2,'XLim');
else
   set(ax2,'XLim',[limits(1) limits(2)],'YLim',[limits(3) limits(4)]);
end

% set the axis limit mode so that it does not change if the
% user resizes the figure window
set(ax2,'xLimMode','manual')

% set up another set of axes to act as floater
ax3 = axes('Position',[refPosition(1) refPosition(2)-ddy refPosition(3) hdy],'fontsize',8);
set(ax3,'box','off','ycolor','w','yticklabel',[],'ytick',[])
set(ax3,'XMinorTick','on','color','none','xcolor',get(hl1,'color'))
set(ax3,'tag','floataxis')

if (nargin < 5)
   set(ax3,'XLim',xlimit)
else
   set(ax3,'XLim',[limits(1) limits(2)],'YLim',[limits(3) limits(4)])
end

% label the axis
if (nargin > 3)
   xlabel(xlbl)
end


end%function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%










