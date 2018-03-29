%% Copyright (c) 2012 Miguel Bazdresch
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.

%% usage: printpgf(c,P)
%%
%% Export a plot to pdf using TeX's pgfplots package
%%
%% This function requires a LaTeX distribution to be installed. It has been
%% tested with TexLive 2011.
%%
%% Input arguments:
%% c : A struct with the following fields:
%%	filename (string, required)
%%		The name of the output tex file.
%%	standalone ([0,1], default 1)
%%		0 : Output file should be included in another TeX file.
%%		1 : Output file is self-contained and uses package standalone to
%%		    produce a pdf of exactly the plot size.
%%	scale (string)
%%		The plot is scaled by this factor
%%	picturegeneric (string)
%%		Passed unaltered as options to the tikzpicture environment
%%	preamble (string)
%%		The contents of a file with this name are included in the output
%%		file's preamble (to help maintain consistency among many plots).
%%	axistype (string, default 'axis')
%%		One of pgfplot's valid axis: 'axis', 'loglogaxis', etc.
%%	xlabel (string)
%%		Horizontal axis' label. May include math between $$.
%%	ylabel (string)
%%		Vertical axis' label.
%%	scaledxticks ([0,1], default 0)
%%	scaledyticks ([0,1], default 0)
%%		Controls whether to scale the numerical labels of the plot's
%%		ticks (eg 0.001 vs 10{-3}).
%%	title (string)
%%		The plot's title. May include math between $$.
%%	grid (string, default '')
%%		One of pgfplot's valid grid styles: 'major', 'both', etc.
%%	axisgeneric (string, default '')
%%		Pgfplots has a myriad ways to configure every aspect of a plot;
%%		adding options to control each one would turn this script into
%%		an unmanageable monster. Instead, with axisgeneric, the user
%%		may add any axis options he needs to fine-tune the plot.
%%	runtex ([0,1], default 0)
%%		If 1, then run a TeX engine on the output file.
%%	texengine (string, default 'pdflatex')
%%		Which TeX engine to run.
%%	runecho ([0,1], default 1)
%%		If 1, show the output of running texengine. Make sure your tex
%%		files are correct before setting runecho to zero: any error will
%%		cause Octave to freeze.
%%
%% P : A cell, each of whose element is a struct. Each struct defines a plot
%%     in the same axes (think running Octave's plot with multiple x,y pairs).
%%     Each struct has the following fields:
%%	x (vector, required)
%%		A vector with horizontal axis coordinates.
%%	y (vector, required)
%%		A vector with vertical axis coordinates.
%%	style (string, default '')
%%		Linestyle, in pgfplot's format.
%%	color (string)
%%		Plot color, in pgfplot's format.
%%	mark (string)
%%		Plot mark, in pgfplot's format.
%%	marksize (string)
%%		Mark size, in pgfplot's format.
%%	onlymarks ([0,1], default 0)
%%		Plot only marks.
%%	solidmarks ([0,1], default 0)
%%		If true, mark style is set to solid (useful when linestyle
%%		is not solid).
%%	legend (string)
%%		Plot legend. Pgfplots offers amazing flexibility in legend
%%		positioning; the default is north-east, but it may be changed
%%		with the axisgeneric field.
%%	smooth ([0,1], default 0)
%%		If 1, lines between coordinate points are smoothed.
%%
%% Example 1: Create a pdf of a sinusoid
%%
%% config.filename = 'sinusoid.tex';
%% config.title = 'A sinusoid';
%% config.runtex = 1;
%% t = 0:0.001:0.03;
%% y = cos(2*pi*100*t);
%% p.x = t;
%% p.y = y;
%% p.onlymarks = 1;
%% p.mark = '*';
%% p.marksize = '0.5pt';
%% P{1} = p;
%% printpgf(config,P);
%%
%% Example 2: Create a pdf of two sinusoids of different phase
%%
%% config.filename = 'phase.tex';
%% config.title = 'Visualizing phase $\phi$';
%% config.runtex = 1;
%% t = 0:0.001:0.03;
%% y1 = cos(2*pi*100*t);
%% y2 = cos(2*pi*100*t+pi/4);
%% p.x = t;
%% p.y = y1;
%% p.smooth = 1;
%% p.color = 'red';
%% p.legend = '$\phi = 0$';
%% P{1} = p;
%% p.y = y2;
%% p.color = 'blue';
%% p.legend = '$\phi = \frac{\pi}{4}$';
%% P{2} = p;
%% printpgf(config,P);

%% Author: Luis Miguel Bazdresch Sierra

function printpgf(c, P)

	% check arguments
	if nargin != 2
		usage('Exactly two arguments are required.');
	end
	if !strcmp(typeinfo(c),'scalar struct')
		usage('First argument must be a scalar struct.');
	end
	if !strcmp(typeinfo(P),'cell')
		usage('Second argument must be a cell.');
	end

	% required fields
	if !isfield(c,'filename')
		usage('A filename must be defined.');
	end
	for i = 1:numel(P)
		v = P{i};
		if !isfield(v,'x') || !isfield(v,'y')
			usage('All plots must have defined x and y coordinates.');
		end
	end

	% open tex file for writing
	[fid msg] = fopen( c.filename, 'wt' );
	if fid == -1
		error( msg );
	end

	% standalone
	% if 'standalone' not specified OR it is not zero
	if !isfield(c,'standalone') || c.standalone != 0
		fdisp( fid, '\documentclass{standalone}' );
		fdisp( fid, '\usepackage{pgfplots}' );
		% preamble. default is empty
		if isfield(c,'preamble')
			fprintf( fid, '\\input{%s}\n', c.preamble );
		end
		fdisp( fid, '\begin{document}' );
	end

	fdisp( fid, '\begin{tikzpicture}[' );

	if isfield(c,'scale')
		fprintf( fid, 'scale=%s,', c.scale );
	end

	if isfield(c,'picturegeneric')
		fprintf( fid, '%s', c.picturegeneric );
	end

	% axistype
	S = 'axis';
	if isfield(c,'axistype')
		S = c.axistype;
	end
	fprintf( fid, ']\\begin{%s}[\n', S);

	% x and y labels
	if isfield(c,'xlabel')
		fprintf( fid, 'xlabel={%s},\n', c.xlabel );
	end
	if isfield(c,'ylabel')
		fprintf( fid, 'ylabel={%s},\n', c.ylabel );
	end

	% plot title
	if isfield(c,'title')
		fprintf( fid, 'title={%s},\n', c.title);
	end

	% plot grid lines
	if isfield(c,'grid')
		fprintf( fid, 'grid=%s,\n', c.grid);
	end

	% if 'scaledxticks' specified AND it is one
	% pgfplots default is 'scaled'
	if isfield(c,'scaledxticks') && c.scaledxticks == 1
		fdisp( fid, 'scaled x ticks = true,' );
	else
		fdisp( fid, 'scaled x ticks = false,' );
		fdisp( fid, 'x tick label style = {/pgf/number format/fixed},' );
	end
	if isfield(c,'scaledyticks') && c.scaledyticks == 1
		fdisp( fid, 'scaled y ticks = true,' );
	else
		fdisp( fid, 'scaled y ticks = false,' );
		fdisp( fid, 'y tick label style = {/pgf/number format/fixed},' );
	end

	% axisgeneric
	% generic text to insert in axis options
	if isfield(c,'axisgeneric')
		fprintf (fid, '%s', c.axisgeneric );
	end

	% end of axis options
	fdisp( fid, ']' );

	for i = 1:numel(P)
		v = P{i}; % v is a struct
		fprintf( fid, '%s', '\addplot[' );

		% smooth
		if isfield(v,'smooth') && v.smooth != 0
			fprintf(fid, '%s,', 'smooth');
		end

		% linestyle
		if isfield(v,'style')
			fprintf(fid, '%s,', v.style);
		end

		% color
		if isfield(v,'color')
			fprintf(fid, 'color=%s,', v.color);
		end

		% mark and size
		if isfield(v,'mark')
			fprintf(fid, 'mark=%s,', v.mark);
		end
		if isfield(v,'marksize')
			fprintf(fid, 'mark size=%s,', v.marksize);
		end

		% onlymarks
		if isfield(v,'onlymarks')
			if v.onlymarks == 1
				fprintf(fid, '%s,', 'only marks');
			end
		end

		% solidmarks
		if isfield(v,'solidmarks') && v.solidmarks != 0
			fprintf(fid, '%s', 'mark options=solid,' );
		end

		fprintf(fid, '%s\n', '] coordinates {');
		for j = 1:numel(v.x)
			fprintf( fid, '(%1.10f, %1.10f)\n', v.x(j), v.y(j) );
		end
		fdisp( fid, '};' );

		% legend
		if isfield(v,'legend')
			fprintf( fid, '\\addlegendentry{%s}\n',v.legend );
		end

	end
	fprintf( fid, '\\end{%s}', S );
	fdisp( fid, '\end{tikzpicture}' );
	if !isfield(c,'standalone') || c.standalone != 0
		fdisp( fid, '\end{document}' );
	end
	fclose( fid );

	% run 'texengine' on generated tex file
	if isfield(c,'runtex') && c.runtex == 1
		if isfield(c,'texengine')
			eng = c.texengine;
		else
			eng = 'pdflatex';
		end
		if isfield(c,'runecho') && c.runecho == 0
			tmp = system([eng ' ' c.filename],'async');
		else
			disp(system([eng ' ' c.filename]));
		end
	end
end
