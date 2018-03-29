function browser_refresh(par, data, slide)

% browser_refresh - refresh browser display
% -----------------------------------------
%
% browser_refresh(par, data, slide)
%
% Input:
% ------
%  par - browser
%  data - browser state
%  slide - indicator

% start = clock;

%---------------------------
% HANDLE INPUT
%---------------------------

%--
% set parent browser and get state if needed
%--

if nargin < 1 || isempty(par)
	par = get_active_browser;
end

if nargin < 2 || isempty(data)
	data = get_browser(par);
end

%--
% set default no slide
%--

if nargin < 3 || isempty(slide)
	slide = 0;
end

%---------------------------
% SETUP
%---------------------------

%--
% get slider time
%--

slider = get_time_slider(par); time = slider.value;

%--
% manage slide event state
%--

persistent LAST_PARENT SLIDE_UPDATE;

if isempty(SLIDE_UPDATE) || isempty(LAST_PARENT)
	SLIDE_UPDATE = 0; LAST_PARENT = par;
else
	if ~isequal(LAST_PARENT, par)
		SLIDE_UPDATE = 0; LAST_PARENT = par;
	end
end
	
%---------------------------
% REFRESH
%---------------------------

%-----------------
% SLIDE
%-----------------

if slide
	browser_time_slide(par, time, data, SLIDE_UPDATE); drawnow; SLIDE_UPDATE = 1; return;
end

% NOTE: if we make it here a full refresh is imminent, the next slide must start again

SLIDE_UPDATE = 0;

%-----------------
% DISPLAY
%-----------------

data.browser.time = time;

% NOTE: this should not be required

data.browser.slider = slider.handle(1);

%--
% perform active detection if needed
%--

% TODO: active detection log is volatile, and typically small, store in environment

try
	data.browser.active_detection_log = active_detection(par, data);
catch
	nice_catch(lasterror, 'Failed to perform active detection');
end
	
%--
% enable and disable navigation menus
%--

browser_navigation_update(par, data);

%--
% update view state array and browser state
%--

% NOTE: this update should not happen all the time, look at top note

data.browser.view = browser_view_update(par, data);

%--
% manage browser marker and selection
%--

marker = get_browser_marker(par, data);

selection = get_browser_selection(par, data);

% NOTE: we delete the selection so we don't orphan any handles

data = delete_marker(par, data);

data = delete_selection(par, data);

%--
% update widget and browser displays
%--

clear_browser_extension_cache;

browser_display(par, 'update', data);

update_widgets(par, 'page', data);

% NOTE: the browser should be a widget

%--
% update browser state
%--

% NOTE: this leaves display (browser/widget) updates working with stale figure data!!

set(par, 'userdata', data);

%--
% update marker and selection if it makes sense
%--

set_browser_marker(par, marker, 1, data);

% TODO: make selection persistence leaner and faster

selection_update(par, data, selection);

% disp([get(par, 'tag'), '  ', sec_to_clock(etime(clock, start))]);
