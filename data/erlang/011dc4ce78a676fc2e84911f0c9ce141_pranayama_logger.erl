%% @docfile "./doc/src/pranayama_types.edoc"

%%%-------------------------------------------------------------------
%%% @author S.A. Rasmussen <sean@erasmos.com>
%%% @copyright (C) 2008, Erasmos /  S.A. Rasmussen
%%% @doc
%%% A convenient facade to pranayama_log, allowing us
%%% to log during unit tests.
%%% 
%%% ==Licence==
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%                         
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%% @end
%%% Created : 22 Nov 2008
%%%-------------------------------------------------------------------
-module(pranayama_logger).

-export([trace/1,trace/2,
	 info/1,info/2,
	 warn/1,warn/2,
	 error/1,error/2
	]).

-include("../include/pranayama_internal.hrl").
-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).
-define(TESTMODE,true).
-else.
-define(TESTMODE,false).
-endif.

%%--------------------------------------------------------------------
%% @spec trace(Template::string())-> ok
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
trace(Template)
  when is_list(Template)->
    trace(Template,[]).

%%--------------------------------------------------------------------
%% @spec trace(Template::string(),Params::list())-> ok
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
trace(Template,Params)
  when is_list(Template),
       is_list(Params)->
    log(trace,Template,Params).

%%--------------------------------------------------------------------
%% @spec info(Template::string())-> ok
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
info(Template)
  when is_list(Template)->
    trace(Template,[]).

%%--------------------------------------------------------------------
%% @spec info(Template::string(),Params::list())-> ok
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
info(Template,Params)
  when is_list(Template),
       is_list(Params)->
    log(info,Template,Params).


%%--------------------------------------------------------------------
%% @spec warn(Template::string())-> ok
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
warn(Template)
  when is_list(Template)->
    trace(Template,[]).

%%--------------------------------------------------------------------
%% @spec warn(Template::string(),Params::list())-> ok
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
warn(Template,Params)
  when is_list(Template),
       is_list(Params)->
    log(warn,Template,Params).


%%--------------------------------------------------------------------
%% @spec error(Template::string())-> ok
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
error(Template)
  when is_list(Template)->
    error(Template,[]).

%%--------------------------------------------------------------------
%% @spec error(Template::string(),Params::list())-> ok
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
error(Template,Params)
  when is_list(Template),
       is_list(Params)->
    log(error,Template,Params).

%%--------------------------------------------------------------------
%% @spec log(Level::atom(),Template::string(),Params::list())-> ok
%% @doc
%%  If we're in test mode we'd have no event manager to call, so
%%  we just print to the screen; note that, as EUnit absorbs stdout
%%  we have to use its macro.
%% @end
%%--------------------------------------------------------------------
log(Level,Template,Params)
  when is_atom(level),
       is_list(Template),
       is_list(Params)->

    case ?TESTMODE of
	true->
	    ?debugFmt(Template,Params),
	    ok;
	false ->
	    pranayama_event_manager:log(
	      #log_message{level=Level,
		   template=Template,
		   params=Params}),
	   ok
    end.
					     


