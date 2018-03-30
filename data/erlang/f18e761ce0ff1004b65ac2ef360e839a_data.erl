-module(data).

-export([import_from_file/1]).

%% http://www.trapexit.org/Reading_Lines_from_a_File

import_from_file(Name) ->
    {ok, Device} = file:open(Name, [read]),
    import_from_file(Device, []).

import_from_file(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  -> 
	    file:close(Device), 
	    Accum;
	"\n" -> 
	    import_from_file(Device, Accum);
        Line ->
	    {ok, Country, 1}  = regexp:sub(Line, "\n", ""),
	    {ok, Capital, 1}  = regexp:sub(io:get_line(Device, ""), "\n", ""),
	    
	    import_from_file(Device, [{Country, Capital}|Accum])
    end.
