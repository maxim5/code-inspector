% Data compliments of finance.yahoo.com (thanks dudes)
-module(msft2).
-export([run/0]).

run() ->
	do_stuff("msft_sp500.csv").

do_stuff(File) ->
	{ok, Binary} = file:read_file(File),
	Lines = string:tokens(erlang:binary_to_list(Binary), "\n"),
	Reversed = lists:reverse(Lines),
	calc_beta(Reversed, 0.1, 0.1).

calc_beta([H|T], PrevClose, PrevSpClose) -> 
	[Date, Open, High, Low, Close, Volume, AdjClose, SpOpen, SpHigh, SpLow, SpClose, SpVolume, SpAdjClose] = re:split(H, "[,]", [{return, list}]),
	CurrentClose = list_to_float(Close),
	CurrentSpClose = list_to_float(SpClose),
	ReturnPct = ((CurrentClose - PrevClose) / PrevClose) * 100, 
	SpReturnPct = ((CurrentSpClose - PrevSpClose) / PrevSpClose) * 100, 
	io:fwrite("Date: ~s Prev Close: ~f Current Close: ~f Return Pct: ~f% SP 500 Return Pct: ~f%\n", [Date, PrevClose, CurrentClose, ReturnPct, SpReturnPct]),
	calc_beta(T, CurrentClose, CurrentSpClose);

calc_beta([], _, _) -> done.
