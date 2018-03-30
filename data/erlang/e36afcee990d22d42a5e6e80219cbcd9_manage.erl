-module(manage).
-compile(export_all).
-include("include/my.hrl").

-spec(boot/1 :: (atom()) -> 'ok').
boot(Application) ->
    ?PRINT("application:load(Application)"),
    ok = application:load(Application),
    ?PRINT("application:start(Application)"),
    ok = application:start(Application),
    ok.

-spec(shutdown/1 :: (atom()) -> 'ok').
shutdown(Application) ->
    ?PRINT("application:stop(Application)"),
    case application:stop(Application) of
        ok -> ok;
        Term  -> io:format("~p~n", [Term])
    end,
    ?PRINT("application:unload(Application)"),
    ok = application:unload(Application),
    ok.

-spec(reboot/1 :: (atom()) -> 'ok').
reboot(Application) ->
    shutdown(Application),
    boot(Application),
    ok.
