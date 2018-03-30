-module(hex_factory).
-export([start/0]).
-export([hex/1, hexes/2]).

start() -> spawn(fun start_factory/0).

start_factory() ->
    factory([
        desert,
        brick, brick, brick,
        lumber, lumber, lumber, lumber,
        grain, grain, grain, grain,
        wool, wool, wool, wool,
        ore, ore, ore
    ]).

hexes(0, _) -> [];
hexes(N, _) when N < 0 -> exit({N, negative});
hexes(N, Factory) when is_pid(Factory) ->
    [hex(Factory)|hexes(N-1, Factory)].

hex(Factory) when is_pid(Factory) ->
    Factory ! {self(), hex},
    receive {Factory, Hex} -> Hex end.

factory(Terrain) ->
    receive
        {Sender, hex} when is_pid(Sender) ->
            Index = random:uniform(length(Terrain)) - 1,
            {Before, [Type|After]} = lists:split(Index, Terrain),
            Sender ! {self(), hex:start(Type)},
            factory(Before++After);
        shutdown ->
            ok
    end.
