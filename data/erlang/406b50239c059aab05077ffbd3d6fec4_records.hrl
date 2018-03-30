%%---------------------------------------------------------------------
%% Data Type: individualState
%% where:
%%    alphabet: A tuple (default is undefined).
%%    fitness: A function (default is udnefined).
%%    genome: A list with values of alphabet (default is undefined).
%%    age: An integer (default is 0).
%%----------------------------------------------------------------------
-record(individualState, {alphabet, fitness, genome, age = 0}).

%%---------------------------------------------------------------------
%% Data Type: environmentState
%% where:
%%    population: A list with individuals (default is []).
%%    new_population: A list with individuals (default is []).
%%    killlist: A list with Pids (default is []).
%%    mutation: An flot (default is 0.0).
%%    age: An integer (default is 0).
%%    ticks: An integer (default is 0).
%%    crossover: A boolean (default is false).
%%----------------------------------------------------------------------
-record(environmentState, {population = [], new_population = [],
	killlist = [], mutation = 0.0, age = 0, ticks = 0,
	crossover = false}).

%%---------------------------------------------------------------------
%% Data Type: monitorState
%% where:
%%    filename: A String (default is undefined).
%%    command: A String (default is undefined).
%%    divisor: An integer (default is 1).
%%    call_format: A boolean (default is false).
%%----------------------------------------------------------------------
-record(monitorState, {filename, command, divisor = 1,
	call_format = false}).
