%%% Runs a prisoner's dilemma tournament among a series of strategies,
%%% implemented as modules.
-module(driver).

%-export([run_tournament/0]).
-compile(export_all).

%% Scores
%%  cooperate, cooperate 8, 8
%%  defect, cooperate 10, 0
%%  cooperate, defect 0, 10
%%  defect, defect    4, 4

-record(player,
        {name, process, score=0, history=[]}).

% series result -- result of a series of a games against a single opponent
% choices is a list of tuples {cooperate|defect, cooperate|defect}, where
% the first elt is own choice, second elt is opponent choice.
-record(sresult, 
        { self, opponent, own_points=0, opponent_points=0, choices=[]}).
                     

create_players() ->
    Players = [
               {"darcy", darcy},      % Edmund,
               {"drew",  drew},       % DH2
               {"science", science},  % Owen
               {"keith", keith},      % Keith
               {"pcavs", pcavs}       % Paul C

              ],
    [ #player{name=Name, process=start_player(Module)}
      || {Name, Module} <- Players ].
                                  
%% Run a tournament among a list of { Name, Module } pairs, print the
%% results out.
run_tournament() ->
    N = 1000,
    Players = create_players(),
    AllPairs = all_pairs(Players),
    AllSResults = lists:flatten(lists:map(
      fun({Player1, Player2}) ->
              { SR1, SR2 } = run_games(N, Player1, Player2),
              [ SR1, SR2 ]
      end,
      AllPairs)),
    lists:foreach(fun stop_player/1, Players),
    SResultsByPlayer = collect_by_player(AllSResults),
    output_scores(SResultsByPlayer),
    ok.

%% Take a list of Series Results (sresult records), where each player will
%% exist more than once in the list, return a list of { Player, [All
%% Series Results for that Player ] } tuples
collect_by_player(AllSResults) ->
    dict:to_list(
      lists:foldl(
        fun(SResult, Dict) ->
                #sresult{self=Player} = SResult,
                dict:update(Player,
                            fun(SResults) -> [SResult | SResults] end,
                            [SResult],
                            Dict)
        end,
        dict:new(),
        AllSResults)).
    

%% Take a list of { Player, [SResults] } tuples, generate output
output_scores(SResultsByPlayer) ->
    WithTotalPoints = 
        [ {Player, SResults, lists:foldl(
                               fun(SResult, TotalPoints) ->
                                       TotalPoints + SResult#sresult.own_points
                               end,
                               0,
                               SResults)} 
          || {Player, SResults} <- SResultsByPlayer ],

    Sorted = lists:reverse(lists:keysort(3, WithTotalPoints)),
    [{Winner, _SResults, Score} | _Rest] = Sorted,
    io:format("Winner:~n ~s (~p points)~n", [Winner#player.name, Score]),
    lists:foreach(
      fun({Player, SResults, TotalScore}) ->
              io:format("~n~s:~n", [Player#player.name]),
              io:format(" Total Points: ~p~n~n", [TotalScore]),
              lists:foreach(
                fun(SResult) ->
                        #sresult{opponent=Opp,
                                 own_points=OwnPts,
                                 opponent_points=OppPts,
                                 choices=Choices} =
                            SResult,
                        io:format(" vs. ~s: ~p points (~p points)~n",
                                  [ Opp#player.name, OwnPts, OppPts ]),
                        io:format(" [~s~s]~n~n", 
                                  [string:join(
                                     lists:map(
                                       fun format_choice/1, lists:sublist(Choices,50)),
                                     ","),
                                   if
                                       length(Choices) > 50 ->
                                           ",...";
                                       true ->
                                           ""
                                   end]),
                        io:format(" Total CC/CD/DC/DD: ~p~n~n~n", [choice_stats(Choices)])
                end,
                SResults)
      end,
      Sorted),
    ok.

format_choice({Own, Opp}) ->
    string:concat(short_str(Own), short_str(Opp)).

short_str(cooperate) ->
    "c";
short_str(defect) ->
    "d".

choice_stats(Choices) ->
    choice_stats(Choices, {0,0,0,0}).

choice_stats([], StatsTuple) ->
    StatsTuple;
choice_stats([H|T], {CC, CD, DC, DD}) ->
    choice_stats(
      T, case H of
             {cooperate, cooperate} ->
                 {CC+1, CD, DC, DD};
             {cooperate, defect} ->
                 {CC, CD+1, DC, DD};
             {defect, cooperate} ->
                 {CC, CD, DC+1, DD};
             {defect, defect} ->
                 {CC, CD, DC, DD+1}
         end).
        
    

all_pairs(Lst) ->
    all_pairs(Lst, []).

all_pairs([], Lst) ->
    Lst;
all_pairs([H|T], Lst) ->
    SomePairs = [ {H, Elt} || Elt <- T ],
    all_pairs(T, lists:append(Lst, SomePairs)).
    
%% Run N games between two players, returning a series result (sresult)
%% record for each:
%%
%% { Player1SResult, Player2SResult }
run_games(N, Player1, Player2) ->
    run_games(N, Player1, Player2, {#sresult{self=Player1, opponent=Player2},
                                    #sresult{self=Player2, opponent=Player1}}).

run_games(0, _Player1, _Player2, SResults) ->
    { P1, P2 } = SResults,
    P1Choices = P1#sresult.choices,
    P2Choices = P2#sresult.choices,
    { P1#sresult{choices=lists:reverse(P1Choices)},
      P2#sresult{choices=lists:reverse(P2Choices)}
     };
run_games(N, Player1, Player2, {SResult1, SResult2}) ->
    {{Player1Choice, Player1Points}, 
     {Player2Choice, Player2Points}} = play_one_game(Player1, Player2),
    {TotP1Points, TotP2Points} = {SResult1#sresult.own_points, 
                                  SResult2#sresult.own_points},
    NewTotP1 = TotP1Points + Player1Points,
    NewTotP2 = TotP2Points + Player2Points,
    {P1Choices, P2Choices} = {SResult1#sresult.choices, 
                              SResult2#sresult.choices},
    run_games(N-1, 
              Player1, 
              Player2, 
              {SResult1#sresult{
                 own_points=NewTotP1,
                 opponent_points=NewTotP2,
                 choices = [ {Player1Choice,Player2Choice} | P1Choices ]},
               SResult2#sresult{
                 own_points=NewTotP2,
                 opponent_points=NewTotP1,
                 choices = [ {Player2Choice,Player1Choice} | P2Choices]}
              }).


%% Run a single game between two player processes, return a tuple that looks
%% like:
%%
%% {{ Player1Choice, Player1Points }, { Player2Choice, Player2Points }}
%%
%% Where points is the number of points they score in the game.

play_one_game(Player1, Player2) ->
    Player1Choice = make_choice(Player1, Player2),
    Player2Choice = make_choice(Player2, Player1),
    send_result(Player1, {Player2, Player1Choice, Player2Choice}),
    send_result(Player2, {Player1, Player2Choice, Player1Choice}),
    {Player1Points, Player2Points} = get_points(Player1Choice, Player2Choice),
    {{Player1Choice, Player1Points}, 
     {Player2Choice, Player2Points}}.
   
%% Return the scores for each player, given their choices
get_points(Player1Choice, Player2Choice) ->
    case { Player1Choice, Player2Choice } of
        { cooperate, cooperate } -> {  8, 8 };
        { defect, cooperate    } -> { 10, 0 };
        { cooperate, defect    } -> {  0, 10 };
        { defect, defect       } -> {  4, 4 }
    end.
        
        

%% RPC calls into the player process
start_player(Module) ->
    spawn(fun() -> server_loop(Module, Module:init()) end).
    
make_choice(Player, Opponent) ->
    P = Player#player.process,
    P ! { self(), play, Opponent },
    receive 
        { P, choice, Choice } ->
            Choice
    end
        .

send_result(Player, {Opponent, OwnChoice, OpponentChoice}) ->
    Player#player.process ! { result, {Opponent, OwnChoice, OpponentChoice} }.

stop_player(Player) ->
    Player#player.process ! stop.

%% Implementation of server loop on top of functional strategy modules
server_loop(Module, State) ->
    receive 
        { DriverPid, play, Opponent } ->
            { Choice, NewState } = Module:play(Opponent, State),
            DriverPid ! { self(), choice, Choice },
            server_loop(Module, NewState) ;

        { result, {Opponent, OwnChoice, OpponentChoice} } ->
            NewState = Module:result({Opponent, OwnChoice, OpponentChoice},
                                     State),
            server_loop(Module, NewState) ;
        
        stop ->
            Module:stop(State)
    end
        .
            
            
