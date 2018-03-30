%% Example class for inheritance demo and for cctrans testing.
%% Part of Computer Shop Demo
-include_lib("ect/include/ect.hrl").

-class(device).
?FIELDS(1, public, {serial,price,customer}).
-methods([print/1, sell/2, dotest/1]).
-export([dotest/0]).

print(#device{serial = Serial, price = Price, customer = Customer}) ->
    %% the head extracted the fields from the objects with pattern matching
    %% obj#device means that the parameter can be also a superclass of device
    io:format("Serial= ~p~nPrice= ~p~nCustomer= ~p~n", [Serial, Price, Customer]).

%% sets the customer
sell(This = #device{}, Customer) ->
    %% in the next line {This}#device... also supports This being a superclass of device
    %% 
    This#device{customer = Customer}.

dotest(_) ->
    pass.

dotest() ->
    {#device{}}:dotest().
