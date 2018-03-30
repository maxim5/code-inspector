%%%----------------------------------------------------------------------
%%% File    : atm_hw.erl
%% @author Jan Henry Nystrom <jann@erlang-consulting.com>
%% @doc The hw of ATM example for the OTP course.
%%% Created : 2 July 2006 by Jan Henry Nystrom 
%% @copyright 2006 Erlang Consulting Ltd (www.erlang-consulting.com)
%% @version '1.0'
%%%----------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MODULE INFO                                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(atm_hw).
-version('1.0').
-behaviour(gen_gs_server).
-author('jann@erlang-consulting.com').
-copyright('Copyright (c) 2006 Erlang Consulting Ltd (www.erlang-consulting.com)').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INCLUDES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%
% Interface
%
-export([start/1,
	 start_link/1,
	 stop/1,
	 display/2,
	 high_light/2,
	 eject/1
	]).

%%%%%
% Gen gs server part
%
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3,
	 handle_gs/5
	]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DEFINES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(WELCOME_MESSAGE,
	"Hello!\n\nWelcome to the Erlang ATM Machine!\n\nPlease insert your card.").
-define(SELECT_GIFS,
	[{select1,
	  "../priv/gif/withdraw_off.gif",
	  "../priv/gif/withdraw_on.gif"},
	 {select2,
	  "../priv/gif/balance_off.gif",
	  "../priv/gif/balance_on.gif"},
	 {select3,
	  "../priv/gif/ministatement_off.gif",
	  "../priv/gif/ministatement_on.gif"}
	]).

-define(MENU_OFFSET, 22).
-define(WINDOW_HEIGHT, 350).
-define(WINDOW_WIDTH, 650).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RECORDS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%
%% @type state() = {dragdrop(),
%%                  account(),
%%                  main(), 
%%                  canvas(), 
%%                  screen(),
%%                  menu(),
%%                  card(), 
%%                  slot()}
%%
%% @type dragdrop() = onoff()
%% @type account() = {atom(account), AccountNumber::integer()}
%% @type main() = gs:'ObjId'()
%% @type canvas() = gs:'ObjId'()
%% @type buttons() = [{Name::atom(), gs:'ObjId'(), Highlight::onoff()}]
%% @type screen() = gs:'ObjId'()
%% @type menu() = gs:'ObjId'()
%% @type card() = gs:'ObjId'()
%% @type slot() = gs:'ObjId'()
%% @type onoff() = atom(off) + atom(on)
%
-record(state, {name,
		dragdrop = off,
		account,
		main,
		canvas,
		buttons,
		screen,
		menu,
		card,
		slot}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%
%% @spec start(atom()) -> {atom(ok), pid()} | {error, Reason::term()}
%
start(Name) -> gen_gs_server:start({local, name(Name)}, ?MODULE, Name, []).

%%%%%
%% @spec start_link(atom()) -> {atom(ok), pid()} | {error, Reason::term()}
%
start_link(Name) ->
  gen_gs_server:start_link({local, name(Name)}, ?MODULE, Name, []).

%%%%%
%% @spec stop() -> atom(ok)
%
stop(Name) -> gen_gs_server:cast(name(Name), stop).

%%%%%
%% @spec display(string()) -> atom(ok)
%
display(String, Name) -> gen_gs_server:cast(name(Name), {display, String}), ok.

%%%%%
%% @spec high_light(atom()) -> atom(ok)
%
high_light(Button, Name) ->
  gen_gs_server:cast(name(Name), {high_light, Button}), ok.


%%%%%
%% @spec eject() -> atom(ok)
%
eject(Name) -> gen_gs_server:cast(name(Name), eject), ok.
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTED FUNCTIONS/GEN_GS_SERVER CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%
%% @spec init(No_args::atom()) -> {ok, state()}
%
init(Name) ->
  Main = gs:create(window, gs:start(),
		   [{motion, true},
		    {width, ?WINDOW_WIDTH},
		    {height, ?WINDOW_HEIGHT + ?MENU_OFFSET},
		    {title, "The Amazing Erlang ATM Machine"},
		    {map, true}]),
  Canvas = gs:create(canvas, Main,
		     [{x, 0},
		      {y, ?MENU_OFFSET},
		      {width, ?WINDOW_WIDTH},
		      {height, ?WINDOW_HEIGHT + ?MENU_OFFSET},
		      {bg, gray}]),
   % Main background
  gs:create(image, Canvas,
	    [{buttonpress, true},
	     {load_gif, "../priv/gif/atmbg.gif"},
	     {coords, [{0,0}]}]),
  Buttons = create_buttons(Canvas),
  Slot = gs:create(image, Canvas,
		   [{load_gif, "../priv/gif/slot.gif"},
		    {coords, [{235, 280}]}]),
  create_keypad(Canvas),
  Screen = gs:create(text, Canvas,
	    [{coords,[{207,78}]},
	     {font, {courier,11}},
	     {fg, green},
	     {text, ?WELCOME_MESSAGE}]),
  % Menu
  MenuBar = gs:create(menubar, Main, []),
  MenuButton = gs:create(menubutton, MenuBar, [{label,{text,"Pick A Card!"}}]),
  AccountMenu = gs:create(menu, MenuButton, []),
  Accounts = backend:account(all),
  populate_accounts(Accounts, AccountMenu),
  {ok, #state{name = Name,
	      main = Main,
	      canvas = Canvas,
	      buttons = Buttons,
	      screen = Screen,
	      menu = MenuButton,
	      slot = Slot
	     }}.

%%%%%
%% @spec handle_call(Call::term(), From::{pid(), reference()}, state()) ->
%%         {atom(stop), Reason::string(), state()}
%
handle_call(_, _, State) -> ignore_call(State).

%%%%%
%% @spec handle_cast(Cast::term(), state()) ->
%%         {atom(noreply), state()}
%
handle_cast(eject, State=#state{slot = Slot, menu = Menu, screen = Screen}) -> 
  gs:config(Slot, [{load_gif, "../priv/gif/slot.gif"}]),
  gs:config(Menu, [{enable, true}]),
  gs:config(Screen, [{text, ?WELCOME_MESSAGE}]),
  {noreply, State};
handle_cast({display, Str}, State=#state{screen = Screen}) ->
  receive after 200 -> gs:config(Screen, [{text, Str}]) end,
  {noreply, State};
handle_cast({high_light, Button}, State) ->
  State1 = high_light_i(Button, State),
  {noreply, State1};
handle_cast(stop, State) -> {stop, normal, State}.


%%%%%
%% @spec handle_gs(gs:'ObjId'(),
%%                 Event::gs:'EventType'(),
%%                 Data::term(), 
%%                 Args::[term()],
%%                 state()) ->
%%         {atom(noreply), state()}
%
handle_gs(Main, motion, _, _, State = #state{dragdrop = off, main = Main}) ->
  {noreply, State};
handle_gs(Main, motion, _, [X,Y|_], State=#state{main = Main, card = Card}) ->
  gs:config(Card, [{coords, [{X, Y}]}]),
  case over_slot(X, Y) of
    false -> {noreply, State};
    true  ->
      gs:destroy(Card),
      gs:config(State#state.slot, [{load_gif, "../priv/gif/slot_card.gif"}]),
      atm:card_inserted(State#state.account, State#state.name),
      {noreply, State#state{dragdrop = off}}
  end;
handle_gs(_, click, {account, Account}, _, State) ->
  Card = gs:create(image, State#state.canvas,
		   [{data, none},
		    {buttonpress, true},
		    {buttonrelease, true},
		    {load_gif, "../priv/gif/card.gif"},
		    {coords, [{10, 10}]}]),
  gs:config(State#state.menu, [{enable, false}]),
  {noreply, State#state{account = Account, dragdrop = on, card = Card}};
handle_gs(Main, destroy, _, _, State = #state{main = Main, name = Name}) ->
  atm:stop(Name),
  {stop, "Main window destroyed", State};
handle_gs(_, buttonpress, cancel, _, State) ->
  State1 = high_light_off(State),
  atm:event(cancel, State#state.name),
  {noreply, State1};  
handle_gs(_, buttonpress, Type, _, State) ->
  atm:event(Type, State#state.name),
  {noreply, State}.

%%%%%
%% @spec handle_info(term(), state()) ->
%
handle_info(_, State) -> ignore_cast(State).
   

%%%%%
%% @spec code_change(OldVsn::term(), state(), Extra::[term()]) ->
%%         {atom(ok), state()}
%
code_change(_, State, _) -> {ok, State}.
  

%%%%%
%% @terminate(Reason::term(), State::state()) -> none()
%
terminate(_, _) -> ok.
   



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%
%% @spec create_buttons(canvas()) -> buttons()
%
create_buttons(Canvas) ->
  lists:map(fun({Id, Event, Gif, X, Y}) ->
		Obj = create_button(Event, Gif, {coords, [{X, Y}]}, Canvas),
		{Id, Obj, off}
	    end,
	    [{select1,
	      {selection, select1},
	      "../priv/gif/withdraw_off.gif", 0, 53},
	     {select2,
	      {selection, select2},
	      "../priv/gif/balance_off.gif", 0, 88},
	     {select3,
	      {selection, select3},
	      "../priv/gif/ministatement_off.gif", 0, 123}]).

%%%%%
%% @spec create_keypad(canvas()) -> none()
%
create_keypad(Canvas) ->
  lists:foreach(fun({Event, Gif, X, Y}) ->
		    create_button(Event, Gif, {coords, [{X, Y}]}, Canvas)
		end,
		[{{digit, "1"}, "../priv/gif/1.gif", 518, 178},
		 {{digit, "2"}, "../priv/gif/2.gif", 556, 178},
		 {{digit, "3"}, "../priv/gif/3.gif", 594, 178},
		 {{digit, "4"}, "../priv/gif/4.gif", 518, 211},
		 {{digit, "5"}, "../priv/gif/5.gif", 556, 211},
		 {{digit, "6"}, "../priv/gif/6.gif", 594, 211},
		 {{digit, "7"}, "../priv/gif/7.gif", 518, 244},
		 {{digit, "8"}, "../priv/gif/8.gif", 556, 244},
		 {{digit, "9"}, "../priv/gif/9.gif", 594, 244},
		 {{digit, "0"}, "../priv/gif/0.gif", 518, 277},
		 {clear, "../priv/gif/clear.gif", 558,277},
		 {enter, "../priv/gif/enter.gif", 540, 100},
		 {cancel, "../priv/gif/cancel.gif", 540, 135}
		]).

%%%%%
%% @spec populate_accounts(Accounts::[backend:account()], 
%%                         AccountMenu::gs:'ObjId'()) ->
%%                        none()
%
populate_accounts(Accounts, Amenu) ->
  lists:foreach(fun({No, Name}) ->
		    Label = io_lib:format("[No:~p]  ~p", [No, Name]),
		    gs:create(menuitem, Amenu,
			      [{data, {account, No}},
			       {label,{text, Label}}])
		end,
		lists:reverse(Accounts)).



%%%%%
%% @spec create_button(event(), string(), coordinates(), canvas()) -> none()
%% @type coordinates() = [{atom(coords), [{integer(), integer()}]}]
%
create_button(Event, Gif, Coords, Canvas) ->
  gs:create(image, Canvas,
	    [{data, Event}, {buttonpress, true}, {load_gif, Gif}, Coords]).  

%%%%%
%% @spec high_light_off(state()) -> state()
%
high_light_off(State) ->
  high_light_off1(State#state.buttons, [], State).

%%%%%
%% @spec high_light_off1(Buttons::buttons(), 
%%                       Accumulator::buttons(), state()) -> 
%%                      state()
%
high_light_off1([], Acc, State) -> State#state{buttons = Acc};
high_light_off1([Button = {_, _, off} | T], Acc, State) ->
  high_light_off1(T, [Button | Acc],  State);
high_light_off1([{Id, Obj, on} | T], Acc, State) ->
  {value, {_, Off, _}} = lists:keysearch(Id, 1, ?SELECT_GIFS),
  gs:config(Obj, [{load_gif, Off}]),
  high_light_off1(T, [{Id, Obj, off} | Acc], State).

%%%%%
%% @spec high_light_i(gs:'ObjId'(), state()) -> state()
%
high_light_i(Button, State) ->
  high_light1(Button, State#state.buttons, [], State).

%%%%%
%% @spec high_light1(gs:'ObjId'(),
%%                   Buttons::buttons(), 
%%                   Accumulator::buttons(), state()) -> state()
%
high_light1(_, [], Acc, State) -> State#state{buttons = Acc};
high_light1(Id, [{Id1, Obj, _} | T], Acc, State) when Id =/= Id1 ->
  {value, {_, Off, _}} = lists:keysearch(Id1, 1, ?SELECT_GIFS),
  gs:config(Obj, [{load_gif, Off}]),
  high_light1(Id, T, [{Id1, Obj, off} | Acc], State);
high_light1(Id, [{Id, Obj, _} | T], Acc, State) ->
  {value, {_, _, On}} = lists:keysearch(Id, 1, ?SELECT_GIFS),
  gs:config(Obj, [{load_gif, On}]),
  high_light1(Id, T, [{Id, Obj, on} | Acc], State).

%%%%%
%% @spec over_slot(integer(), integer()) -> bool()
%%
%
over_slot(X, Y) ->
  ((X+80 > 235) and (X+25 < 420)) and ((Y+120 > 280) and (Y+45 < 325)).

%%%%%
%% @spec name(atom(Name)) -> atom(CompleteName).
%
name(Name) -> list_to_atom("atm_hw_" ++ atom_to_list(Name)).

%%%%%
%% @spec 
%
ignore_call(State) -> {reply, ok, State}.

%%%%%
%% @spec 
%
ignore_cast(State) -> {noreply, State}.
  
