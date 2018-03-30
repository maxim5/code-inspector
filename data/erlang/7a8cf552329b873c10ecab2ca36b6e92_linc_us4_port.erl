%%------------------------------------------------------------------------------
%% Copyright 2014 FlowForwarding.org
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%-----------------------------------------------------------------------------

%% @author Erlang Solutions Ltd. <openflow@erlang-solutions.com>
%% @copyright 2014 FlowForwarding.org

%% @doc Module to represent OpenFlow port.
%% It abstracts out underlying logic of either hardware network stack or virtual
%% TAP stack. It provides Open Flow ports represented as gen_server processes
%% with port configuration and statistics according to OpenFlow specification.
%% It allows to create and attach queues to given ports and supports queue
%% statistics as well. OpenFlow ports can be programatically started and
%% stopped by utilizing API provided by this module.

-module(linc_us4_port).

% XXX no longer a gen_server -- need to rewrite this so port information is stored in a table.

%% Port API
-export([start_link/2,
         initialize/1,
         terminate/1,
         modify/2,
         get_desc/1,
         get_state/2,
         set_state/3,
         get_config/2,
         set_config/3,
         get_features/2,
         get_advertised_features/2,
         set_advertised_features/3,
         get_all_ports_state/1,
         get_all_queues_state/1]).

-include_lib("of_config/include/of_config.hrl").
-include_lib("of_protocol/include/of_protocol.hrl").
-include_lib("of_protocol/include/ofp_v4.hrl").
-include("ofs_store_logger.hrl").
-include("ofs_store.hrl").
-include("linc_us4.hrl").
-include("linc_us4_port.hrl").

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%-----------------------------------------------------------------------------
%%% API functions
%%%-----------------------------------------------------------------------------

%% @doc Start Open Flow port with provided configuration.
-spec start_link(integer(), list(linc_port_config())) -> {ok, pid()} |
                                                         ignore |
                                                         {error, term()}.
start_link(SwitchId, PortConfig) ->
    gen_server:start_link(?MODULE, [SwitchId, PortConfig], []).

-spec initialize(integer()) -> ok.
initialize(SwitchId) ->
    LincPorts = ets:new(linc_ports, [public,
                                     {keypos, #linc_port.port_no},
                                     {read_concurrency, true}]),
    linc:register(SwitchId, linc_ports, LincPorts),
    ok.

-spec terminate(integer()) -> ok.
terminate(SwitchId) ->
    [ok = remove(SwitchId, PortNo) || PortNo <- get_all_port_no(SwitchId)],
    true = ets:delete(linc:lookup(SwitchId, linc_ports)).

%% @doc Change config of the given OF port according to the provided port mod.
-spec modify(integer(), ofp_port_mod()) -> ok |
                                           {error, {Type :: atom(),
                                                    Code :: atom()}}.
modify(SwitchId, #ofp_port_mod{port_no = PortNo} = PortMod) ->
    case get_port_pid(SwitchId, PortNo) of
        {error, invalid} ->
            {error, {bad_request, bad_port}};
        {error, nonexistent} ->
            {error, {port_mod_failed, bad_port}};
        Pid ->
            gen_server:call(Pid, {port_mod, PortMod})
    end.

%% @doc Return list of all OFP ports present in the switch.
-spec get_desc(integer()) -> ofp_port_desc_reply().
get_desc(SwitchId) ->
    L = ets:foldl(fun(#linc_port{pid = Pid}, Ports) ->
                          Port = gen_server:call(Pid, get_port),
                          [Port | Ports]
                  end, [], linc:lookup(SwitchId, linc_ports)),
    #ofp_port_desc_reply{body = L}.

-spec get_state(integer(), ofp_port_no()) -> [ofp_port_state()].
get_state(SwitchId, PortNo) ->
    case get_port_pid(SwitchId, PortNo) of
        {error, _} ->
            {error, {bad_request, bad_port}};
        Pid ->
            gen_server:call(Pid, get_port_state)
    end.

-spec set_state(integer(), ofp_port_no(), [ofp_port_state()]) -> ok.
set_state(SwitchId, PortNo, PortState) ->
    case get_port_pid(SwitchId, PortNo) of
        {error, _} ->
            {error, {bad_request, bad_port}};
        Pid ->
            gen_server:call(Pid, {set_port_state, PortState})
    end.

-spec get_config(integer(), ofp_port_no()) -> [ofp_port_config()].
get_config(SwitchId, PortNo) ->
    case get_port_pid(SwitchId, PortNo) of
        {error, _} ->
            {error, {bad_request, bad_port}};
        Pid ->
            gen_server:call(Pid, get_port_config)
    end.

-spec set_config(integer(), ofp_port_no(), [ofp_port_config()]) -> ok.
set_config(SwitchId, PortNo, PortConfig) ->
    case get_port_pid(SwitchId, PortNo) of
        {error, _} ->
            {error, {bad_request, bad_port}};
        Pid ->
            gen_server:call(Pid, {set_port_config, PortConfig})
    end.

-spec get_features(integer(), ofp_port_no()) -> tuple([ofp_port_feature()],
                                                      [ofp_port_feature()],
                                                      [ofp_port_feature()],
                                                      [ofp_port_feature()]).
get_features(SwitchId, PortNo) ->
    case get_port_pid(SwitchId, PortNo) of
        {error, _} ->
            {error, {bad_request, bad_port}};
        Pid ->
            gen_server:call(Pid, get_features)
    end.

-spec get_advertised_features(integer(), ofp_port_no()) -> [ofp_port_feature()].
get_advertised_features(SwitchId, PortNo) ->
    case get_port_pid(SwitchId, PortNo) of
        {error, _} ->
            {error, {bad_request, bad_port}};
        Pid ->
            gen_server:call(Pid, get_advertised_features)
    end.

-spec set_advertised_features(integer(), ofp_port_no(), [ofp_port_feature()]) -> ok.
set_advertised_features(SwitchId, PortNo, AdvertisedFeatures) ->
    case get_port_pid(SwitchId, PortNo) of
        {error, _} ->
            {error, {bad_request, bad_port}};
        Pid ->
            gen_server:call(Pid, {set_advertised_features, AdvertisedFeatures})
    end.

-spec get_all_ports_state(integer()) -> list({ResourceId :: string(),
                                              ofp_port()}).
get_all_ports_state(SwitchId) ->
    lists:map(fun(PortNo) ->
                      Pid = get_port_pid(SwitchId, PortNo),
                      gen_server:call(Pid, get_info)
              end, get_all_port_no(SwitchId)).

-spec get_all_queues_state(integer()) -> list(tuple(string(), integer(), integer(),
                                                    integer(), integer())).
get_all_queues_state(SwitchId) ->
    lists:flatmap(fun(PortNo) ->
                          linc_us4_queue:get_all_queues_state(SwitchId, PortNo)
                  end, get_all_port_no(SwitchId)).

%%%-----------------------------------------------------------------------------
%%% gen_server callbacks
%%%-----------------------------------------------------------------------------

%% @private
init([SwitchId, {port, PortNo, PortOpts}]) ->
    process_flag(trap_exit, true),
    %% epcap crashes if this dir does not exist.
    filelib:ensure_dir(filename:join([code:priv_dir(epcap), "tmp", "ensure"])),
    PortName = "Port" ++ integer_to_list(PortNo),
    Advertised = case lists:keyfind(features, 1, PortOpts) of
                     {features, undefined} ->
                         ?FEATURES;
                     {features, Features} ->
                         linc_ofconfig:convert_port_features(Features)
                 end,
    PortConfig = case lists:keyfind(config, 1, PortOpts) of
                     {config, undefined} ->
                         [];
                     {config, Config} ->
                         linc_ofconfig:convert_port_config(Config)
                 end,
    Port = #ofp_port{port_no = PortNo,
                     name = PortName,
                     config = PortConfig,
                     state = [live],
                     curr = ?FEATURES,
                     advertised = Advertised,
                     supported = ?FEATURES, peer = ?FEATURES,
                     curr_speed = ?PORT_SPEED, max_speed = ?PORT_SPEED},
    SwitchName = "LogicalSwitch" ++ integer_to_list(SwitchId),
    ResourceId =  SwitchName ++ "-" ++ PortName,
    {interface, Interface} = lists:keyfind(interface, 1, PortOpts),
    State = #state{resource_id = ResourceId, interface = Interface, port = Port,
                   switch_id = SwitchId},
    Type = case lists:keyfind(type, 1, PortOpts) of
        {type, Type1} ->
            Type1;
        _ ->
            %% The type is not specified explicitly.
            %% Guess from the interface name.
            case re:run(Interface, "^tap.*$", [{capture, none}]) of
                match ->
                    tap;
                nomatch ->
                    eth
            end
    end,
    case Type of
        %% When switch connects to a tap interface, erlang receives file
        %% descriptor to read/write ethernet frames directly from the
        %% desired /dev/tapX character device. No socket communication
        %% is involved.
        tap ->
            case linc_us4_port_native:tap(Interface, PortOpts) of
                {stop, shutdown} ->
                    {stop, shutdown};
                {ErlangPort, Pid, HwAddr} ->
                    ets:insert(linc:lookup(SwitchId, linc_ports),
                               #linc_port{port_no = PortNo, pid = self()}),
                    ets:insert(linc:lookup(SwitchId, linc_port_stats),
                               #ofp_port_stats{port_no = PortNo,
                                               duration_sec = erlang:now()}),
                    {ok, State#state{erlang_port = ErlangPort,
                                     port_ref = Pid,
                                     port = Port#ofp_port{hw_addr = HwAddr}}}
            end;
        %% When switch connects to a hardware interface such as eth0
        %% then communication is handled by two channels:
        %% * receiving ethernet frames is done by libpcap wrapped-up by
        %%   a epcap application
        %% * sending ethernet frames is done by writing to
        %%   a RAW socket binded with given network interface.
        %%   Handling of RAW sockets differs between OSes.
        eth ->
            {Socket, IfIndex, EpcapPid, HwAddr} =
                linc_us4_port_native:eth(Interface),
            ets:insert(linc:lookup(SwitchId, linc_ports),
                       #linc_port{port_no = PortNo, pid = self()}),
            ets:insert(linc:lookup(SwitchId, linc_port_stats),
                       #ofp_port_stats{port_no = PortNo,
                                       duration_sec = erlang:now()}),
            {ok, State#state{socket = Socket,
                             ifindex = IfIndex,
                             epcap_pid = EpcapPid,
                             port = Port#ofp_port{hw_addr = HwAddr}}}
    end.

%% @private
handle_call({port_mod, #ofp_port_mod{hw_addr = PMHwAddr,
                                     config = Config,
                                     mask = _Mask,
                                     advertise = Advertise}}, _From,
            #state{port = #ofp_port{hw_addr = HWAddr} = Port} = State) ->
    {Reply, NewPort} = case PMHwAddr == HWAddr of
                           true ->
                               {ok, Port#ofp_port{config = Config,
                                                  advertised = Advertise}};
                           false ->
                               {{error, {port_mod_failed, bad_hw_addr}}, Port}
                       end,
    {reply, Reply, State#state{port = NewPort}};
handle_call(get_port, _From, #state{port = Port} = State) ->
    {reply, Port, State};
handle_call(get_port_state, _From,
            #state{port = #ofp_port{state = PortState}} = State) ->
    {reply, PortState, State};
handle_call({set_port_state, NewPortState}, _From,
            #state{port = Port, switch_id = SwitchId} = State) ->
    NewPort = Port#ofp_port{state = NewPortState},
    PortStatus = #ofp_port_status{reason = modify,
                                  desc = NewPort},
    linc_logic:send_to_controllers(SwitchId, #ofp_message{body = PortStatus}),
    {reply, ok, State#state{port = NewPort}};
handle_call(get_port_config, _From,
            #state{port = #ofp_port{config = PortConfig}} = State) ->
    {reply, PortConfig, State};
handle_call({set_port_config, NewPortConfig}, _From,
            #state{port = Port, switch_id = SwitchId} = State) ->
    NewPort = Port#ofp_port{config = NewPortConfig},
    PortStatus = #ofp_port_status{reason = modify,
                                  desc = NewPort},
    linc_logic:send_to_controllers(SwitchId, #ofp_message{body = PortStatus}),
    {reply, ok, State#state{port = NewPort}};
handle_call(get_features, _From,
            #state{port = #ofp_port{
                             curr = CurrentFeatures,
                             advertised = AdvertisedFeatures,
                             supported = SupportedFeatures,
                             peer  = PeerFeatures
                            }} = State) ->
    {reply, {CurrentFeatures, AdvertisedFeatures,
             SupportedFeatures, PeerFeatures}, State};
handle_call(get_advertised_features, _From,
            #state{port = #ofp_port{advertised = AdvertisedFeatures}} = State) ->
    {reply, AdvertisedFeatures, State};
handle_call({set_advertised_features, AdvertisedFeatures}, _From,
            #state{port = Port, switch_id = SwitchId} = State) ->
    NewPort = Port#ofp_port{advertised = AdvertisedFeatures},
    PortStatus = #ofp_port_status{reason = modify,
                                  desc = NewPort},
    linc_logic:send_to_controllers(SwitchId, #ofp_message{body = PortStatus}),
    {reply, ok, State#state{port = NewPort}};
handle_call(get_info, _From, #state{resource_id = ResourceId,
                                    port = Port} = State) ->
    {reply, {ResourceId, Port}, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, #state{port = #ofp_port{port_no = PortNo},
                          switch_id = SwitchId}) ->
    true = ets:delete(linc:lookup(SwitchId, linc_ports), PortNo).

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @doc Return list of all OFP port numbers present in the switch.
-spec get_all_port_no(integer()) -> [integer()].
get_all_port_no(SwitchId) ->
    ets:foldl(fun(#linc_port{port_no = PortNo}, Acc) ->
                      [PortNo | Acc]
              end, [], linc:lookup(SwitchId, linc_ports)).

%% @doc Removes given OF port from the switch, as well as its port stats entry,
%% all queues connected to it and their queue stats entries.
-spec remove(integer(), ofp_port_no()) -> ok | bad_port.
remove(SwitchId, PortNo) ->
    case get_port_pid(SwitchId, PortNo) of
        {error, _} ->
            bad_port;
        Pid ->
            Sup = linc:lookup(SwitchId, linc_us4_port_sup),
            ok = supervisor:terminate_child(Sup, Pid)
    end.

-spec get_port_pid(integer(), ofp_port_no()) -> pid() | {error, invalid | nonexistent}.
get_port_pid(_SwitchId, PortNo) when is_atom(PortNo); PortNo > ?OFPP_MAX ->
    {error, invalid};
get_port_pid(SwitchId, PortNo) ->
    case ets:lookup(linc:lookup(SwitchId, linc_ports), PortNo) of
        [] ->
            {error, nonexistent};
        [#linc_port{pid = Pid}] ->
            Pid
    end.
