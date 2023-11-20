-module(mock_phy_network).
-behaviour(gen_server).

-include("../src/mac_frame.hrl").


-export([start_link/2]).
-export([start/2]).
-export([stop_link/0]).

-export([transmit/2]).
-export([reception/0]).
-export([reception/1]).
-export([disable_rx/0]).

-export([read/1]).
-export([write/2]).

%%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

start_link(_Connector, Params) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).

start(_Connector, Params) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Params, []).

stop_link() ->
    gen_server:stop(?MODULE).

transmit(Frame, Options) ->
    gen_server:call(?MODULE, {transmit, Frame, Options}).

reception() -> 
    #{rxfwto := RXFWTO} = gen_server:call(?MODULE, {read, rx_fwto}),
    Ref = make_ref(),
    gen_server:cast(?MODULE, {reception, Ref, self()}),
    receive 
        {Ref, Ret} -> Ret
    after RXFWTO -> rxfwto
    end.

reception(_RxOpts) ->
    gen_server:call(?MODULE, {reception}).

disable_rx() ->
    gen_server:call(?MODULE, {disable_rx}).

read(Reg) ->
    gen_server:call(?MODULE, {read, Reg}).

write(Reg, Value) ->
    gen_server:call(?MODULE, {write, Reg, Value}).

%--- Internal: gen server callbacks ---------------------------------------------

init(#{network := NetworkNode}) ->
    {network_loop, NetworkNode} ! {register, node()},
    ets:new(callback_table, [public, named_table]),
    {ok, #{regs => pmod_uwb_registers:default(), network => NetworkNode}}.

handle_call({read, Reg}, _From, #{regs := Regs} = State) -> {reply, pmod_uwb_registers:get_value(Regs, Reg), State};
handle_call({write, Reg, Value}, _From, #{regs := Regs} = State) -> {reply, ok, State#{regs => pmod_uwb_registers:update_reg(Regs, Reg, Value)}};
handle_call({reception}, _From, #{regs := #{eui := ExtAddress, panadr := #{short_addr := ShortAddress}, rx_fwto := #{rxfwto := RXFWTO}}} = State) -> {reply, rx(ShortAddress, ExtAddress, RXFWTO), State};
handle_call({transmit, Frame, _}, _From, #{network := NetworkNode} = State) -> {reply, tx(NetworkNode, Frame), State};
handle_call(_Call, _From, State) -> io:format("Call not recognized"), {reply, ok, State}.

handle_cast({reception, Ref, From}, State) -> {noreply, State#{waiting => {From, Ref}}}.

handle_info({frame, Frame}, #{network := NetworkNode, waiting := {From, Ref}, regs := #{eui := #{eui := ExtAddress}, panadr := #{short_addr := ShortAddress}}} = State) ->
    case check_address(Frame, ShortAddress, ExtAddress) of
        ok -> ack_reply(NetworkNode, Frame), From ! {Ref, {byte_size(Frame), Frame}};
        _ -> ok
    end,
    {noreply, maps:remove(waiting, State)};
handle_info({frame, _}, State) ->
    {noreply, State}.

terminate(Reason, #{network := NetworkNode}) ->
    io:format("Terminate: ~w", [Reason]),
    {network_loop, NetworkNode} ! {unreg, node()}.

%--- Internal -------------------------------------------------------------------
tx(NetworkNode, Frame) ->
    {network_loop, NetworkNode} ! {tx, Frame},
    ok.

rx(ShortAddress, ExtAddress, RXFWTO) ->
    receive 
        {frame, Frame} ->
            case check_address(Frame, ShortAddress, ExtAddress) of
                ok -> {byte_size(Frame), Frame};
                continue -> rx(ShortAddress, ExtAddress, RXFWTO)
            end
    after RXFWTO -> rxfwto 
    end.

check_address(Frame, ShortAddress, ExtAddress) ->
    {_, MacHeader, _} = mac_frame:decode(Frame),
    case MacHeader#mac_header.dest_addr of
        ShortAddress -> ok;
        ExtAddress -> ok;
        _ -> continue 
    end.

ack_reply(NetworkNode, Frame) ->
    <<_:2, ACKREQ:1, _/bitstring>> = Frame,
    io:format("Ack req: ~w ~n ~w", [ACKREQ, Frame]),
    case Frame of
        <<_:2, ?ENABLED:1, _:13, Seqnum:8, _/bitstring>> -> io:format("Ack requested~n"), tx(NetworkNode, mac_frame:encode_ack(?DISABLED, Seqnum));
        _ -> io:format("No Ack requested~n"), ok
    end.
