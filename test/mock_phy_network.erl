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

-export([set_preamble_timeout/1]).
-export([disable_preamble_timeout/0]).

-export([suspend_frame_filtering/0]).
-export([resume_frame_filtering/0]).

-export([read/1]).
-export([write/2]).

-export([rx_ranging_info/0]).
-export([signal_power/0]).
-export([rx_preamble_repetition/0]).
-export([rx_data_rate/0]).
-export([prf_value/0]).

%%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

%--- Records -------------------------------------------------------------------



%--- API -----------------------------------------------------------------------

start_link(_Connector, Params) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).

start(_Connector, Params) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Params, []).

stop_link() ->
    gen_server:stop(?MODULE).

transmit(Frame, Options) ->
    gen_server:call(?MODULE, {transmit, Frame, Options}).

reception() -> 
    case {read(drx_conf), read(rx_fwto)} of
        {#{drx_pretoc := 0}, #{rxfwto := RXFWTO}} -> rx_(RXFWTO, rxfwto);
        {#{drx_pretoc := PRETOC}, _} -> rx_(round(PRETOC/10), rxpto)
    end.

rx_(Timeout, TimeoutError) -> 
    try
        gen_server:call(?MODULE, {reception}, Timeout)
    catch
        exit:{timeout, _} -> TimeoutError
    end.

reception(_RxOpts) ->
    reception().
    % gen_server:call(?MODULE, {reception}).

disable_rx() ->
    gen_server:call(?MODULE, {disable_rx}).

set_preamble_timeout(Timeout) ->
    write(drx_conf, #{drx_pretoc => Timeout - 1}).

disable_preamble_timeout() ->
    write(drx_conf, #{drx_pretoc => 0}).

suspend_frame_filtering() ->
    write(sys_cfg, #{ffen => 0}).

resume_frame_filtering() ->
    write(sys_cfg, #{ffen => 1}).

read(Reg) ->
    gen_server:call(?MODULE, {read, Reg}).

write(Reg, Value) ->
    gen_server:call(?MODULE, {write, Reg, Value}).

%--- API: Getters --------------------------------------------------------------
rx_ranging_info() ->
    #{rng := RNG} = read(rx_finfo),
    RNG.

%% @doc Returns the estimated value of the signal power in dBm
%% cf. user manual section 4.7.2
signal_power() ->
    C = channel_impulse_resp_pow() , % Channel impulse resonse power value (CIR_PWR)
    A = case prf_value() of
            16 -> 113.77;
            64 -> 121.74
        end, % Constant. For PRF of 16 MHz = 113.77, for PRF of 64MHz = 121.74
    N = preamble_acc(), % Preamble accumulation count value (RXPACC but might be ajusted)
    Num = C* math:pow(2, 17),
    Dem = math:pow(N, 2),
    Log = math:log10(Num / Dem),
    10 *  Log - A.

preamble_acc() ->
    #{rxpacc := RXPACC} = read(rx_finfo),
    #{rxpacc_nosat := RXPACC_NOSAT} = read(drx_conf),
    if 
        RXPACC == RXPACC_NOSAT -> RXPACC;
        true -> RXPACC - 5
    end.

channel_impulse_resp_pow() ->
    #{cir_pwr := CIR_POW} = read(rx_fqual),
    CIR_POW.

%% @doc Gives the value of the PRF in MHz 
-spec prf_value() -> 16 | 64.
prf_value() ->
    #{agc_tune1 := AGC_TUNE1} = read(agc_ctrl),
    case AGC_TUNE1 of
        16#8870 -> 16;
        16#889B -> 64
    end.

%% @doc returns the preamble symbols repetition
rx_preamble_repetition() ->
    #{rxpsr := RXPSR} = read(rx_finfo),
    case RXPSR of
        0 -> 16;
        1 -> 64;
        2 -> 1024;
        3 -> 4096
    end.

%% @doc returns the data rate of the received frame in kbps
rx_data_rate() ->
    #{rxbr := RXBR} = read(rx_finfo),
    case RXBR of
        0 -> 110;
        1 -> 850;
        3 -> 6800
    end.

%--- Internal: gen server callbacks --------------------------------------------

init(#{network := NetworkNode}) ->
    {network_loop, NetworkNode} ! {register, node()},
    ets:new(callback_table, [public, named_table]),
    {ok, #{regs => pmod_uwb_registers:default(), network => NetworkNode}}.

handle_call({read, Reg}, _From, #{regs := Regs} = State) -> {reply, pmod_uwb_registers:get_value(Regs, Reg), State};
handle_call({write, Reg, Value}, _From, #{regs := Regs} = State) -> {reply, ok, State#{regs => pmod_uwb_registers:update_reg(Regs, Reg, Value)}};
handle_call({transmit, Frame, Options}, _From, #{network := NetworkNode} = State) ->
    #{regs := Regs} = State,
    NewRegs = pmod_uwb_registers:update_reg(Regs, tx_fctrl, #{tr => Options#tx_opts.ranging}),
    PhyFrame = {Options#tx_opts.ranging, Frame},
    {reply, tx(NetworkNode, PhyFrame), State#{regs => NewRegs}};
handle_call({disable_rx}, _From, State) -> {reply, ok, maps:remove(waiting, State)};
handle_call({reception}, From, State) -> {noreply, State#{waiting => From}};
handle_call(_Call, _From, State) -> io:format("Call not recognized in mock pmod_uwb: ~p", [_Call]), {reply, ok, State}.

handle_cast(Request, _) ->
    error({unknown_cast, Request}).


handle_info({frame, Frame}, #{network := NetworkNode, waiting := From, regs := #{eui := #{eui := ExtAddress}, panadr := #{short_addr := ShortAddress}, sys_cfg := #{ffen := 1}}} = State) ->
    #{regs := Regs} = State,
    {Ranging, RawFrame} = Frame,
    case check_address(RawFrame, ShortAddress, ExtAddress) of
        ok -> ack_reply(NetworkNode, RawFrame),
              NewRegs = pmod_uwb_registers:update_reg(Regs, rx_finfo, #{rng => Ranging}),
              NewState = maps:remove(waiting, State),
              gen_server:reply(From, {byte_size(RawFrame), RawFrame}),
              {noreply, NewState#{regs := NewRegs}};
        _  -> 
            gen_server:reply(From, affrej),
            {noreply, maps:remove(waiting, State)}
    end;
handle_info({frame, Frame}, #{waiting := From, regs := #{sys_cfg := #{ffen := 0}}} = State) ->
    #{regs := Regs} = State,
    {Ranging, RawFrame} = Frame,
    NewRegs = pmod_uwb_registers:update_reg(Regs, rx_finfo, #{rng => Ranging}),
    NewState = maps:remove(waiting, State),
    gen_server:reply(From, {byte_size(RawFrame), RawFrame}),
    {noreply, NewState#{regs := NewRegs}};
handle_info({frame, _}, State) ->
    {noreply, State}.

terminate(Reason, _) ->
    io:format("Terminate: ~w", [Reason]).

%--- Internal -------------------------------------------------------------------
tx(NetworkNode, Frame) ->
    ct:log("Tx frame"),
    {network_loop, NetworkNode} ! {tx, node(), Frame},
    ok.

check_address(Frame, ShortAddress, ExtAddress) -> % This will need to check the PAN and accept broadcast address at some point
    {_, MacHeader, _} = mac_frame:decode(Frame),
    case MacHeader#mac_header.dest_addr of
        ShortAddress -> ok;
        ExtAddress -> ok;
        _ -> continue 
    end.

ack_reply(NetworkNode, Frame) ->
    <<_:2, _ACKREQ:1, _/bitstring>> = Frame,
    % io:format("Ack req: ~w ~n ~w", [ACKREQ, Frame]),
    case Frame of
        <<_:2, ?ENABLED:1, _:13, Seqnum:8, _/bitstring>> ->
            % io:format("Ack requested~n"),
            AckFrame = {?DISABLED, mac_frame:encode_ack(?DISABLED, Seqnum)},
            tx(NetworkNode, AckFrame);
        _ ->
            % io:format("No Ack requested~n"),
            ok
    end.
