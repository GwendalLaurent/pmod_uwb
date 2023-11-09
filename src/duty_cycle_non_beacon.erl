-module(duty_cycle_non_beacon).

-behaviour(gen_duty_cycle).

% gen_duty_cycle callbacks

-export([init/1]).
-export([on/2]).
-export([off/1]).
-export([tx/2]).
-export([rx/1]).
-export([terminate/2]).

% Include

-include("gen_mac_layer.hrl").
-include("mac_frame.hrl").

%% @doc 
%% The module implementing this behaviour is responsible for the duty cycling of the stack. This includes:
%% <li> IEEE 802.15.4 duty cycling: Beacon enabled network, non-beacon enabled network </li>
%% <li> pmod uwb duty cycling: low power listening, sniff mode </li>
%% @end

%--- Records ----------------------------------------------------------
-record(state, {sniff_ont, sniff_offt, phy_layer, loop_pid, callback}).

%--- gen_duty_cycle callbacks -------------------------------------------
init(PhyMod) ->
    #state{sniff_ont = 3, sniff_offt = 4, phy_layer = PhyMod, loop_pid = undefined, callback = undefined}.

on(#state{phy_layer = PhyMod, sniff_ont = SNIFF_ONT, sniff_offt = SNIFF_OFFT} = State, Callback) ->
    LoopPid = turn_on_rx_loop(PhyMod, SNIFF_ONT, SNIFF_OFFT, Callback),
    {ok, State#state{loop_pid = LoopPid, callback = Callback}}.

off(#state{phy_layer = PhyMod, loop_pid = LoopPid} = State) ->
    turn_off_rx_loop(PhyMod, LoopPid, shutdown),
    {ok, State#state{loop_pid = undefined}}.

-spec tx(State::term(), Frame::bitstring()) -> {ok, State::term()} | {error, State::term(), Error::no_ack|frame_too_long|channel_access_failure|atom()}.
tx(#state{phy_layer = PhyMod, loop_pid = undefined} = State, Frame) ->
    case tx_(PhyMod, Frame) of
        ok -> {ok, State};
        {error, Error} -> {error, State, Error}
    end;
tx(#state{phy_layer = PhyMod, loop_pid = LoopPid, callback = Callback, sniff_ont = SNIFF_ONT, sniff_offt = SNIFF_OFFT} = State, Frame) ->
    suspend_rx_loop(PhyMod, LoopPid),
    TxStatus = tx_(PhyMod, Frame),
    NewLoopPid = resume_rx_loop(PhyMod, LoopPid, SNIFF_ONT, SNIFF_OFFT, Callback),
    case TxStatus of
        ok -> {ok, State#state{loop_pid = NewLoopPid}};
        {error, Error} -> {error, State#state{loop_pid = NewLoopPid}, Error}
    end.

-spec rx(State::term()) -> {ok, State::term(), Frame::bitstring()} | {error, State::term(), Error::atom}.
rx(#state{phy_layer = PhyMod} = State) ->
   case rx_(PhyMod) of
       {ok, Frame} -> {ok, State, Frame};
       {error, Error} -> {error, State, Error}
   end.

-spec terminate(State::term(), Reason::term()) -> ok.
terminate(#state{loop_pid = undefined}, _) -> ok;
terminate(#state{loop_pid = LoopPid, phy_layer = PhyMod}, Reason) ->
    turn_off_rx_loop(PhyMod, LoopPid, Reason),
    ok.

%--- internal --------------------------------------------------------------

% Note: this loop should disappear in the futur when we use OS interrupts
-spec rx_(Phy::module()) -> {ok, Frame::bitstring()} | {error, Error::atom()}. 
rx_(Phy) ->
    case Phy:reception() of
        {_Length, Frame} -> {ok, Frame};
        Err -> {error, Err}
    end.

rx_loop(Phy, Callback) -> 
    case rx_(Phy) of
        {ok, Frame} -> 
            Callback(Frame), 
            rx_loop(Phy, Callback);
        {error, _Err} -> rx_loop(Phy, Callback) % Log that an error occured ?
    end.

turn_on_rx_loop(PhyMod, SNIFF_ONT, SNIFF_OFFT, Callback) ->
    PhyMod:write(sys_cfg, #{rxwtoe => 0}),
    PhyMod:write(pmsc, #{pmsc_ctrl1 => #{arx2init => 2#1}}),
    PhyMod:write(rx_sniff, #{sniff_ont => SNIFF_ONT, sniff_offt => SNIFF_OFFT}),
    spawn_link(fun() -> rx_loop(PhyMod, Callback) end).

turn_off_rx_loop(PhyMod, LoopPid, Reason) ->
    unlink(LoopPid), 
    exit(LoopPid, Reason),
    PhyMod:write(pmsc, #{pmsc_ctrl1 => #{arx2init => 2#0}}),
    PhyMod:write(rx_sniff, #{sniff_ont => 2#0, sniff_offt => 2#0}),
    PhyMod:disable_rx(),
    PhyMod:write(sys_cfg, #{rxwtoe => 1}).

suspend_rx_loop(PhyMod, LoopPid) ->
    turn_off_rx_loop(PhyMod, LoopPid, shutdown).

-spec resume_rx_loop(PhyMod::module(), LoopPid::pid(), SNIFF_ONT::integer(), SNIFF_OFFT::integer(), Callback::function()) -> pid().
resume_rx_loop(PhyMod, _, SNIFF_ONT, SNIFF_OFFT, Callback) -> turn_on_rx_loop(PhyMod, SNIFF_ONT, SNIFF_OFFT, Callback). 

% @private
tx_(PhyMod, <<_:2, ?ENABLED:1, _:13, Seqnum:8, _/binary>> = Frame) -> tx_ar(PhyMod, Frame, Seqnum, 0);
tx_(PhyMod, Frame) ->
    mac_tx:tx(PhyMod, Frame).

% @private
% @doc This function transmits a frame with AR=1
% If the ACK isn't received before the timeout, a retransmission is done
% If the frame has been transmitted MACMAXFRAMERETRIES times then the error `no_ack' is returned
% @end
tx_ar(_, _, _, ?MACMAXFRAMERETRIES) ->  {error, no_ack};
tx_ar(PhyMod, Frame, Seqnum, Retry) ->
    case mac_tx:tx(PhyMod, Frame, #tx_opts{wait4resp = ?ENABLED}) of
        ok -> 
            case PhyMod:reception(true) of
                {_, <<_:16, Seqnum:8>>} -> ok;
                _  -> tx_ar(PhyMod, Frame, Seqnum, Retry+1)
            end;
        {error, _} -> tx_ar(PhyMod, Frame, Seqnum, Retry+1)
    end.

