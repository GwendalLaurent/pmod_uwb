-module(duty_cycle_non_beacon).

-behaviour(gen_duty_cycle).

% gen_duty_cycle callbacks

-export([init/1]).
-export([on/2]).
-export([off/1]).
-export([tx/6]).
-export([rx/1]).
-export([terminate/2]).

% Include

-include("gen_mac_layer.hrl").
-include("mac_frame.hrl").
-include("ieee802154.hrl").

%% @doc 
%% The module implementing this behaviour is responsible for the duty cycling of the stack. This includes:
%% <li> IEEE 802.15.4 duty cycling: Beacon enabled network, non-beacon enabled network </li>
%% <li> pmod uwb duty cycling: low power listening, sniff mode </li>
%% @end

%--- Types ---------------------------------------------------------------------


%--- Records -------------------------------------------------------------------
-export_type([state/0]).

-record(state, {sniff_ont, sniff_offt, phy_layer, loop_pid, callback, mac_tx_state}).
-opaque state() :: #state{}.

%--- gen_duty_cycle callbacks --------------------------------------------------
-spec init(PhyMod) -> State when
      PhyMod :: module(),
      State  :: state().
init(PhyMod) ->
    MacTXState = gen_mac_tx:start(unslotted_CSMA, PhyMod),
    #state{sniff_ont = 3, sniff_offt = 4, phy_layer = PhyMod, loop_pid = undefined, callback = undefined, mac_tx_state = MacTXState}.

-spec on(State, Callback) -> {ok, State} when
      State    :: state(),
      Callback :: function().
on(#state{phy_layer = PhyMod, sniff_ont = SNIFF_ONT, sniff_offt = SNIFF_OFFT} = State, Callback) ->
    LoopPid = turn_on_rx_loop(PhyMod, SNIFF_ONT, SNIFF_OFFT, Callback),
    {ok, State#state{loop_pid = LoopPid, callback = Callback}}.

-spec off(State) -> {ok, State} when
      State :: state().
off(#state{phy_layer = PhyMod, loop_pid = LoopPid} = State) ->
    turn_off_rx_loop(PhyMod, LoopPid, shutdown),
    {ok, State#state{loop_pid = undefined}}.

-spec tx(State, Frame, MacMinBE, MacMaxBE, MacMaxCSMABackoffs, CW0) -> Result when
    State              ::state(), 
    Frame              ::bitstring(), 
    MacMinBE           ::mac_min_BE(), 
    MacMaxBE           ::mac_max_BE(), 
    MacMaxCSMABackoffs ::max_max_csma_backoff(), 
    CW0                ::cw0(),
    Result             :: {ok, State::term()} | {error, State::term(), Error::no_ack|frame_too_long|channel_access_failure|atom()}.
tx(#state{phy_layer = PhyMod, mac_tx_state = MacTXState, loop_pid = undefined} = State, Frame, MacMinBE, MacMaxBE, MacMaxCSMABackoffs, CW0) ->
    case tx_(MacTXState, PhyMod, Frame, MacMinBE, MacMaxBE, MacMaxCSMABackoffs, CW0) of
        {ok, NewMacTxState} -> {ok, State#state{mac_tx_state = NewMacTxState}};
        {error, NewMacTxState, Error} -> {error, State#state{mac_tx_state = NewMacTxState}, Error}
    end;
tx(#state{phy_layer = PhyMod, mac_tx_state = MacTXState, loop_pid = LoopPid, callback = Callback, sniff_ont = SNIFF_ONT, sniff_offt = SNIFF_OFFT} = State, Frame, MacMinBE, MacMaxBE, MacMaxCSMABackoffs, CW0) ->
    suspend_rx_loop(PhyMod, LoopPid),
    TxStatus = tx_(MacTXState, PhyMod, Frame, MacMinBE, MacMaxBE, MacMaxCSMABackoffs, CW0),
    NewLoopPid = resume_rx_loop(PhyMod, LoopPid, SNIFF_ONT, SNIFF_OFFT, Callback),
    case TxStatus of
        {ok, NewMacTxState} -> {ok, State#state{loop_pid = NewLoopPid, mac_tx_state = NewMacTxState}};
        {error, NewMacTxState, Error} -> {error, State#state{loop_pid = NewLoopPid, mac_tx_state = NewMacTxState}, Error}
    end.

-spec rx(State) -> {ok, State, Frame} | {error, State, Error} when
      State :: state(),
      Frame :: bitstring(),
      Error :: atom().
rx(#state{phy_layer = PhyMod} = State) ->
   case rx_(PhyMod) of
       {ok, Frame} -> {ok, State, Frame};
       {error, Error} -> {error, State, Error}
   end.

-spec terminate(State, Reason) -> ok when
      State :: state(),
      Reason :: atom().
terminate(#state{loop_pid = LoopPid, phy_layer = PhyMod, mac_tx_state = MacTXState}, Reason) ->
    turn_off_rx_loop(PhyMod, LoopPid, Reason),
    gen_mac_tx:stop(MacTXState, Reason),
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

turn_off_rx_loop(_, undefined, _) -> ok;
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
% @returns {ok, NewMacTxState} | {error, NewMacTxState, Error}
-spec tx_(MacTXState, PhyMod, Frame, MacMinBE, MacMaxBE, MacMaxCSMABackoffs, CW0) -> {ok, MacTXState} | {error, MacTXState, Error} when
      MacTXState         :: gen_mac_tx:state(),
      PhyMod             :: module(),
      Frame              :: bitstring(),
      MacMinBE           :: pos_integer(),
      MacMaxBE           :: pos_integer(),
      MacMaxCSMABackoffs :: pos_integer(),
      CW0                :: pos_integer(),
      Error              :: atom().
tx_(MacTXState, PhyMod, <<_:2, ?ENABLED:1, _:13, Seqnum:8, _/binary>> = Frame, MacMinBE, MacMaxBE, MacMaxCSMABackoffs, CW0) -> tx_ar(MacTXState, PhyMod, Frame, Seqnum, 0, MacMinBE, MacMaxBE, MacMaxCSMABackoffs, CW0);
tx_(MacTXState, _PhyMod, Frame, MacMinBE, MacMaxBE, MacMaxCSMABackoffs, CW0) ->
    gen_mac_tx:transmit(MacTXState, Frame, MacMinBE, MacMaxBE, MacMaxCSMABackoffs, CW0, #tx_opts{}).

% @private
% @doc This function transmits a frame with AR=1
% If the ACK isn't received before the timeout, a retransmission is done
% If the frame has been transmitted MACMAXFRAMERETRIES times then the error `no_ack' is returned
% @returns {ok, NewMacTxState} | {error, NewMacTxState, Error}
-spec tx_ar(MacTXState, PhyMod, Frame, Seqnum, Retry, MacMinBE, MacMaxBE, MacMaxCSMABackoffs, CW0) -> {ok, MacTxState} | {error, MacTxState, Error} when
      MacTXState         :: gen_mac_tx:state(),
      PhyMod             :: module(),
      Frame              :: bitstring(),
      Seqnum             :: non_neg_integer(),
      Retry              :: non_neg_integer(),
      MacMinBE           :: pos_integer(),
      MacMaxBE           :: pos_integer(),
      MacMaxCSMABackoffs :: pos_integer(),
      CW0                :: pos_integer(),
      Error              :: atom().
tx_ar(MacTxState, _, _, _, ?MACMAXFRAMERETRIES, _, _, _, _) ->  {error, MacTxState, no_ack};
tx_ar(MacTXState, PhyMod, Frame, Seqnum, Retry, MacMinBE, MacMaxBE, MacMaxCSMABackoffs, CW0) ->
    case gen_mac_tx:transmit(MacTXState, Frame, MacMinBE, MacMaxBE, MacMaxCSMABackoffs, CW0, #tx_opts{wait4resp = ?ENABLED}) of
        {ok, NewMacTxState} -> 
            case PhyMod:reception(true) of
                {_, <<_:16, Seqnum:8>>} -> {ok, NewMacTxState};
                _  -> tx_ar(NewMacTxState, PhyMod, Frame, Seqnum, Retry+1, MacMinBE, MacMaxBE, MacMaxCSMABackoffs, CW0)
            end;
        {error, NewMacTxState, _Error} -> tx_ar(NewMacTxState, PhyMod, Frame, Seqnum, Retry+1, MacMinBE, MacMaxBE, MacMaxCSMABackoffs, CW0)
    end.

