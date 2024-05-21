-module(duty_cycle_non_beacon).

-behaviour(gen_duty_cycle).

% gen_duty_cycle callbacks

-export([init/1]).
-export([on/3]).
-export([off/1]).
-export([tx/4]).
-export([rx/1]).
-export([terminate/2]).

% Include

-include("mac_frame.hrl").
-include("ieee802154_pib.hrl").
-include("ieee802154.hrl").

%% @doc
%% The module implementing this behaviour manages the duty cycling of the stack.
%% This includes:
%% <li>
%%  IEEE 802.15.4 duty cycling:
%%  Beacon enabled network, non-beacon enabled network
%% </li>
%% <li> pmod uwb duty cycling: low power listening, sniff mode </li>
%% @end

%--- Types ---------------------------------------------------------------------


%--- Records -------------------------------------------------------------------
-export_type([state/0]).

-record(state,
        {sniff_ont,
         sniff_offt,
         phy_layer,
         loop_pid,
         callback,
         mac_tx_state,
         ranging}).
-opaque state() :: #state{}.

%--- gen_duty_cycle callbacks --------------------------------------------------
-spec init(PhyMod) -> State when
      PhyMod :: module(),
      State  :: state().
init(PhyMod) ->
    MacTXState = gen_mac_tx:start(unslotted_CSMA, PhyMod),
    #state{sniff_ont = 3,
           sniff_offt = 4,
           phy_layer = PhyMod,
           loop_pid = undefined,
           callback = undefined,
           mac_tx_state = MacTXState}.

-spec on(State, Callback, Ranging) -> Result when
      State    :: state(),
      Callback :: gen_duty_cycle:input_callback_raw_frame(),
      Ranging  :: boolean(),
      Result   :: {ok, State} | {error, State, rx_already_on}.
on(#state{loop_pid = undefined} = State, Callback, Ranging) ->
    IntState = State#state{callback = Callback},
    LoopPid = rx_loop_on(IntState, Ranging),
    {ok, IntState#state{loop_pid = LoopPid,
                     ranging = Ranging}
    };
on(State, _, _) ->
    {error, State, rx_already_on}.

-spec off(State) -> {ok, State} when
      State :: state().
off(#state{phy_layer = PhyMod, loop_pid = LoopPid} = State) ->
    turn_off_rx_loop(PhyMod, LoopPid, shutdown),
    {ok, State#state{loop_pid = undefined}}.

-spec tx(State, Frame, CsmaParams, Ranging) -> Result when
    State       :: state(),
    Frame       :: binary(),
    CsmaParams  :: pib_state(),
    Ranging     :: ranging_tx(),
    Result      :: {ok, State, RangingInfo}
                   | {error, State, Error},
    RangingInfo :: ranging_informations(),
    Error       :: tx_error().
tx(#state{loop_pid = undefined} = State, Frame, CsmaParams, Ranging) ->
    case tx_(State, Frame, CsmaParams, Ranging) of
        {ok, NewMacTxState} ->
            RangingInfo = tx_ranging_infos(Ranging, State),
            {ok, State#state{mac_tx_state = NewMacTxState}, RangingInfo};
        {error, NewMacTxState, Error} ->
            {error, State#state{mac_tx_state = NewMacTxState}, Error}
    end;
tx(State, Frame, CsmaParams, Ranging) ->
    suspend_rx_loop(State),
    TxStatus = tx_(State, Frame, CsmaParams, Ranging),
    NewLoopPid = resume_rx_loop(State),
    case TxStatus of
        {ok, NewMacTxState} ->
            RangingInfo = tx_ranging_infos(Ranging, State),
            {ok,
             State#state{loop_pid = NewLoopPid,
                         mac_tx_state = NewMacTxState},
            RangingInfo};
        {error, NewMacTxState, Error} ->
            {error, State#state{loop_pid = NewLoopPid,
                                mac_tx_state = NewMacTxState},
             Error}
    end.

-spec rx(State) -> {ok, State, Frame} | {error, State, Error} when
      State :: state(),
      Frame :: binary(),
      Error :: atom().
rx(#state{phy_layer = PhyMod, loop_pid = undefined} = State) ->
   case rx_(PhyMod) of
       {ok, Frame} -> {ok, State, Frame};
       {error, Error} -> {error, State, Error}
   end;
rx(State) ->
    {error, State, rx_already_on}.

-spec terminate(State, Reason) -> ok when
      State :: state(),
      Reason :: atom().
terminate(State, Reason) ->
    LoopPid = State#state.loop_pid,
    PhyMod = State#state.phy_layer,
    MacTXState = State#state.mac_tx_state,
    turn_off_rx_loop(PhyMod, LoopPid, Reason),
    gen_mac_tx:stop(MacTXState, Reason),
    ok.

%--- internal --------------------------------------------------------------

% Note: this loop should disappear in the futur when we use OS interrupts
-spec rx_(PhyMod) -> Result when
      PhyMod :: module(),
      Result :: {ok, Frame} | {error, Error},
      Frame  :: binary(),
      Error  :: atom().
rx_(PhyMod) ->
    case PhyMod:reception() of
        {_Length, Frame} -> {ok, Frame};
        Err -> {error, Err}
    end.

%% @doc loop function for the reception
%% This function waits for a reception event to occur
%% If the event is the reception of a frame,
%% it will call the callback function to notify the next higher level/layer
%% If the event is an error, the function ignores it
%% @end
-spec rx_loop(PhyMod, Callback, Ranging) -> ok when
      PhyMod :: module(),
      Callback :: gen_duty_cycle:input_callback_raw_frame(),
      Ranging :: flag().
rx_loop(PhyMod, Callback, Ranging) ->
    case rx_(PhyMod) of
        {ok, Frame} ->
            RNG = PhyMod:rx_ranging_info(),
            RangingInfo = rx_ranging_infos(Ranging, RNG, PhyMod),
            Callback(Frame,
                     snr(PhyMod),
                     PhyMod:prf_value(),
                     #security{},
                     PhyMod:rx_preamble_repetition(),
                     PhyMod:rx_data_rate(),
                     RangingInfo),
            rx_loop(PhyMod, Callback, Ranging);
        % Log that an error occured ?
        {error, _Err} -> rx_loop(PhyMod, Callback, Ranging)
    end.

%% @doc
%% Sets the settings for reception and turns on the reception loop process
%% Returns the pid of the loop process
%% Note: this function will change when OS interrupts are introduced
%% @end
-spec rx_loop_on(State, Ranging) -> pid() when
      State      :: state(),
      Ranging    :: flag().
rx_loop_on(State, Ranging) ->
    PhyMod = State#state.phy_layer,
    SniffOnT = State#state.sniff_ont,
    SniffOffT = State#state.sniff_offt,
    Callback = State#state.callback,
    PhyMod:write(tx_fctrl, #{tr => 1}),
    PhyMod:write(sys_cfg, #{rxwtoe => 0}),
    PhyMod:write(pmsc, #{pmsc_ctrl1 => #{arx2init => 2#1}}),
    PhyMod:write(rx_sniff, #{sniff_ont => SniffOnT, sniff_offt => SniffOffT}),
    spawn_link(fun() -> rx_loop(PhyMod, Callback, Ranging) end).

-spec turn_off_rx_loop(PhyMod, LoopPid, Reason) -> ok when
      PhyMod  :: module(),
      LoopPid :: pid() | undefined,
      Reason  :: atom().
turn_off_rx_loop(_, undefined, _) -> ok;
turn_off_rx_loop(PhyMod, LoopPid, Reason) ->
    unlink(LoopPid),
    exit(LoopPid, Reason),
    PhyMod:write(pmsc, #{pmsc_ctrl1 => #{arx2init => 2#0}}),
    PhyMod:write(rx_sniff, #{sniff_ont => 2#0, sniff_offt => 2#0}),
    PhyMod:disable_rx(),
    PhyMod:write(sys_cfg, #{rxwtoe => 1}).

-spec suspend_rx_loop(State) -> ok when
      State :: state().
suspend_rx_loop(State) ->
    PhyMod = State#state.phy_layer,
    LoopPid = State#state.loop_pid,
    turn_off_rx_loop(PhyMod, LoopPid, shutdown).

-spec resume_rx_loop(State) -> pid() when
      State :: state().
resume_rx_loop(State) ->
    Ranging = State#state.ranging,
    rx_loop_on(State, Ranging).

% @private
-spec tx_(State, Frame, Pib, Ranging) -> Result when
      State   :: state(),
      Frame   :: bitstring(),
      Pib     :: pib_state(),
      Ranging :: ranging_tx(),
      Result  :: {ok, MacTXState} | {error, MacTXState, Error},
      Error   :: atom().
tx_(State, <<_:2, ?ENABLED:1, _:13, Seqnum:8, _/binary>> = Frame, Pib, Ranging) ->
    MacTXState = State#state.mac_tx_state,
    PhyMod = State#state.phy_layer,
    tx_ar(MacTXState, PhyMod, Frame, Seqnum, 0, Pib, Ranging);
tx_(State, Frame, CsmaParams, Ranging) ->
    MacTXState = State#state.mac_tx_state,
    TxOpts = #tx_opts{ranging = Ranging},
    gen_mac_tx:transmit(MacTXState, Frame, CsmaParams, TxOpts).

% @private
% @doc This function transmits a frame with AR=1
% If the ACK isn't received before the timeout, a retransmission is done
% If the frame has been transmitted MACMAXFRAMERETRIES times then the error
% `no_ack' is returned
% @end
-spec tx_ar(MacTXState, PhyMod, Frame, Seqnum, Retry, Pib, Ranging) -> Result when
      MacTXState :: gen_mac_tx:state(),
      PhyMod     :: module(),
      Frame      :: bitstring(),
      Seqnum     :: non_neg_integer(),
      Retry      :: non_neg_integer(),
      Pib        :: pib_state(),
      Ranging    :: ranging_tx(),
      Result     :: {ok, MacTxState} | {error, MacTxState, Error},
      Error      :: atom().
tx_ar(MacTxState, _, _, _, ?MACMAXFRAMERETRIES, _, _) ->
    {error, MacTxState, no_ack};
tx_ar(MacTXState, PhyMod, Frame, Seqnum, Retry, Pib, Ranging) ->
    TxOpts = #tx_opts{wait4resp = ?ENABLED, ranging = Ranging},
    case gen_mac_tx:transmit(MacTXState, Frame, Pib, TxOpts) of
        {ok, NewMacTxState} ->
            case PhyMod:reception(true) of
                {_, <<_:16, Seqnum:8>>} -> {ok, NewMacTxState};
                _  -> tx_ar(NewMacTxState,
                            PhyMod,
                            Frame,
                            Seqnum,
                            Retry+1,
                            Pib,
                            Ranging)
            end;
        {error, NewMacTxState, _Error} ->
            tx_ar(NewMacTxState,
                  PhyMod,
                  Frame,
                  Seqnum,
                  Retry+1,
                  Pib,
                  Ranging)
    end.

%--- Internal: Ranging helpers
% Source: https://forum.qorvo.com/t/how-to-calculate-the-signal-to-noise-ratio-snr-of-dw1000/5585/3
snr(PhyMod) ->
    Delta = 87-7.5,
    RSL = PhyMod:signal_power(),
    RSL + Delta.

%% @doc
%% @param DeviceRanging: Ranging value passed in the LME-RX-ENABLE
%% @param RxRNG: RNG bit of the received frame
%% @end
-spec rx_ranging_infos(DeviceRanging, RxRNG, PhyMod) -> Result when
      DeviceRanging :: flag(),
      RxRNG         :: flag(),
      PhyMod        :: module(),
      Result        :: ranging_informations().
rx_ranging_infos(?DISABLED, ?ENABLED, _) ->
    #ranging_informations{
       ranging_received = ?RANGING_REQUESTED_BUT_NOT_SUPPORTED
      };
rx_ranging_infos(_, ?DISABLED, _) ->
    #ranging_informations{ranging_received = ?NO_RANGING_REQUESTED};
rx_ranging_infos(?ENABLED, ?ENABLED, PhyMod) ->
    #{rx_stamp := RxStamp} = PhyMod:read(rx_time),
    #{tx_stamp := TxStamp} = PhyMod:read(tx_time),
    #{rxtofs := RXTOFS} = PhyMod:read(rx_ttcko),
    #{rxttcki := RXTTCKI} = PhyMod:read(rx_ttcki),
    #ranging_informations{
       ranging_received = ?RANGING_ACTIVE,
       ranging_counter_start = RxStamp,
       ranging_counter_stop = TxStamp,
       ranging_tracking_interval = RXTTCKI,
       ranging_offset = RXTOFS,
       ranging_FOM = <<0:8>>
      }.

tx_ranging_infos(?NON_RANGING, _) ->
    #ranging_informations{ranging_received = false};
tx_ranging_infos(?ENABLED, State) ->
    #state{phy_layer = PhyMod} = State,
    #{rx_stamp := RxStamp} = PhyMod:read(rx_time),
    #{tx_stamp := TxStamp} = PhyMod:read(tx_time),
    #{rxtofs := RXTOFS} = PhyMod:read(rx_ttcko),
    #{rxttcki := RXTTCKI} = PhyMod:read(rx_ttcki),
    #ranging_informations{
       ranging_received = true,
       ranging_counter_start = TxStamp,
       ranging_counter_stop = RxStamp,
       ranging_tracking_interval = RXTTCKI,
       ranging_offset = RXTOFS,
       ranging_FOM = <<0:8>>
      }.
