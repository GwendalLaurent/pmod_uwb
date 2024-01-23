-module(double_sided).

-behaviour(gen_statem).

-include("../ieee802154.hrl").
-include("../mac_frame.hrl").
-include("ranging_utils.hrl").

% API
-export([start_link/0]).
-export([initiator/1]).
-export([rx_callback/4]).

%%% gen_statem callbacks
-export([init/1]).
-export([callback_mode/0]).
-export([state_name/3]).
-export([handle_event/4]).
-export([terminate/3]).

% gen_statem state functions
-export([idle/3]).
-export([middle_init/3]).
-export([wait_report/3]).

%--- Macros --------------------------------------------------------------------

%--- Rx callback ---------------------------------------------------------------
rx_callback(Frame, _LQI, _Security, RangingInfos) ->
    rx(Frame, RangingInfos).

%--- API -----------------------------------------------------------------------
start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, #{}, []).

initiator(0) ->
    Measures = get_measures(),
    ranging_utils:do_stats(Measures);
initiator(N) ->
    Ref = make_ref(),
    {Frame, RangingInfos} = send_poll(),
    tx(Frame, RangingInfos, {self(), Ref}),
    receive 
        {Ref, continue} -> ok
    after 
        500 -> flush_current()
    end,
    initiator(N-1).

%--- API: internal
tx(Frame, RangingInfos, From) ->
    gen_statem:cast(?MODULE, {tx, From, Frame, RangingInfos}).

rx(Frame, RangingInfos) ->
    gen_statem:cast(?MODULE, {rx, Frame, RangingInfos}).

get_measures() ->
    gen_statem:call(?MODULE, {measures}).

flush_current() ->
    gen_statem:cast(?MODULE, {flush}).

send_poll() ->
    FC = #frame_control{ack_req = ?ENABLED,
                        src_addr_mode = ?SHORT_ADDR,
                        dest_addr_mode = ?SHORT_ADDR},

    Seqnum = rand:uniform(255),
    MH = #mac_header{seqnum = Seqnum,
                     src_pan = <<16#CAFE:16>>,
                     src_addr = <<16#0001:16>>,
                     dest_pan = <<16#CAFE:16>>,
                     dest_addr = <<16#0002:16>>},
    Payload = <<"RANGING">>,
    Frame = {FC, MH, Payload},
    {ok, RangingInfos} = ieee802154:transmission(Frame, ?ALL_RANGING),
    {Frame, RangingInfos}.

%--- gen_statem callbacks ------------------------------------------------------
init(_) ->
    {ok, idle, #{measures => [], fails => 0}}.

callback_mode() ->
  [state_functions].

idle({call, From}, {measures}, Data) ->
    #{measures := Measures} = Data,
    {keep_state, Data#{measures := []}, {reply, From, Measures}};
idle(cast, {tx, From, _, RangingInfos}, Data) ->
    #ranging_informations{ranging_counter_start = PollTx, 
                          ranging_counter_stop = RespRx} = RangingInfos,
    {next_state, middle_init, Data#{poll_tx => PollTx, resp_rx => RespRx, from => From}};
idle(cast, {rx, Frame, RangingInfos}, Data) ->
    #ranging_informations{ranging_counter_start = PollRx,
                          ranging_counter_stop = RespTx} = RangingInfos,

    {_, SymRangingInfos} = send_sym(Frame),
    #ranging_informations{ranging_counter_start = SymTx,
                          ranging_counter_stop = FinalRx} = SymRangingInfos,

    send_report(Frame, #{poll_rx => PollRx,
                         resp_tx => RespTx,
                         sym_tx => SymTx,
                         final_rx => FinalRx}),

    {next_state, idle, Data}.

middle_init(cast, {rx, _, RangingInfos}, Data) ->
    #ranging_informations{ranging_counter_start = SymRx,
                          ranging_counter_stop = FinalTx} = RangingInfos,
    {next_state, wait_report, Data#{sym_rx => SymRx, final_tx => FinalTx}}.

wait_report(cast, {flush}, Data) ->
    #{measures := Measures, fails := Fails} = Data,
    {next_state, idle, #{measures => Measures, fails => Fails+1}};
wait_report(cast, {rx, <<"RANGING SYM">>, _}, Data) ->
    {keep_state, Data};
wait_report(cast, {rx, Frame, _}, Data) ->
    #{poll_tx := PollTx, resp_rx := RespRx, sym_rx := SymRx, final_tx := FinalTx, measures := Measures, from := From} = Data,
    {_, _, Payload} = Frame,
    <<"REPORT", PollRx:40, RespTx:40, SymTx:40, FinalRx:40>> = Payload,
    InitiatorTS = {PollTx, RespRx, SymRx, FinalTx},
    ResponderTS = {PollRx, RespTx, SymTx, FinalRx},
    Distance = distance(InitiatorTS, ResponderTS),
    {Pid, Ref} = From,
    Pid ! {Ref, continue},
    {next_state, idle, Data#{measures := [Distance | Measures]}}.

state_name(_, _, _) ->
    error(not_implemented).

handle_event(_, _, _, _) ->
    error(not_implemented).

terminate(_Reason, _State, _Data) ->
    ok.

%--- Internal: gen_server ------------------------------------------------------
send_sym(RxFrame) ->
    {FC, RxMH, _} = RxFrame,
    #mac_header{src_pan = DestPAN,
                src_addr = DestAddr,
                dest_pan = SrcPAN,
                dest_addr = SrcAddr} = RxMH,
    Seqnum = rand:uniform(255),
    NewMH = #mac_header{seqnum = Seqnum,
                        src_pan = SrcPAN,
                        src_addr = SrcAddr,
                        dest_pan = DestPAN,
                        dest_addr = DestAddr},
    Payload = <<"RANGING SYM">>,
    Frame = {FC, NewMH, Payload},
    ieee802154:transmission(Frame, ?ALL_RANGING).

send_report(RxFrame, StoredTS) ->
    {FC, RxMH, _} = RxFrame,
    NewFC = FC#frame_control{ack_req = ?DISABLED},
    #mac_header{src_pan = DestPAN,
                src_addr = DestAddr,
                dest_pan = SrcPAN,
                dest_addr = SrcAddr} = RxMH,
    Seqnum = rand:uniform(255),
    NewMH = #mac_header{seqnum = Seqnum,
                        src_pan = SrcPAN,
                        src_addr = SrcAddr,
                        dest_pan = DestPAN,
                        dest_addr = DestAddr},
    #{poll_rx := PollRx,
      resp_tx := RespTx,
      sym_tx := SymTx,
      final_rx := FinalRx} = StoredTS,

    Payload = <<"REPORT", PollRx:40, RespTx:40, SymTx:40, FinalRx:40>>,
    ieee802154:transmission({NewFC, NewMH, Payload}).

distance(InitiatorTS, ResponderTS) ->
    % io:format("Initiator TS: ~p~n", [InitiatorTS]),
    % io:format("Responder TS: ~p~n", [ResponderTS]),
    {PollTx, RespRx, SymRx, FinalTx} = InitiatorTS,
    {PollRx, RespTx, SymTx, FinalRx} = ResponderTS,

    TRound1 = RespRx - PollTx,
    TReply1 = RespTx - PollRx,
    TRound2 = FinalRx - SymTx,
    TReply2 = FinalTx - SymRx,

    Sum = TRound1 + TRound2 + TReply1 + TReply2,
    TProp = ( (TRound1*TRound2) - (TReply1*TRound2 ) ) / Sum,
    Tof = TProp * ?TU,
    Distance = Tof * ?C,
    Distance.
