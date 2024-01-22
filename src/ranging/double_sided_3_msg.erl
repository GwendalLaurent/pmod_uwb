-module(double_sided_3_msg).

-behaviour(gen_statem).

-include("../ieee802154.hrl").
-include("../mac_frame.hrl").

% API
-export([start_link/0]).
-export([initiator/0, initiator/1]).
-export([rx_callback/4]).

% gen_statem callbacks
-export([init/1]).
-export([callback_mode/0]).
-export([idle/3]).
-export([wait_final/3]).
-export([wait_report/3]).
-export([state_name/3]).
-export([handle_event/4]).
-export([terminate/3]).

%--- Macros --------------------------------------------------------------------

-define(TU, 15.65e-12).
-define(C, 299792458).

-define(TX_ANTD, 16450).
-define(RX_ANTD, 16450).

%--- API -----------------------------------------------------------------------
start_link() ->
    pmod_uwb:write(tx_antd, #{tx_antd => ?TX_ANTD}),
    pmod_uwb:write(lde_if, #{lde_rxantd => ?RX_ANTD}),
    gen_statem:start_link({local, ?MODULE}, ?MODULE, #{}, []).

initiator(0) ->
    Measures = get_results(),
    do_stats(Measures);
initiator(N) ->
    initiator(),
    initiator(N-1).

initiator() ->
    ieee802154:set_mac_short_address(<<16#0001:16>>),
    ieee802154:set_pan_id(<<16#CAFE:16>>),
    ieee802154:rx_on(?ENABLED),
    {ok, RangingInfos, Frame} = send_poll(),
    Ref = make_ref(),
    From = {self(), Ref},
    tx(Frame, RangingInfos, From),
    receive 
        {Ref, ok} -> ok
    after 1000 -> error(poll_to)
    end.

rx_callback(Frame, _LQI, _Security, RangingInfos) ->
    rx(Frame, RangingInfos).

%--- Internal API
send_poll() ->
    FC = #frame_control{ack_req = ?ENABLED,
                        src_addr_mode = ?SHORT_ADDR,
                        dest_addr_mode = ?SHORT_ADDR},
    Seqnum = rand:uniform(255),
    MH = #mac_header{src_pan = <<16#CAFE:16>>,
                     src_addr = <<16#0001:16>>,
                     dest_pan = <<16#CAFE:16>>,
                     dest_addr = <<16#0002:16>>,
                     seqnum = Seqnum},
    Payload = <<"RANGING">>,
    Frame = {FC, MH, Payload},
    {ok, RangingInfos} = ieee802154:transmission(Frame, ?ALL_RANGING),
    {ok, RangingInfos, Frame}.

%--- gen_statem callbacks ------------------------------------------------------
init(_) ->
    {ok, idle, #{measures => [], fails => 0}}.

callback_mode() ->
    [state_functions].

idle({call, From}, {measures}, Data) ->
    #{measures := Measures} = Data,
    {keep_state, Data#{measures := []}, {reply, From, Measures}};
idle(cast, {tx, From, Frame, RangingInfos}, Data) ->
    #ranging_informations{ranging_counter_start = PollTx, 
                          ranging_counter_stop = RespRx} = RangingInfos,
    {ok, FinalRangingInfos} = send_final(Frame),
    #ranging_informations{ranging_counter_start = FinalTx} = FinalRangingInfos,
    {next_state, wait_report, Data#{poll_tx => PollTx, resp_rx => RespRx, final_tx => FinalTx, from => From}};
idle(cast, {rx, _Frame, RangingInfos}, Data) ->
    #ranging_informations{ranging_counter_start = PollRx,
                          ranging_counter_stop = RespTx} = RangingInfos,

    {next_state, wait_final, Data#{poll_rx => PollRx, resp_tx => RespTx}}.

wait_final(cast, {rx, Frame, RangingInfos}, Data) ->
    #ranging_informations{ranging_counter_start = FinalRx} = RangingInfos,
    #{poll_rx := PollRx, resp_tx := RespTx} = Data,
    send_report(PollRx, RespTx, FinalRx, Frame),
    {next_state, idle, #{}}.

wait_report(cast, {rx, Frame, _}, Data) ->
    {_, _, Payload} = Frame,
    <<"REPORT", PollRx:40, RespTx:40, FinalRx:40>> = Payload,
    #{poll_tx := PollTx, resp_rx := RespRx, final_tx := FinalTx, measures := Measures, from := From} = Data,
    Poll = {PollTx, PollRx},
    Resp = {RespTx, RespRx},
    Final = {FinalTx, FinalRx},
    Distance = distance(Poll, Resp, Final),
    {Pid, Ref} = From,
    Pid ! {Ref, ok},
    {next_state, idle, Data#{measures => [Distance | Measures]}}.

state_name(_, _, _) ->
    error(not_implemented).

handle_event(_, _, _, _) ->
    error(not_implemented).

terminate(_Reason, _State, _Data) ->
    ok.

%--- Internal ------------------------------------------------------------------
tx(Frame, RangingInfos, From) ->
    gen_statem:cast(?MODULE, {tx, From, Frame, RangingInfos}).

rx(Frame, RangingInfos) ->
    gen_statem:cast(?MODULE, {rx, Frame, RangingInfos}).

get_results() ->
    gen_statem:call(?MODULE, {measures}).

do_stats(Measures) ->
    Nbr = length(Measures),
    Avg = lists:sum(Measures)/Nbr,
    Min = lists:min(Measures),
    Max = lists:max(Measures),
    StdDev = std_dev(Measures, Avg, Nbr, 0),
    io:format("-------------------------------- Summary --------------------------------~n"),
    io:format("Average distance measured: ~w - standard deviation: ~w ~n", [Avg, StdDev]),
    io:format("Total: ~w - Min: ~w - Max ~w~n", [Nbr, Min, Max]),
    io:format("-------------------------------------------------------------------------~n"). 

-spec std_dev(Measures :: [number()], Mean :: float(), N :: non_neg_integer(), Acc :: number()) -> float().
std_dev([], _, N, Acc) ->
    math:sqrt(Acc/N);
std_dev([H | T], Mean, N, Acc) ->
    std_dev(T, Mean, N, Acc + math:pow(H-Mean, 2)).
%--- gen_statem internal -------------------------------------------------------
send_final(PollFrame) -> 
    {FC, MH, _} = PollFrame,
    NewFC = FC#frame_control{ack_req = ?DISABLED},
    Seqnum = rand:uniform(255),
    NewMH = MH#mac_header{seqnum = Seqnum},
    Payload = <<"FINAL">>,
    Frame = {NewFC, NewMH, Payload},
    ieee802154:transmission(Frame, ?ALL_RANGING).

send_report(PollRx, RespTx, FinalRx, FinalFrame) -> 
    {FC, MH, _} = FinalFrame,
    #mac_header{src_pan = DestPan,
                src_addr = DestAddr,
                dest_pan = SrcPan,
                dest_addr = SrcAddr} = MH,
    Seqnum = rand:uniform(255),
    NewMH = MH#mac_header{seqnum = Seqnum,
                          src_pan = SrcPan,
                          src_addr = SrcAddr,
                          dest_pan = DestPan,
                          dest_addr = DestAddr},
    Payload = <<"REPORT", PollRx:40, RespTx:40, FinalRx:40>>,
    Frame = {FC, NewMH, Payload},
    ieee802154:transmission(Frame).

distance(Poll, Resp, Final) ->
    {PollTx, PollRx} = Poll,
    {RespTx, RespRx} = Resp,
    {FinalTx, FinalRx} = Final,

    TRound1 = RespRx - PollTx,
    TReply1 = RespTx - PollRx,
    TRound2 = FinalRx - RespTx,
    TReply2 = FinalTx - RespRx,

    Sum = TRound1 + TRound2 + TReply1 + TReply2,
    TProp = ((TRound1*TRound2) - (TReply1*TRound2) )/Sum,
    ToF = TProp * ?TU,
    % io:format("Distance: ~p m~n", [ToF*?C]),
    ToF * ?C.
