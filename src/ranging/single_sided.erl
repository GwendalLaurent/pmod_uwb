-module(single_sided).

-include("../ieee802154.hrl").
-include("../mac_frame.hrl").
-include("ranging_utils.hrl").

-behaviour(gen_server).


-export([start_link/0]).
-export([initiator/0]).
-export([initiator_multiple/0]).
-export([responder_data/3]).

% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([terminate/2]).

%--- Macros --------------------------------------------------------------------
-define(FREQ_OFFSET_MULTIPLIER, 1/( 131072 * 2 * (1024/998.4e6))).
-define(HERTZ_TO_PPM_MUL, 1.0e-6/6489.6e6).

%--- API -----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, #{}, []).

initiator() ->
    Payload = <<"RANGING">>,
    Seqnum = rand:uniform(255),
    FrameControl = #frame_control{ack_req = ?ENABLED,
                                  src_addr_mode = ?SHORT_ADDR,
                                  dest_addr_mode = ?SHORT_ADDR},
    MacHeader = #mac_header{seqnum = Seqnum,
                            dest_pan = <<16#CAFE:16>>,
                            src_pan = <<16#CAFE:16>>,
                            src_addr = <<16#0001:16>>,
                            dest_addr = <<16#0002:16>>},
    Frame = {FrameControl, MacHeader, Payload},
    {ok, RangingInfo} = ieee802154:transmission(Frame, ?ALL_RANGING),
    initiator_data(Seqnum, RangingInfo).

initiator_multiple() ->
    initiator_loop(100),
    {ok, Measures} = dump_measures(),
    ranging_utils:do_stats(Measures).

initiator_loop(0) ->
    ok;
initiator_loop(N) ->
    initiator(),
    timer:sleep(100),
    initiator_loop(N-1).


-spec initiator_data(Id, RangingInfo) -> ok when
      Id :: integer(),
      RangingInfo :: ranging_informations().
initiator_data(Id, RangingInfo) ->
    gen_server:call(?MODULE, {initiator, RangingInfo, Id}).

responder_data(Id, RangingStart, RangingStop) ->
    gen_server:call(?MODULE, {response, Id, RangingStart, RangingStop}).

dump_measures() ->
    gen_server:call(?MODULE, {dump}).


%--- gen_server callbacks ------------------------------------------------------
init(_Args) ->
    {ok, #{nb => 0, pairs => #{}, measures => []}}.

handle_call({initiator, RangingInfos, Id}, _, State) ->
    #{pairs := Pairs, nb := Nb} = State,
    {reply, ok, State#{pairs => maps:put(Id, RangingInfos, Pairs), nb => Nb+1}};
handle_call({response, Id, RespRangingStart, RespRangingStop}, _, State) ->
    #{pairs := Pairs, measures := Measures} = State,
    InitRangingInfos = maps:get(Id, Pairs),
    % TODO compute the distance
    Distance = single_sided_distance(InitRangingInfos,
                                     RespRangingStart,
                                     RespRangingStop),
    io:format("Ranging info - Computed distance: ~w m~n", [Distance]),
    {reply, ok, State#{pairs => maps:remove(Id, Pairs),
                       measures => [Distance | Measures]}
    };
handle_call({dump}, _, State) ->
    #{measures := Measures} = State,
    {reply, {ok, Measures}, State#{measures => []}};
handle_call(_Request, _From, _State) ->
    error(call_not_recognized).

handle_cast(_Request, _State) ->
    error(cast_not_recognized).

terminate(_Reason, _State) ->
    ok.

%--- Internal ------------------------------------------------------------------
single_sided_distance(InitRangingInfos, RespStartTS, RespStopTS) ->
    #{drx_car_int := DrxCarInt} = pmod_uwb:read(drx_conf),
    ClockOffsetRatio = DrxCarInt * ?FREQ_OFFSET_MULTIPLIER * ?HERTZ_TO_PPM_MUL,
    InitStartTS = InitRangingInfos#ranging_informations.ranging_counter_start,
    InitStopTS = InitRangingInfos#ranging_informations.ranging_counter_stop,
    % io:format("Init start: ~w - Init stop: ~w ~n", [InitStartTS, InitStopTS]),
    % io:format("Resp start: ~w - Resp stop: ~w ~n", [RespStartTS, RespStopTS]),
    TRound = InitStopTS - InitStartTS,
    TReply = RespStopTS - RespStartTS,
    % io:format("Clock offset ratio: ~w~n", [ClockOffsetRatio]),
    % io:format("TRound: ~w - TReply: ~w ~n", [TRound, TReply]),
    ToF = ((TRound-TReply) *((1-ClockOffsetRatio)/2.0)) * ?TU,
    % io:format("ToF: ~w - D: ~w ~n", [ToF, ToF * ?C]),
    ToF * ?C.
