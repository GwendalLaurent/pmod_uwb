-module(twr).

-include("ieee802154.hrl").
-include("mac_frame.hrl").

-behaviour(gen_server).


-export([start_link/0]).
-export([initiator/0]).
-export([responder_data/3]).

% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([terminate/2]).

%--- Macros --------------------------------------------------------------------
-define(TU, 15.65e-12).
-define(C, 299792458).

%--- API -----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, #{}, []).

initiator() ->
    ieee802154:set_mac_short_address(<<16#0001:16>>),
    ieee802154:set_pan_id(<<16#CAFE:16>>),
    ieee802154:rx_on(?ENABLED),
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

initiator_data(Id, RangingInfo) ->
    gen_server:call(?MODULE, {initiator, RangingInfo, Id}).

responder_data(Id, RangingStart, RangingStop) ->
    gen_server:call(?MODULE, {response, Id, RangingStart, RangingStop}).


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
    Distance = single_sided_distance(InitRangingInfos, RespRangingStart, RespRangingStop),
    io:format("Ranging info - Computed distance: ~w m~n", [Distance]),
    {reply, ok, State#{pairs => maps:remove(Id, Pairs),
                       measures => [Distance | Measures]}
    };
handle_call(_Request, _From, _State) ->
    error(call_not_recognized).

handle_cast(_Request, _State) ->
    error(cast_not_recognized).

terminate(_Reason, _State) ->
    ok.

%--- Internal ------------------------------------------------------------------
single_sided_distance(InitRangingInfos, RespStartTS, RespStopTS) ->
    InitStartTS = InitRangingInfos#ranging_informations.ranging_counter_start,
    InitStopTS = InitRangingInfos#ranging_informations.ranging_counter_stop,
    io:format("Init start: ~w - Init stop: ~w ~n", [InitStartTS, InitStopTS]),
    io:format("Resp start: ~w - Resp stop: ~w ~n", [RespStartTS, RespStopTS]),
    TRound = InitStopTS - InitStartTS,
    TReply = RespStopTS - RespStartTS,
    io:format("TRound: ~w - TReply: ~w ~n", [TRound, TReply]),
    TProp = 0.5*(TRound-TReply),
    ToF = TProp * ?TU,
    io:format("TProp: ~w - ToF: ~w - D: ~w ~n", [TProp, ToF, ToF * ?C]),
    ToF * ?C.
