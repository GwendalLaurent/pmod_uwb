-module(robot).

-behaviour(application).

-include("mac_frame.hrl").
-include("ieee802154.hrl").

-export([tx/0]).
-export([tx_ranging/0]).
-export([rx/0]).
-export([rx_on/0]).
-export([rx_off/0]).

% Benchmarking
-export([tx_benchmark/0]).
-export([rx_benchmark/0]).

% CSMA tests
-export([jammer/0]).

% Ranging
-export([rx_ranging/0]).

% Callbacks
-export([start/2]).
-export([stop/1]).

-define(JAMMING_DATA, <<"JAMMING">>).
-define(DATALENGTH, byte_size(?JAMMING_DATA)).

-define(BENCHMARK_DATA, <<16#F:(111*8)>>).
-define(BENCHMARK_DATA_LENGTH, bit_size(?BENCHMARK_DATA)).

-define(PANID, <<16#CAFE:16>>).
-define(SENDER_ADDR, <<16#0001:16>>).
-define(RECEIVER_ADDR, <<16#0002:16>>).

-define(CCA_DURATION, 283).

% Sends/receive only 1 frame
tx() ->
    % FrameControl = #frame_control{ack_req = ?ENABLED},
    pmod_uwb:set_preamble_timeout(?CCA_DURATION),
    FrameControl = #frame_control{},
    MacHeader = #mac_header{},
    ieee802154:transmission({FrameControl, MacHeader, <<"Test">>}).

tx_ranging() ->
    pmod_uwb:set_preamble_timeout(?CCA_DURATION),
    FrameControl = #frame_control{},
    MacHeader = #mac_header{},
    ieee802154:transmission({FrameControl, MacHeader, <<"Test">>}, ?ALL_RANGING).


-spec rx_callback(Frame, LinkQuality, Security, Ranging) -> ok when
      Frame       :: frame(),
      LinkQuality :: integer(),
      Security    :: ieee802154:security(),
      Ranging     :: ieee802154:ranging_informations().
rx_callback({_FrameControl, _MacHeader, _Payload}, LQI, Security, Ranging) ->
    io:format("------ Frame report ------~n"),
    io:format("Link quality: ~p ~n", [LQI]),
    io:format("Security: ~w~n", [Security]),
    io:format("Ranging: ~w~n", [Ranging]),
    io:format("Called twice ?~n"),
    io:format("-------------------------~n").
    % io:format("Received frame with seqnum: ~w - Payload: ~w ~n",
    %           [_MacHeader#mac_header.seqnum, _Payload]).

rx_on() -> ieee802154:rx_on().
rx_off() -> ieee802154:rx_off().

tx(0, Total, Success, Error) -> {Success, Error, Total};
tx(N, Total, Success, Error) ->
    % io:format("~w~n", [N]),
    Seqnum = Total rem 512,
    case ieee802154:transmission({#frame_control{pan_id_compr = ?ENABLED, ack_req = ?ENABLED}, #mac_header{seqnum = Seqnum, dest_pan = ?PANID, dest_addr = ?RECEIVER_ADDR, src_addr = ?SENDER_ADDR}, ?BENCHMARK_DATA}) of
        {ok, _} -> tx(N-1, Total+1, Success+1, Error);
        _ -> tx(N-1, Total+1, Success, Error+1)
    end.

jammer() ->
    ieee802154:rx_off(),
    jammer(1000).

jammer(0) ->
    ok;
jammer(N) ->
    pmod_uwb:write_tx_data(?JAMMING_DATA),
    pmod_uwb:write(tx_fctrl, #{txboffs => 2#0, tr => 2#0, tflen => ?DATALENGTH}),
    pmod_uwb:write(sys_ctrl, #{txstrt => 2#1, txdlys => 0}), % start transmission and some options
    pmod_uwb:wait_for_transmission(),
    jammer(N-1).

tx_benchmark() ->
    ieee802154:set_pan_id(?PANID),
    ieee802154:set_mac_short_address(?SENDER_ADDR),
    pmod_uwb:set_preamble_timeout(?CCA_DURATION),
    NbrFrames = 100,
    % NbrFrames = 1000,
    Start = os:timestamp(),
    {Success, Error, Total} = tx(NbrFrames, 0, 0, 0), 
    End = os:timestamp(),
    Time = timer:now_diff(End, Start)/1000000,
    io:format("------------------- Report -------------------~n"),
    io:format("Sent ~w frames - Success rate ~.3f (~w/~w) - Error rate ~.3f (~w/~w)~n", [Total, Success/Total, Success, Total, Error/Total, Error, Total]),
    io:format("Data rate ~.1f b/s - ~w b in ~w s ~n", [(?BENCHMARK_DATA_LENGTH*NbrFrames)/Time, ?BENCHMARK_DATA_LENGTH*NbrFrames, Time]),
    io:format("----------------------------------------------~n").

rx_benchmark() ->
    ieee802154:set_pan_id(?PANID),
    ieee802154:set_mac_short_address(?RECEIVER_ADDR),
    % rx().
    ieee802154:rx_on().

rx() ->
    ieee802154:reception(),
    rx().

rx_ranging() ->
    ieee802154:set_pan_id(?PANID),
    ieee802154:set_mac_short_address(?RECEIVER_ADDR),
    ieee802154:rx_on(?ENABLED).

start(_Type, _Args) ->
    {ok, Supervisor} = robot_sup:start_link(),
    grisp:add_device(spi2, pmod_uwb),
    ieee802154:start_link(#ieee_parameters{mac_layer = mac_layer, input_callback = fun rx_callback/4}),
    % pmod_uwb:write(rx_fwto, #{rxfwto => 16#FFFF}),
    % pmod_uwb:write(sys_cfg, #{rxwtoe => 2#1}),
    % ieee802154:rx_on(),
    {ok, Supervisor}.

% @private
stop(_State) -> ok.
