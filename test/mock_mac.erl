-module(mock_mac).
-include("../src/mac_frame.hrl").

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_mac_layer).

-export([send_data/3]).
% -export([send_data/4]). 
-export([reception/0]).
% -export([reception/1]).

%%% gen_server callbacks
-export([init/1]).
-export([tx/4]).
-export([rx/2]).
-export([rx_on/2]).
-export([rx_off/1]).
-export([terminate/2]).


% --- API ------------------
reception() -> 
    mac_layer_behaviour:rx().

send_data(FrameControl, MacHeader, Payload) ->
    mac_layer_behaviour:tx(FrameControl, MacHeader, Payload).

% --- Callbacks -----------
init(Params) ->
    PhyModule = case Params of
              #{phy_layer := PHY} -> PHY;
              _ -> pmod_uwb
          end,
    #{phy_layer => PhyModule, rx => off}. 


tx(State, #frame_control{ack_req = ?ENABLED} = FrameControl, #mac_header{seqnum = Seqnum} = MacHeader, Payload) -> 
    % TODO use the mock phy as well for this one ?
    transmission(FrameControl, MacHeader, Payload),
    {ok, State, receive_ack(Seqnum)};
tx(State, FrameControl, MacHeader, Payload) -> {transmission(FrameControl, MacHeader, Payload), State}.

rx(State, _) -> {ok, State, receive_()}.

rx_on(State, _) ->
    {ok, State#{rx => on}}.

rx_off(State) ->
    {ok, State#{rx => off}}.

terminate(_State, _Reason) -> ok.

% --- Internals ---------

receive_() -> 
    FrameControl = #frame_control{pan_id_compr = ?ENABLED, frame_version = 2#00},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#DECA:16>>, dest_addr = <<"RX">>, src_pan = <<16#DECA:16>>, src_addr = <<"TX">>},
    {FrameControl, MacHeader, <<"Hello">>}.

% Received MAC frame for an ACK is only composed of the Frame control, the seqnum and the FCS
receive_ack(Seqnum) ->
    FrameControl = #frame_control{frame_type = ?FTYPE_ACK},
    MacHeader = #mac_header{seqnum = Seqnum},
    {FrameControl, MacHeader, <<>>}. 

transmission(FrameControl, MacHeader, Payload) ->
    Frame = mac_frame:encode(FrameControl, MacHeader, Payload),
    io:format("~w~n", [Frame]),
    ok.

