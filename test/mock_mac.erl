-module(mock_mac).
-include("../src/mac_layer.hrl").

-behaviour(gen_server).

-export([send_data/3]).
% -export([send_data/4]). 
-export([reception/0]).
% -export([reception/1]).

-export([start_link/1]).
-export([stop_link/0]).

%%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).

reception() -> 
    gen_server:call(?MODULE, rx).

send_data(FrameControl, MacHeader, Payload) ->
    gen_server:call(?MODULE, {tx, FrameControl, MacHeader, Payload}).

start_link(Params) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).

stop_link() ->
    gen_server:stop(?MODULE).

%%% gen_server callbacks
init({Params, _State}) ->
    PhyModule = case Params of
              #{phy_layer := PHY} -> PHY;
              _ -> pmod_uwb
          end,
    {ok, #{phy => PhyModule}}. 

handle_call(rx, _From, #{ack_req := ?ENABLED, seqnum := Seqnum} = State) -> {reply, rx_ack(Seqnum), State#{ack_req => ?DISABLED}};
handle_call(rx, _From, State) -> {reply, rx(), State};
handle_call({tx, #frame_control{ack_req = ?ENABLED} = FrameControl, #mac_header{seqnum = Seqnum} = MacHeader, Payload}, _From, State) ->
    {reply, tx(FrameControl, MacHeader, Payload), State#{ack_req => ?ENABLED, seqnum => Seqnum}};
handle_call({tx, FrameControl, MacHeader, Payload}, _From, State) -> {reply, tx(FrameControl, MacHeader, Payload), State};
handle_call(Request, _From, _State) -> error({unknown_call, Request}).

handle_cast(_, _) ->
  error(not_implemented).

rx() -> 
    FrameControl = #frame_control{pan_id_compr = ?ENABLED, frame_version = 2#00},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#DECA:16>>, dest_addr = <<"RX">>, src_pan = <<16#DECA:16>>, src_addr = <<"TX">>},
    {FrameControl, MacHeader, <<"Hello">>}.

% Received MAC frame for an ACK is only composed of the Frame control, the seqnum and the FCS
rx_ack(Seqnum) ->
    FrameControl = #frame_control{frame_type = ?FTYPE_ACK},
    MacHeader = #mac_header{seqnum = Seqnum},
    {FrameControl, MacHeader, <<>>}. 

tx(FrameControl, MacHeader, Payload) ->
    Frame = mac_layer:mac_frame(FrameControl, MacHeader, Payload),
    io:format("~w~n", [Frame]),
    ok.

