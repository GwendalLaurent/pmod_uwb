-module(ieee802154).
-behaviour(gen_statem).

-include("mac_layer.hrl").

-define(NAME, ?MODULE).

-export([create_stack/1]).
-export([start_link/1]).
-export([stop_link/0]).

-export([transmition/3]).
-export([reception/0]).
-export([init/0]).

%%% gen_statem callbacks
-export([init/1]).
-export([callback_mode/0]).
-export([terminate/3]).
-export([code_change/4]).

-export([idle/3]).
-export([rx/3]).
-export([tx/3]).

create_stack(Params) ->
    network_sup:start_child(?NAME, ?MODULE, [Params]),
    #{mac_layer := #{module := MacModule, state := MacState, parameters := MacParams}} = Params,
    network_sup:start_child(mac_layer, MacModule, [{MacParams, MacState}]).
    % For now, the PHY/pmod is started from the mac layer using the MacParams


start_link(Params) -> gen_statem:start_link({local, ?NAME}, ?MODULE, Params, []).

stop_link() ->
    gen_server:stop(?NAME).

-spec transmition(FrameControl :: #frame_control{}, FrameHeader :: #mac_header{}, Payload :: bitstring()) -> ok.
transmition(FrameControl, FrameHeader, Payload) -> gen_statem:call(?MODULE, {tx, FrameControl, FrameHeader, Payload}).

reception() -> 
    gen_statem:call(?NAME, rx).


% Function to help with testing (TO DELETE later)
init() ->
    init(#{mac_layer => #{module => mac_layer, state => {}, parameters => #{phy => pmod_uwb}}}).

%%% gen_statem callbacks
init(#{mac_layer := MAC} = Params) ->
    Data = #{cache => #{tx => [], rx => []}, mac_layer => MAC},
    {ok, idle, Data}.

callback_mode() ->
    state_functions.

idle({call, From}, {tx, FrameControl, FrameHeader, Payload}, Data) -> 
    {next_state, tx, Data, [{next_event, internal, {tx, FrameControl, FrameHeader, Payload, From}}]};

idle({call, From}, rx, Data) -> 
    {next_state, rx, Data, [{next_event, internal, {rx, From}}]}.

rx(EventType, {rx, From}, Data) ->
    io:format("Reception ~n"),
    {next_state, idle, Data, [{reply, From, rx_content}]}.

tx(_EventType, {tx,FrameControl, FrameHeader, Payload, From}, Data) -> 
    case mac_layer:send_data(FrameControl, FrameHeader, Payload) of
        ok -> {next_state, idle, Data, [{reply, From, ok_tx}]};
        _ -> {next_state, idle, Data, [{reply, From, error_tx}]}
    end.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_, _, _, _) ->
    error(not_implemented).


