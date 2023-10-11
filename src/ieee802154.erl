-module(ieee802154).
-behaviour(gen_statem).

-include("mac_layer.hrl").

-define(NAME, ?MODULE).

-export([create_stack/3]).
-export([start_link/1]).
-export([stop_link/0]).

-export([transmition/3]).
-export([reception/0]).

%%% gen_statem callbacks
-export([init/1]).
-export([callback_mode/0]).
-export([terminate/3]).
-export([code_change/4]).

-export([idle/3]).
-export([rx/3]).
-export([tx/3]).


%-------------------------------------------------------------------------------
% @doc Creates the IEEE 802.15.4 network stack and links the different layers to the appropriate supervisor
% @param IeeeParams: The parameters for the IEEE module
% @param MacLayer: A tuple containing the name of the module to use, the parameters in a map and the initial state of the layer
% @param PhyLayer:A tuple containing the name of the module to use, the parameters in a map and the initial state of the layer
% @end
%-------------------------------------------------------------------------------
-spec create_stack(
        IeeeParams::tuple(), 
        MacLayer::{MacModule::module(), MacParams::map(), MacState::tuple()}, 
        PhyLayer::{PhyModule::module(), PhyParams::map(), PhyState::tuple()}
       ) -> ok.
create_stack(IeeeParams, MacLayer, PhyLayer) ->
    {PhyModule, PhyState, PhyParams} = PhyLayer,
    case PhyModule of
        pmod_uwb -> grisp:add_device(spi2, pmod_uwb);
        _ -> network_sup:start_child(phy_layer, PhyModule, [PhyState, PhyParams]) % grisp_device_sup isn't started
    end,
    {MacModule, MacState, MacParams} = MacLayer,
    network_sup:start_child(mac_layer, MacModule, [{maps:put(phy_layer, PhyModule, MacParams), MacState}]),
    network_sup:start_child(?NAME, ?MODULE, [maps:put(mac_layer, MacModule, IeeeParams)]).
    

start_link(Params) -> gen_statem:start_link({local, ?NAME}, ?MODULE, Params, []).

stop_link() ->
    gen_server:stop(?NAME).

-spec transmition(FrameControl :: #frame_control{}, FrameHeader :: #mac_header{}, Payload :: bitstring()) -> ok.
transmition(FrameControl, FrameHeader, Payload) -> gen_statem:call(?MODULE, {tx, FrameControl, FrameHeader, Payload}, infinity).

reception() -> 
    gen_statem:call(?NAME, rx, infinity).


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
    case mac_layer:reception() of
        {FrameControl, FrameHeader, Payload} -> {next_state, idle, Data, [{reply, From, {FrameControl, FrameHeader, Payload}}]};
        Err -> {next_state, idle, Data, [{reply, From, Err}]}
    end.

tx(_EventType, {tx,FrameControl, FrameHeader, Payload, From}, Data) -> 
    case mac_layer:send_data(FrameControl, FrameHeader, Payload) of
        ok -> {next_state, idle, Data, [{reply, From, ok_tx}]};
        _ -> {next_state, idle, Data, [{reply, From, error_tx}]}
    end.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_, _, _, _) ->
    error(not_implemented).


