-module(mac_layer_behaviour).

-include("mac_layer.hrl").

-callback init(State::term(), Params::term()) -> State :: term().
-callback tx(State::term(), From::term(), {FrameControl::#frame_control{}, MacHeader::#mac_header{}, Payload::bitstring()}) -> {reply, State::term(), From::term(), Reply::term()}.
-callback rx(State::term(), From::term()) -> {reply, State::term(), From::term(), {FrameControl::#frame_control{}, MacHeader::#mac_header{}, Payload::bitstring()}}.

-export([start_link/3]).
-export([stop_link/0]).
% -export([reply/2]).
-export([tx/3]).
-export([rx/0]).

% Inspiration for all this: https://learnyousomeerlang.com/what-is-otp#its-the-open-telecom-platform

% Example:
% ieee802154;
%
% Pid = layer_behaviour:start_link(mac_layer, #{}, #{})
%
% For transmission (using the Pid created just before
% tx(Pid, FrameControl, MacHeader, Payload).
% (Same idea for rx
%
% On the mac layer side in the sync callback functions, the module has to call layer_behaviour:reply(From, Message) as a "return" otherwise, the behaviour will get stuck

% ---------------------------------------------------------------------
% @doc Starts a mac_layer_behaviour process
% @param Module: A module implementing the behaviour
% @param State; The initial state of the layer
% @param Params: The parameters used to initialize the behaviour
% @end
% ---------------------------------------------------------------------
-spec start_link(Module::module(), State::term(), Params::map()) -> pid().
start_link(Module, State, Params) ->
    Pid = spawn_link(fun() -> init(Module, State, Params) end),
    register(mac_layer, Pid),
    Pid.

% ---------------------------------------------------------------------
% @doc Stops the mac_layer process
% @end
% ---------------------------------------------------------------------
stop_link() ->
    Ref = monitor(process, mac_layer),
    Message = {stop, {self(), Ref}},
    sync(Ref, Message).

% ---------------------------------------------------------------------
% @doc Transmission request to the MAC layer of a MAC frame
% @param FrameControl: The frame control of the MAC frame
% @param MacHeader: The Mac header of the frame
% @param Payload: The payload of the frame
% @end
% ---------------------------------------------------------------------
-spec tx(FrameControl::#frame_control{}, MacHeader::#mac_header{}, Payload::bitstring()) -> ok.
tx(FrameControl, MacHeader, Payload) ->
    Ref = monitor(process, mac_layer),
    Message = {tx, {self(), Ref}, FrameControl, MacHeader, Payload},
    sync(Ref, Message). 

% ---------------------------------------------------------------------
% @doc Transmission request to the MAC layer of a MAC frame
% @returns a tuple containing the Frame control, the Mac header and the payload of the received frame
% @end
% ---------------------------------------------------------------------
-spec rx() -> {FrameControl::#frame_control{}, MacHeader::#mac_header{}, Payload::bitstring()} | Error::atom().
rx() ->
    Ref = monitor(process, mac_layer),
    Message = {rx, {self(), Ref}},
    sync(Ref, Message).

% reply(From, Reply) -> mac_layer ! {From, Reply}.

% ---- Internal -------------------------------------------------------
% Used to create a synchronous response to the calling process
response({reply, State, {Pid, Ref}, Reply}) ->
    Pid ! {Ref, Reply},
    State;
response(_) -> error(wrong_reply).

% Private
init(Module, State, Params) ->
    {ok, NewState} = Module:init(State, Params),
    loop(Module, NewState).

loop(Module, State) ->
    receive
        {tx, From, FrameControl, MacHeader, Payload} -> loop(Module, response(Module:tx(State, From, {FrameControl, MacHeader, Payload})));
        {rx, From} -> loop(Module, response(Module:rx(State, From)));
        {stop, From} -> response({reply, State, From, ok})
    end.

sync(Ref, Message) ->
    mac_layer ! Message,
    receive
        {Ref, Reply} -> demonitor(Ref, [flush]), Reply;
        {'DOWN', Ref, process, _, Reason} -> erlang:error(Reason)
    end. % TODO set a timeout later on ?
