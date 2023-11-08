-module(gen_duty_cycle).

-callback init(PhyModule::module()) -> State::term().
-callback on(State::term(), Callback::function()) -> {ok, State::term()}.
-callback off(State::term()) -> {ok, State::term()}.
% Add suspend and resume later
-callback tx(State::term(), Frame::bitstring()) -> {ok, State::term()} | {error, State::term(), Error::no_ack|frame_too_long|channel_access_failure|atom()}.
-callback rx(State::term()) -> {ok, State::term(), Frame::bitstring()} | {error, State::term(), Error::atom()}.
-callback terminate(State::term(), Reason::term()) -> ok.

-export([start/2]).
-export([turn_on/2]).
-export([turn_off/1]).
-export([tx_request/2]).
-export([rx_request/1]).
-export([stop/2]).

% @doc This module defines a generic behaviour for duty cycling on the IEEE 802.15.4
%
% @TODO complete documentation
% The module implementing the behaviour will be responsible to manage the duty cylcing of the IEEE 802.15.4 stack (not the power optimization of the pmod)
% For example, the module implementing this behaviour for a beacon enabled network will have the task to manage the CFP, the CAP and the beacon reception
% When an application will request a transmission the module has to suspend the rx before transmitting
% At the transmission of a frame, the module will have the task to check if there is enough time to transmit the frame (e.g. before the next beacon)
% At the transmisson of a data frame with AR=1 the module has to manage the retransmission of the frame if the ACK isn't correctly received
%   This is because this module will be responsible to check if the retransmission can be done (no beacons or not a CAP) and the reception can't be resumed between retransmission (both are responsabilities of this module) 
%
% Beacon enabled
% No transmission during beacon
% TX during CAP
% No TX during CFP unless a slot is attributed to the node
% 
% Manage the RX loop (suspend/resume)
%
% @end

% @doc initialize the duty cycle module
% @end
-spec start(Module::module(), PhyModule::module()) -> State::term().
start(Module, PhyModule) ->
    {Module, Module:init(PhyModule)}.

% @doc turns on the continuous reception 
% @TODO specify which RX module has to be used
-spec turn_on(State::term(), Callback::function()) -> State::term().
turn_on({Mod, Sub}, Callback) ->
    {ok, Sub2} = Mod:on(Sub, Callback),
    {Mod, Sub2}.

% @doc turns off the continuous reception
-spec turn_off(State::term()) -> State::term().
turn_off({Mod, Sub}) ->
    {ok, Sub2} = Mod:off(Sub),
    {Mod, Sub2}.

% @doc request a transmission to the duty cycle
% The frame is an encoded MAC frame ready to be transmitted
% If the frame request an ACK, the retransmission is managed by the module
%
% Errors:
% <li> `no_ack': No acknowledgment received after macMaxFrameRetries</li>
% <li> `frame_too_long': The frame was too long for the CAP or GTS</li>
% <li> `channel_access_failure': the CSMA-CA algorithm failed</li>
% @end
-spec tx_request(State::term(), Frame::bitstring()) -> {ok, State::term()} | {error, State::term(), Error::atom()}.
tx_request({Mod, Sub}, Frame) ->
    case Mod:tx(Sub, Frame) of
        {ok, Sub2} -> {ok, {Mod, Sub2}};
        {error, Sub2, Err} -> {error, {Mod, Sub2}, Err}
    end.

-spec rx_request(State::term()) -> {ok, State::term(), Frame::bitstring()} | {error, State::term(), Error::atom()}.
rx_request({Mod, Sub}) ->
    case Mod:rx(Sub) of
        {ok, Sub2, Frame} -> {ok, {Mod, Sub2}, Frame};
        {error, Sub2, Error} -> {error, {Mod, Sub2}, Error}
    end.

% @doc stop the duty cycle module
-spec stop(State::term(), Reason::atom()) -> ok.
stop({Mod, Sub}, Reason) ->
    Mod:terminate(Sub, Reason).
