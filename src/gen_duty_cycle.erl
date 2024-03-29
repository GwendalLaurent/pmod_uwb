% @doc This module defines a generic behaviour for duty cycling on the IEEE 802.15.4
%
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
-module(gen_duty_cycle).

-include("ieee802154.hrl").

-callback init(PhyModule) -> State when
      PhyModule :: module(),
      State     :: term().
-callback on(State, Callback, Ranging) -> Result when
      State       :: term(),
      Callback    :: gen_mac_rx:input_callback_raw_frame(),
      Ranging     :: boolean(),
      Result      :: {ok, State}
                     | {error, State, Error},
      Error       :: atom().
-callback off(State) -> {ok, State} when
      State :: term().
% Add suspend and resume later
-callback tx(State, Frame, CsmaParams, Ranging) -> Result when
      State :: term(),
      Frame :: bitstring(),
      CsmaParams  :: csma_params(),
      Ranging     :: ranging_tx(),
      Result      :: {ok, State, RangingInfo}
                    | {error, State, Error},
      RangingInfo :: ranging_informations(),
      Error       :: tx_error().
-callback rx(State) -> Result when
      State  :: term(),
      Result :: {ok, State, Frame}
                | {error, State, Error},
      Frame  :: bitstring(),
      Error  :: atom().
-callback terminate(State, Reason) -> ok when
      State  :: term(),
      Reason :: term().

-export([start/2]).
-export([turn_on/3]).
-export([turn_off/1]).
-export([tx_request/4]).
-export([rx_request/1]).
-export([stop/2]).

%--- Types ---------------------------------------------------------------------

-export_type([state/0, input_callback_raw_frame/0]).

-opaque state() :: {Module::module(), Sub::term()}.

-type input_callback_raw_frame() :: fun((Frame                  :: binary(),
                                         LQI                    :: integer(),
                                         UWBPRF                 :: pmod_uwb:uwb_PRF(),
                                         Security               :: ieee802154:security(),
                                         UWBPreambleRepetitions :: pmod_uwb:uwb_preamble_symbol_repetition(),
                                         DataRate               :: pmod_uwb:data_rate(),
                                         Ranging                :: ieee802154:ranging_informations())
                                        -> ok).

%--- API -----------------------------------------------------------------------

% @doc initialize the duty cycle module
% @end
-spec start(Module, PhyModule) -> State when
      Module :: module(),
      PhyModule :: module(),
      State :: state().
start(Module, PhyModule) ->
    {Module, Module:init(PhyModule)}.

% @doc turns on the continuous reception
% @TODO specify which RX module has to be used
-spec turn_on(State, Callback, Ranging) -> Result when
      State    :: state(),
      Callback :: input_callback_raw_frame(),
      Ranging  :: pmod_uwb:flag(),
      Result   :: {ok, State} | {error, State, Error},
      Error    :: atom().
turn_on({Mod, Sub}, Callback, Ranging) ->
    case Mod:on(Sub, Callback, Ranging) of
        {ok, Sub2} -> {ok, {Mod, Sub2}};
        {error, Sub2, Error} -> {error, {Mod, Sub2}, Error}
    end.

% @doc turns off the continuous reception
-spec turn_off(State) -> State when
      State :: state().
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
-spec tx_request(State, Frame, CsmaParams, Ranging) -> Result when
      State       :: state(),
      Frame       :: bitstring(),
      CsmaParams  :: csma_params(),
      Ranging     :: ranging_tx(),
      State       :: state(),
      Result      :: {ok, State, RangingInfo}
                     | {error, State, Error},
      RangingInfo :: ranging_informations(),
      Error       :: tx_error().
tx_request({Mod, Sub}, Frame, CsmaParams, Ranging) ->
    case Mod:tx(Sub, Frame, CsmaParams, Ranging) of
        {ok, Sub2, RangingInfo} ->
            {ok, {Mod, Sub2}, RangingInfo};
        {error, Sub2, Err} ->
            {error, {Mod, Sub2}, Err}
    end.

-spec rx_request(State) -> {ok, State, Frame} | {error, State, Error} when
      State :: state(),
      Frame :: bitstring(),
      Error :: atom().
rx_request({Mod, Sub}) ->
    case Mod:rx(Sub) of
        {ok, Sub2, Frame} -> {ok, {Mod, Sub2}, Frame};
        {error, Sub2, Error} -> {error, {Mod, Sub2}, Error}
    end.

% @doc stop the duty cycle module
-spec stop(State, Reason) -> ok when
      State  :: state(),
      Reason :: atom().
stop({Mod, Sub}, Reason) ->
    Mod:terminate(Sub, Reason).
