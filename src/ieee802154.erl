-module(ieee802154).
-behaviour(gen_server).

%%% @headerfile "ieee802154.hrl"

% API
-export([start_link/1]).
-export([start/1]).
-export([stop_link/0]).
-export([stop/0]).

-export([transmission/1]).
-export([transmission/2]).
-export([reception/0]).

-export([rx_on/0]).
-export([rx_on/1]).
-export([rx_off/0]).

-export([get_mac_extended_address/0]).
-export([set_mac_extended_address/1]).

-export([get_pib_attribute/1]).
-export([set_pib_attribute/2]).

-export([reset/1]).

% gen_server callbacks
-export([init/1]).
-export([terminate/2]).
-export([code_change/4]).
-export([handle_call/3]).
-export([handle_cast/2]).


% Includes
-include_lib("eunit/include/eunit.hrl").

-include("ieee802154.hrl").
-include("mac_frame.hrl").

%--- Types ---------------------------------------------------------------------
-type pib_attribute() :: cw0
                       | mac_max_BE
                       | mac_min_BE
                       | mac_max_csma_backoffs
                       | mac_pan_id
                       | mac_short_address.

-type pib_attributes() :: #{cw0 := cw0(),
                            mac_max_BE := mac_max_BE(),
                            mac_min_BE := mac_max_BE(),
                            mac_max_csma_backoffs := mac_max_csma_backoff(),
                            mac_pan_id := mac_pan_id(),
                            mac_short_address := mac_short_address()}.

-type pib_set_error() :: read_only | unsupported_attribute | invalid_parameter.

-type state() :: #{phy_layer := module(),
                   duty_cycle := gen_duty_cycle:state(),
                   attributes := pib_attributes(),
                   input_callback := input_callback(),
                   _:=_}.

%--- API -----------------------------------------------------------------------

%% @doc Starts the IEEE 812.15.4 stack and creates a link
%%
%% ```
%% The following code will start the stack with the default parameters
%% 1> ieee802154:start_link(#ieee_parameters{}).
%%
%% Using a custom callback function
%% 2> ieee802154:start_link(#ieee_parameters{input_callback = fun callback/4}).
%%
%% Using a custom phy module
%% 3> ieee802154:start_link(#ieee_parameters{phy_layer = mock_phy_network}).
%% '''
%%
%% @param Params: A map containing the parameters of the IEEE stack
%%
%% @end
-spec start_link(Params) -> {ok, pid()} | {error, any()} when
      Params :: ieee_parameters().
start_link(Params) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).

%% @doc Same as start_link/1 but no link is created
%% @end
-spec start(Params) -> {ok, pid()} | {error, any()} when
      Params :: ieee_parameters().
start(Params) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Params, []).

stop_link() ->
    gen_server:stop(?MODULE).

stop() -> gen_server:stop(?MODULE).

%% @doc
%% @equiv transmission(Frame, 0)
%% @end
-spec transmission(Frame) -> Result when
      Frame        :: frame(),
      Result       :: {ok, Ranging} | {error, Error},
      Ranging      :: ranging_informations(),
      Error        :: tx_error().
transmission(Frame) ->
    transmission(Frame, ?NON_RANGING).

%% @doc Performs a transmission on the defined IEEE 802.15.4 stack
%% When ranging has been activated for the frame, the second element of the
%% tuple contains different values that can be used for ranging operations
%% For more informations please consult the IEEE 802.15.4 standard.
%% Note that the variable `Timestamp' is omited because its value is the same
%% as `Ranging counter start'
%%
%% When Ranging isn't activated, the 2nd element of the tuple shall be ignored
%% ```
%% Ranging not activated for transmission
%% 1> ieee802154:transmission(Frame, ?NON_RANGING).
%%
%% Activate ranging for the transmission
%% 2> ieee802154:transmission(Frame, ?ALL_RANGING).
%% '''
%% @end
-spec transmission(Frame, Ranging) -> Result when
      Frame        :: frame(),
      Ranging      :: ranging_tx(),
      Result       :: {ok, RangingInfos} | {error, Error},
      RangingInfos :: ranging_informations(),
      Error        :: tx_error().
transmission(Frame, Ranging) ->
    {FH, _, _} = Frame,
    case FH of
        #frame_control{dest_addr_mode = ?NONE, src_addr_mode = ?NONE} ->
            {error, invalid_address};
        _ ->
            gen_server:call(?MODULE,
                            {tx, Frame, Ranging},
                            infinity)
    end.

%% @doc Performs a reception on the IEEE 802.15.4 stack
%% @deprecated This function will be deprecated
%% @end
-spec reception() -> Result when
      Result :: {ok, frame()} | {error, atom()}.
reception() ->
    gen_server:call(?MODULE, {rx}, infinity).

%% @doc
%% @equiv rx_on(0)
%% @end
-spec rx_on() -> Result when
      Result :: ok | {error, atom()}.
rx_on() ->
    gen_server:call(?MODULE, {rx_on, ?DISABLED}).

%% @doc Turns on the continuous reception
%% When a frame is received, the callback function is called
%% (see {@link start_link})
%%
%% The ranging parameter is used to specify if ranging is activated during rx
%%
%% ```
%% Ranging not activated
%% 1> ieee802154:rx_on(?DISABLED).
%%
%% Ranging activated
%% 2> ieee802154:rx_on(?ACTIVATED).
%% '''
%% @end
-spec rx_on(Ranging) -> ok when
      Ranging :: ?DISABLED | ?ENABLED.
rx_on(Ranging) ->
    gen_server:call(?MODULE, {rx_on, Ranging}).

%% @doc Turns off the continuous reception
%% @end
rx_off() ->
    gen_server:call(?MODULE, {rx_off}).

%% @doc Gets the extended address of the device
%% The function returns the address as a bitstring
-spec get_mac_extended_address() -> bitstring().
get_mac_extended_address() ->
    gen_server:call(?MODULE, {get, mac_extended_address}).

%% @doc Sets the mac extended address of the device
%% The Value is a bitstring with a size of 64 bits
-spec set_mac_extended_address(Value::bitstring()) -> ok.
set_mac_extended_address(Value) ->
    gen_server:call(?MODULE, {set, mac_extended_address, Value}).

%% @doc Get the value of a PIB attribute
%% @end
-spec get_pib_attribute(Attribute) -> Value when
      Attribute :: pib_attribute(),
      Value     :: term().
get_pib_attribute(Attribute) ->
    gen_server:call(?MODULE, {get, Attribute}).


%% @doc Set the value of a PIB attribute
%% @end
-spec set_pib_attribute(Attribute, Value) -> ok when
      Attribute :: pib_attribute(),
      Value     :: term().
set_pib_attribute(Attribute, Value) ->
    gen_server:call(?MODULE, {set, Attribute, Value}).

-spec reset(SetDefaultPIB) -> Result when
      SetDefaultPIB :: boolean(),
      Result :: ok.
reset(SetDefaultPIB) ->
    gen_server:call(?MODULE, {reset, SetDefaultPIB}).

%--- gen_statem callbacks ------------------------------------------------------

-spec init(Params) -> {ok, State} when
      Params :: ieee_parameters(),
      State  :: state().
init(Params) ->
    PhyMod = Params#ieee_parameters.phy_layer,
    write_default_conf(PhyMod),

    DutyCycleState = gen_duty_cycle:start(Params#ieee_parameters.duty_cycle,
                                          PhyMod),

    Data = #{phy_layer => PhyMod,
             duty_cycle => DutyCycleState,
             attributes => default_attribute_values(),
             ranging => ?DISABLED,
             input_callback => Params#ieee_parameters.input_callback},
    {ok, Data}.

-spec terminate(Reason, State) -> ok when
      Reason :: term(),
      State :: state().
terminate(Reason, #{duty_cycle := GenDutyCycleState}) ->
    gen_duty_cycle:stop(GenDutyCycleState, Reason).

code_change(_, _, _, _) ->
    error(not_implemented).

-spec handle_call(_, _, State) -> Result when
      State   :: state(),
      Result  :: {reply, term(), State}.
handle_call({rx_on, RangingFlag}, _From, State) ->
    #{duty_cycle := DCState,
      input_callback := Callback} = State,
    NewCallback = fun(Frame, LQI, _PRF, Sec, _PreambleRep, _DataRate, Rng) ->
                    Callback(mac_frame:decode(Frame), LQI, Sec, Rng)
                  end,
    case gen_duty_cycle:turn_on(DCState, NewCallback, RangingFlag) of
        {ok, NewDutyCycleState} ->
            {reply, ok, State#{duty_cycle => NewDutyCycleState}};
        {error, NewDutyCycleState, Error} ->
            {reply, {error, Error}, State#{duty_cycle => NewDutyCycleState,
                                           ranging := RangingFlag}}
    end;
handle_call({rx_off}, _From, #{duty_cycle := DCState} = State) ->
    NewDCState = gen_duty_cycle:turn_off(DCState),
    {reply, ok, State#{duty_cycle => NewDCState}};
handle_call({tx, Frame, Ranging}, _From, State) ->
    #{duty_cycle := DCState, attributes := Attributes} = State,
    CsmaParams = get_csma_params(Attributes),
    {FrameControl, MacHeader, Payload} = Frame,
    EncFrame = mac_frame:encode(FrameControl, MacHeader, Payload),
    case gen_duty_cycle:tx_request(DCState, EncFrame, CsmaParams, Ranging) of
        {ok, NewDCState, RangingInfos} ->
            {reply, {ok, RangingInfos}, State#{duty_cycle => NewDCState}};
        {error, NewDCState, Error} ->
            {reply, {error, Error}, State#{duty_cycle => NewDCState}}
        end;
handle_call({rx}, _From, #{duty_cycle := DutyCycleState} = State) ->
    case gen_duty_cycle:rx_request(DutyCycleState) of
        {ok, NewDutyCycleState, Frame} ->
            DecFrame = mac_frame:decode(Frame),
            {reply, {ok, DecFrame}, State#{duty_cycle => NewDutyCycleState}};
        {error, NewDutyCycleState, Error} ->
            {reply, {error, Error}, State#{duty_cycle => NewDutyCycleState}}
    end;
handle_call({get, Attribute}, _From, State) ->
    #{phy_layer := PhyMod, attributes := Attributes} = State,
    case get(PhyMod, Attributes, Attribute) of
        {ok, Value} ->
            {reply, Value, State};
        {error, Error} ->
            {reply, {error, Error}, State}
    end;
handle_call({set, Attribute, Value}, _From, State) ->
    #{phy_layer := PhyMod, attributes := Attributes} = State,
    case set(PhyMod, Attributes, Attribute, Value) of
        {ok, NewAttributes} ->
            {reply, ok, State#{attributes => NewAttributes}};
        {error, Error} ->
            {reply, {error, Error}, State}
    end;
handle_call({reset, SetDefaultPIB}, _From, State) ->
    #{phy_layer := PhyMod, duty_cycle := DCState} = State,
    NewState = case SetDefaultPIB of
                   true ->
                       PhyMod:write(panadr, #{pan_id => <<16#FFFF:16>>,
                                              short_addr => <<16#FFFF:16>>}),
                       State#{attributes => default_attribute_values()};
                   _ ->
                       State
               end,
    NewDCState = gen_duty_cycle:turn_off(DCState),
    {reply, ok, NewState#{duty_cycle => NewDCState, ranging => ?DISABLED}};
handle_call(_Request, _From, _State) ->
    error(call_not_recognized).

handle_cast(_, _) ->
    error(not_implemented).

%--- Internal ------------------------------------------------------------------
-spec write_default_conf(PhyMod :: module()) -> ok.
write_default_conf(PhyMod) ->
    PhyMod:write(rx_fwto, #{rxfwto => ?MACACKWAITDURATION}),
    PhyMod:write(sys_cfg, #{ffab => 1,
                            ffad => 1,
                            ffaa => 1,
                            ffam => 1,
                            ffen => 1,
                            autoack => 1,
                            rxwtoe => 1}).

-spec default_attribute_values() -> Attributes when
      Attributes   :: #{cw0 := 2,
                      mac_max_BE := 5,
                      mac_max_csma_backoffs := 4,
                      mac_min_BE := 3,
                      mac_pan_id := <<_:16>>,
                      mac_short_address := <<_:16>>}.
default_attribute_values() ->
    #{
      cw0 => 2, % cf. p.22 standard
      mac_max_BE => 5,
      mac_max_csma_backoffs => 4,
      mac_min_BE => 3,
      mac_pan_id => <<16#FFFF:16>>,
      mac_short_address => <<16#FFFF:16>>
     }.

-spec get_csma_params(Attributes) -> CsmaParams when
      Attributes :: pib_attributes(),
      CsmaParams :: csma_params().
get_csma_params(Attributes) ->
    #csma_params{mac_min_BE = maps:get(mac_min_BE, Attributes),
                 mac_max_BE = maps:get(mac_max_BE, Attributes),
                 mac_max_csma_backoff = maps:get(mac_max_csma_backoffs,
                                                 Attributes),
                 cw0 = maps:get(cw0, Attributes)}.

%--- Internal: getter/setter PiB
-spec get(PhyMod, Attributes, Attribute) -> Result  when
    PhyMod     :: module(),
    Attributes :: pib_attributes(),
    Attribute  :: pib_attribute(),
    Result     :: {ok, Value} | {error, unsupported_attribute},
    Value      :: term().
get(PhyMod, _Attributes, mac_extended_address) ->
    #{eui := EUI} = PhyMod:read(eui),
    {ok, EUI};
get(_PhyMod, Attributes, Attribute) when is_map_key(Attribute, Attributes) ->
    {ok, maps:get(Attribute, Attributes)};
get(_, _, _) ->
    {error, unsupported_attribute}.

-spec set(PhyMod, Attributes, Attribute, Value) -> Results when
    PhyMod        :: module(),
    Attributes    :: pib_attributes(),
    Attribute     :: pib_attribute(),
    Value         :: term(),
    Results       :: {ok, NewAttributes} | {error, Error},
    NewAttributes :: pib_attributes(),
    Error         :: pib_set_error().
set(PhyMod, Attributes, mac_extended_address, Value) ->
    PhyMod:write(eui, #{eui => Value}), % TODO check the range/type/value given
    {ok, Attributes};
set(PhyMod, Attributes, mac_short_address, Value) ->
    PhyMod:write(panadr, #{short_addr => Value}),
    {ok, Attributes#{mac_short_address => Value}};
set(PhyMod, Attributes, mac_pan_id, Value) ->
    PhyMod:write(panadr, #{pan_id => Value}),
    {ok, Attributes#{mac_pan_id => Value}};
set(_, Attributes, Attribute, Value) when is_map_key(Attribute, Attributes) ->
    NewAttributes = maps:update(Attribute, Value, Attributes),
    {ok, NewAttributes};
set(_, _, _, _) ->
    {error, unsupported_attribute}. % TODO detect if PIB is a read only attribute
