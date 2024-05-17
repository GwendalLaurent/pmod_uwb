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

-export([get_pib_attribute/1]).
-export([set_pib_attribute/2]).

-export([reset/1]).
-export([scan/4]).

% gen_server callbacks
-export([init/1]).
-export([terminate/2]).
-export([code_change/4]).
-export([handle_call/3]).
-export([handle_cast/2]).


% Includes
-include("ieee802154.hrl").
-include("ieee802154_pib.hrl").
-include("mac_frame.hrl").

%--- Types ---------------------------------------------------------------------

-type state() :: #{phy_layer := module(),
                   duty_cycle := gen_duty_cycle:state(),
                   pib := pib_state(),
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

% In our case, channel page is 4 in all cases (cf. p149 sec. 8.1.2.4)
-spec scan(Type, Channels, Duration, Security) -> Result when
      Type     :: scan_type(),
      Channels :: [phy_channel(), ...],
      Duration :: 0..14,
      Security :: security(),
      Result   :: {scan_status(), scan_result()}.
scan(Type, Channels, Duration, Security) ->
    gen_server:call(?MODULE, {scan,
                              Type,
                              Channels,
                              Duration,
                              Security}, 2500*length(Channels)*4).
% 2500ms the uppper bound a single scan for a given channel and preamble code
% Depending on the prf, a channel can have 2 or 4 preamble codes

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
             pib => ieee802154_pib:init(PhyMod),
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
    #{duty_cycle := DCState, pib := Pib} = State,
    {FrameControl, MacHeader, Payload} = Frame,
    EncFrame = mac_frame:encode(FrameControl, MacHeader, Payload),
    case gen_duty_cycle:tx_request(DCState, EncFrame, Pib, Ranging) of
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
    #{pib := Pib} = State,
    case ieee802154_pib:get(Pib, Attribute) of
        {error, Error} ->
            {reply, {error, Error}, State};
        Value ->
            {reply, Value, State}
    end;
handle_call({set, Attribute, Value}, _From, State) ->
    #{pib := Pib} = State,
    case ieee802154_pib:set(Pib, Attribute, Value) of
        {ok, NewPib} ->
            {reply, ok, State#{pib => NewPib}};
        {error, NewPib, Error} ->
            {reply, {error, Error}, State#{pib => NewPib}}
    end;
handle_call({reset, SetDefaultPIB}, _From, State) ->
    #{phy_layer := PhyMod, pib := Pib, duty_cycle := DCState} = State,
    NewState = case SetDefaultPIB of
                   true ->
                       PhyMod:write(panadr, #{pan_id => <<16#FFFF:16>>,
                                              short_addr => <<16#FFFF:16>>}),
                       State#{pib => ieee802154_pib:reset(Pib)};
                   _ ->
                       State
               end,
    NewDCState = gen_duty_cycle:turn_off(DCState),
    {reply, ok, NewState#{duty_cycle => NewDCState, ranging => ?DISABLED}};
handle_call({scan, Type, Channels, Duration, _Security}, _From, State) ->
    % For now, we concider that the device always has the capacity to store all
    % the results. Might change later
    #{phy_layer := PhyMod, attributes := Attributes} = State,
    ScanDurationSymb = ?aBaseSuperframeDuration * (math:pow(2, Duration)+1),
    ScanDurationMS = round(ScanDurationSymb * ?SymbToMicroSec),
    NewAttributes = setup_scan(State, Type),
    Result = lists:map(fun(Channel) ->
                               do_scan(PhyMod, Type, ScanDurationMS, Channel)
                       end, Channels),
    NewState = teardown_scan(NewAttributes, Type, Attributes),
    ResultSize = length(Result),
    ScanResult = case Type of
                     ed ->
                         #scan_result{result_list_size = ResultSize,
                                      uwb_en_det_list = Result};
                     _ ->
                         #scan_result{result_list_size = ResultSize,
                                      pan_descr_list = Result}
                 end,
    ScanStatus = case ResultSize of
                     0 -> no_beacon;
                     _ -> ok
                 end,
    {reply, {ScanStatus, ScanResult}, NewState};
handle_call(_Request, _From, _State) ->
    error(call_not_recognized).

handle_cast(_, _) ->
    error(not_implemented).

%--- Internal ------------------------------------------------------------------
%--- Internal: Scan setup/teardown
-spec setup_scan(State, Type) -> NewState when
      State    :: state(),
      Type     :: scan_type(),
      NewState :: state().
setup_scan(State, ed) ->
    general_scan_setup(State),
    State;
setup_scan(State, Type) when Type == active orelse Type == passive ->
    #{phy_layer := PhyMod, attributes := Attributes} = State,
    general_scan_setup(State),
    NewAttributes = set(PhyMod, Attributes, mac_pan_id, <<16#FFFF:16>>),
    State#{attributes => NewAttributes};
setup_scan(_, _) ->
    error(not_implemented).

general_scan_setup(State) ->
    #{phy_layer := PhyMod} = State,
    PhyMod:write(sys_cfg, #{ffab => 1,
                            ffad => 0,
                            ffaa => 0,
                            ffam => 1,
                            ffen => 1,
                            autoack => 0,
                            rxwtoe => 1}).

-spec teardown_scan(State, Type, OldAttributes) -> ok when
      State         :: state(),
      Type          :: scan_type(),
      OldAttributes :: pib_attributes().
teardown_scan(State, ed, _OldAttributes) ->
    general_scan_teardown(State);
teardown_scan(State, Type, OldAttributes) when Type == active
                                                orelse Type == passive ->
    #{phy_layer := PhyMod, attributes := Attributes} = State,
    general_scan_teardown(State),
    {ok, PanId} = get(PhyMod, OldAttributes, mac_pan_id),
    set(PhyMod, Attributes, mac_pan_id, PanId);
teardown_scan(_, _, _) ->
    error(not_implemented).

general_scan_teardown(State) ->
    #{phy_layer := PhyMod} = State,
    PhyMod:write(sys_cfg, #{ffab => 1,
                            ffad => 1,
                            ffaa => 1,
                            ffam => 1,
                            ffen => 1,
                            autoack => 0,
                            rxwtoe => 1}).

%--- Internal: Performing Scan

-spec do_scan(PhyMod, Type, Duration, Channel) -> Result when
      PhyMod :: module(),
      Type   :: scan_type(),
      Duration :: miliseconds(),
      Channel :: phy_channel(),
      Result   :: [] | [integer()] | [pan_descr()].
do_scan(PhyMod, ed, Duration, Channel) ->
    PRF = PhyMod:prf_value(),
    PreambleCodes = ?PCode(Channel, PRF),
    lists:foldl(
      fun(PCode, Acc) ->
              PhyMod:set_channel(Channel, PCode),
              Now = erlang:timestamp(),
              Noise = do_ed(PhyMod, Channel, Duration, Now, -0.0, -0.0),
              max(Acc, Noise)
      end, -0.0, PreambleCodes);
do_scan(_, _, _, _) ->
    error(not_implemented).

%--- Internal: ED scan
-spec do_ed(PhyMod, Channel, D, TStart, Elapsed, Acc) -> EDMeasurement when
      PhyMod        :: module(),
      Channel       :: phy_channel(),
      D             :: miliseconds(),
      TStart        :: erlang:timestamp(),
      Elapsed       :: integer(),
      Acc           :: float(),
      EDMeasurement :: float().
do_ed(_, _, D, _, Elapsed, Acc) when Elapsed >= D ->
    Acc;
do_ed(PhyMod, Channel, D, TStart, _Elapsed, Acc) ->
    PhyMod:write(agc_ctrl, #{agc_ctrl1 => #{dis_am => 2#0}}),
    PhyMod:write(sys_ctrl, #{rxenab => 2#1}),
    timer:sleep(1),
    PhyMod:write(agc_ctrl, #{agc_ctrl1 => #{dis_am => 2#1}}),
    PhyMod:write(sys_ctrl, #{trxoff => 2#1}),
    #{agc_stat1 := #{edv2 := EDV2, edg1 := EDG1}} = PhyMod:read(agc_ctrl),
    ApproxNoise = (EDV2 - 40) * math:pow(10, EDG1) * ?SCH(Channel),
    TNow = erlang:timestamp(),
    NowDiff = timer:now_diff(TNow, TStart),
    NewAcc = max(Acc, ApproxNoise),
    do_ed(PhyMod, Channel, D, TStart, NowDiff, NewAcc).

-spec send_beacon_request(State) -> Result when
      State :: state(),
      Result :: {ok, NewState} | {error, NewState},
      NewState :: state().
send_beacon_request(State) ->
    #{duty_cycle := DCState, attributes := Attributes} = State,
    Csma = get_csma_params(Attributes),
    BeacReq = mac_frame:encode_beacon_request(),
    case gen_duty_cycle:tx_request(DCState, BeacReq, Csma, ?NON_RANGING) of
        {ok, NewDCState, _} ->
            {ok, State#{duty_cycle => NewDCState}};
        {error, NewDCState, channel_access_failure} ->
            {error, State#{duty_cycle => NewDCState}};
        {error, _NewDCState, Error} ->
            error(unexpected_tx_error, [Error])
    end.

-spec receive_beacon(ReqRes, Channel, PreambleCodes) -> Result when
      ReqRes        :: {ok, State} | {error, State},
      Channel       :: phy_channel(),
      PreambleCodes :: [integer()],
      State         :: state(),
      Result        :: {ok, State, PanDesc}
                     | {ok, State, empty}
                     | {error, State, beacon_tx_error},
      PanDesc       :: pan_descr().
receive_beacon({ok, State}, Channel, PreambleCodes) ->
    #{duty_cycle := DCState} = State,
    case gen_duty_cycle:rx_request(DCState) of
        {ok, NewDCState, Frame} ->
            PanDesc = pan_descr(Frame, Channel, 0, PreambleCodes),
            {ok, State#{duty_cycle => NewDCState}, PanDesc};
        {error, NewDCState, rxrfto} ->
            {ok, State#{duty_cycle => NewDCState}, empty};
        {error, _NewDCState, Error} ->
            error(unexpected_rx_error, [Error]) % ! Might have other t.o., how should it be handled ?
    end;
receive_beacon({error, State}, _, _) ->
    {error, State, beacon_tx_error}.

-spec pan_descr(Frame, ChannelNbr, Timestamp, PreambleCodes) -> pan_descr() when
      Frame         :: binary(),
      ChannelNbr    :: phy_channel(),
      Timestamp     :: non_neg_integer(),
      PreambleCodes :: [preamble_code()].
pan_descr(Frame, ChannelNbr, Timestamp, PreambleCodes) ->
    {FC, MH, {Metadatas, _BeaconPayload}} = mac_frame:decode(Frame),
    {SuperframeSpecs, GTSFields, _} = Metadatas,
    #pan_descr{coord_addr_mode = FC#frame_control.src_addr_mode,
               coord_pan_id = MH#mac_header.src_pan,
               coord_addr = MH#mac_header.src_addr,
               channel_nbr = ChannelNbr,
               superframe_specs = SuperframeSpecs,
               gts_permit = GTSFields#gts_fields.gts_permit,
               link_quality = 0, % TODO
               timestamp = Timestamp,
               security_status = success, % No security for the moment
               security = #security{},
               code_list = PreambleCodes}.

%--- Internal: PIB values
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