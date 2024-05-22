%--- Macros --------------------------------------------------------------------
% Only valid for PRF at 16 MHz and Data rate at 6.81 Mbps
% cf. Table 13, P.19 of DW1000 data sheet
-define(SymbToMicroSec, 0.128).

%--- MAC constants
-define(aBaseSlotDuration, 60). % Value in symbols
-define(aBaseSuperframeDuration, 960). % aBaseSlotDuration * aNumSuperframeSlots
-define(aNumSuperframeSlots, 16). 

%--- MCPS-DATA.indication Parameters

% Ranging Received values:
-define(NO_RANGING_REQUESTED, 0).
-define(RANGING_REQUESTED_BUT_NOT_SUPPORTED, 1).
-define(RANGING_ACTIVE, 2).

% Ranging Transmission values:
-define(NON_RANGING, 0).
-define(ALL_RANGING, 1).
% PHY_HEADER_ONLY => Not supported in our case

% CSMA constants
-define(MACMAXFRAMERETRIES, 3).
-define(MACACKWAITDURATION, 4000).  % works with 2000 µs but calculations give me 4081µs
% -define(MACACKWAITDURATION, 2000).  % works with 2000 µs but calculations give me 4081µs

%--- Types ---------------------------------------------------------------------

%--- Record types
-record(ieee_parameters, {duty_cycle = duty_cycle_non_beacon :: module(),
                          phy_layer = pmod_uwb :: module(),
                          input_callback = fun(_, _, _, _) -> ok end :: input_callback()}).


-record(ranging_informations, {ranging_received = ?NO_RANGING_REQUESTED :: ranging_received() | boolean(),
                               ranging_counter_start = 0                :: integer(),
                               ranging_counter_stop = 0                 :: integer(),
                               ranging_tracking_interval = 0            :: integer(),
                               ranging_offset = 0                       :: integer(),
                               ranging_FOM = <<16#00:8>>                :: bitstring()}).

-type ranging_informations() :: #ranging_informations{}.

-record (pan_descr, {coord_addr_mode   :: mac_frame:addr_mode(),
                     coord_pan_id      :: <<_:16>>,
                     coord_addr        :: <<_:16>> | <<_:64>>,
                     channel_nbr       :: channel(),
                     channel_page = 4  :: 4,
                     superframe_specs  :: mac_frame:superframe_specs(),
                     gts_permit        :: boolean(),
                     link_quality      :: integer(),
                     timestamp         :: integer(),
                     security_status   :: security_status(),
                     security          :: security(),
                     code_list         :: [integer()]}).

-type pan_descr() :: #pan_descr{}.

%% No EnergyDetectList because we are only working with UWB
%% => The list will always be empty
-record(scan_result, {unscanned_channels = [] :: [integer()],
                      result_list_size   = 0  :: non_neg_integer(),
                      pan_descr_list     = [] :: [pan_descr()],
                      detected_cat       = 1  :: non_neg_integer(),
                      uwb_en_det_list    = [] :: [integer()]}).

-type scan_result() :: #scan_result{}.

% For now security isn't enabled
-type security_level() :: 0..7.
-type key_id_mode() :: 0..3.
-type key_source() :: bitstring().
-type key_index() :: 0..255.

-record(security, {security_level = 0       :: security_level(),
                   key_id_mode = 0          :: key_id_mode(),
                   key_source = <<16#00:8>> :: key_source(),
                   key_index = 0            :: key_index()}).

-type security() :: #security{}.

-type security_status() :: success 
                         | counter_error
                         | improper_key_type
                         | improper_security_level
                         | security_error
                         | unavailable_key
                         | unsupported_legacy
                         | unsuppperted_security.

%--- IEEE 802.15.4 parameter types
-export_type([ieee_parameters/0, ranging_informations/0, security/0, input_callback/0, ranging_tx/0, tx_error/0]).

-type ranging_received() :: ?NO_RANGING_REQUESTED | ?RANGING_REQUESTED_BUT_NOT_SUPPORTED | ?RANGING_ACTIVE.
-type ranging_tx() :: ?NON_RANGING | ?ALL_RANGING. % PHY_HEADER_ONLY no used in our case

% *** indicates unusefull parameters for higher layers for now
-type input_callback() :: fun((Frame                  :: mac_frame:frame(),
                               LQI                    :: integer(), % Still buggy
                               % UWBPRF                 :: gen_mac_layer:uwb_PRF(),
                               Security               :: security(),
                               % UWBPreambleRepetitions :: pmod_uwb:uwb_preamble_symbol_repetition(),
                               % DataRate               :: pmod_uwb:data_rate(),
                               Ranging                :: ranging_informations())
                              -> ok).

-type ieee_parameters() :: #ieee_parameters{}.

-type tx_error() :: invalid_address | invalid_gts | transaction_overflow | transaction_expired | no_ack | frame_too_long | channel_access_failure.

-type scan_type() :: ed | active | passive | orphan.

%% @doc Supported channels by the DW1000
%% @end
-type channel() :: 1 | 2 | 3 | 4 | 5 | 7.

-type preamble_code() ::  1..8.
% ok <=> SUCCESS
-type scan_status() :: ok
                     | limit_reached
                     | no_beacon
                     | scan_in_progress
                     | counter_error
                     | frame_too_long
                     | unvailable_key
                     | unsupported_security
                     | invalid_parameter.
