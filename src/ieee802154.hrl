%--- Macros --------------------------------------------------------------------

%--- MCPS-DATA.indication Parameters

% Ranging Received values:
-define(NO_RANGING_REQUESTED, 0).
-define(RANGING_REQUESTED_BUT_NOT_SUPPORTED, 1).
-define(RANGING_ACTIVE, 2).

% Ranging Transmission values:
-define(NON_RANGING, 0).
-define(ALL_RANGING, 1).
% PHY_HEADER_ONLY => Not supported in our case

%--- Types ---------------------------------------------------------------------

%--- Record types
-record(ieee_parameters, {mac_layer = mac_layer :: module(),
                          mac_parameters = #{phy_layer => pmod_uwb, duty_cycle => duty_cycle_non_beacon} :: map(),
                          input_callback = fun(_, _, _, _) -> ok end :: input_callback()}).


-record(ranging_informations, {ranging_received = ?NO_RANGING_REQUESTED :: ranging_received(),
                               ranging_counter_start = 0                :: integer(),
                               ranging_counter_stop = 0                 :: integer(),
                               ranging_tracking_interval = 0            :: integer(),
                               ranging_offset = 0                       :: integer(),
                               ranging_FOM = <<16#00:8>>                :: bitstring()}).

-opaque ranging_informations() :: #ranging_informations{}.

% For now security isn't enabled
-record(security, {security_level = 0       :: integer(),
                   key_id_mode = 0          :: integer(),
                   key_source = <<16#00:8>> :: bitstring()}).

-opaque security() :: #security{}.

-record(csma_params, {mac_min_BE           :: mac_min_BE(),
                          mac_max_BE           :: mac_max_BE(),
                          mac_max_csma_backoff :: mac_max_csma_backoff(),
                          cw0                  :: cw0()}).

%--- IEEE 802.15.4 parameter types
-export_type([ieee_parameters/0, ranging_informations/0, security/0, input_callback/0, ranging_tx/0]).
-type cw0() :: 1 | 2.
-type mac_max_BE() :: 3..8.
-type mac_max_csma_backoff() :: 0..5.
-type mac_min_BE() :: 0..8.

-type ranging_received() :: ?NO_RANGING_REQUESTED | ?RANGING_REQUESTED_BUT_NOT_SUPPORTED | ?RANGING_ACTIVE.
-type ranging_tx() :: ?NON_RANGING | ?ALL_RANGING. % PHY_HEADER_ONLY no used in our case

% *** indicates unusefull parameters for higher layers for now
-type input_callback() :: fun((Frame                  :: mac_frame:frame(),
                               LQI                    :: integer(),
                               % UWBPRF                 :: gen_mac_layer:uwb_PRF(),
                               Security               :: security(),
                               % UWBPreambleRepetitions :: pmod_uwb:uwb_preamble_symbol_repetition(),
                               % DataRate               :: pmod_uwb:data_rate(),
                               Ranging                :: ranging_informations())
                              -> ok).

-type ieee_parameters() :: #ieee_parameters{}.

-type csma_params() :: #csma_params{}.

