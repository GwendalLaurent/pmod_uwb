% This module has the responsability of managing the channel access (CSMA/CA algorithm)
-module(unslotted_CSMA).

-include("ieee802154.hrl").
-include("ieee802154_pib.hrl").
-include("pmod_uwb.hrl").

-behaviour(gen_mac_tx).

-export([init/1]).
-export([tx/4]).
-export([terminate/2]).

%--- Macros --------------------------------------------------------------------

% According to Qorov forums, 1 symbol ~ 1 µs => The unit of AUNITBACKOFFPERIOD are in µs
-define(AUNITBACKOFFPERIOD, 20). % The number of symbols forming the basic time period used in CSMA-CA (src. IEEE 802.15.4 stdMA-CA (src. IEEE 802.15.4 std.)


%% CCA Mode 5 should last at least the maximum packet duration + the maximum period for ACK
%% Maximum packet duration = 1207.79µs
%% Maximum period for ACK = 1058.21µs + 12µs
%% Sum => 2272µs
%% Since PRETOC units are in PAC size, we know that the default PAC is 8 symbols and 1 symbol ~ 1µs
%% We can conclude that CCA_DURATION = ceil(2272/8) = 284
-define(CCA_DURATION, 284).

%--- Records -------------------------------------------------------------------

%--- gen_mac_tx Callbacks ------------------------------------------------------

-spec init(PhyMod) -> State when
      PhyMod :: module(),
      State  :: map().
init(PhyMod) -> #{phy_layer => PhyMod}.

%% @doc Tries to transmit a frame using unslotted CSMA-CA
%% @param MacMinBE: The minimum value of the backoff exponent as described in the standard
%% @param MacMaxCSMABackoffs: The maximum amount of time the CSMA algorithm will backoff if the channel is busy
%% @param CW0: Not needed in this version of the algorithm. Ignored by this function
-spec tx(State, Frame, Pib, TxOpts) -> {ok, State} | {error, State, channel_access_failure} when
      State  :: map(),
      Frame  :: bitstring(),
      Pib    :: pib_state(),
      TxOpts :: tx_opts().
tx(#{phy_layer := PhyMod} = State, Frame, Pib, TxOpts) ->
    PhyMod:set_preamble_timeout(?CCA_DURATION),
    MacMinBE = ieee802154_pib:get(Pib, mac_min_BE),
    MacMaxBE = ieee802154_pib:get(Pib, mac_max_BE),
    MacMaxCSMABackoffs = ieee802154_pib:get(Pib, mac_max_csma_backoffs),
    Ret = case try_cca(PhyMod, 0, MacMinBE, MacMaxBE, MacMaxCSMABackoffs) of
              ok ->
                  PhyMod:transmit(Frame, TxOpts),
                  {ok, State};
              error ->
                  {error, State, channel_access_failure}
          end,
    PhyMod:disable_preamble_timeout(),
    Ret.

terminate(_State, _Reason) -> ok.

%--- Internal -----------------------------------------------------------------

-spec try_cca(PhyMod, NB, BE, MacMaxBE, MacMaxCSMABackoffs) -> Result when
      PhyMod             :: module(),
      NB                 :: non_neg_integer(),
      BE                 :: non_neg_integer(),
      MacMaxBE           :: mac_max_BE(),
      MacMaxCSMABackoffs :: mac_max_csma_backoff(),
      Result             :: ok | error.
try_cca(_, NB, _, _, MacMaxCSMABackoffs) when NB > MacMaxCSMABackoffs ->
    error;
try_cca(PhyMod, NB, BE, MacMaxBE, MacMaxCSMABackoffs) ->
    RandBackOff = rand:uniform(trunc(math:pow(2,BE)-1)) * ?AUNITBACKOFFPERIOD,
    SleepTime = trunc(math:ceil(RandBackOff/1000)),
    timer:sleep(SleepTime),
    case cca(PhyMod) of
        ok -> ok;
        error -> try_cca(PhyMod, NB+1, min(BE+1,MacMaxBE), MacMaxBE, MacMaxCSMABackoffs)
    end.

-spec cca(PhyMod) -> Result when
      PhyMod :: module(),
      Result :: ok | error.
cca(PhyMod) ->
    case PhyMod:reception() of
        rxpto -> ok;
        rxsfdto -> ok;
        rxprd -> error;
        rxsfdd -> error; % theoritically, this should cover any frame rx (i.e. channel is busy)
        _ -> error % In case you receive a frame -> ? Could this happen ?
    end.
