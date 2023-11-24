-module(unslotted_CSMA).

-include("pmod_uwb.hrl").

-behaviour(gen_mac_tx).

-export([init/1]).
-export([tx/6]).
-export([terminate/2]).


-define(AUNITBACKOFFPERIOD, 20). % The number of symbols forming the basic time period used in CSMA-CA (src. IEEE 802.15.4 stdMA-CA (src. IEEE 802.15.4 std.)

%% CCA Mode 5 should last at least the maximum packet duration + the maximum period for ACK
%% Maximum packet duration = 1207.79µs
%% Maximum period for ACK = 1058.21µs
%% Sum => 2260µs
%% Since PRETOC units are in PAC size, we know that the default PAC is 8 symbols and 1 symbol ~ 1µs
%% We can conclude that CCA_DURATION = ceil(2260/8) = 283
-define(CCA_DURATION, 283).


% This module has the responsability of managing the channel access (CSMA/CA algorithm)

%--- gen_mac_tx Callbacks ------------------------------------------------------

init(PhyMod) -> #{phy_layer => PhyMod}.

-spec tx(State, Frame, MacMinBE, MacMaxCSMABackoffs, CW0, TxOpts) -> {ok, State} | {error, State, channel_access_failure} when
      State              :: map(),
      Frame              :: bitstring(),
      MacMinBE           :: non_neg_integer(),
      MacMaxCSMABackoffs :: non_neg_integer(),
      CW0                :: non_neg_integer(),
      TxOpts             :: #tx_opts{}.
tx(#{phy_layer := PhyMod} = State, Frame, MacMinBE, MacMaxCSMABackoffs, _, TxOpts) ->
    PhyMod:set_preamble_timeout(?CCA_DURATION),
    % PhyMod:suspend_frame_filtering(), % Frame filtering will trigger a flag if a frame is rejected (other than rxpto and rxsfdto)
    Ret = case unslotted_CSMA(PhyMod, Frame, 0, MacMinBE, MacMinBE, MacMaxCSMABackoffs) of 
              ok -> 
                  PhyMod:transmit(Frame, TxOpts),
                  {ok, State};
              error -> 
                  {error, State, channel_access_failure}
          end,
    PhyMod:disable_preamble_timeout(),
    % PhyMod:resume_frame_filtering(),
    Ret.

terminate(_State, _Reason) -> ok.

%--- Internal ------------------------------------------------------------------

-spec unslotted_CSMA(PhyMod, Frame, NB, BE, MacMinBE, MacMaxCSMABackoffs) -> ok | error when
      PhyMod             :: module(),
      Frame              :: bitstring(),
      NB                 :: non_neg_integer(),
      BE                 :: non_neg_integer(),
      MacMinBE           :: non_neg_integer(),
      MacMaxCSMABackoffs :: non_neg_integer().
unslotted_CSMA(_, _, NB, _, _, MacMaxCSMABackoffs) when NB > MacMaxCSMABackoffs ->
    error;
unslotted_CSMA(PhyMod, Frame, NB, BE, MacMinBE, MacMaxCSMABackoffs) ->
    % TODO: random backoff -> currently impossible ??
    % CCA
    case PhyMod:reception() of
        rxpto -> ok;
        rxsfdto -> ok; % In some cases, DW1000 can detect a false peamble. In these cases, rxsfdto is triggered
        _ -> unslotted_CSMA(PhyMod, Frame, NB+1, min(BE+1, MacMinBE), MacMinBE, MacMaxCSMABackoffs) % Still need to check if other rx error occured
    end. 
