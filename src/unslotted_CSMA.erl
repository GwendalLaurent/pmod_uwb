-module(unslotted_CSMA).

-include("pmod_uwb.hrl").

-behaviour(gen_mac_tx).

-export([init/1]).
-export([tx/6]).
-export([terminate/2]).


% This module has the responsability of managing the channel access (CSMA/CA algorithm)

init(PhyMod) -> #{phy_layer => PhyMod}.

tx(#{phy_layer := PhyMod} = State, Frame, MacMinBE, MacMaxCSMABackoffs, _, TxOpts) ->
    PhyMod:transmit(Frame, TxOpts),
    {ok, State}.

terminate(_State, _Reason) -> ok.
