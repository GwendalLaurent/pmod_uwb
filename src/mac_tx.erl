-module(mac_tx).

-include("pmod_uwb.hrl").

-export([tx/2]).
-export([tx/3]).

% This module has the responsability of managing the channel access (CSMA/CA algorithm)

tx(PhyMod, Frame) ->
    PhyMod:transmit(Frame, #tx_opts{}).
tx(PhyMod, Frame, TxOpts) ->
    PhyMod:transmit(Frame, TxOpts).
