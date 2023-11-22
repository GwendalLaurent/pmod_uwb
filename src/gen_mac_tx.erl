%% @doc This generic module defines the behaviour for any module implementing the transmission of the IEEE 802.15.4 stack

-module(gen_mac_tx).

-export([start/2]).
-export([transmit/6]).
-export([stop/2]).

-include("pmod_uwb.hrl").

%--- Callbacks -----------------------------------------------------------------

-callback init(PhyMod::module()) -> State::mod_state().
-callback tx(State::mod_state(), MacMinBe:: Frame::bitstring(), MacMinBE::pos_integer(), MacMaxCSMABackoffs::pos_integer(), CW0::pos_integer(), TxOptions::#tx_opts{}) -> {ok, Newstate::mod_state()} | {error, Newstate::mod_state(), Error::tx_error()}.
-callback terminate(State::mod_state(), Reason::atom()) -> ok.

%--- Types ---------------------------------------------------------------------

-export_type([state/0, mod_state/0]).

-opaque state() :: {Module::module(), Sub::map()}.
-type mod_state() :: map().
-type tx_error() :: atom(). % TODO add the correct atoms for the errors

%--- API -----------------------------------------------------------------------
-spec start(Module, PhyMod) -> State when
      PhyMod :: module(),
      Module :: module(),
      State  :: state().
start(Module, PhyMod) ->
    {Module, Module:init(PhyMod)}.

-spec transmit(State, Frame, MacMinBE, MacMaxCSMABackoffs, CW0, TxOptions) -> {ok, NewState} | {error, NewState, Error} when
      State              :: state(),
      Frame              :: bitstring(),
      MacMinBE           :: pos_integer(), % Custom type later ?
      MacMaxCSMABackoffs :: pos_integer(), % Custom type later ?
      CW0                :: pos_integer(), % Custom type later ?
      TxOptions          :: #tx_opts{}, % TODO create an opaque type
      NewState           :: state(),
      Error              :: tx_error().
transmit({Module, Sub}, Frame, MacMinBE, MacMaxCSMABackoffs, CW0, TxOptions) ->
    case Module:tx(Sub, Frame, MacMinBE, MacMaxCSMABackoffs, CW0, TxOptions) of
        {ok, Sub2} -> {ok, {Module, Sub2}};
        {error, Sub2, Error} -> {error, {Module, Sub2}, Error}
    end.

-spec stop(State, Reason) -> ok when
      State  :: state(),
      Reason :: atom().
stop({Module, Sub}, Reason) ->
    Module:terminate(Sub, Reason).
