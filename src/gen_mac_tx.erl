%% @doc This generic module defines the behaviour for any module implementing the transmission of the IEEE 802.15.4 stack

-module(gen_mac_tx).

-export([start/2]).
-export([transmit/7]).
-export([stop/2]).

-include("pmod_uwb.hrl").

%--- Callbacks -----------------------------------------------------------------

-callback init(PhyMod::module()) -> State::term().
-callback tx(State::term(), MacMinBe:: Frame::bitstring(), MacMinBE::pos_integer(), MacMaxBE::non_neg_integer(), MacMaxCSMABackoffs::pos_integer(), CW0::pos_integer(), TxOptions::#tx_opts{}) -> {ok, Newstate::term()} | {error, Newstate::term(), Error::tx_error()}.
-callback terminate(State::term(), Reason::atom()) -> ok.

%--- Types ---------------------------------------------------------------------

-export_type([state/0]).

-opaque state() :: {Module::module(), Sub::term()}.
-type tx_error() :: atom(). % TODO add the correct atoms for the errors

%--- API -----------------------------------------------------------------------
-spec start(Module, PhyMod) -> State when
      PhyMod :: module(),
      Module :: module(),
      State  :: state().
start(Module, PhyMod) ->
    {Module, Module:init(PhyMod)}.

-spec transmit(State, Frame, MacMinBE, MacMaxBE, MacMaxCSMABackoffs, CW0, TxOptions) -> {ok, NewState} | {error, NewState, Error} when
      State              :: state(),
      Frame              :: bitstring(),
      MacMinBE           :: pos_integer(), % Custom type later ?
      MacMaxBE           :: non_neg_integer(),
      MacMaxCSMABackoffs :: pos_integer(), % Custom type later ?
      CW0                :: pos_integer(), % Custom type later ?
      TxOptions          :: #tx_opts{}, % TODO create an opaque type
      NewState           :: state(),
      Error              :: tx_error().
transmit({Module, Sub}, Frame, MacMinBE, MacMaxBE, MacMaxCSMABackoffs, CW0, TxOptions) ->
    case Module:tx(Sub, Frame, MacMinBE, MacMaxBE, MacMaxCSMABackoffs, CW0, TxOptions) of
        {ok, Sub2} -> {ok, {Module, Sub2}};
        {error, Sub2, Error} -> {error, {Module, Sub2}, Error}
    end.

-spec stop(State, Reason) -> ok when
      State  :: state(),
      Reason :: atom().
stop({Module, Sub}, Reason) ->
    Module:terminate(Sub, Reason).
