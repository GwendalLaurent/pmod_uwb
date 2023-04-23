% @doc robot public API.
-module(robot).

-behavior(application).

-export([transmit/1]).
-export([blink_all/0]).
% Callbacks
-export([start/2]).
-export([stop/1]).


%--- API -----------------------------------------------------------------------

transmit(TxData) when is_list(TxData) ->
    transmit(list_to_binary(TxData));
transmit(TxData) when is_bitstring(TxData) -> 
    Frame = <<16#C5, 16#00, TxData/bitstring, 16#00, 16#00>>,
    FrameLength = byte_size(Frame), 
    pmod_uwb:write_tx_data(Frame),
    pmod_uwb:write(tx_fctrl, #{txboffs => 2#0, tr => 2#0, tflen => FrameLength}),
    pmod_uwb:write(sys_ctrl, #{txstrt => 2#1}).

blink_all() -> 
    pmod_uwb:write(pmsc, #{pmsc_ledc => #{blnknow => 2#1111}}),
    pmod_uwb:write(pmsc, #{pmsc_ledc => #{blnken => 2#1}}).

%--- Callbacks -----------------------------------------------------------------

% @private
start(_Type, _Args) -> 
    {ok, Supervisor} = robot_sup:start_link(),
    grisp:add_device(spi2, pmod_uwb),
    % Res = pmod_uwb:read(dev_id),
    {ok, Supervisor}.

% @private
stop(_State) -> ok.
