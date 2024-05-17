-module(robot).

-behaviour(application).

-include("mac_frame.hrl").
-include("ieee802154.hrl").

% Callbacks
-export([start/2]).
-export([stop/1]).


%--- Macros --------------------------------------------------------------------
-define(JAMMING_DATA, <<"JAMMING">>).
-define(DATALENGTH, byte_size(?JAMMING_DATA)).

-define(BENCHMARK_DATA, <<16#F:(111*8)>>).
-define(BENCHMARK_DATA_LENGTH, bit_size(?BENCHMARK_DATA)).

-define(PANID, <<16#CAFE:16>>).
-define(SENDER_ADDR, <<16#0001:16>>).
-define(RECEIVER_ADDR, <<16#0002:16>>).

-define(CCA_DURATION, 283).

-define(TX_ANTD, 16450).
-define(RX_ANTD, 16450).

%--- API -----------------------------------------------------------------------

start(_Type, _Args) ->
    {ok, Supervisor} = robot_sup:start_link(),
    grisp:add_device(spi2, pmod_uwb),

    ieee802154:start_link(
      #ieee_parameters{duty_cycle = duty_cycle_non_beacon,
                       input_callback = fun(_, _, _, _) ->
                                                logger:notice("Received smth")
                                        end}
     ),

    {ok, Supervisor}.

% @private
stop(_State) -> ok.
