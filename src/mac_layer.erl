-module(mac_layer).
-behaviour(gen_mac_layer).

-include("mac_frame.hrl").
-include("gen_mac_layer.hrl").

%% This module implmements the gen_mac_layer behaviour.
%% This file can be taken as an example when you want to build a mock-up for the MAC layer

%%% gen_mac_layer callbacks
-export([init/1]).
-export([tx/4]).
-export([rx/2]).
-export([rx_on/2]).
-export([rx_off/1]).        
-export([terminate/2]).

%--- mac_layer_behaviour callback functions ---------------------------------------------

% @doc Initialize the MAC layer
% 
% By default, it activate fthe frame filtering with auto-acknowledgment
% It also set the frame waiting time out to MACACKWAITDURATION. However, the timeout isn't activated
% If you want to use it, it has to be activated later on.
% 
% Params is a map that can contain different elements:
% * `phy_layer': specifying which driver it should use. By default it uses the driver of the pmod_uwb
% @end
-spec init(Params::term()) -> State :: term().
init(Params) ->
    PhyModule = case Params of
              #{phy_layer := PHY} -> PHY;
              _ -> pmod_uwb
          end,
    PhyModule:write(rx_fwto, #{rxfwto => ?MACACKWAITDURATION}),
    % Enabling frame filtering & autoack feature
    PhyModule:write(sys_cfg, #{ffab => 1, ffad => 1, ffaa => 1, ffam => 1, ffen => 1, autoack => 1}),
    #{phy_layer => PhyModule, retries => 0}. 

% @doc transmits a frame using the physical layer
% 
% If the frame control specify that an ACK is requested (AR=1), the function will wait for the ACK before returning
% Moreover, if the ACK doesn't arrive before MACACKWAITDURATION, a retransmission is done
% After MACMAXFRAMERETRIES retransmission, if the ACK doesn't arrive, the function returns the error `no_ack'
% @end
tx(#{retries := ?MACMAXFRAMERETRIES} = State, Framecontrol, _, _) ->
    case Framecontrol#frame_control.frame_type of
        ?FTYPE_DATA -> {error, State#{retries => 0}, no_ack}; % This error should be triggered in a direct transmission 
        _ -> {error, State#{retries => 0}, mac_max_frame_retries} % this error is a place holder before other error cases appears
    end;
% @doc When the frame has AR=1, the Mac sublayer hes to wait until it receives the ACK back from the recipient before returning
tx(#{phy_layer := PhyModule, retries := Retries} = State, #frame_control{ack_req = ?ENABLED} = FrameControl, #mac_header{seqnum = Seqnum} = MacHeader, Payload) ->
    case PhyModule:transmit(mac_frame:encode(FrameControl, MacHeader, Payload), #tx_opts{wait4resp = ?ENABLED}) of
        ok -> 
            case rx(State, true) of
                {ok, NewState, {#frame_control{frame_type = ?FTYPE_ACK}, #mac_header{seqnum = Seqnum}, _}} -> {ok, NewState};
                _ -> tx(State#{retries => Retries+1}, FrameControl, MacHeader, Payload)
            end;
        Error -> {error, State, Error}
    end;
tx(#{phy_layer := PhyModule} = State, FrameControl, MacHeader, Payload) ->
    case PhyModule:transmit(mac_frame:encode(FrameControl, MacHeader, Payload), #tx_opts{}) of
        ok -> {ok, State};
        Error -> {error, State, Error}
    end.

% @doc Performs the reception of a single packet
% 
% @param State: The state of the module
% @param RxEnabled: Specifies if the reception has been enabled previously. In most cases this value should be `false'
% @end
rx(#{phy_layer := PhyModule} = State, RxEnabled) ->
    case PhyModule:reception(RxEnabled) of
        {_Length, Frame} -> {ok, State, mac_frame:decode(Frame)};
        Err -> {error, State, Err}
    end.

% @doc Turns on the continuous reception
% 
% When a packet is received, it will call the function specified in the parameter `Callback'
% If the received packet is a data packet requesting an ACL and the autoack is turned on (by default)
% the ACK is handled by the hardware before calling the callback
% 
% @end
rx_on(#{phy_layer := PhyModule} = State, Callback) ->
    PhyModule:write(sys_cfg, #{rxwtoe => 0}),
    LoopPid = spawn_link(fun() -> rx_loop(State, Callback) end), 
    {ok, State#{loop_pid => LoopPid}}.

% @doc Turns off the continuous reception
% @end
rx_off(#{phy_layer := PhyModule} = State) ->
    case State of
        #{loop_pid := LoopPid} -> unlink(LoopPid), exit(LoopPid, shutdown);
        _ -> ok
    end,
    PhyModule:disable_rx(),
    PhyModule:write(sys_cfg, #{rxwtoe => 1}),
    {ok, maps:remove(loop_pid, State)}.

% @doc clean up function to stop the mac layer
% @end
terminate(State, _Reason) -> 
    case State of
        #{loop_pid := LoopPid} -> unlink(LoopPid), exit(LoopPid, stop);
        _ -> ok
    end.

%--- Internal: RXloop ----------------------------------------------------------

% NOTE : ! If the loop crashes, there's no way to detect it
rx_loop(MacState, Callback) -> 
    case rx(MacState, false) of
        {ok, State, Frame} -> 
            Callback(Frame), 
            rx_loop(State, Callback);
        {error, State, _Err} -> rx_loop(State, Callback) % Log that an error occured ?
    end.
