PMOD UWB
=====

This repository contains the implementation of the pmod uwb's driver as well as an implementation of the MAC layer and a few examples.
The whole implementation was done during my master thesis at UCLouvain.

It also contains the implementation of a IEEE 802.15.4 stack. This job is still in progress.

Structure of the repository
-----

* doc: documentation of the pmod generated with edocs
* docs: additional documentation
* examples: (outdated)
* measurements: contains the python script used to draw some graphs for the two-way ranging measurements + the csv containing the measurements
* src: contains the source code of the driver
* test: contains the tests
* mcd.sh: the bash script I use to deploy the applications on my SD card. (Only for MacOS)

Deploy the examples
-------------------

Before trying to deploy an example, you have to change the path of the SD card in the file `rebar.config` to match the path of your SD card. By default they are configured to be deployed on a SD card named *GRISP_SD* located at `/run/media/michel/`.
When this is done, you have to run `rebar3 compile` to build the application, and `rebar3 grisp deploy` to deploy the app on your SD card.

Each example contain an API function for the *sender* and a function for the *receiver*.

Run the tests
------
To run the tests, you can run the following command in your shell at the root of the project:
```
rebar3 eunit
```

Robot Example
-------------

```erlang
start(_Type, _Args) ->
    Callback = fun(Frame, LQI, Security, Ranging) ->
                    {FrameControl, MacHeader, Payload} = Frame,
                    io:format("~p~n", [Payload])
                end,
    {ok, Supervisor} = robot_sup:start_link(),
    grisp:add_device(spi2, pmod_uwb), % Starts the driver for the pmod
    ieee802154:start_link(#ieee_parameters{phy_layer = pmod_uwb, callback = Callback}), % Create the IEEE 802.15.4 stack
    {ok, Supervisor}.
```

Simple transmission function
```erlang
tx() ->
    FrameControl = #frame_control{}, % default frame control
    MacHeader = #mac_header{}, % default frame control
    {ok, _} = ieee802154:transmition(FrameControl, MacHeader, <<"Test">>).
```

To activate the continuous reception without enabling ranging:
```erlang
ieee802154:rx_on(?DISABLED).
```

Testing
-------
To run all the tests you have to run the following command:
```
rebar3 ct --sname=test
```
More informations about the tests can be found [here]("docs/tests.md")


Callback function
-----------------
At the reception of a frame, the IEEE 802.15.4 stack will call the specified callback function. More precisely, the process running the `ieee802154.erl` gen_server will call the callback.
For that reason, the callback can't directly call API function of `ieee802154.erl` module unless they are non-blocking.

IEEE 802.15.4 stack structure
-----------------------------
<p align="center">
<img src="./doc/images/ieee802154_stack.png" alt="IEEE stack">
</p>

The diagram displays the structure of the IEEE 802.15.4 stack.
The blue blocks are modules that can be changed to create a new behaviour.

The following subsections gives an indication of the roles and responsabilities of the different modules of the stack

### ieee802154.erl

* <u>Role</u>: 
    * Entry point for the API of the IEEE 802.15.4 stack
    * Encoding and decoding of the MAC frames (using the module `mac_frames.erl`)
    * Managing the *PIB* of the mac sublayer
    * Implementing the *MLME* and *MPDU* primitives
* <u>Responsabilities</u>: Keep the state of all the child modules in memory

Interaction with the IEEE 802.15.4 stack has to be done through this module.

### gen_duty_cycle

This is a custom behaviour module.

* <u>Role</u>: Manage one the duty cycling configuration defined by the mac sublayer for a PAN (i.e. non-beacon enabled PAN or beacon-enabled PAN)

Since the module has to manage the duty cycling, it has to verify if a frame can be transmitted in time (in the current superframe in the case of a beacon enabled PAN).

Additionally, It has to manage the ACK reception and the potential retransmission of frames using the process defined

Modules implementing the behaviour:
* `duty_cycle_non_beacon.erl`: This module implements the duty cycle when a non-beacon configuration is used.

### gen_mac_tx

This is a custom behaviour module.

* <u>Role</u>: Manage the transmission of a frame using one of the medium access algorithm defined by the IEEE 802.15.4 standard

Modules implementing the behaviour:
* `unslotted_csma.erl`: This module implements the unslotted CSMA algorithm used in a non-beacon enabled PAN
