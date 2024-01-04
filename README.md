PMOD UWB
=====

This repository contains the implementation of the pmod uwb's driver as well as an implementation of the MAC layer and a few examples.
The whole implementation was done during my master thesis at UCLouvain.

It also contains the implementation of a IEEE 802.15.4 stack. This job is still in progress.

Structure of the repository
-----

* doc: documentation of the pmod generated with edocs
* examples: contains some examples of applications used in the thesis. (the examples are not up to date)
    * `ack_no_jitter`: This application can perform an exchange of a specified number of a specified size between 2 devices. At the end of a run, the *sender* will display the statistics of the exchange
    * `ack_jitter`: This application will perform the same operation as `ack_no_jitter` but will introduce articifial jitter in the network.
    * `ack_fast_tx`: This application also perform an exchange of a specified number of frame. But here, the same hardcoded frame is sent every time. The goal of this application is to test the limit of the driver.
    * `ss_twr`: This application performs a single-sided two-way ranging between two devices. Keep in mind that in the current configuration the results are too unprecise to be used in a RTLS application.
    * `ds_twr`: This application performs a double-sided two-way ranging between two devices.
* exploring: notes made during litterature exploration (no use)
* measurements: contains the python script used to draw some graphs for the two-way ranging measurements + the csv containing the measurements
* src: contains the source code of the driver
* test: contains the unit tests used for the MAC layer
* mcd.sh: the bash script I use to deploy the applications on my SD card. (Works on Manjaro, not tested for other OS)

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
    {ok, Supervisor} = robot_sup:start_link(),
    grisp:add_device(spi2, pmod_uwb), % Starts the driver for the pmod
    ieee802154:start_link(#{mac_layer => mac_layer}), % Create the IEEE 802.15.4 stack
    {ok, Supervisor}.
```

Simple transmission function
```erlang
tx() ->
    FrameControl = #frame_control{}, % default frame control
    MacHeader = #mac_header{}, % default frame control
    ieee802154:transmition(FrameControl, MacHeader, <<"Test">>). % 
```

Simple reception function
```erlang
rx() ->
    ieee802154:reception().
```

IEEE 802.15.4 stack structure
-----------------------------
<p align="center">
<img src="./doc/images/ieee802154_stack.png" alt="IEEE stack">
</p>

The diagram displays the structure of the IEEE 802.15.4 stack.
The blue blocks are modules that can be changed to create a new behaviour.

The following subsections gives an indication of the roles and responsabilities of the different modules of the stack

### ieee802154.erl

* <u>Role</u>: Entry point for the API of the IEEE 802.15.4 stack
* <u>Responsabilities</u>: Keep the state of all the child modules in memory

Interaction with the IEEE 802.15.4 stack has to be done through this module.

### gen_mac_layer.erl

This is a custom behaviour module.

* <u>Role</u>: 
    * Encoding and decoding of the MAC frames (using the module `mac_frames.erl`)
    * Managing the *PIB* of the mac sublayer
    * Implementing the *MLME* and *MPDU* primitives

Modules implementing the behaviour:
* `mac_layer.erl`: The module that has to be used in normal working conditions
* `mock_mac.erl`: Mock module used to test the API of the IEEE 802.15.4 stack

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