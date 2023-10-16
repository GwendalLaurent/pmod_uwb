PMOD UWB
=====

This repository contains the implementation of the pmod uwb's driver as well as an implementation of the MAC layer and a few examples.
The whole implementation was done during my master thesis at UCLouvain.

It also contains the implementation of a IEEE 802.15.4 stack. This job is still in progress.

Structure of the repository
-----

* doc: documentation of the pmod generated with edocs
* examples: contains some examples of applications used in the thesis.
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