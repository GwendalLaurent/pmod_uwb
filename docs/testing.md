# Testing
The tests are done using [Common test](https://erlang.org/documentation/doc-10.3/lib/common_test-1.17/doc/html/common_test.html)

To run the test you can use the following command:

```
rebar3 ct --sname=test
```

## Physical layer simulation
Some test might require that multiple nodes communicate with each other. Such scenarios require a special setup to run it.
1. You must setup a node acting as the simulated physical UWB. Such node can be started with `boot_network_node/{0,1}`
2. You must start an IEEE 802.15.4 stack on each working node using `boot_ieee802154_node/{5,6}`

### Unreliable Mode
The simulated physical network can have two modes:
1. Perfect mode: All transmission will be without any loss. It's the default behaviour.
2. Unreliable mode: Some transmission will have loss.

To activate the unreliable mode, you must start the node hosting the simulated physical UWB with the following line:
```erlang
ieee802154_node:boot_network_node(#{loss => true})
```

It's important to note that the unreliability of the physical layer is deterministic. The physical layer will block a frame sent by a node exactly once

If we have an exchange from node A to node B and we have a sequence of frames [#1, #2, ..., #n].
At the first iteration, frame #1 will be dropped and all frames from #2 to #n will be transmitted.
At the ith iteration, frame #i will be dropped and all the other frames [#1, ..., #i-1, #i+1, ..., #n] will be transmitted.

If we concider the exchange between node A and node B with the following sequence [#1AB, #1BA, #2AB, #2BA, ..., #nAB, #nBA]
The mock-up will differentiate frames from A to B and frames from B to A  and will block only the first unseen frame.
For example, if the mock-up already blocked #1AB and #2AB, then in the following sequences #1BA will be blocked:
* [1#BA, 1#AB, #2AB]
* [1#AB, #1BA, #2AB]
* [1#AB, 2#AB, #1BA]

Such observation can also be made with n nodes.

### Create test with unreliability
To test all possibilities, you have to find the total number of frame that should be exchanged (let's call that number n)
Then you have to repeat the execution of a particular scenario n times (or n+1 times if you want to test without any loss as well)

Common test allows you to repeat the execution of a specific group of test cases. Thus a test structure can resemble like this:
```erlang
groups() -> [
    {main_group, [sequential], repeated_group},
    {repeated_group, [parallel, {repeat, n}], [testcase1, testcase2]}
].
```
The `main_group` is there to keep the same simulated physical layer (with it's state) during the repetition of the `repeated_group`.
Without it, the simulated physical layer would be stopped and restarted at each iteration. 
