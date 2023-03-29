#!/bin/bash

udisksctl mount -b /dev/sdc1

rebar3 compile

rebar3 grisp deploy