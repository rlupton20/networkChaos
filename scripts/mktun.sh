#!/bin/bash

# Command line arguments:
# $1 is the name of the new device
# $2 is the address to assign to the machine, and network

ip tuntap add dev $1 mode tun user richard
ip link set $1 up
ip addr add $2 dev $1
