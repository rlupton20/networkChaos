#!/bin/bash

ip tuntap add dev vgl0 mode tun user richard
ip link set vgl0 up
ip addr add 10.0.0.1/24 dev vgl0
