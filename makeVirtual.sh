#!\bin\bash

modprobe dummy
ip link add rvl0 type dummy
ifconfig rvl0 up

route add -net 10.0.0.0 netmask 255.255.255.0 dev rvl0
