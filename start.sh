#!/bin/bash

LOCAL_IP=`ifconfig -a|grep inet|grep -v 127.0.0.1|grep -v inet6|awk '{print $2}'|tr -d "addr:"`

# 不能换行
NODE_ID=1 IP=$LOCAL_IP ./_build/default/rel/asiastar/bin/asiastar start