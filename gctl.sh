#!/bin/bash

SCRIPT=./_build/default/rel/asiastar/bin/asiastar
IP=`ifconfig eth0|grep inet|grep -v 127.0.0.1|grep -v inet6|awk '{print $2}'|tr -d "addr:"`

#ctl_status() {
#  ID=$1
#  NODE_ID=$ID IP=$IP $SCRIPT status
#}
#
#ctl_pid() {
#  ID=$1
#  NODE_ID=$ID IP=$IP $SCRIPT pid
#}

ctl_start() {
    ID=$1
    STATUS=$(ctl_status $ID)
    if [ "$STATUS" == '' ] ; then
        echo ">>> Server $ID is running, pid: $(ctl_pid)"
        exit 1
    else
        NODE_ID=$ID IP=$IP $SCRIPT start
    fi
}

#ctl_stop() {
#  ID=$1
#  NODE_ID=$ID IP=$IP $SCRIPT stop
#}

# Check the first argument for instructions
case "$1" in
    start)
        shift
        ctl_start
        ;;
#
#    stop)
#		shift
#		ctl_stop
#        ;;
#
#    restart)
#        ;;
#
#    pid)
#        ;;
#
#    ping)
#        ;;
#
#    remote_console)
#        ;;
#
#    upgrade|downgrade|install|unpack|uninstall)
#        ;;
#
#    versions)
#        ;;
#
#    rpc)
#        ;;
#
#    rpcterms)
#        ;;
#
#    eval)
#        ;;
#
#    status)
#        ;;
#
    help)
        cat help.txt
        exit 1
        ;;
    *)
        cat help.txt
        exit 1
        ;;
esac

exit 0
