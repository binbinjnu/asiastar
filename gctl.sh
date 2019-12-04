#!/bin/bash

SCRIPT=./_build/default/rel/asiastar/bin/asiastar
IP=`ifconfig eth0|grep inet|grep -v 127.0.0.1|grep -v inet6|awk '{print $2}'|tr -d "addr:"`

ctl_status() {
    ID=$1
    NODE_ID=$ID IP=$IP $SCRIPT status
}

ctl_pid() {
    ID=$1
    NODE_ID=$ID IP=$IP $SCRIPT pid
}

ctl_ping() {
    ID=$1
    NODE_ID=$ID IP=$IP $SCRIPT ping
}

ctl_start() {
    ID=$1
    STATUS=$(ctl_status $ID)
    echo $ID
    echo $STATUS
    if [ "$STATUS" == '' ] ; then
        echo ">>> Node $ID is running, pid: $(ctl_pid $ID)"
        exit 1
    else
        NODE_ID=$ID IP=$IP $SCRIPT start
    fi
}

ctl_stop() {
    ID=$1
    NODE_ID=$ID IP=$IP $SCRIPT stop
}

ctl_remote_console() {
    ID=$1
    NODE_ID=$ID IP=$IP $SCRIPT remote_console
}

ctl_versions() {
    ID=$1
    NODE_ID=$ID IP=$IP $SCRIPT versions
}

# Check the first argument for instructions
case "$1" in
    start)
        shift
        ctl_start $@
        ;;
    stop)
		shift
		ctl_stop $@
        ;;
    restart)
        ;;
    pid)
        shift
        ctl_pid $@
        ;;
    ping)
        shift 
        ctl_ping $@
        ;;
    remote_console)
        shift
        ctl_remote_console $@
        ;;
    upgrade|downgrade|install|unpack|uninstall)
        ;;
    versions)
        shift
        ctl_versions $@
        ;;
    rpc)
        ;;
    rpcterms)
        ;;
    eval)
        ;;
    status)
        ;;
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
