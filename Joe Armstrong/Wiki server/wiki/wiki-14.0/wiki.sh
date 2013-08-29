#!/bin/sh
 
##
## usage wiki.sh {start|stop|debug}
##
 
## These point to where
## the wiki code and database is stored
##   PA1   = path to the wiki program
##   PA2   = path to the pico server
##   STORE = path to the wiki store
##   PORT  = port to run as
 
WIKI=$HOME/services/wiki.sics/wiki-14.0
PICO=$HOME/services/wiki.sics/pico-11.0
#PICO=$HOME/erl/erl.now/pico-11.0
STORE=$WIKI/store
PORT=4999
ERL=/home/joe/installed/otp_src_R8B-0/bin/erl
HOSTNAME=`hostname`
export HEART_COMMAND="$WIKI/wiki.sh start"

case $1 in
  start)
    ${ERL} -boot start_sasl -sname wiki001 -pa ${WIKI} -pa ${PICO} \
           -s wiki start ${PORT} ${STORE}  -heart -detached
    echo  "Starting Wiki"
    ;;
 
  debug)
    ${ERL} -sname $2 -pa ${WIKI} -pa ${PICO} -s wiki start ${PORT} ${STORE}
    ;;
 
  stop)
    ${ERL} -noshell -sname wiki_stopper -pa ${WIKI} -pa {PICO} \
           -s wiki stop wiki001@${HOSTNAME}
    echo "Stopping wiki"
    ;;
 
  *)
    echo "Usage: $0 {start|stop|debug}"
    exit 1
esac
 
exit 0
                    
