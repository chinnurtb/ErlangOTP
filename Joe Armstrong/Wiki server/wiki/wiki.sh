#!/bin/sh
 
##
## usage wiki.sh {start|stop|debug}
##
 
## You will have to edit the following three variables
##   $ROOT = path to the wiki code and store
##   $PORT = port to run as
##   $ERL  = location of erlang
 
ROOT=/home/joe/installed/wiki 
PORT=4992
ERL=/home/joe/installed/otp_src_R8B-2/bin/erl

WIKI=${ROOT}/wiki-14.0
PICO=${ROOT}/pico-11.0
STORE=${ROOT}/store
 
HOSTNAME=`hostname`
export HEART_COMMAND="$ROOT/wiki.sh start"

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
                    
