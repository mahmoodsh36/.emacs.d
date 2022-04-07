#!/bin/bash

HERE=$(dirname $(readlink -f $0))
SCREEN_SIZE=${SCREEN_SIZE:-1366x768}
XDISPLAY=${XDISPLAY:-:1}
LOG_LEVEL=${LOG_LEVEL:-INFO}
if [[ -z $PYTHON ]]; then
        PYTHON=python
fi

Xephyr +extension RANDR -screen ${SCREEN_SIZE} ${XDISPLAY} -ac &
XEPHYR_PID=$!
(
  sleep 1
    env DISPLAY=${XDISPLAY} ${PYTHON} /usr/bin/qtile -l ${LOG_LEVEL} $@ &
      QTILE_PID=$!
        env DISPLAY=${XDISPLAY} xterm &
          wait $QTILE_PID
            kill $XEPHYR_PID
        )
