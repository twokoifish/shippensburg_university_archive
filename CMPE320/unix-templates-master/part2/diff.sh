#!/bin/bash

wget --quiet -O current.html http://www.ship.edu 2>&1 > /dev/null

DIFF=$(diff current.html previous.html)
if [ "$DIFF" != "" ]; then
  echo Different
fi

mv current.html previous.html

