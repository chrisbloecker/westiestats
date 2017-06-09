#!/bin/bash

# remove the possibly still existing lock file for the database
rm -f /westiestats/state/Database/open.lock

# run westiestats, but switch parallel garbage collection off
/westiestats/westiestats +RTS -qg
