#!/bin/bash

# remove the possibly still existing lock file for the database
rm -f /westiestats/state/Database/open.lock

/westiestats/westiestats
