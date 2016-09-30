#!/bin/bash

# remove the possibly still existing lock file for the database
rm -f /headjudge/state/Database/open.lock

/westiestats/westiestats
