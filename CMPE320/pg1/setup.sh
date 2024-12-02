#!/bin/bash

# this updates the APT repository list and ensures the correct versions
apt-get update

# this installs necessary packages used for running programs and tests.
apt-get install -y valgrind libjson-maybexs-perl libtext-diff-perl build-essential make zip a2ps

