#!/bin/sh

set -eu

jq -n --rawfile x "$1" --argjson top 1 -f day01.jq
jq -n --rawfile x "$1" --argjson top 3 -f day01.jq
