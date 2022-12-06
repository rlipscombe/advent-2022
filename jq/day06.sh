#!/bin/sh

set -eu

jq -n --arg input "mjqjpqmgbljsphdztnvjfqwrcgsmlb" --argjson len 4 -f day06.jq
jq -n --arg input "bvwbjplbgvbhsrlpgdmjqwftvncz" --argjson len 4 -f day06.jq
jq -n --arg input "nppdvjthqldpwncqszvftbrmjlhg" --argjson len 4 -f day06.jq
jq -n --arg input "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" --argjson len 4 -f day06.jq
jq -n --arg input "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" --argjson len 4 -f day06.jq

jq -n --rawfile input ../input06.txt --argjson len 4 -f day06.jq

jq -n --arg input "mjqjpqmgbljsphdztnvjfqwrcgsmlb" --argjson len 14 -f day06.jq
jq -n --arg input "bvwbjplbgvbhsrlpgdmjqwftvncz" --argjson len 14 -f day06.jq
jq -n --arg input "nppdvjthqldpwncqszvftbrmjlhg" --argjson len 14 -f day06.jq
jq -n --arg input "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" --argjson len 14 -f day06.jq
jq -n --arg input "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" --argjson len 14 -f day06.jq

jq -n --rawfile input ../input06.txt --argjson len 14 -f day06.jq
