def sum: reduce .[] as $it (0; . + ($it | tonumber));

$x | rtrimstr("\n") | split("\n\n")
    | reduce .[] as $p ([]; . + [$p | split("\n") | sum])
    | sort | reverse | .[:$top] | sum
