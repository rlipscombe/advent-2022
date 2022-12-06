$x | rtrimstr("\n") | split("\n\n")
    | reduce .[] as $p ([]; . + [$p | split("\n") | map(. | tonumber) | add])
    | sort | reverse | .[:$top] | add
