#!/usr/bin/env bash

intersection() {
    a=$1
    b=$2

    for (( i=0; i<${#a}; i++ )); do
        ch=${a:$i:1}
        if [[ $b =~ $ch ]]; then
            if [[ ! $result =~ $ch ]]; then
                result+=$ch
            fi
        fi
    done

    echo "$result"
}

priority() {
    n=$(printf "%d" "'$1")
    case $1 in
        [a-z])
            echo "$(( 1 + n - $(printf "%d" "'a") ))";;
        [A-Z])
            echo "$(( 1 + 26 + n - $(printf "%d" "'A") ))";;
    esac
}

sum=0
while read -r line; do
    split=$(( ${#line} / 2 ))
    a=${line:0:$split}
    b=${line:$split}
    shared="$(intersection "$a" "$b")"
    prio="$(priority "$shared")"
    sum=$(( sum + prio ))
done
echo "$sum"
