# gawk -f day02.awk < input02.txt

# printf "%s\n" "/"{A,B,C}" "{X,Y,Z}"/ { todo(); }"
/A X/ { part1 += 3 + 1; }
/A Y/ { part1 += 6 + 2; }
/A Z/ { part1 += 0 + 3; }
/B X/ { part1 += 0 + 1; }
/B Y/ { part1 += 3 + 2; }
/B Z/ { part1 += 6 + 3; }
/C X/ { part1 += 6 + 1; }
/C Y/ { part1 += 0 + 2; }
/C Z/ { part1 += 3 + 3; }

END { print part1; }
