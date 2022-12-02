# gawk -f day02.awk < input02.txt

# printf "%s\n" "/"{A,B,C}" "{X,Y,Z}"/ { todo(); }"
/A X/ { part1 += 3 + 1; part2 += 3 + 0; }
/A Y/ { part1 += 6 + 2; part2 += 1 + 3; }
/A Z/ { part1 += 0 + 3; part2 += 2 + 6; }
/B X/ { part1 += 0 + 1; part2 += 1 + 0; }
/B Y/ { part1 += 3 + 2; part2 += 2 + 3; }
/B Z/ { part1 += 6 + 3; part2 += 3 + 6; }
/C X/ { part1 += 6 + 1; part2 += 2 + 0; }
/C Y/ { part1 += 0 + 2; part2 += 3 + 3; }
/C Z/ { part1 += 3 + 3; part2 += 1 + 6; }

END {
	print part1;
	print part2;
}
