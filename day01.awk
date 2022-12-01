# gawk -f day01.awk < input.txt

BEGIN { elf = 0; }
/[0-9]+/ { calories[elf] += $1; }
/^$/ { elf += 1; }
END {
	n = asort(calories, sorted);
	print sorted[n];
	print sorted[n-2] + sorted[n-1] + sorted[n];
}

