# gawk -f day02.awk < input02.txt

function todo() { print "todo: " $0; exit 1; }

# printf "%s\n" "/"{A,B,C}" "{X,Y,Z}"/ { todo(); }"
/A X/ { todo(); }
/A Y/ { todo(); }
/A Z/ { todo(); }
/B X/ { todo(); }
/B Y/ { todo(); }
/B Z/ { todo(); }
/C X/ { todo(); }
/C Y/ { todo(); }
/C Z/ { todo(); }
