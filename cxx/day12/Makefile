CXXFLAGS = -std=c++23

all: day12 example12.dot example12.dot.png

example12.dot: day12 example12.txt
	./day12 example12.txt > example12.dot

example12.dot.png: example12.dot
	dot -O -Tpng example12.dot
