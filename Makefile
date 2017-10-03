#all:
#	gcc front.c -o front.o -c -g
#	gcc main.c -o main.o -c -g
#	gcc back.c -o back.o -c -g
#	g++ -std=c++14 -pedantic -Wall -Wextra back_llvm.cpp -o test -g `llvm-config-3.8 --cxxflags --ldflags` main.o front.o back.o -lLLVM -g

all:
	gcc -Wall -Wextra -pedantic -std=c99 -g *.c -o test -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast
