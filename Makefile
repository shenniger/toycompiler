all: test_llvm test_c chdrconv

CC ?= gcc
CXX ?= g++
LLVM_CONFIG ?= llvm-config

MODE ?= -g -O0

%.o: %.c prg.h
	$(CC) -c -Wall -Wextra -pedantic -std=c11 \
		-Wno-int-to-pointer-cast -Wno-pointer-to-int-cast $< -o $@ $(MODE)

back_llvm.o: back_llvm.cpp prg.h 
	$(CXX) -c -Wall -Wextra -pedantic -std=c++14 -o back_llvm.o back_llvm.cpp \
		$(shell $(LLVM_CONFIG) --cxxflags) $(MODE) \
		-Wno-int-to-pointer-cast -fno-rtti

test_c: main.o reader.o parser.o middle.o back_c.o
	$(CC) $^ -o $@ -lm $(MODE)

test_llvm: main.o reader.o parser.o middle.o back_llvm.o
	$(CXX) $^ -o $@ -lm -fno-rtti \
		$(shell $(LLVM_CONFIG) --ldflags --system-libs --libs core) $(MODE) -lncurses

chdrconv.o: chdrconv.c
	$(CC) -c -Wall -Wextra -pedantic -std=c99 \
		-Wno-int-to-pointer-cast -Wno-pointer-to-int-cast $< -o $@ $(MODE) \
		-I /usr/lib/llvm-3.9/include

chdrconv: chdrconv.o
	$(CC) $^ -o $@ $(MODE) -L /usr/lib/llvm-3.9/lib -lclang

.PHONY: report clean
report:
	@echo Using C compiler: $(CC)
	@echo Using C++/LLVM compiler: $(CXX)
	@echo Using LLVM version: $(shell $(LLVM_CONFIG) --version)
	@echo Debug/Release flags: $(MODE)

clean:
	rm *.o || true
	rm *.dwo || true
	rm test_llvm || true
	rm test_c || true
