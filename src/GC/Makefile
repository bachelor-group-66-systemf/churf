mkfile_path = $(abspath $(lastword $(MAKEFILE_LIST)))
current_dir = $(notdir $(patsubst %/,%,$(dir $(mkfile_path))))
CC 			= clang++

PWD_V 		= /Users/valtermiari/Desktop/DV/Bachelors/code/language/src/GC/include
LIB_INCL 	= -I/home/virre/dev/systemF/org/language/src/GC/include
LIB_SO 		= -L/home/virre/dev/systemF/org/language/src/GC/lib/libheap.so

CFLAGS 		= -Wall -Wextra -v -g -std=gnu++20 -stdlib=libc++ -I
VGFLAGS 	= --leak-check=full --show-leak-kinds=all
STDFLAGS 	= -std=gnu++20 -stdlib=libc++
WFLAGS 		= -Wall -Wextra
DBGFLAGS 	= -g

heap:
	$(CC) $(WFLAGS) $(STDFLAGS) $(LIB_INCL) lib/heap.cpp

h_test:
	rm -f tests/h_test.out
	$(CC) $(WFLAGS) $(STDFLAGS) $(LIB_INCL) tests/h_test.cpp lib/heap.cpp -o tests/h_test.out

h_test_vg:
	make h_test
	valgrind $(VGFLAGS) tests/h_test.out

h_test_dbg:
	make h_test
	lldb tests/h_test.out launch

linker:
	rm -f tests/linker.out
	$(CC) $(WFLAGS) $(STDFLAGS) $(LIB_INCL) tests/linker.cpp lib/heap.cpp -o tests/linker.out

linker_vg:
	make linker
	valgrind $(VGFLAGS) tests/linker.out

extern_lib:
	rm -f lib/heap.o lib/libheap.so tests/extern_lib.out
	$(CC) $(STDFLAGS) -c -fPIC -o lib/heap.o lib/heap.cpp
	$(CC) $(STDFLAGS) -shared -o lib/libheap.so lib/heap.o
	$(CC) $(LIB_INCL) $(LIB_SO) -v -Wall -o tests/extern_lib.out tests/extern_lib.cpp