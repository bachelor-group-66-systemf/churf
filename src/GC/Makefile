CC 			= clang++
CWD			= $(shell pwd)
LIB_INCL 	= -I$(CWD)/include
LIB_SO 		= -L$(CWD)/lib
LIB_LINK	= $(CWD)/lib
CFLAGS 		= -Wall -Wextra -v -g -std=gnu++20 -stdlib=libc++ -I
VGFLAGS 	= --leak-check=full --show-leak-kinds=all
STDFLAGS 	= -std=gnu++20 -stdlib=libc++
WFLAGS 		= -Wall -Wextra
DBGFLAGS 	= -g

advance:
	$(CC) $(WFLAGS) $(STDFLAGS) tests/advance.cpp -o tests/advance.out

file:
	$(CC) $(WFLAGS) $(STDFLAGS) tests/file.cpp -o tests/file.out

heap:
	$(CC) $(WFLAGS) $(STDFLAGS) $(LIB_INCL) lib/heap.cpp

h_test:
	rm -f tests/h_test.out
	$(CC) $(WFLAGS) $(STDFLAGS) $(LIB_INCL) tests/h_test.cpp lib/heap.cpp lib/profiler.cpp lib/event.cpp -o tests/h_test.out

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

game:
	rm -f tests/game.out
	$(CC) $(WFLAGS) $(STDFLAGS) $(LIB_INCL) tests/game.cpp lib/heap.cpp lib/profiler.cpp lib/event.cpp -o tests/game.out	

extern_lib:
	rm -f lib/heap.o lib/libheap.so tests/extern_lib.out
	$(CC) $(STDFLAGS) -c -fPIC -o lib/heap.o lib/heap.cpp
	$(CC) $(STDFLAGS) -shared -o lib/libheap.so lib/heap.o
	$(CC) $(STDFLAGS) $(WFLAGS) $(LIB_INCL) -v tests/extern_lib.cpp lib/heap.cpp -o tests/extern_lib.out
	$(CC) $(STDFLAGS) $(LIB_INCL) $(LIB_SO) -v -Wall -o tests/extern_lib.out tests/extern_lib.cpp -lheap
	LD_LIBRARY_PATH=$(LIB_LINK) tests/extern_lib.out