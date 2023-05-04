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

h_test: static_lib
	rm -f tests/h_test.out
#	$(CC) $(WFLAGS) $(STDFLAGS) $(LIB_INCL) tests/h_test.cpp lib/heap.cpp lib/profiler.cpp lib/event.cpp -o tests/h_test.out
	$(CC) $(STDFLAGS) $(WFLAGS) $(LIB_INCL) -O3 -g -o tests/h_test.out tests/h_test.cpp lib/gcoll.a

h_test_vg: h_test
	valgrind $(VGFLAGS) tests/h_test.out

h_test_dbg: h_test
	lldb tests/h_test.out launch

linker:
	rm -f tests/linker.out
	$(CC) $(WFLAGS) $(STDFLAGS) $(LIB_INCL) tests/linker.cpp lib/heap.cpp -o tests/linker.out

linker_vg: linker
	valgrind $(VGFLAGS) tests/linker.out

game:
	rm -f tests/game.out
	$(CC) $(WFLAGS) $(STDFLAGS) $(LIB_INCL) tests/game.cpp lib/heap.cpp lib/profiler.cpp lib/event.cpp -o tests/game.out	

wrapper_test:
	rm -f lib/event.o lib/profiler.o lib/heap.o lib/coll.a tests/wrapper_test.out
# compile object files
	$(CC) $(STDFLAGS) $(WFLAGS) $(LIB_INCL) -g -c -o lib/event.o lib/event.cpp -fPIC
	$(CC) $(STDFLAGS) $(WFLAGS) $(LIB_INCL) -g -c -o lib/profiler.o lib/profiler.cpp -fPIC
	$(CC) $(STDFLAGS) $(WFLAGS) $(LIB_INCL) -g -c -o lib/heap.o lib/heap.cpp -fPIC
	$(CC) $(STDFLAGS) $(WFLAGS) $(LIB_INCL) -g -c -o lib/cheap.o lib/cheap.cpp -fPIC
# compile object files into library
	ar rcs lib/gcoll.a lib/event.o lib/profiler.o lib/heap.o lib/cheap.o
	clang -stdlib=libc++ $(WFLAGS) $(LIB_INCL) -o tests/wrapper_test.out tests/wrapper_test.c lib/gcoll.a -lstdc++

extern_lib:
# remove old files
	rm -f lib/heap.o lib/libheap.so tests/extern_lib.out
# compile heap to object file
	$(CC) $(STDFLAGS) -c -fPIC -o lib/heap.o lib/heap.cpp

	$(CC) $(STDFLAGS) -shared -o lib/libheap.so lib/heap.o
	$(CC) $(STDFLAGS) $(WFLAGS) $(LIB_INCL) -v tests/extern_lib.cpp lib/heap.cpp -o tests/extern_lib.out
	$(CC) $(STDFLAGS) $(LIB_INCL) $(LIB_SO) -v -Wall -o tests/extern_lib.out tests/extern_lib.cpp -lheap
	LD_LIBRARY_PATH=$(LIB_LINK) tests/extern_lib.out

static_lib:
# remove old files
	rm -f lib/event.o lib/profiler.o lib/heap.o lib/gcoll.a tests/extern_lib.out
# compile object files
	$(CC) $(STDFLAGS) $(WFLAGS) $(LIB_INCL) -c -o lib/event.o lib/event.cpp -fPIC
	$(CC) $(STDFLAGS) $(WFLAGS) $(LIB_INCL) -c -o lib/profiler.o lib/profiler.cpp -fPIC
	$(CC) $(STDFLAGS) $(WFLAGS) $(LIB_INCL) -c -o lib/heap.o lib/heap.cpp -fPIC
# create static library
	ar r lib/gcoll.a lib/event.o lib/profiler.o lib/heap.o

# create test program
static_lib_test: static_lib
	$(CC) $(STDFLAGS) $(WFLAGS) $(LIB_INCL) -o tests/extern_lib.out tests/extern_lib.cpp lib/gcoll.a

<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
static_lib_c:
# remove old files
	rm -f lib/event.o lib/profiler.o lib/heap.o lib/gcoll.a tests/extern_lib.out
# compile object files
=======
wrapper: 
	rm -f lib/event.o lib/profiler.o lib/heap.o lib/coll.a tests/wrapper.out
>>>>>>> 12816ea (Wrapper works)
	$(CC) $(STDFLAGS) $(WFLAGS) $(LIB_INCL) -O3 -g -c -o lib/event.o lib/event.cpp -fPIC
	$(CC) $(STDFLAGS) $(WFLAGS) $(LIB_INCL) -O3 -g -c -o lib/profiler.o lib/profiler.cpp -fPIC
	$(CC) $(STDFLAGS) $(WFLAGS) $(LIB_INCL) -O3 -g -c -o lib/heap.o lib/heap.cpp -fPIC
	$(CC) $(STDFLAGS) $(WFLAGS) $(LIB_INCL) -O3 -g -c -o lib/cheap.o lib/cheap.cpp -fPIC
<<<<<<< HEAD
# create static library
	ar r lib/gcoll.a lib/event.o lib/profiler.o lib/heap.o
=======
=======
=======
alloc_free_list: static_lib
	$(CC) $(STDFLAGS) $(WFLAGS) $(LIB_INCL) -o tests/alloc_fl.out tests/alloc_free_list.cpp lib/gcoll.a

linked_list_test: static_lib
	$(CC) $(STDFLAGS) $(WFLAGS) $(LIB_INCL) -o tests/linkedlist.out tests/linkedlist.cpp lib/gcoll.a

<<<<<<< HEAD
>>>>>>> d7ea27e (testing testing...)
=======
revrange: static_lib
	$(CC) $(STDFLAGS) $(WFLAGS) $(LIB_INCL) -o tests/revrange.out tests/revrange.cpp lib/gcoll.a

pointers: static_lib
	$(CC) $(STDFLAGS) $(WFLAGS) $(LIB_INCL) -o tests/pointers.out tests/pointers.cpp lib/gcoll.a

>>>>>>> a910d54 (rip gc)
wrapper:
# remove old files 
	rm -f lib/event.o lib/profiler.o lib/heap.o lib/coll.a tests/wrapper.out
# compile object files
	$(CC) $(STDFLAGS) $(WFLAGS) $(LIB_INCL) -O3 -c -o lib/event.o lib/event.cpp -fPIC
	$(CC) $(STDFLAGS) $(WFLAGS) $(LIB_INCL) -O3 -c -o lib/profiler.o lib/profiler.cpp -fPIC
	$(CC) $(STDFLAGS) $(WFLAGS) $(LIB_INCL) -O3 -c -o lib/heap.o lib/heap.cpp -fPIC
	$(CC) $(STDFLAGS) $(WFLAGS) $(LIB_INCL) -O3 -c -o lib/cheap.o lib/cheap.cpp -fPIC
# compile object files into library
>>>>>>> 01c93a6 (Wrapper docs)
	ar rcs lib/gcoll.a lib/event.o lib/profiler.o lib/heap.o lib/cheap.o
# compile test program wrapper.c with normal clang
	clang -stdlib=libc++ $(WFLAGS) $(LIB_INCL) -o tests/wrapper.out tests/wrapper.c lib/gcoll.a -lstdc++
>>>>>>> 12816ea (Wrapper works)
