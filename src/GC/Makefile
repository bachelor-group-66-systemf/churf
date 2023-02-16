CC = clang++
PWD = /home/virre/dev/systemF/org/language/src/GC/include
CFLAGS = -Wall -Wextra -v -g -std=gnu++20 -stdlib=libc++ -I

heap:
	$(CC) $(CFLAGS)$(PWD) lib/heap.cpp

h_test:
	rm -f tests/h_test.out
	$(CC) $(CFLAGS)$(PWD) tests/h_test.cpp lib/heap.cpp -o tests/h_test.out

linker:
	rm -f tests/linker.out
	$(CC) $(CFLAGS)$(PWD) tests/linker.cpp lib/heap.cpp -o tests/linker.out