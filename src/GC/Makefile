mkfile_path = $(abspath $(lastword $(MAKEFILE_LIST)))
current_dir = $(notdir $(patsubst %/,%,$(dir $(mkfile_path))))
CC = clang++
PWD = /home/virre/dev/systemF/org/language/src/GC/include
CFLAGS = -Wall -Wextra -v -g -std=gnu++20 -stdlib=libc++ -I
VGFLAGS = --leak-check=full --show-leak-kinds=all

heap:
	$(CC) $(CFLAGS)$(PWD) lib/heap.cpp

h_test:
	rm -f tests/h_test.out
	$(CC) $(CFLAGS)$(PWD) tests/h_test.cpp lib/heap.cpp -o tests/h_test.out

h_test_vg:
	make h_test
	valgrind $(VGFLAGS) tests/h_test.out

linker:
	rm -f tests/linker.out
	$(CC) $(CFLAGS)$(PWD) tests/linker.cpp lib/heap.cpp -o tests/linker.out

linker_vg:
	make linker
	valgrind $(VGFLAGS) tests/linker.out