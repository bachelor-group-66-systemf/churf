# GC library - reference guide

The Heap class is the core of the library and contains all necessary
functions for using the library. This class exposes four public functions
which are `init`, `dispose`, `alloc`, and `set_profiler`.

To use the library, simply include it as `#include "heap.hpp"` and link
it during compilation. Or you can compile it to a static library using
the target `make static_lib` which compiles everything to an .a file.
It can also be compiled to a shared library if necessary with the target
`make shared_lib` which produces an .so file.

## Quick guide
1. If you want a profiler, call `Heap::set_profiler(true)`. Otherwise this can be skipped.
2. Call `Heap::init()` to initialize the heap before using `alloc` (**crucial**).
3. Use `Heap::alloc()` as you want.
4. At program exit, call `Heap::dispose()` to free up all the memory used.

## Functions

### Heap::init()
When using the library, the user has to, at the start of the program,
call the `void init()` function, which initiates the Heap singleton
and the class member `m_stack_top`. **It is crucial** that this
functions is called from the `main` function of the end program,
as `init` uses the intrinsic function `__builtin_frame_address`
to find the address of the **first** stack frame of the end program.
If the function **is not** called from the `main` function
of the end program, it is not guaranteed that the garbage collector
will collect all objects.

The intrinsic function used is technically unsafe for this use,
but during testing it has only shown to segfault for values greater
than the one used in `init`. If you run into a segfault, please
contact the developers.


### Heap::set_profiler(bool mode)
This function is used to enable or disable the profiler connected
to the Heap. The profiler is primarily used for testing, but can
also be used in general to keep track of the programs history.

This function takes a single boolean as an argument to represent
the state of the profiler. `true` means that the profiler is enabled
and `false` means that the profiler is disabled. This function
can theoretically be called at any time during program execution,
but it's probably a bad idea. It is recommended to call this function
before the call to `init` or at least at before the first call to
`alloc`.

### Heap::alloc(size_t size)
The probably most important function in this library. This function
is called to request memory from the "heap". `alloc` takes a single
argument which is a `size_t` (unsigned long) to represent the amount
of bytes to allocate on the heap. The allocation is C-style, meaning
that alloc returns a `void` pointer similar to `malloc`, which
is then supposed to be cast by the user to a proper pointer. When
`alloc` is called and there is already not enough memory left on
the heap to accommodate the request, a collection is triggered
to free up memory for the allocation. Hence the user does not
need to make their own calls to `free` or manually free up memory.

`alloc` can also return a null pointer, if the user requests to
allocate 0 bytes. This is not recommended.

`alloc` can also throw runtime errors in two cases. The first one
is of there is not enough memory on the heap available after
a collection, which in case the allocation cannot complete.
The second case is during a collection, where the function
`collect` throws a runtime error if the heap is not already
initialized by a call to `init`. Calls to `alloc` can technically
take place without properly initializing the heap, but this is
not recommended.

### Heap::dispose()
This function is used to dispose the heap at the program exit.
If the profiler is enabled, it is also disposed from a call
to `dispose`. When the profiler is disposed, a log file is
dumped containing the events on the heap. If the profiler
is disabled, nothing happens to the profiler during `dispose`.
After the profiler is disposed, the heap is deleted which
frees up all the memory used and deletes (hopefully) all
the remaining objects in memory.