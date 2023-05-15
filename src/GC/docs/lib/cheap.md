# cheap.h & cheap.cpp

A wrapper interface for the class `GC::Heap` for easier use
in LLVM (no nasty namespaces). This interface is relatively
straight-forward and only defines functions to use the already
public functions in the class `GC::Heap`.

The functions are declared in a normal C-style header and
defined as "pure" C-functions. Because the public functions
exposed in `GC::Heap` are static, some of the functions
just call the static functions but are wrapped as C-functions.

For the non-static function `GC::Heap::set_profiler()` and the
singleton get-instance function `GC::Heap::the()` a struct
is used to encapsulate the heap-object. If this library is
compiled with `DEBUG` defined a struct is typedef-ed and
can be used everywhere, otherwise this struct is opaque
and cannot be used explicitly. This struct only contains
a pointer to the heap instance and is called `cheap_t`.

## Functions
`cheap_t *cheap_the()`: Returns an encapsulated singleton
instance. It is encapsulated in an opaque struct as the
instance itself is not meant to be used outside the C++
library.

`void cheap_init()`: Simply calls the `Heap::init()`
function.

`void cheap_dispose()`: Only calls the `Heap::dispose()`
function.

`void *cheap_alloc(unsigned long size)`: Calls `Heap::alloc(size_t size)`
and returns whatever `alloc` returns.

`void cheap_set_profiler(cheap_t *cheap, bool mode)`:
The argument `cheap` is the encapsulated Heap singleton instance.
`mode` is the same as for `Heap::set_profiler(bool mode)`.

For more documentation on functionality, see `src/GC/docs/lib/heap.md`.