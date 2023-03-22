# chunk.hpp

A chunk struct object is the basic element of what can be
stored on the heap. When `Heap::alloc` is called a
chunk may be created to represent the space of memory
that was allocated on the heap by `alloc`.

## Members
`bool m_marked`: A boolean flag to mark an object during mark/sweep.

`uintptr_t *const m_start`: A constant pointer pointing to the start
address of the memory space that was allocated.

`const size_t m_size`: The size of the memory space that was allocated.

## Constructors
There are three constructors for a chunk. One regular constructor
and two copy constructors.

`Chunk(size_t size, uintptr_t *start)`: Used for creating new chunks in
`Heap::alloc`.

`Chunk(const Chunk *const c)`: A copy constructor used by the profiler
to store chunk data after the initial chunk is deleted.

`Chunk(const Chunk &c)`: A secondary copy constructor used for debugging.