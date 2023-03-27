# heap.hpp & heap.cpp

## Members
`char *const m_heap`: This is the pointer to the simulated heap which
collection occurs on. It's a byte array with a constant pointer.

`size_t m_size`: The size of bytes that has been allocated on the heap.

`inline static Heap *m_instance`: The singleton instance of Heap. Before
the heap is initialized this is initialized to the null pointer.

`uintptr_t *m_stack_top`: The address of the topmost stack frame which
serves as the stop for scanning the stack. Initialized as the null pointer
but assigned to the correct address in `Heap::init()`.

`bool m_profiler_enable`: The state of the profiler, `true` if the
profiler is enabled, `false` otherwise. It is initialized as `false`.

`std::vector<Chunk *> m_allocated_chunks`: Contains pointers to all
chunks that are allocated on the heap and can be reachable (if
a collection has been triggered previously).

`std::vector<Chunk *> m_freed_chunks`: Contains pointer to
chunks that have been freed, used to try and recycle chunks.

## Constructors
`Heap()`: Default constructor which guarantees to initialize
the `m_heap` pointer and the byte array. Declared private
in accordance with the singleton pattern.

## Destructors
`~Heap()`: Frees the `m_heap` byte array. Declared private
in accordance with the singleton pattern.

## Functions
`static void init()`: Initializes the heap singleton and the member
`m_instance`. Must be called before any calls to `alloc()`.

`static void dispose()`: Disposes the heap singleton which frees
the heap. If the profiler is enabled the profiler is also disposed.

`static void *alloc(size_t size)`: Tries to allocate `size` amount
of bytes on the heap. The allocation is C-style, meaning `alloc()`
returns a `void *` similar to `malloc` and the user should cast
this pointer to an appropriate type. If this function is called with
the argument of 0, it will return the null pointer. This function can throw
runtime errors on two occasions. One if there is not enough memory
on the heap after a collection is triggered, it will throw a runtime
error with the message "Out of memory". The other occasion is when
a collection is triggered and the heap has not been initialized
properly by calling `init()`.

`static void set_profiler(bool mode)`: Enables or disables (`true`
or `false`) the profiler.