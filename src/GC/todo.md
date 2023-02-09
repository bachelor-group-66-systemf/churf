# Garbage collection

## Algorithm

Potential algorithms:
- mark & sweep
    - easy to implement
    - slow
- mark & compact
    - no memory fragmentation
    - slow
- stop-copy algorithms (?)
    - no memory fragmentation
    - slow
    - maybe good for FP langs?

## Type hierarchy

- Heap class
    - Holds all memory
        - Resizeable array?
    - Singleton instance

- Allocator class
    - Allocates chunks of memory
    - keeps track of chunks that are available and their sizes
    - Several instances of allocator class with different sizes?

- HeapObj class
    - parent of all heap objects
    - contains metadata
        - size
        - location
        - marked bit