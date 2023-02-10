# Garbage collection

## Project

Goal for next week (17/2):
- Functioning garbage collector
- Test it with valgrind

TODO:
- Merge to main branch

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
    - exposes alloc

- Heap chunks
    - size
    - addr
    - marked bit