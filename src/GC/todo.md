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

## Tests TODO
### Library linking
Compile the GC lib and a test separately, link them together
and evalutate the following:

    __builtin_frame_address(0)
    __builtin_frame_address(1)
    __builtin_return_address(0)
    __builtin_return_address(1)

