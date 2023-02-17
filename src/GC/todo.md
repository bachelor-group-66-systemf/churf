# Garbage collection

## Project

Goal for next week (17/2):
- Functioning garbage collector
- Test it with valgrind

## GC TODO:
- Merge to main branch
- Don't empty m_free_chunks, reuse in a better way **Victor fixes this**

## Tests TODO
### Library linking
**Victor fixes this**
Compile the GC lib and a test separately, link them together
and evalutate the following:

    __builtin_frame_address(0)
    __builtin_frame_address(1)
    __builtin_return_address(0)
    __builtin_return_address(1)

### GC Init and __b_f_a
1. Save the first stack fram globally as the stack start
2. For each call to collect, save the prev stack frame as the stack end
3. Scan through the span

    gc_init()
        global stack_end = __builtin_frame_address(1)

    collect()
        global stack_start = __builtin_frame_address(1)

    sweep()
        for all addr in range(stack_end, stack_start)
            mark if chunk
