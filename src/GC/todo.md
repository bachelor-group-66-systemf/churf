# Garbage collection

## Project

Goal for next week (24/2):
- Debug 

## GC TODO:
- Merge to main branch
- Fix singleton references
- Think about how we want to determine if some object is a pointer or not, probably will have to discuss that with Samuel. Since it is not ideal to determine in the GC if an object is a pointer or not. It should preferably be done in a previous stage.

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
        local stack_start = __builtin_frame_address(1)

    sweep()
        for all addr in range(stack_end, stack_start)
            mark if chunk
