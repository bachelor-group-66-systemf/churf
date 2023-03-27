#include <cstring>
#include <iostream>

#include "heap.hpp"

GC::Heap& singleton_test();
void init_gc(GC::Heap& heap);
void frame_test(GC::Heap& heap);

int main() {
    std::cout << "in main" << std::endl;
    GC::Heap &heap = singleton_test();

    init_gc(heap);
    frame_test(heap);

    heap.dispose();

    return 0;
}

/**
 * This test is supposed to determine if the singleton pattern
 * implementation is working correctly. This test passes if the
 * first and second call prints the same memory address.
 * 
 * Result: pass
 * 
 * @return Pointer to the Heap singleton instance
*/
GC::Heap& singleton_test() {
    std::cout << "TESTING SINGLETON INSTANCES" << std::endl;
    std::cout << "===========================" << std::endl;
    std::cout << "Call 1:\t" << &GC::Heap::the() << std::endl;   // First call which initializes the singleton instance
    GC::Heap &heap = GC::Heap::the();                           // Second call which should return the initialized instance
    std::cout << "Call 2:\t" << &heap << std::endl;
    std::cout << "===========================" << std::endl;
    return heap;
}


/**
 * This test calls Heap::init() which saves the stack-frame
 * address from the calling function (this function).
 * Heap::init() is supposed to be called at the absolute
 * start of the program to save the address of the
 * topmost stack frame. This test doesn't do anything
 * but prepares for the next test(s).
 * 
 * @param heap The Heap pointer to the singleton instance.
 * 
*/
void init_gc(GC::Heap& heap){
    std::cout << "\n\n   INITIALIZING THE HEAP" << std::endl;
    std::cout << "===========================" << std::endl;
    heap.init();
    heap.set_profiler(true);
    std::cout << "===========================" << std::endl;
}

/**
 * This function tests the functionality of the intrinsic
 * function `__builtin_frame_address` which returns the
 * address of the corresponding level of stack frame.
 * When given a param of 0, it returns the current stack frame.
 * When given a param of 1, it returns the previous stack
 * frame, and so on.
 * 
 * This test passes on two conditions:
 * 1)   if the address of the current frame is smaller than
 *      the address of the previous frame (assumed).
 * 2)   if the previous frame has the same address as the one
 *      saved in the Heap instance after running Heap::init().
 * 
 * Result: pass 
 * 
 * @param heap The Heap instance
*/
void frame_test(GC::Heap& heap) {
    std::cout << "\n\n  TESTING FRAME ADDRESSES" << std::endl;
    std::cout << "===========================" << std::endl;

#pragma clang diagnostic ignored "-Wframe-address" // clang++ directive to ignore warnings about __b_f_a
    auto curr_frame = reinterpret_cast<uintptr_t *>(__builtin_frame_address(0)); // addr of curr stack frame
    std::cout << "Current stack frame:\t" << curr_frame << std::endl;
#pragma clang diagnostic ignored "-Wframe-address"
    auto prev_frame = reinterpret_cast<uintptr_t *>(__builtin_frame_address(1)); // addr of prev stack frame
    std::cout << "Previous stack frame:\t" << prev_frame << std::endl;

    heap.check_init(); // prints the saved absolute top of the stack
    // auto alloced = heap->alloc(sizeof(unsigned long));
    
    std::cout << "===========================" << std::endl;
}