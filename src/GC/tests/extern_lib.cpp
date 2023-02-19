#include <cstring>
#include <iostream>

#include "heap.hpp"

// using namespace std;

// GC::Heap *singleton_test();
// void init_gc(GC::Heap *heap);
// void frame_test(GC::Heap *heap);

GC::Heap *singleton_test() {
    std::cout << "TESTING SINGLETON INSTANCES" << std::endl;
    std::cout << "===========================" << std::endl;
    std::cout << "Call 1:\t" << GC::Heap::the() << std::endl;
    GC::Heap *heap = GC::Heap::the();
    std::cout << "Call 2:\t" << heap << std::endl;
    std::cout << "===========================" << std::endl;
    return heap;
}

void init_gc(GC::Heap *heap){
    std::cout << "\n\n   INITIALIZING THE HEAP" << std::endl;
    std::cout << "===========================" << std::endl;
    heap->init();
    std::cout << "===========================" << std::endl;
}

void frame_test(GC::Heap *heap) {
    std::cout << "\n\n  TESTING FRAME ADDRESSES" << std::endl;
    std::cout << "===========================" << std::endl;

#pragma clang diagnostic ignored "-Wframe-address"
    auto curr_frame = reinterpret_cast<uintptr_t *>(__builtin_frame_address(0));
    std::cout << "Current stack frame:\t" << curr_frame << std::endl;
#pragma clang diagnostic ignored "-Wframe-address"
    auto prev_frame = reinterpret_cast<uintptr_t *>(__builtin_frame_address(1));
    std::cout << "Previous stack frame:\t" << prev_frame << std::endl;

    heap->check_init();
    
    std::cout << "===========================" << std::endl;
}

int main() {
    auto heap = singleton_test();

    init_gc(heap);
    frame_test(heap);

    return 0;
}