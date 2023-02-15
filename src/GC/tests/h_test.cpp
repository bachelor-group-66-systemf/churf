#include "../include/heap.hpp"

GC::Heap gc;

void init() {
    gc = GC::Heap::the(); // pointer to the heap
    std::vector<int> live_int_vec(100);
    std::vector<int> dead_int_vec(100);
    gc.alloc(sizeof(live_int_vec));
    gc.alloc(sizeof(dead_int_vec));
}

int main() {
    gc.print_contents();
    return 0;
}