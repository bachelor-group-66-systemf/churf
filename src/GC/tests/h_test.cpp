#include "heap.hpp"

GC::Heap *gc = GC::Heap::the2();

void init() {
    std::vector<int> live_int_vec{1, 2, 3, 4, 5};
    std::vector<int> dead_int_vec(10, 1);
    int *a_ptr;
    int a = 10;
    a_ptr = &a;
    auto temp1 = gc->alloc(sizeof(a_ptr));
    auto temp2 = gc->alloc(sizeof(live_int_vec));
    auto temp3 = gc->alloc(sizeof(dead_int_vec));
    a_ptr = nullptr;
}

int main() {
    init();
    gc->force_collect();
    gc->print_contents();
    //delete gc;
    return 0;
}