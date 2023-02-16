#include "heap.hpp"

GC::Heap *gc = GC::Heap::the2();

void init() {
    std::vector<int> live_int_vec{1, 2, 3, 4, 5};
    std::vector<int> dead_int_vec(1000000, 1);
    int *arr = static_cast<int *>(gc->alloc(sizeof(int) * 300));
    int *a_ptr;
    int a = 10;
    a_ptr = &a;
    auto temp1 = gc->alloc(sizeof(a_ptr));
    auto temp2 = gc->alloc(sizeof(live_int_vec));
    auto temp3 = gc->alloc(sizeof(dead_int_vec));

    // *arr, *temp1, *temp2 and *temp3 should all be on the stack
    //a_ptr = nullptr;
}

int main() {
    //init();
    for (int i = 1; i < 10; i++) {
        gc->alloc(sizeof(i));
    }
    gc->collect(MARK | SWEEP);
    gc->print_contents();
    //delete gc;
    return 0;
}