#include "../include/heap.hpp"

GC::Heap *gc = GC::Heap::the2();

void init() {

    auto stack_start = reinterpret_cast<uintptr_t *>(__builtin_frame_address(0));
    auto stack_end = stack_start - 160;
    std::cout << "Stack start from init:\t" << stack_start << std::endl;
    std::cout << "Imaginary stack end:\t" << stack_end << std::endl;
    int *arr = static_cast<int *>(gc->alloc(sizeof(int) * 300));
    for (int i = 0; i < (sizeof(int) * 300); i++) {
        arr[i] = i;
    }
    std::cout << "First stack pointer:\t" << &arr << std::endl;
    long *l = static_cast<long *>(gc->alloc(sizeof(long)));
    *l = 20;
    // This doesn't get allocated on our heap, but how is it viewed on valgr?
    int *arr2 = new int[1000];
    for (int i = 0; i < 1000; i++) {
        arr2[i] = i;
    }
}

int main() {
    auto stack_start = reinterpret_cast<uintptr_t *>(__builtin_frame_address(0));
    std::cout << "Stack start from main:\t" << stack_start << std::endl;
    init();
    gc->collect(MARK | SWEEP);
    gc->print_contents();
    //delete gc;
    return 0;
}