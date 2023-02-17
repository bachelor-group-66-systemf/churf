#include "../include/heap.hpp"

GC::Heap *gc = GC::Heap::the2();

void init() {

    auto stack_start = reinterpret_cast<uintptr_t *>(__builtin_frame_address(0));
    auto stack_end = stack_start - 40;
    std::cout << "Stack start from init:\t" << stack_start << std::endl;
    std::cout << "Imaginary stack end:\t" << stack_end << std::endl;
    int *arr = static_cast<int *>(gc->alloc(sizeof(int) * 100));
    std::cout << "Arr_ptr" << std::hex << arr << "\n\n\n" << std::endl;
    for (int i = 0; i < (sizeof(int) * 100); i++) {
        arr[i] = i;
    }
    std::cout << "First stack pointer:\t" << &arr << std::endl;
    long a = 20;
    long *l = static_cast<long *>(gc->alloc(sizeof(long)));
    l = &a;
    //*l = 20;
}

int main() {
    auto stack_start = reinterpret_cast<uintptr_t *>(__builtin_frame_address(0));
    std::cout << "Stack start from main:\t" << stack_start << std::endl;
    init();
    gc->collect(MARK | SWEEP | FREE);
    gc->print_contents();
    //delete gc;
    return 0;
}