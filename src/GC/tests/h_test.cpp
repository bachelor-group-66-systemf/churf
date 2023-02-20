#include "../include/heap.hpp"

GC::Heap *gc = GC::Heap::the();

/* void assign(int *dst) {
    int *local = static_cast<int *>(gc->alloc(sizeof(int)));
    *local = 10;
    dst = reinterpret_cast<int **>(dst);
    *dst = local;
} */

void init() {

    auto stack_start = reinterpret_cast<uintptr_t *>(__builtin_frame_address(0));
    //auto stack_end = stack_start - 40;
    std::cout << "Stack start from init:\t" << stack_start << std::endl;
    //std::cout << "Imaginary stack end:\t" << stack_end << std::endl;
    int *arr = static_cast<int *>(gc->alloc(sizeof(int) * 100));
    //std::cout << "Arr_ptr" << std::hex << arr << "\n\n\n" << std::endl;
    for (int i = 0; i < (sizeof(int) * 100); i++) {
        arr[i] = i;
    }
    for (int i = 0; i < (sizeof(int) * 20); i++) {
        gc->alloc(sizeof(int));
    }
    std::cout << "Pointer for arr:\t" << &arr << std::endl;
    long a = 20;
    long *l = static_cast<long *>(gc->alloc(sizeof(long)));
    std::cout << "Pointer for l:\t\t" << &l << std::endl;
    int *i = static_cast<int *>(gc->alloc(sizeof(int)));
    std::cout << "Pointer for i:\t\t" << &i << std::endl;
    //l = &a;
    //*l = 20;
}

int main() {
    auto stack_start = reinterpret_cast<uintptr_t *>(__builtin_frame_address(0));
    std::cout << "Stack start from main:\t" << stack_start << std::endl;
    init();
    gc->collect(MARK | SWEEP); // some bug in free (vector out of range)
    gc->print_contents();
    //delete gc;
    return 0;
}