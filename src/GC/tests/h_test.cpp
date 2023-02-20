#include "../include/heap.hpp"

GC::Heap *gc = GC::Heap::the();

/* void assign(int *dst) {
    int *local = static_cast<int *>(gc->alloc(sizeof(int)));
    *local = 10;
    dst = reinterpret_cast<int **>(dst);
    *dst = local;
} */

void create_array(size_t size) {
    int *arr = static_cast<int *>(gc->alloc(sizeof(int) * size));
}

void detach_pointer(long **ptr) {
    long dummy = 10; // dummy value
    long *dummy_ptr = &dummy;
    *ptr = dummy_ptr;
    std::cout << "Dummy pointer: \t" << dummy_ptr << std::endl;
    std::cout << "Detach pointer result:\t" << ptr << std::endl;
}

void init() {

    auto stack_start = reinterpret_cast<uintptr_t *>(__builtin_frame_address(0));
    std::cout << "Stack start from init:\t" << stack_start << std::endl;
    int *arr = static_cast<int *>(gc->alloc(sizeof(int) * 100));
    create_array(100);
    //arr = create_array(100);
    //std::cout << "Arr_ptr" << std::hex << arr << "\n\n\n" << std::endl;
    for (int i = 0; i < (sizeof(int) * 100); i++) {
        arr[i] = i;
    }
    std::cout << "Pointer for arr:\t" << &arr << std::endl;
    long *l = static_cast<long *>(gc->alloc(sizeof(long)));
    std::cout << "l points to:\t\t" << l << std::endl;
    detach_pointer(&l);
    std::cout << "l points to:\t\t" << l << std::endl;
    // l still gets marked, which is not supposed to happen
}

int main() {
    auto stack_start = reinterpret_cast<uintptr_t *>(__builtin_frame_address(0));
    std::cout << "Stack start from main:\t" << stack_start << std::endl;
    init();
    gc->collect(MARK); // some bug in free (vector out of range)
    gc->print_contents();
    return 0;
}