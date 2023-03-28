#include <stdio.h>

#include "heap.hpp"

struct Obj {
    int a;
    int b;
    int c;
};

int main() {
    GC::Heap::init();
    Obj *obj;

    for (int i = 0; i < 4; i++) {
        obj = static_cast<Obj *>(GC::Heap::alloc(sizeof(Obj)));
        obj->a = i * i + 1;
        obj->b = i * i + 2;
        obj->c = i * i + 3;
    }

    // heap->force_collect();
    auto heap = GC::Heap::debug_the();
    heap->collect(COLLECT_ALL);

    std::cout << obj->a << ", " << obj->b << ", " << obj->c << std::endl;

    //delete heap;
    GC::Heap::dispose();

    return 0;
}