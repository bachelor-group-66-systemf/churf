#include <stdio.h>

#include "heap.hpp"

struct Obj {
    int a;
    int b;
    int c;
};

int main() {
    GC::Heap *heap = GC::Heap::the2();
    Obj *obj;

    for (int i = 0; i < 4; i++) {
        obj = static_cast<Obj *>(heap->alloc(sizeof(Obj)));
        obj->a = i * i + 1;
        obj->b = i * i + 2;
        obj->c = i * i + 3;
    }

    // heap->force_collect();

    std::cout << obj->a << ", " << obj->b << ", " << obj->c << std::endl;

    //delete heap;

    return 0;
}