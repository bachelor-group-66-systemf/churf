#include <stdio.h>

#include "heap.hpp"

struct Obj {
    int a;
    int b;
    int c;
};

int main() {
    auto heap = GC::Heap::the2();

    std::cout << "heap:\t" << heap << std::endl;
    
    auto obj = static_cast<Obj *>(heap->alloc(sizeof(Obj)));

    std::cout << "obj: \t" << obj << std::endl;

    obj->a = 3;
    obj->b = 4;
    obj->c = 5;

    std::cout << obj->a << ", " << obj->b << ", " << obj->c << std::endl;

    heap->print_contents();
    //delete heap;

    return 0;
}