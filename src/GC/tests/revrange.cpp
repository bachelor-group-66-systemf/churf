#include <iostream>
#include <stdint.h>
#include <stdlib.h>

#include "heap.hpp"

#define allocNode   static_cast<Node *>(GC::Heap::alloc(sizeof(Node)))

using std::cout, std::endl;

struct Node {
    int value;
    Node *next {nullptr};
};

void revRange(int n) {
    Node *next = nullptr;
    Node *prev = allocNode;
    while (n > 0) {
        next = allocNode;
        prev->next = next;
        prev->value = n--;
        prev = next;
    }
}

void make_test() {
    int n = 10;
    while (n > 0)
        revRange(1000);
}

int main() {
    GC::Heap::init();
    GC::Heap &heap = GC::Heap::the();
    heap.set_profiler(true);
    GC::Profiler::set_log_options(GC::FunctionCalls);

    make_test();

    GC::Heap::dispose();
    return 0;
}