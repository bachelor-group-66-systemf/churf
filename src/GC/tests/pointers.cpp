#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include "heap.hpp"

using std::cout, std::endl, std::hex;

struct Node {
    int value;
    Node *next {nullptr};
};

void test(Node *n) {
    size_t n_size = 16;

    auto c_start = reinterpret_cast<uintptr_t>(n);
    auto c_size = reinterpret_cast<uintptr_t>(n_size);
    auto c_end = reinterpret_cast<uintptr_t>(c_start + c_size);

    cout << "Node *n:\t" << n << "\n";
    cout << "n_size: \t0x" << std::hex << n_size << "\n";
    cout << "c_start:\t0x" << std::hex << c_start << "\n";
    cout << "c_size: \t0x" << std::hex << c_size << "\n";
    cout << "c_end:  \t0x" << std::hex << c_end << endl;
}

int main() {
    GC::Heap::init();
    GC::Heap &heap = GC::Heap::the();
    heap.set_profiler(true);
    heap.set_profiler_log_options(GC::FunctionCalls);

    Node *n = static_cast<Node *>(GC::Heap::alloc(sizeof(Node)));
    test(n);

    GC::Heap::dispose();
    return 0;
}