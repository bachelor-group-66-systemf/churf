#include "heap.hpp"

struct Node {
    int id;
    Node *child;
};

Node *create_chain(int depth) {
    std::vector<Node*> nodes;
    if (depth > 0) {
        Node *last_node = static_cast<Node *>(GC::Heap::alloc(sizeof(Node)));
        last_node->id = depth;
        last_node->child = nullptr;
        nodes.push_back(last_node);
        for (size_t i = 0; i < depth; i++) {
            Node *node = static_cast<Node *>(GC::Heap::alloc(sizeof(Node)));
            node->id = depth-i;
            node->child = nodes[i];
            nodes.push_back(node);
        }
        return nodes[depth];
    }
    else
        return 0;
}

void create_array(size_t size) {
    int *arr = static_cast<int *>(GC::Heap::alloc(sizeof(int) * size));
}

void detach_pointer(long **ptr) {
    long *dummy_ptr = nullptr;
    *ptr = dummy_ptr;
}

Node *test_chain(int depth, bool detach) {
    auto stack_start = reinterpret_cast<uintptr_t *>(__builtin_frame_address(0));

    Node *node_chain = create_chain(depth);
    if (detach)
        node_chain->child = nullptr;
    return node_chain;

}

void test_some_types() {
    auto stack_start = reinterpret_cast<uintptr_t *>(__builtin_frame_address(0));
    std::cout << "Stack start from test_some_types:\t" << stack_start << std::endl;

    long *l = static_cast<long *>(GC::Heap::alloc(sizeof(long)));
    std::cout << "l points to:\t\t" << l << std::endl;
    detach_pointer(&l);
    std::cout << "l points to:\t\t" << l << std::endl;

    // Some more dummy values of different sizes, to test stack pointer alignment
    int *i = static_cast<int *>(GC::Heap::alloc(sizeof(int)));
    char *c = static_cast<char *>(GC::Heap::alloc(sizeof(int))); 
    short *s = static_cast<short *>(GC::Heap::alloc(sizeof(short)));
}

int main() {
    using namespace std::literals;

    auto start = std::chrono::high_resolution_clock::now();
    //std::cout << "Value of start: " << start.time_since_epoch().count() << std::endl;
    GC::Heap::init();
    GC::Heap &gc = GC::Heap::the();
    gc.set_profiler(true);
    gc.check_init();
    auto stack_start = reinterpret_cast<uintptr_t *>(__builtin_frame_address(0));

    Node *root1 = static_cast<Node *>(gc.alloc(sizeof(Node)));
    //Node *root2 = static_cast<Node *>(gc.alloc(sizeof(Node)));
    root1 = test_chain(60000, false);
    //root2 = test_chain(50000, true);


    gc.collect(GC::COLLECT_ALL);     
    auto end = std::chrono::high_resolution_clock::now();
    //std::cout << "Value of end: " << end.time_since_epoch().count() << std::endl;

    gc.print_summary();
    //gc.dispose();

    std::cout
        << "Execution time: "
        << std::chrono::duration_cast<std::chrono::microseconds>(end - start).count() << " ≈ "
        << (end - start) / 1ms << "ms ≈ " 
        << (end - start) / 1s << "s.\n";

    return 0;
}