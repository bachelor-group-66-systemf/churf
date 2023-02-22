#include "../include/heap.hpp"

GC::Heap *gc = GC::Heap::the();

struct Node {
    int id;
    Node *child;
};

Node *create_chain(int depth) {
    Node* nodes[depth]; 
    if (depth > 0) {
        Node *last_node = static_cast<Node *>(gc->alloc(sizeof(Node)));
        last_node->id = depth;
        last_node->child = nullptr;
        nodes[0] = last_node;
        for (int i = 1; i < depth; i++) {
            Node *node = static_cast<Node *>(gc->alloc(sizeof(Node)));
            node->id = depth-i;
            node->child = nodes[i-1];
            nodes[i] = node;
        }
        return nodes[depth];
    }
    else
        return 0;
}

void create_array(size_t size) {
    int *arr = static_cast<int *>(gc->alloc(sizeof(int) * size));
}

void detach_pointer(long **ptr) {
    long *dummy_ptr = nullptr;
    *ptr = dummy_ptr;
}

void test_chain(int depth, bool detach) {
    auto stack_start = reinterpret_cast<uintptr_t *>(__builtin_frame_address(0));
    std::cout << "Stack start from test_chain:\t" << stack_start << std::endl;

    Node *node_chain = create_chain(depth);
    // This generates a segmentation fault (should be investigated further)
    if (detach)
        node_chain->child = nullptr;

}

void test_some_types() {
    auto stack_start = reinterpret_cast<uintptr_t *>(__builtin_frame_address(0));
    std::cout << "Stack start from test_some_types:\t" << stack_start << std::endl;

    long *l = static_cast<long *>(gc->alloc(sizeof(long)));
    std::cout << "l points to:\t\t" << l << std::endl;
    detach_pointer(&l);
    std::cout << "l points to:\t\t" << l << std::endl;
    // l still gets marked, which is not supposed to happen

    // Some more dummy values of different sizes, to test stack pointer alignment
    int *i = static_cast<int *>(gc->alloc(sizeof(int)));
    char *c = static_cast<char *>(gc->alloc(sizeof(int))); 
    short *s = static_cast<short *>(gc->alloc(sizeof(short)));
}

int main() {
    auto stack_start = reinterpret_cast<uintptr_t *>(__builtin_frame_address(0));
    std::cout << "Stack start from main:\t" << stack_start << std::endl;
    // This is allocated outside of the scope of the GC, thus garbage
    for (int i = 0; i < 10; i++) {
        gc->alloc(sizeof(int));
    }
    /* int depth = 10;
    Node* nodes[depth];
    Node *last_node = static_cast<Node *>(gc->alloc(sizeof(Node)));
    last_node->id = depth;
    last_node->child = nullptr;
    nodes[0] = last_node;
    for (int i = 1; i < depth; i++) {
        Node *node = static_cast<Node *>(gc->alloc(sizeof(Node)));
        node->id = depth-i;
        node->child = nodes[i-1];
        nodes[i] = node;
    } */
    //test_chain(10, false);
    //test_some_types();
    gc->collect(MARK | SWEEP | FREE); // free misses some chunks
    gc->print_contents();
    return 0;
}