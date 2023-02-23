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

Node *test_chain(int depth, bool detach) {
    auto stack_start = reinterpret_cast<uintptr_t *>(__builtin_frame_address(0));
    std::cout << "Stack start from test_chain:\t" << stack_start << std::endl;

    Node *node_chain = create_chain(depth);
    // This generates a segmentation fault (should be investigated further)
    if (detach)
        node_chain->child = nullptr;
    return node_chain;

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
    gc->init();
    gc->check_init();
    auto stack_start = reinterpret_cast<uintptr_t *>(__builtin_frame_address(0));
    std::cout << "Stack start from main:\t" << stack_start << std::endl;
    
    // char *c = static_cast<char *>(gc->alloc(sizeof(char))); // 0x0      | 0x0
    // int *i = static_cast<int *>(gc->alloc(sizeof(int)));    // 0x1-0x4  | 0x4-0x8
    // char *c2 = static_cast<char *>(gc->alloc(sizeof(char)));// 0x5      | 0x9-0x
    // long *l = static_cast<long *>(gc->alloc(sizeof(long))); // 0x6-0xd  | 0x
    
    // This is allocated outside of the scope of the GC (if gc->init() isn't called), thus garbage
    /* long *longs[21];
    std::cout << "Pointer to ints:\t" << longs << std::endl;
    for (int i = 0; i < 21; i++) {
        longs[i] = static_cast<long *>(gc->alloc(sizeof(long)));
    } */
    //Node *root;
    Node *root = test_chain(3, false);
    std::cout << "Adress of root:\t" << &root << std::endl;
    std::cout << "Root points to:\t" << root << std::endl;
    // 0x7ffdd7556bd8
    int *i = static_cast<int *>(gc->alloc(sizeof(int)));
    std::cout << "Adress of i:\t" << &i << std::endl;

    gc->collect(MARK); // free misses some chunks
    gc->print_contents();
    return 0;
}