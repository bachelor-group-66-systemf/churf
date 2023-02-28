#include <iostream>

#include "heap.hpp"

using namespace std;

struct Node {
    int value;
    Node *left;
    Node *right;
};

int getValue();
Node *createNode();
void insert();

int main() {
    GC::Heap::init();
    Node *node = static_cast<Node *>(GC::Heap::alloc(sizeof(Node)));

    return 0;
}

int getValue() {
    cout << "Enter a value to insert: ";
    int value;
    cin >> value;
    return value;
}

Node *createNode() {
    Node *node = static_cast<Node *>(GC::Heap::alloc(sizeof(Node)));
    node->value = getValue();
    return node;
}

void insert(Node *root) {
    Node *node = createNode();
    Node *curr = root;
    while (curr)
}