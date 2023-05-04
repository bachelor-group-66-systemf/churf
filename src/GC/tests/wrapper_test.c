#include <stdbool.h>
#include <stdio.h>

#include "cheap.h"

typedef struct node {
    int id;
    struct node *child;
} Node;

// Global variables make the test less complex
Node *HEAD = NULL;
Node *CURRENT = NULL;

void insert_first(int node_id) {
    Node *new_head;
    new_head = (Node*)(cheap_alloc(sizeof(Node)));
    new_head->id = node_id;
    new_head->child = HEAD;

    HEAD = new_head;
}

// Creates a linked list of length depth. Global head "HEAD" is updated.
Node *create_linked_list(int depth) {
    HEAD = (Node*)(cheap_alloc(sizeof(Node)));
    HEAD->id = 0;
    // Purposely omitting adding a child to "last_node", since its the last node
    for (int i = 1; i < depth - 1; i++) {
        insert_first(i);
    }
    return HEAD;
}

void create_garbage(int amount) {
    for (int i = 0; i < amount; i++) {
        long *garbage = (long*)(cheap_alloc(sizeof(long)));
    }
}

int main () {
    cheap_init();
    cheap_t *heap = cheap_the();
    cheap_set_profiler(heap, true);

    // Every node in this list should be marked
    Node *head = create_linked_list(5);
    // Everything create here should be swept
    create_garbage(30);

    cheap_dispose();
    return 0;
}