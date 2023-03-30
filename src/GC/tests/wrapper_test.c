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

// Creates a linked list of length depth. Global head "HEAD" is updated.
void *create_linked_list(int depth) {
    HEAD = (Node*)(cheap_alloc(sizeof(Node)));
    HEAD->id = 0;
    // Purposely omitting adding a child to "last_node", since its the last node
    for (int i = 1; i < depth - 1; i++) {
        insert_first(i);
    }
}

void *insert_first(int node_id) {
    Node *new_head;
    new_head = (Node*)(cheap_alloc(sizeof(Node)));
    new_head->id = node_id;
    new_head->child = HEAD;

    HEAD = new_head;
}

void test_linked_list(int list_length){
    cheap_init();
    cheap_t *heap = cheap_the();
    cheap_set_profiler(heap, true);
    create_linked_list(list_length);
    cheap_dispose();
    free(heap);
}

int main (int argc, char **argv) {
    test_linked_list(30);
}