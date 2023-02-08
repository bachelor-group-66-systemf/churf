#include <iostream> 
#include <vector>
#define HEAP_SIZE 65536 // Arbitrary for now, 2^16
using namespace std;

/* A simple mark and sweep algorithm */

// Shouldn't be exposed. For now, it is
struct ObjectHeader {
    size_t size = sizeof(this);
    bool marked = false;

};

struct Object : ObjectHeader {
    char name; // should be something like id, but for testing sake its char
    Object* child;
    //Object(char name) {}
    //Object(char name, Object* child) {}
};

// Representing the heap as a simple struct for now
struct Heap {
    Object heap_space[HEAP_SIZE];
};

// For now it assumes that it is given root objects from the start, no root finding included
class MarkSweep {
    public:
        void mark(Object* obj) {
            if (!markedBit(obj)) {
                markBit(obj);
                Object* ref = obj->child;
                if (ref != nullptr) {
                    mark(ref);
                }
            }
        }

        void sweep(vector<Object*> worklist) {
            for (Object* obj: worklist) {
                if (!markedBit(obj) && obj != nullptr) {
                    delete obj;
                }
            }
        }

    private:
        bool markedBit(Object* obj) {
            return obj->marked;
        }

        void markBit(Object* obj) {
            obj->marked = true;
        }

};

int main() {
    Object* b = new Object();
    b->name = 'B';
    b->child = nullptr;
    Object* c = new Object();
    c->name = 'C';
    c->child = b; // c -> d
    Object* d = new Object();
    d->name = 'D';
    d->child = nullptr;

    //Heap* heap = new Heap{*c, *b, *d};
    vector<Object*> worklist = {c, b, d};
    MarkSweep* gc = new MarkSweep();

    gc->mark(c);
    cout << "Expected 1, got: " << b->marked << '\n';
    cout << "Expected 1, got: " << c->marked << '\n';
    cout << "Expected 0, got: " << d->marked << '\n';

    gc->sweep(worklist); 
    cout << b->name << '\n';
    cout << c->name << '\n';
    cout << d->name << '\n'; // The object at d is now deleted (freed)
    return 0;
}