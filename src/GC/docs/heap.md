## Heap Documentation

### Algorithm notes

    void mark_test(vector<Chunk *> worklist) {
        while (worklist.size() > 0) {
            Chunk *ref = worklist.pop_back();
            Chunk *child = (Chunk*) *ref;
            if (child != NULL && !child->marked) {
                child->marked = true;
                worklist.push_back(child);
                mark_test(worklist);
            }
        }
    }

    void mark_from_roots(uintptr_t *start, const uintptr_t *end)  {
        vector<Chunk *> worklist;
        for (;start > end; start--) {
            Chunk *ref = *start;
            if (ref != NULL && !ref->marked) {
                ref->marked = true;
                worklist.push_back(ref);
                mark_test(worklist);
            }
        }
    }
    
Alternative marking, pseudocode

    mark_from_roots():
        worklist <- empty
        for fld in Roots
          ref <- *fld
          if ref ≠ null && !marked(ref)
            set_marked(ref)
            worklist.add(ref)
            mark()

      mark():
        while size(worklist) > 0
          ref <- remove_first(worklist)
          for fld in Pointers(ref)
            child <- *fld
            if child ≠ null && !marked(child)
              set_marked(child)
              worklist.add(child)