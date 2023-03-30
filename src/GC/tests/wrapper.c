#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "cheap.h"

typedef struct object
{
    int x, y, z;
    double velocity;
} Object;

void test_init()
{
    printf("----- IN TEST_INIT ----------------------------\n");
    
    cheap_init();

    printf("----- EXIT TEST_INIT --------------------------\n");
}

/* Uncomment ONLY if run with DEBUG defined in cheap.h */

// cheap_t *test_the()
// {
//     printf("----- IN TEST_THE -----------------------------\n");

//     cheap_t *fst_heap = cheap_the();

//     printf("Heap 1:\t%p\n", fst_heap->obj);

//     cheap_t *snd_heap = cheap_the();

//     printf("Heap 2:\t%p\n", snd_heap->obj);

//     printf("----- EXIT TEST_THE ---------------------------\n");

//     free(snd_heap);
//     return fst_heap;
// }

void test_profiler(cheap_t *heap)
{
    printf("----- IN TEST_PROFILER ------------------------\n");

    cheap_set_profiler(heap, false);
    cheap_set_profiler(heap, true);

    printf("----- EXIT TEST_PROFILER ----------------------\n");
}

Object *test_alloc()
{
    printf("----- IN TEST_ALLOC ---------------------------\n");
    
    Object *o;
    o = (Object *)(cheap_alloc(sizeof(Object)));

    o->x = 3;
    o->y = 4;
    o->z = 5;
    o->velocity = 1.0f;

    printf("----- EXIT TEST_ALLOC -------------------------\n");
    return o;
}

void test_dispose()
{
    printf("----- IN TEST_DISPOSE -------------------------\n");

    cheap_dispose();

    printf("----- EXIT TEST_DISPOSE -----------------------\n");
}

int main()
{
    test_init();

    /* Uncomment ONLY if run with DEBUG defined in cheap.h */
    // cheap_t *heap = test_the();
    // test_profiler(heap);

    Object *o = test_alloc();
    printf("Object size: %lu\n", sizeof(Object));
    printf("Object:\n\tx: %d\n\ty: %d\n\tz: %d\n\tvel: %f\n", o->x, o->y, o->z, o->velocity);

    test_dispose();

    /* Sefault I don't understand, don't uncomment */
    // free(heap);
    // free(o);
    return 0;
}