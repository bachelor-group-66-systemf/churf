#ifndef CHEAP_H
#define CHEAP_H

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

<<<<<<< HEAD
#define WRAPPER_DEBUG
=======
#define DEBUG
>>>>>>> 74e0282 (Added Hash map marking)

#ifdef WRAPPER_DEBUG
typedef struct cheap
{
    void *obj;
} cheap_t;
#else
struct cheap;
typedef struct cheap cheap_t;
#endif

#define FuncCallsOnly   0x1E
#define ChunkOpsOnly    0x3E0

cheap_t *cheap_the();
void cheap_init();
void cheap_dispose();
void *cheap_alloc(unsigned long size);
void cheap_set_profiler(cheap_t *cheap, bool mode);
void cheap_profiler_log_options(cheap_t *cheap, unsigned long flag);

#ifdef __cplusplus
}
#endif

#endif /* __CHEAP_H__ */