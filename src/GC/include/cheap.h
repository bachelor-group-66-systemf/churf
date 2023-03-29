#ifndef CHEAP_H
#define CHEAP_H

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

#define DEBUG

#ifdef DEBUG
typedef struct cheap
{
    void *obj;
} cheap_t;
#else
struct cheap;
typedef struct cheap cheap_t;
#endif

cheap_t *cheap_the() noexcept;
void cheap_init() noexcept;
void cheap_dispose() noexcept;
void *cheap_alloc(unsigned long size) noexcept;
void cheap_set_profiler(cheap_t *cheap, bool mode) noexcept;

#ifdef __cplusplus
}
#endif

#endif /* __CHEAP_H__ */