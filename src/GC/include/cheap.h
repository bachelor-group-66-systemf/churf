#ifndef __CHEAP_H__
#define __CHEAP_H__

#ifdef __cplusplus
extern "C" {
#endif

struct cheap;
typedef struct cheap cheap_t;

cheap_t *cheap_the();
void cheap_init();
void cheap_dispose();
void *cheap_alloc(unsigned long size);
void cheap_set_profiler(bool mode);

#ifdef __cplusplus
}
#endif


#endif /* __CHEAP_H__ */