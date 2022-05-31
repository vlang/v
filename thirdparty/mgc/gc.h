/*
 * gc - A simple mark and sweep garbage collector for C.
 */

#ifndef __GC_H__
#define __GC_H__

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

struct AllocationMap;

typedef struct GarbageCollector {
    struct AllocationMap* allocs; // allocation map
    bool paused;                  // (temporarily) switch gc on/off
    void *bos;                    // bottom of stack
    size_t min_size;
} GarbageCollector;

extern GarbageCollector gc;  // Global garbage collector for all
                             // single-threaded applications

/*
 * Starting, stopping, pausing, resuming and running the GC.
 */
void gc_start(GarbageCollector* gc, void* bos);
void gc_start_ext(GarbageCollector* gc, void* bos, size_t initial_size, size_t min_size,
                  double downsize_load_factor, double upsize_load_factor, double sweep_factor);
size_t gc_stop(GarbageCollector* gc);
void gc_pause(GarbageCollector* gc);
void gc_resume(GarbageCollector* gc);
size_t gc_run(GarbageCollector* gc);

void GC_gcollect() {
    gc_run(&gc);
}

/*
 * Allocating and deallocating memory.
 */
void* gc_malloc(GarbageCollector* gc, size_t size);
void* gc_malloc_static(GarbageCollector* gc, size_t size, void (*dtor)(void*));
void* gc_malloc_ext(GarbageCollector* gc, size_t size, void (*dtor)(void*));
void* gc_calloc(GarbageCollector* gc, size_t count, size_t size);
void* gc_calloc_ext(GarbageCollector* gc, size_t count, size_t size, void (*dtor)(void*));
void* gc_realloc(GarbageCollector* gc, void* ptr, size_t size);
void gc_free(GarbageCollector* gc, void* ptr);


/*
 * Lifecycle management
 */
void* gc_make_static(GarbageCollector* gc, void* ptr);

/*
 * Helper functions and stdlib replacements.
 */
char* gc_strdup (GarbageCollector* gc, const char* s);

#endif /* !__GC_H__ */
