// Boehm's macros and callback casts belong in a cached native object.
// macOS TinyCC defines __APPLE__ without __MACH__; select Boehm's Darwin configuration.
#if defined(__APPLE__) && !defined(GC_DARWIN_THREADS)
#define GC_DARWIN_THREADS 1
#endif
#if defined(__TINYC__) && defined(__linux__)
#include <gc/gc.h>
#else
#include <gc.h>
#endif

void* v_gc_malloc(size_t size) { return GC_MALLOC(size); }
void* v_gc_malloc_atomic(size_t size) { return GC_MALLOC_ATOMIC(size); }
void* v_gc_malloc_uncollectable(size_t size) { return GC_MALLOC_UNCOLLECTABLE(size); }
void* v_gc_realloc(void* ptr, size_t size) { return GC_REALLOC(ptr, size); }
void v_gc_free(void* ptr) { GC_FREE(ptr); }
void v_gc_init(void) { GC_INIT(); }
void v_gc_register_displacement(size_t offset) { GC_REGISTER_DISPLACEMENT(offset); }
void v_gc_set_warn_proc(GC_warn_proc cb) { GC_set_warn_proc(cb); }
void* v_gc_get_warn_proc(void) { return (void*)GC_get_warn_proc(); }
void v_gc_set_abort_func(GC_abort_func cb) { GC_set_abort_func(cb); }
void* v_gc_get_abort_func(void) { return (void*)GC_get_abort_func(); }
void v_gc_call_abort_func(void* cb, const char* msg) { ((GC_abort_func)cb)(msg); }

#if defined(GC_PTHREADS)
#include <pthread.h>
#include <signal.h>
#include <dlfcn.h>

// Preserve the collector's redirects when generated C uses bodyless bindings.
int v_gc_pthread_create(void* thread, void* attr, void* start, void* arg) {
	return pthread_create((pthread_t*)thread, (const pthread_attr_t*)attr,
		(void* (*)(void*))start, arg);
}
int v_gc_pthread_join(void* thread, void* retval) {
	return pthread_join((pthread_t)thread, (void**)retval);
}
int v_gc_pthread_detach(void* thread) { return pthread_detach((pthread_t)thread); }
int v_gc_pthread_sigmask(int how, const void* set, void* oldset) {
	return pthread_sigmask(how, (const sigset_t*)set, (sigset_t*)oldset);
}
// Android does not implement POSIX thread cancellation.
#if !defined(__ANDROID__)
int v_gc_pthread_cancel(void* thread) { return pthread_cancel((pthread_t)thread); }
#endif
void v_gc_pthread_exit(void* result) { pthread_exit(result); }
void* v_gc_dlopen(const char* path, int mode) { return dlopen(path, mode); }
#endif

#if defined(__linux__)
#define v__gc_can_register_main_data_roots_linux v_gc_can_register_main_data_roots_impl
#define v__gc_debugger_present_linux v_gc_debugger_present_impl
#define v__gc_register_main_data_roots_linux v_gc_register_main_data_roots_impl
#include "gc_debugger_linux.h"
#undef v__gc_can_register_main_data_roots_linux
#undef v__gc_debugger_present_linux
#undef v__gc_register_main_data_roots_linux

int v__gc_can_register_main_data_roots_linux(void) {
	return v_gc_can_register_main_data_roots_impl();
}
int v__gc_debugger_present_linux(void) { return v_gc_debugger_present_impl(); }
void v__gc_register_main_data_roots_linux(void) { v_gc_register_main_data_roots_impl(); }
#endif
