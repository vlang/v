// Boehm's macros and callback casts belong in a cached native object.
// macOS TinyCC defines __APPLE__ without __MACH__; select Boehm's Darwin configuration.
#if defined(__APPLE__) && !defined(GC_DARWIN_THREADS)
#define GC_DARWIN_THREADS 1
#endif
#include <stdlib.h>
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

// The native backends' generated runtime (maps, closures, string helpers) calls
// the libc allocator by name. With the collector linked in, the arm64 backend
// sends those calls here, so every block V code allocates is scanned and
// collected. `free` and `realloc` still accept libc blocks that C libraries
// handed to V code.
void* v_native_gc_malloc(size_t size) { return GC_MALLOC(size); }
void* v_native_gc_calloc(size_t count, size_t size) {
	if (size != 0 && count > ((size_t)-1) / size) {
		return 0;
	}
	return GC_MALLOC(count * size); // GC_MALLOC clears the block
}
void* v_native_gc_realloc(void* ptr, size_t size) {
	if (ptr != 0 && GC_base(ptr) == 0) {
		return realloc(ptr, size);
	}
	return GC_REALLOC(ptr, size);
}
void v_native_gc_free(void* ptr) {
	if (ptr == 0) {
		return;
	}
	if (GC_base(ptr) == 0) {
		free(ptr);
		return;
	}
	GC_FREE(ptr);
}
void* v_native_gc_aligned_alloc(size_t alignment, size_t size) {
	return GC_memalign(alignment, size);
}
// The arm64 backend keeps its per-thread globals in slots found through pthread
// keys. The collector does not scan thread-specific data, so the slots are
// uncollectable (but scanned) blocks, freed by the key's destructor when the
// thread exits; see gc_pthread_redirects.h.
void* v_native_gc_tls_calloc(size_t count, size_t size) {
	if (size != 0 && count > ((size_t)-1) / size) {
		return 0;
	}
	return GC_MALLOC_UNCOLLECTABLE(count * size); // also cleared
}
void v_native_gc_tls_free(void* ptr) {
	if (ptr != 0) {
		GC_FREE(ptr);
	}
}
int v_native_gc_posix_memalign(void** result, size_t alignment, size_t size) {
	void* ptr = GC_memalign(alignment, size);
	if (ptr == 0) {
		return 12; // ENOMEM
	}
	*result = ptr;
	return 0;
}

#if defined(GC_PTHREADS)
#include <pthread.h>
#include <signal.h>
#include <dlfcn.h>

// Preserve the collector's redirects when generated C uses bodyless bindings.
int v_gc_pthread_create(void* thread, void* attr, void* start, void* arg) {
	return pthread_create((pthread_t*)thread, (const pthread_attr_t*)attr,
		(void* (*)(void*))start, arg);
}
int v_gc_pthread_join(pthread_t thread, void* retval) {
	return pthread_join(thread, (void**)retval);
}
int v_gc_pthread_detach(pthread_t thread) { return pthread_detach(thread); }
int v_gc_pthread_sigmask(int how, const void* set, void* oldset) {
	return pthread_sigmask(how, (const sigset_t*)set, (sigset_t*)oldset);
}
// Android does not implement POSIX thread cancellation.
#if !defined(__ANDROID__)
int v_gc_pthread_cancel(pthread_t thread) { return pthread_cancel(thread); }
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
