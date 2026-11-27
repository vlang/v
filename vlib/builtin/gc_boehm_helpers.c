// Boehm's macros and callback casts belong in a cached native object.
// `-c99` builds this object with -std=c99, where glibc hides the POSIX declarations
// used below (sigset_t, pthread_sigmask). Request glibc's default set, which GNU C
// modes already get, before the first header.
#if defined(__linux__) && !defined(_DEFAULT_SOURCE)
#define _DEFAULT_SOURCE 1
#endif
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

#ifdef V_GC_PROPORTIONAL_GROWTH
//libgc grows the heap by at most MAXHINCR (8 MiB without LARGE_CONFIG, 16 MiB with
// it) and collects after each growth. A program whose live heap only grows then
// does one full collection per increment, so the total marking work is
// quadratic in the heap size (#32272). Grow in proportion to the heap instead,
// as Go and V8 do, while keeping a small heap small. This is opt-in because it
// trades up to about 2x the live heap for far fewer full collections.
#define V_GC_GROWTH_CHECK_MASK 1023UL
#define V_GC_GROWTH_MIN_HEAP ((size_t)16 * 1024 * 1024)
#define V_GC_GROWTH_CHECK_BYTES ((size_t)1024 * 1024)
#define V_GC_GROWTH_MAX_STEP ((size_t)1024 * 1024 * 1024)
static volatile unsigned long v_gc_growth_checks = 0;
static volatile unsigned long long v_gc_expansions = 0;

static void v_gc_maybe_expand_heap(size_t allocation_bytes) {
	// Small allocations check occasionally; a large one checks every time, before
	// it can push the heap past the next collection threshold.
	if (allocation_bytes < V_GC_GROWTH_CHECK_BYTES) {
		if (++v_gc_growth_checks & V_GC_GROWTH_CHECK_MASK) {
			return;
		}
	}
	if (!GC_is_init_called()) {
		return;
	}
	GC_word heap_size = 0;
	GC_word free_bytes = 0;
	GC_word unmapped = 0;
	GC_word since_gc = 0;
	GC_word total = 0;
	GC_get_heap_usage_safe(&heap_size, &free_bytes, &unmapped, &since_gc, &total);
	if (heap_size < V_GC_GROWTH_MIN_HEAP || free_bytes >= heap_size / 4) {
		return;
	}
	size_t step = (size_t)heap_size;
	if (step > V_GC_GROWTH_MAX_STEP) {
		step = V_GC_GROWTH_MAX_STEP;
	}
	if (GC_expand_hp(step) != 0) {
		++v_gc_expansions;
	}
}

unsigned long long v_gc_expansion_count(void) { return v_gc_expansions; }
#else
#define v_gc_maybe_expand_heap(size) ((void)0)
#endif

void* v_gc_malloc(size_t size) {
	void* p = GC_MALLOC(size);
	v_gc_maybe_expand_heap(size);
	return p;
}
void* v_gc_malloc_atomic(size_t size) {
	void* p = GC_MALLOC_ATOMIC(size);
	v_gc_maybe_expand_heap(size);
	return p;
}
void* v_gc_malloc_uncollectable(size_t size) {
	void* p = GC_MALLOC_UNCOLLECTABLE(size);
	v_gc_maybe_expand_heap(size);
	return p;
}
void* v_gc_realloc(void* ptr, size_t size) {
	void* p = GC_REALLOC(ptr, size);
	v_gc_maybe_expand_heap(size);
	return p;
}
void v_gc_free(void* ptr) { GC_FREE(ptr); }
void v_gc_init(void) { GC_INIT(); }
void v_gc_register_displacement(size_t offset) { GC_REGISTER_DISPLACEMENT(offset); }
void v_gc_set_warn_proc(GC_warn_proc cb) { GC_set_warn_proc(cb); }
void* v_gc_get_warn_proc(void) { return (void*)GC_get_warn_proc(); }
void v_gc_set_abort_func(GC_abort_func cb) { GC_set_abort_func(cb); }
void* v_gc_get_abort_func(void) { return (void*)GC_get_abort_func(); }
// Match the V C binding's `&char`; GC_abort_func accepts a const view of this buffer.
void v_gc_call_abort_func(void* cb, char* msg) { ((GC_abort_func)cb)(msg); }

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
