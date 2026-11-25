#include "initializers.h"
#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

typedef void (*v_bare_initializer)(int, char **, char **);
typedef void (*v_bare_finalizer)(void);

/* GNU ELF linkers synthesize these bounds when the corresponding input
 * sections exist. Weak references also support images without callbacks. */
extern v_bare_initializer __preinit_array_start[] __attribute__((weak));
extern v_bare_initializer __preinit_array_end[] __attribute__((weak));
extern v_bare_initializer __init_array_start[] __attribute__((weak));
extern v_bare_initializer __init_array_end[] __attribute__((weak));
extern v_bare_finalizer __fini_array_start[] __attribute__((weak));
extern v_bare_finalizer __fini_array_end[] __attribute__((weak));

static int v_bare_preinitialized, v_bare_startup_running;
static v_bare_finalizer v_bare_loader_fini;
extern void v_bare_tls_init(void *);

/* The ELF loader calls the main preinit array before DSO constructors. Those
 * constructors already need the process environment and V's TLS ownership. */
static void v_bare_preinitialize(int argc, char **argv, char **envp) {
    extern void v_freestanding_program_names(int32_t, uintptr_t);
    v_freestanding_program_names(argc, (uintptr_t)argv);
    extern char **environ;
    if (!v_bare_startup_running) {
        environ = envp;
        v_bare_tls_init((uintptr_t *)argv - 1);
    }
    v_bare_preinitialized = 1;
}
__attribute__((used, section(".preinit_array")))
static v_bare_initializer v_bare_preinit_entry = v_bare_preinitialize;

/* GNU's no-libc loader keeps its startup bump allocator: per-thread calls to
 * _dl_allocate_tls/_dl_deallocate_tls cannot reclaim arbitrary allocations.
 * Keep one loader-owned sizing probe, and initialize caller-owned TLS/DTV
 * mappings instead. Each mapping is then reclaimed by the ordinary join or
 * detached register-only munmap path. GNU ABI: sysdeps/generic/dl-dtv.h and
 * sysdeps/{x86_64,aarch64}/nptl/tls.h. These interfaces are GNU-private. */
#include <errno.h>
#include <sys/mman.h>
#ifdef V_BARE_DYNAMIC_LOADER
#define V_BARE_LOADER_ABI
#else
#define V_BARE_LOADER_ABI __attribute__((weak))
#endif
extern void *_dl_allocate_tls(void *) V_BARE_LOADER_ABI;
extern void *_dl_allocate_tls_init(void *, bool) V_BARE_LOADER_ABI;
extern void _dl_get_tls_static_info(size_t *, size_t *) V_BARE_LOADER_ABI;
union v_bare_gnu_dtv {
    size_t count;
    struct { void *value, *to_free; } pointer;
};
_Static_assert(sizeof(union v_bare_gnu_dtv) == 2 * sizeof(void *), "GNU ELF DTV ABI");
static int v_bare_loader_tls_lock;
static void *v_bare_loader_tls_probe;
static union v_bare_gnu_dtv *v_bare_get_dtv(void *pointer) {
#if defined(__x86_64__)
    return ((union v_bare_gnu_dtv **)pointer)[1];
#else
    return ((union v_bare_gnu_dtv **)pointer)[0];
#endif
}
void *v_bare_loader_tls_allocate(void **thread_pointer, size_t *mapping_size) {
    if (!_dl_allocate_tls || !_dl_allocate_tls_init || !_dl_get_tls_static_info) {
        errno = ENOTSUP;
        return NULL;
    }
    while (__atomic_exchange_n(&v_bare_loader_tls_lock, 1, __ATOMIC_ACQUIRE)) {}
    if (!v_bare_loader_tls_probe) v_bare_loader_tls_probe = _dl_allocate_tls(NULL);
    if (!v_bare_loader_tls_probe) {
        __atomic_store_n(&v_bare_loader_tls_lock, 0, __ATOMIC_RELEASE);
        errno = ENOMEM;
        return NULL;
    }
    size_t size, alignment;
    _dl_get_tls_static_info(&size, &alignment);
    size_t capacity = v_bare_get_dtv(v_bare_loader_tls_probe)[-1].count;
    if (alignment < 16) alignment = 16;
    size_t dtv_bytes = 0, total = 0;
    if ((alignment & (alignment - 1)) ||
        capacity > SIZE_MAX / sizeof(union v_bare_gnu_dtv) - 2) goto overflow;
    dtv_bytes = (capacity + 2) * sizeof(union v_bare_gnu_dtv);
    if (dtv_bytes > SIZE_MAX - alignment ||
        size > (SIZE_MAX - dtv_bytes - alignment) / 2) goto overflow;
    /* Reserving SIZE on both sides of TP covers either ELF TLS variant,
     * without guessing the loader's private sizeof(struct pthread). */
    total = dtv_bytes + size * 2 + alignment;
    void *mapping = mmap(NULL, total, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (mapping == MAP_FAILED) {
        __atomic_store_n(&v_bare_loader_tls_lock, 0, __ATOMIC_RELEASE);
        return NULL;
    }
    union v_bare_gnu_dtv *dtv = (union v_bare_gnu_dtv *)mapping + 1;
    dtv[-1].count = capacity;
    uintptr_t tp = ((uintptr_t)mapping + dtv_bytes + size + alignment - 1) & ~(alignment - 1);
#if defined(__x86_64__)
    /* GNU x86-64 needs both its ABI TCB pointer and pthread self pointer. */
    ((void **)tp)[0] = (void *)tp;
    ((void **)tp)[1] = dtv;
    ((void **)tp)[2] = (void *)tp;
#else
    ((void **)tp)[0] = dtv;
#endif
    _dl_allocate_tls_init((void *)tp, false);
    /* All supported modules are linked at process startup. A deferred TLS
     * module would require loader-owned allocations with different lifetimes. */
    for (size_t i = 1; i <= capacity; ++i) {
        uintptr_t value = (uintptr_t)dtv[i].pointer.value;
        if ((value && (value < (uintptr_t)mapping || value >= (uintptr_t)mapping + total)) ||
            dtv[i].pointer.to_free) {
            munmap(mapping, total);
            __atomic_store_n(&v_bare_loader_tls_lock, 0, __ATOMIC_RELEASE);
            errno = ENOTSUP;
            return NULL;
        }
    }
    *thread_pointer = (void *)tp;
    *mapping_size = total;
    __atomic_store_n(&v_bare_loader_tls_lock, 0, __ATOMIC_RELEASE);
    return mapping;
overflow:
    __atomic_store_n(&v_bare_loader_tls_lock, 0, __ATOMIC_RELEASE);
    errno = ENOMEM;
    return NULL;
}

void v_bare_run_initializers(int argc, char **argv, void (*loader_fini)(void)) {
    extern void v_freestanding_program_names(int32_t, uintptr_t);
    v_freestanding_program_names(argc, (uintptr_t)argv);
    int loaded = v_bare_preinitialized;
    v_bare_startup_running = 1;
    if (loaded) v_bare_loader_fini = loader_fini;
    char **envp = argv + argc + 1;
    extern char **environ;
    if (!loaded) environ = envp;
    if (!loaded && __preinit_array_start != NULL && __preinit_array_end != NULL) {
        for (v_bare_initializer *entry = __preinit_array_start;
             entry != __preinit_array_end; ++entry) {
            (*entry)(argc, argv, envp);
        }
    }
    if (__init_array_start != NULL && __init_array_end != NULL) {
        /* A shared object's constructor or a preinitializer may replace environ. */
        envp = environ;
        for (v_bare_initializer *entry = __init_array_start;
             entry != __init_array_end; ++entry) {
            (*entry)(argc, argv, envp);
        }
    }
}

void v_bare_run_finalizers(void) {
    static int finished;
    if (__atomic_exchange_n(&finished, 1, __ATOMIC_ACQ_REL)) return;
    extern void v_freestanding_exit_callbacks(void);
    extern void v_freestanding_tls_destructors(void);
    v_freestanding_tls_destructors();
    v_freestanding_exit_callbacks();
    if (v_bare_loader_fini) {
        v_bare_loader_fini();
    } else if (__fini_array_start != NULL && __fini_array_end != NULL) {
        v_bare_finalizer *entry = __fini_array_end;
        while (entry != __fini_array_start) {
            (*--entry)();
        }
    }
    /* Destructors may themselves buffer output; flush after the whole list. */
    extern void v_freestanding_stdio_flush(void);
    v_freestanding_stdio_flush();
}
