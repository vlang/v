// vgc_platform.h - Platform abstractions for V Garbage Collector
// Translated from Go's runtime GC (golang/go src/runtime/mgc*.go, malloc.go, mheap.go)

#ifndef VGC_PLATFORM_H
#define VGC_PLATFORM_H

#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h> // abort (span-registry-full hard fail in vgc_span_alloc)

// ============================================================
// Global / BSS data-segment roots
// V `__global` variables (e.g. rand.default_rng, and any user global holding a
// heap pointer) compile to C globals in the main executable's writable data
// segments. The collector must scan them as roots; otherwise an object reachable
// ONLY through a global is reclaimed, and code that later touches it (e.g. a
// module's at-exit _deinit) dereferences freed memory -> SIGSEGV. Boehm scans the
// data/bss segments for exactly this reason. We enumerate the main image's
// writable segments (data + bss live inside them: getsegmentdata's size is the
// segment vmsize, which spans bss) and hand the ranges to the conservative scan.
// (vgc's own globals in here — vgc_heap.allspans/caches/central — hold pointers to
// span STRUCTS allocated outside the GC arenas, which vgc_shade ignores, so
// scanning them cannot over-retain heap objects.)
// ============================================================
#if defined(__APPLE__)
  #include <mach-o/dyld.h>
  #include <mach-o/getsect.h>
  static inline int vgc_data_segments(uintptr_t* los, uintptr_t* his, int max_ranges) {
      const struct mach_header_64* mh = (const struct mach_header_64*)_dyld_get_image_header(0);
      if (mh == 0) return 0;
      static const char* const seg_names[] = {"__DATA", "__DATA_CONST", "__DATA_DIRTY"};
      int n = 0;
      for (int i = 0; i < 3 && n < max_ranges; i++) {
          unsigned long sz = 0;
          uint8_t* p = getsegmentdata(mh, seg_names[i], &sz);
          if (p != 0 && sz > 0) {
              los[n] = (uintptr_t)p;
              his[n] = (uintptr_t)p + (uintptr_t)sz;
              n++;
          }
      }
      return n;
  }
#elif defined(__linux__)
  // Linux: enumerate the MAIN executable's writable PT_LOAD segments via
  // dl_iterate_phdr (the ELF analog of mach-o getsegmentdata). p_memsz — NOT
  // p_filesz — is used so the range spans .bss (zero-initialized globals live
  // there, and a heap pointer parked in an uninitialized-at-link global must
  // still be scanned). dl_iterate_phdr's FIRST callback is always the main
  // program object (dlpi_name == ""), matching darwin's "image 0 only" scope:
  // V `__global`s and vgc's own globals compile into the main binary, and the
  // program's static deps (mbedtls, etc.) link in there too. Shared-library .data is not
  // scanned (same as darwin) — V places no GC roots there. dl_iterate_phdr takes
  // the loader lock, but the collector calls this with the world stopped during a
  // RARE backstop cycle, not from a signal handler, so it is safe here.
  #include <link.h>
  typedef struct { uintptr_t* los; uintptr_t* his; int max; int n; } vgc_seg_ctx;
  static int vgc__phdr_cb(struct dl_phdr_info* info, size_t size, void* data) {
      (void)size;
      vgc_seg_ctx* ctx = (vgc_seg_ctx*)data;
      for (int i = 0; i < info->dlpi_phnum && ctx->n < ctx->max; i++) {
          const ElfW(Phdr)* ph = &info->dlpi_phdr[i];
          if (ph->p_type != PT_LOAD) continue;
          if (!(ph->p_flags & PF_W)) continue; // writable (data + bss) only
          uintptr_t lo = (uintptr_t)info->dlpi_addr + (uintptr_t)ph->p_vaddr;
          uintptr_t hi = lo + (uintptr_t)ph->p_memsz; // memsz spans bss
          ctx->los[ctx->n] = lo;
          ctx->his[ctx->n] = hi;
          ctx->n++;
      }
      return 1; // stop after the first (main program) object
  }
  static inline int vgc_data_segments(uintptr_t* los, uintptr_t* his, int max_ranges) {
      vgc_seg_ctx ctx = { los, his, max_ranges, 0 };
      dl_iterate_phdr(vgc__phdr_cb, &ctx);
      return ctx.n;
  }
#else
  // Other platforms (Windows/BSD): not yet implemented.
  static inline int vgc_data_segments(uintptr_t* los, uintptr_t* his, int max_ranges) {
      (void)los; (void)his; (void)max_ranges;
      return 0;
  }
#endif

// ============================================================
// Thread-local storage
// ============================================================
#ifdef _WIN32
  #define VGC_TLS __declspec(thread)
#elif defined(__TINYC__)
  #define VGC_TLS __thread
#else
  #define VGC_TLS __thread
#endif

static VGC_TLS int _vgc_cache_idx = -1;

static inline int vgc_get_cache_idx(void) { return _vgc_cache_idx; }
static inline void vgc_set_cache_idx(int idx) { _vgc_cache_idx = idx; }

// DEBUG/BASELINE (-d vgc_coarse_alloc): a per-thread re-entrancy guard so the
// coarse allocator lock is taken only at the OUTERMOST malloc/free/realloc entry
// (realloc nests malloc+free). vgc_alloc_try_enter returns 1 iff this thread is
// not already inside the locked region (caller must then lock); vgc_alloc_exit
// clears it. Lets us serialize ALL mutator allocation to confirm a residual
// crash is an allocator data race + provide a known-correct baseline.
static __thread int _vgc_alloc_held = 0;
static inline int vgc_alloc_try_enter(void) { if (_vgc_alloc_held) return 0; _vgc_alloc_held = 1; return 1; }
static inline void vgc_alloc_exit(void) { _vgc_alloc_held = 0; }

// ============================================================
// Atomic operations
// ============================================================
#if defined(__TINYC__)
  // TCC: use __sync builtins
  #define vgc_atomic_load_u32(ptr) (*(volatile uint32_t*)(ptr))
  #define vgc_atomic_store_u32(ptr, val) do { *(volatile uint32_t*)(ptr) = (val); __sync_synchronize(); } while(0)
  #define vgc_atomic_load_u64(ptr) (*(volatile uint64_t*)(ptr))
  #define vgc_atomic_store_u64(ptr, val) do { *(volatile uint64_t*)(ptr) = (val); __sync_synchronize(); } while(0)
  #define vgc_atomic_add_u64(ptr, val) __sync_add_and_fetch((volatile uint64_t*)(ptr), (uint64_t)(val))
  #define vgc_atomic_sub_u64(ptr, val) __sync_sub_and_fetch((volatile uint64_t*)(ptr), (uint64_t)(val))
  #define vgc_atomic_add_u32(ptr, val) __sync_add_and_fetch((volatile uint32_t*)(ptr), (uint32_t)(val))
  #define vgc_atomic_cas_u32(ptr, expected, desired) __sync_bool_compare_and_swap((volatile uint32_t*)(ptr), *(expected), (desired))
  #define vgc_atomic_exchange_u32(ptr, val) __sync_lock_test_and_set((volatile uint32_t*)(ptr), (val))
  #define vgc_atomic_fence() __sync_synchronize()
#elif defined(_MSC_VER)
  #include <intrin.h>
  #pragma intrinsic(_InterlockedCompareExchange, _InterlockedExchange, _InterlockedExchangeAdd64)
  static inline uint32_t vgc_atomic_load_u32_fn(volatile uint32_t* p) { uint32_t v = *p; _ReadBarrier(); return v; }
  static inline void vgc_atomic_store_u32_fn(volatile uint32_t* p, uint32_t v) { _WriteBarrier(); *p = v; }
  static inline uint64_t vgc_atomic_load_u64_fn(volatile uint64_t* p) { uint64_t v = *p; _ReadBarrier(); return v; }
  static inline void vgc_atomic_store_u64_fn(volatile uint64_t* p, uint64_t v) { _WriteBarrier(); *p = v; }
  #define vgc_atomic_load_u32(ptr) vgc_atomic_load_u32_fn((volatile uint32_t*)(ptr))
  #define vgc_atomic_store_u32(ptr, val) vgc_atomic_store_u32_fn((volatile uint32_t*)(ptr), (val))
  #define vgc_atomic_load_u64(ptr) vgc_atomic_load_u64_fn((volatile uint64_t*)(ptr))
  #define vgc_atomic_store_u64(ptr, val) vgc_atomic_store_u64_fn((volatile uint64_t*)(ptr), (val))
  #define vgc_atomic_add_u64(ptr, val) _InterlockedExchangeAdd64((volatile int64_t*)(ptr), (int64_t)(val))
  #define vgc_atomic_sub_u64(ptr, val) _InterlockedExchangeAdd64((volatile int64_t*)(ptr), -(int64_t)(val))
  #define vgc_atomic_add_u32(ptr, val) _InterlockedExchangeAdd((volatile long*)(ptr), (long)(val))
  #define vgc_atomic_cas_u32(ptr, expected, desired) (_InterlockedCompareExchange((volatile long*)(ptr), (long)(desired), (long)*(expected)) == (long)*(expected))
  #define vgc_atomic_exchange_u32(ptr, val) (uint32_t)_InterlockedExchange((volatile long*)(ptr), (long)(val))
  #define vgc_atomic_fence() _ReadWriteBarrier()
#else
  // GCC/Clang
  #define vgc_atomic_load_u32(ptr) __atomic_load_n((volatile uint32_t*)(ptr), __ATOMIC_ACQUIRE)
  #define vgc_atomic_store_u32(ptr, val) __atomic_store_n((volatile uint32_t*)(ptr), (val), __ATOMIC_RELEASE)
  #define vgc_atomic_load_u64(ptr) __atomic_load_n((volatile uint64_t*)(ptr), __ATOMIC_ACQUIRE)
  #define vgc_atomic_store_u64(ptr, val) __atomic_store_n((volatile uint64_t*)(ptr), (val), __ATOMIC_RELEASE)
  #define vgc_atomic_add_u64(ptr, val) __atomic_add_fetch((volatile uint64_t*)(ptr), (uint64_t)(val), __ATOMIC_ACQ_REL)
  #define vgc_atomic_sub_u64(ptr, val) __atomic_sub_fetch((volatile uint64_t*)(ptr), (uint64_t)(val), __ATOMIC_ACQ_REL)
  #define vgc_atomic_add_u32(ptr, val) __atomic_add_fetch((volatile uint32_t*)(ptr), (uint32_t)(val), __ATOMIC_ACQ_REL)
  #define vgc_atomic_cas_u32(ptr, expected, desired) __atomic_compare_exchange_n((volatile uint32_t*)(ptr), (uint32_t*)(expected), (uint32_t)(desired), 0, __ATOMIC_ACQ_REL, __ATOMIC_ACQUIRE)
  #define vgc_atomic_exchange_u32(ptr, val) __atomic_exchange_n((volatile uint32_t*)(ptr), (uint32_t)(val), __ATOMIC_ACQ_REL)
  #define vgc_atomic_fence() __atomic_thread_fence(__ATOMIC_SEQ_CST)
#endif

// ============================================================
// OS memory allocation
// ============================================================
#ifdef _WIN32
  #ifndef WIN32_LEAN_AND_MEAN
    #define WIN32_LEAN_AND_MEAN
  #endif
  #include <windows.h>
  static inline void* vgc_os_alloc(size_t size) {
      return VirtualAlloc(NULL, size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
  }
  static inline void vgc_os_free(void* ptr, size_t size) {
      (void)size;
      VirtualFree(ptr, 0, MEM_RELEASE);
  }
  static inline void vgc_os_decommit(void* ptr, size_t size) {
      VirtualFree(ptr, size, MEM_DECOMMIT);
  }
  static inline int vgc_num_cpus(void) {
      DWORD count = GetActiveProcessorCount(ALL_PROCESSOR_GROUPS);
      return count > 0 ? (int)count : 1;
  }
#else
  #include <sys/mman.h>
  #include <unistd.h>
  static inline void* vgc_os_alloc(size_t size) {
      void* p = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
      return (p == MAP_FAILED) ? NULL : p;
  }
  static inline void vgc_os_free(void* ptr, size_t size) {
      munmap(ptr, size);
  }
  static inline void vgc_os_decommit(void* ptr, size_t size) {
      madvise(ptr, size, MADV_DONTNEED);
  }
  static inline int vgc_num_cpus(void) {
      long count = sysconf(_SC_NPROCESSORS_ONLN);
      return count > 0 ? (int)count : 1;
  }
#endif

// ============================================================
// Stack pointer / frame address
// ============================================================
// vgc_real_sp reads the ACTUAL stack pointer register. This matters wherever the
// value is used as the low bound of a conservative root scan: __builtin_frame_address(0)
// returns the FRAME POINTER, which sits ABOVE this frame's locals (incl. the
// setjmp jmp_buf that holds the spilled callee-saved registers). A range anchored
// at the frame pointer would EXCLUDE that spill area, so a live mutator root held
// only in a callee-saved register — routine under -Os — would never be scanned and
// would be reclaimed while live. The real SP covers the whole current frame.
#if defined(__GNUC__) || defined(__clang__)
  __attribute__((always_inline)) static inline uintptr_t vgc_real_sp(void) {
      uintptr_t sp;
  #if defined(__aarch64__) || defined(__arm64__)
      __asm__ __volatile__("mov %0, sp" : "=r"(sp));
  #elif defined(__x86_64__)
      __asm__ __volatile__("mov %%rsp, %0" : "=r"(sp));
  #elif defined(__i386__)
      __asm__ __volatile__("mov %%esp, %0" : "=r"(sp));
  #else
      sp = (uintptr_t)__builtin_frame_address(0);
  #endif
      return sp;
  }
#else
  static inline uintptr_t vgc_real_sp(void) { return (uintptr_t)__builtin_frame_address(0); }
#endif

#if defined(_MSC_VER)
  #include <intrin.h>
  static inline void* vgc_get_sp(void) { return _AddressOfReturnAddress(); }
#else
  static inline void* vgc_get_sp(void) { return (void*)vgc_real_sp(); }
#endif

// ============================================================
// Stack bounds
// ============================================================
#if defined(__APPLE__)
  #include <pthread.h>
  static inline int vgc_get_stack_bounds(uintptr_t* lo, uintptr_t* hi) {
      void* stack_hi = pthread_get_stackaddr_np(pthread_self());
      size_t stack_size = pthread_get_stacksize_np(pthread_self());
      if (stack_hi == NULL || stack_size == 0) return 0;
      *hi = (uintptr_t)stack_hi;
      *lo = *hi - stack_size;
      return 1;
  }
#elif defined(__linux__) || defined(__ANDROID__)
  #include <pthread.h>
  static inline int vgc_get_stack_bounds(uintptr_t* lo, uintptr_t* hi) {
      pthread_attr_t attr;
      if (pthread_getattr_np(pthread_self(), &attr) != 0) return 0;
      void* stack_lo = NULL;
      size_t stack_size = 0;
      int ok = pthread_attr_getstack(&attr, &stack_lo, &stack_size) == 0 && stack_lo != NULL && stack_size != 0;
      pthread_attr_destroy(&attr);
      if (!ok) return 0;
      *lo = (uintptr_t)stack_lo;
      *hi = *lo + stack_size;
      return 1;
  }
#elif defined(__FreeBSD__) || defined(__DragonFly__) || defined(__NetBSD__) || defined(__OpenBSD__)
  #include <pthread.h>
  static inline int vgc_get_stack_bounds(uintptr_t* lo, uintptr_t* hi) {
      pthread_attr_t attr;
      if (pthread_attr_init(&attr) != 0) return 0;
      if (pthread_attr_get_np(pthread_self(), &attr) != 0) {
          pthread_attr_destroy(&attr);
          return 0;
      }
      void* stack_lo = NULL;
      size_t stack_size = 0;
      int ok = pthread_attr_getstack(&attr, &stack_lo, &stack_size) == 0 && stack_lo != NULL && stack_size != 0;
      pthread_attr_destroy(&attr);
      if (!ok) return 0;
      *lo = (uintptr_t)stack_lo;
      *hi = *lo + stack_size;
      return 1;
  }
#else
  static inline int vgc_get_stack_bounds(uintptr_t* lo, uintptr_t* hi) {
      (void)lo;
      (void)hi;
      return 0;
  }
#endif

// ============================================================
// Bitmap operations (for mark/alloc bitmaps)
// ============================================================
static inline int vgc_bitmap_get(const uint8_t* bits, uint32_t idx) {
    return (bits[idx >> 3] >> (idx & 7)) & 1;
}

static inline void vgc_bitmap_set(uint8_t* bits, uint32_t idx) {
    bits[idx >> 3] |= (uint8_t)(1u << (idx & 7));
}

static inline void vgc_bitmap_clear(uint8_t* bits, uint32_t idx) {
    bits[idx >> 3] &= (uint8_t)~(1u << (idx & 7));
}

// Returns 1 if bit was already set, 0 if newly set
static inline int vgc_bitmap_test_and_set(uint8_t* bits, uint32_t idx) {
    uint8_t mask = (uint8_t)(1u << (idx & 7));
    uint8_t* byte_ptr = &bits[idx >> 3];
    if (*byte_ptr & mask) return 1;
    *byte_ptr |= mask;
    return 0;
}

// Popcount on a byte - count number of set bits
static inline int vgc_popcount8(uint8_t x) {
    x = x - ((x >> 1) & 0x55);
    x = (x & 0x33) + ((x >> 2) & 0x33);
    return (x + (x >> 4)) & 0x0f;
}

// ============================================================
// Size class tables (translated from Go's runtime/sizeclasses.go)
// 68 classes (0 = unused, 1-67 = actual size classes)
// ============================================================

// Object sizes per class
static const uint32_t vgc_class_sizes[68] = {
    0,     8,     16,    24,    32,    48,    64,    80,
    96,    112,   128,   144,   160,   176,   192,   208,
    224,   240,   256,   288,   320,   352,   384,   416,
    448,   480,   512,   576,   640,   704,   768,   896,
    1024,  1152,  1280,  1408,  1536,  1792,  2048,  2304,
    2688,  3072,  3200,  3456,  4096,  4864,  5376,  6144,
    6528,  6784,  6912,  8192,  9472,  9728,  10240, 10880,
    12288, 13568, 14336, 16384, 18432, 19072, 20480, 21760,
    24576, 27264, 28672, 32768
};

// Pages per span for each class
static const uint32_t vgc_class_npages[68] = {
    0, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 2, 1, 2, 1, 2,
    1, 3, 2, 3, 1, 3, 2, 3,
    4, 5, 6, 1, 7, 6, 5, 4,
    3, 5, 7, 2, 9, 7, 5, 8,
    3, 10, 7, 4
};

// Objects per span for each class
static const uint32_t vgc_class_nobjs[68] = {
    0,    1024, 512,  341,  256,  170,  128,  102,
    85,   73,   64,   56,   51,   46,   42,   39,
    36,   34,   32,   28,   25,   23,   21,   19,
    18,   17,   16,   14,   12,   11,   10,   9,
    8,    7,    6,    11,   5,    9,    4,    7,
    3,    8,    5,    7,    2,    5,    3,    4,
    5,    6,    7,    1,    6,    5,    4,    3,
    2,    3,    4,    1,    4,    3,    2,    3,
    1,    3,    2,    1
};

// Size-to-class lookup tables (computed at init)
static uint8_t vgc_s2c8[129];   // sizes 1..1024: index = (size+7)/8
static uint8_t vgc_s2c128[249]; // sizes 1025..32768: index = (size-1024+127)/128

static inline uint32_t vgc_get_class_size(int cls) { return vgc_class_sizes[cls]; }
static inline uint32_t vgc_get_class_npages(int cls) { return vgc_class_npages[cls]; }
static inline uint32_t vgc_get_class_nobjs(int cls) { return vgc_class_nobjs[cls]; }

static void vgc_init_size_tables(void) {
    int cls = 1;
    for (int i = 0; i < 129; i++) {
        uint32_t size = (uint32_t)(i + 1) * 8;
        while (cls < 67 && vgc_class_sizes[cls] < size) {
            cls++;
        }
        vgc_s2c8[i] = (uint8_t)cls;
    }
    cls = 33;
    for (int i = 0; i < 249; i++) {
        uint32_t size = 1024 + (uint32_t)(i + 1) * 128;
        while (cls < 67 && vgc_class_sizes[cls] < size) {
            cls++;
        }
        vgc_s2c128[i] = (uint8_t)cls;
    }
}

// Get size class for a given allocation size
static inline uint8_t vgc_size_class(uint32_t size) {
    if (size == 0) return 0;
    if (size <= 1024) {
        return vgc_s2c8[(size + 7) >> 3];
    }
    if (size <= 32768) {
        return vgc_s2c128[(size - 1024 + 127) >> 7];
    }
    return 0; // large allocation
}

// ============================================================
// Pause / yield for spinlocks
// ============================================================
#if defined(__x86_64__) || defined(__i386__) || defined(_M_X64) || defined(_M_IX86)
  #define vgc_cpu_pause() __asm__ __volatile__("pause")
#elif defined(__aarch64__) || defined(_M_ARM64)
  #define vgc_cpu_pause() __asm__ __volatile__("yield")
#else
  #define vgc_cpu_pause() ((void)0)
#endif

#ifdef _WIN32
  #define vgc_thread_yield() SwitchToThread()
#else
  #include <sched.h>
  #define vgc_thread_yield() sched_yield()
#endif

// ============================================================
// Simple mutex (adaptive spinlock with backoff)
// ============================================================
static inline void vgc_mutex_lock(volatile uint32_t* lock) {
    for (int spin = 0; ; spin++) {
        uint32_t expected = 0;
        if (vgc_atomic_cas_u32(lock, &expected, 1)) {
            return;
        }
        if (spin < 16) {
            vgc_cpu_pause();
        } else if (spin < 32) {
            for (int i = 0; i < spin; i++) vgc_cpu_pause();
        } else {
            vgc_thread_yield();
        }
    }
}

static inline void vgc_mutex_unlock(volatile uint32_t* lock) {
    vgc_atomic_store_u32(lock, 0);
}

// ============================================================
// Arena address index for O(1) pointer-to-arena lookup
// Divides address space into 1GB chunks, maps to arena index.
// ============================================================
// Granularity MUST match the arena size (vgc_arena_size = 64MB = 1<<26): with the
// old 1GB shift, one map chunk spanned ~16 arenas but held a single arena index,
// so 15/16 of addresses in a populated chunk resolved to the WRONG arena and
// forced vgc_find_span's linear fallback on nearly every scanned heap-range word
// (catastrophic mark slowdown). At 64MB granularity each chunk maps to ~one arena,
// so the O(1) hint is right for the body of every arena and the linear fallback
// only fires for the rare unaligned boundary chunk shared by two adjacent arenas.
#define VGC_ADDR_SHIFT 26  // 64MB chunks (== vgc_arena_size)
#define VGC_ADDR_MAP_SIZE 65536 // 65536 * 64MB = 4TB address coverage (same as old 4096*1GB)

// Maps (address >> 30) -> arena_index+1 (0 = not mapped)
static uint16_t vgc_addr_map[VGC_ADDR_MAP_SIZE];

static inline void vgc_addr_map_register(uintptr_t base, size_t size, int arena_idx) {
    uintptr_t lo = base >> VGC_ADDR_SHIFT;
    uintptr_t hi = (base + size - 1) >> VGC_ADDR_SHIFT;
    for (uintptr_t i = lo; i <= hi && i < VGC_ADDR_MAP_SIZE; i++) {
        vgc_addr_map[i] = (uint16_t)(arena_idx + 1);
    }
}

// Returns arena index or -1
static inline int vgc_addr_to_arena(uintptr_t addr) {
    uintptr_t idx = addr >> VGC_ADDR_SHIFT;
    if (idx >= VGC_ADDR_MAP_SIZE) return -1;
    int v = vgc_addr_map[idx];
    return v ? v - 1 : -1;
}

// ============================================================
// Thread creation (for GC workers - avoids V's spawn which
// causes import cycles with builtin -> sync.threads -> builtin)
// ============================================================
typedef void (*vgc_thread_fn)(void);

#ifdef _WIN32
static inline void vgc_start_thread(vgc_thread_fn fn) {
    CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)fn, NULL, 0, NULL);
}
#else
#include <pthread.h>
static void* _vgc_thread_trampoline(void* arg) {
    vgc_thread_fn fn = (vgc_thread_fn)arg;
    fn();
    return NULL;
}
static inline void vgc_start_thread(vgc_thread_fn fn) {
    pthread_t tid;
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    pthread_create(&tid, &attr, _vgc_thread_trampoline, (void*)fn);
    pthread_attr_destroy(&attr);
}
#endif

// ============================================================
// Thread-exit deregistration.
// A pthread-key destructor runs in normal thread context when a
// thread exits and calls back into the V GC, which marks the cache
// slot unregistered and reclaims it. This is what lets stop-the-world
// count only LIVE mutators (not every thread that ever registered)
// and keeps the fixed cache array from being exhausted by churn.
// ============================================================
#include <setjmp.h>
// Run a GC cycle with the COLLECTOR thread's callee-saved registers spilled
// onto this frame and its stack range recorded to include them. The triggering
// thread is typically mid-mutator-loop (e.g. holding `last` in a register), and
// its own root scan would otherwise miss those registers -> swept live data.
// `buf` stays alive across the whole collection (mark+sweep) since the body
// runs nested here. vgc_gc_start must NOT re-record the collector's own range.
extern void vgc_gc_start(void);
static inline void vgc_run_gc_spilled(uintptr_t* lo, uintptr_t* hi, uintptr_t base) {
    jmp_buf buf;
    setjmp(buf);
    // Anchor at the REAL stack pointer (not the frame pointer): `buf` holding the
    // spilled callee-saved registers lives BELOW the frame pointer, so a range
    // anchored there would miss it (see vgc_real_sp). Clamp to <= &buf so the spill
    // area is covered even on a target without an SP-read primitive.
    uintptr_t sp = vgc_real_sp();
    if ((uintptr_t)&buf < sp) { sp = (uintptr_t)&buf; }
    if (base >= sp) { *lo = sp; *hi = base; } else { *lo = base; *hi = sp; }
    vgc_gc_start();
    __asm__ __volatile__("" : : "r"(&buf) : "memory");
}

// Park at a safepoint with callee-saved registers spilled onto THIS frame, so
// the GC's conservative stack scan sees roots that live only in registers (a
// hot loop variable like `last` is often kept in a callee-saved register and
// never written to the stack). `buf` is kept alive across the spin so the
// recorded [lo,hi] range, which covers this frame, includes the spilled regs.
static inline void vgc_park_spill(uint32_t* stop_flag, uint32_t* stopped_count,
                                  uint32_t* my_stopped, uintptr_t* range_lo,
                                  uintptr_t* range_hi, uintptr_t stack_base) {
    jmp_buf buf;
    setjmp(buf);
    // Real SP, not the frame pointer: the spilled-register jmp_buf lives below the
    // frame pointer and must be inside the scanned range (see vgc_real_sp).
    uintptr_t sp = vgc_real_sp();
    if ((uintptr_t)&buf < sp) { sp = (uintptr_t)&buf; }
    if (stack_base >= sp) { *range_lo = sp; *range_hi = stack_base; }
    else { *range_lo = stack_base; *range_hi = sp; }
    vgc_atomic_store_u32(my_stopped, 1);
    vgc_atomic_add_u32(stopped_count, 1);
    while (vgc_atomic_load_u32(stop_flag) != 0) { vgc_cpu_pause(); }
    vgc_atomic_store_u32(my_stopped, 0);
    __asm__ __volatile__("" : : "r"(&buf) : "memory");
}

#ifndef _WIN32
extern void vgc_thread_exit_cb(int idx);
static pthread_key_t _vgc_exit_key;
static pthread_once_t _vgc_exit_once = PTHREAD_ONCE_INIT;
static void _vgc_exit_destructor(void* val) {
    int idx = (int)(intptr_t)val - 1;
    if (idx >= 0) vgc_thread_exit_cb(idx);
}
static void _vgc_exit_key_init(void) {
    pthread_key_create(&_vgc_exit_key, _vgc_exit_destructor);
}
static inline void vgc_install_thread_exit(int idx) {
    pthread_once(&_vgc_exit_once, _vgc_exit_key_init);
    pthread_setspecific(_vgc_exit_key, (void*)(intptr_t)(idx + 1));
}
#else
static inline void vgc_install_thread_exit(int idx) { (void)idx; }
#endif

// ============================================================
// OS-level stop-the-world (darwin/mach) — the CORRECT STW for the minimal
// backstop collector. Unlike cooperative alloc-path safepoints, mach
// thread_suspend stops a thread in ANY state (incl. blocked in a syscall or a
// tight non-allocating loop), and thread_get_state yields its SP + full
// register file for root scanning. The suspend_world + stw_root_scan
// primitives were validated standalone before integration.
// ============================================================
#if defined(__APPLE__)
  #include <mach/mach.h>
  #include <mach/thread_act.h>
  static inline uint32_t vgc_thread_self_port(void) {
      return (uint32_t)pthread_mach_thread_np(pthread_self());
  }
  // thread_suspend() increments the suspend count but is ASYNCHRONOUS: if the target
  // is running on another core it is not necessarily descheduled by the time this
  // returns, and a subsequent thread_get_state() can observe a still-advancing /
  // stale frame -> the stack range computed from that SP misses the thread's true
  // (deeper) frames -> a live root there is not scanned -> swept while live. So after
  // suspending, spin-read the register state until it is STABLE (two consecutive
  // reads agree on SP+PC), which means the thread is genuinely off-CPU and its frame
  // is frozen for the upcoming root scan.
  static inline void vgc_suspend_thread(uint32_t t) {
      if (thread_suspend((thread_act_t)t) != KERN_SUCCESS) return;
      uintptr_t prev_sp = 0, prev_pc = 0;
      for (int i = 0; i < 200000; i++) {
        #if defined(__arm64__) || defined(__aarch64__)
          arm_thread_state64_t st;
          mach_msg_type_number_t n = ARM_THREAD_STATE64_COUNT;
          if (thread_get_state((thread_act_t)t, ARM_THREAD_STATE64, (thread_state_t)&st, &n) != KERN_SUCCESS) return;
          uintptr_t sp = (uintptr_t)arm_thread_state64_get_sp(st);
          uintptr_t pc = (uintptr_t)arm_thread_state64_get_pc(st);
        #elif defined(__x86_64__)
          x86_thread_state64_t st;
          mach_msg_type_number_t n = x86_THREAD_STATE64_COUNT;
          if (thread_get_state((thread_act_t)t, x86_THREAD_STATE64, (thread_state_t)&st, &n) != KERN_SUCCESS) return;
          uintptr_t sp = (uintptr_t)st.__rsp;
          uintptr_t pc = (uintptr_t)st.__rip;
        #else
          return;
        #endif
          if (i > 0 && sp == prev_sp && pc == prev_pc) return; // frozen => stopped
          prev_sp = sp;
          prev_pc = pc;
      }
  }
  static inline void vgc_resume_thread(uint32_t t) { thread_resume((thread_act_t)t); }
  // Fill *sp_out + up to `max` callee/general register values; return count.
  static inline int vgc_thread_regs(uint32_t t, uintptr_t* sp_out, uintptr_t* regs, int max) {
    #if defined(__arm64__) || defined(__aarch64__)
      arm_thread_state64_t st;
      mach_msg_type_number_t n = ARM_THREAD_STATE64_COUNT;
      if (thread_get_state((thread_act_t)t, ARM_THREAD_STATE64, (thread_state_t)&st, &n) != KERN_SUCCESS) return 0;
      *sp_out = (uintptr_t)arm_thread_state64_get_sp(st);
      int c = 0;
      for (int i = 0; i < 29 && c < max; i++) regs[c++] = (uintptr_t)st.__x[i];
      if (c < max) regs[c++] = (uintptr_t)arm_thread_state64_get_fp(st);
      if (c < max) regs[c++] = (uintptr_t)arm_thread_state64_get_lr(st);
      return c;
    #elif defined(__x86_64__)
      x86_thread_state64_t st;
      mach_msg_type_number_t n = x86_THREAD_STATE64_COUNT;
      if (thread_get_state((thread_act_t)t, x86_THREAD_STATE64, (thread_state_t)&st, &n) != KERN_SUCCESS) return 0;
      *sp_out = (uintptr_t)st.__rsp;
      uintptr_t r[15] = { st.__rax, st.__rbx, st.__rcx, st.__rdx, st.__rsi, st.__rdi,
                          st.__rbp, st.__r8, st.__r9, st.__r10, st.__r11, st.__r12,
                          st.__r13, st.__r14, st.__r15 };
      int c = 0;
      for (int i = 0; i < 15 && c < max; i++) regs[c++] = r[i];
      return c;
    #else
      (void)t; (void)sp_out; (void)regs; (void)max; return 0;
    #endif
  }
#elif defined(__linux__)
  // ----------------------------------------------------------------------------
  // Linux OS-level stop-the-world via signal-based suspension.
  //
  // Unlike mach, Linux cannot read another thread's registers from outside it.
  // The model (the same one Boehm/Go-on-Linux use): the collector sends a private
  // real-time signal to each target thread; the target's handler captures its OWN
  // interrupted register file + SP from the signal `ucontext` into a per-thread
  // slot, publishes an ACK, then parks (spins) inside the handler until released.
  //
  // THE ACK IS THE STOP-SETTLE (the Linux analog of the mach thread_suspend async
  // fix): tgkill() returns BEFORE the handler runs, so the collector must spin on
  // `acked` before it trusts the captured SP/regs. Once acked==1 the target is
  // provably stopped at a known point (inside its handler) and its captured frame
  // is frozen for the whole scan window — every non-self mutator is parked at once
  // while the collector scans + marks + sweeps, then resume releases them all.
  //
  // The `mach_port u32` cache field carries the kernel TID (gettid) on Linux; it
  // is the signal target (via tgkill) and the per-thread slot key.
  // ----------------------------------------------------------------------------
  #include <signal.h>
  #include <pthread.h>
  #include <ucontext.h>
  #include <unistd.h>
  #include <sys/syscall.h>

  // Private suspend signal. SIGRTMIN+6 mirrors Boehm's default GC_SIG_SUSPEND so
  // it does not clobber an application's SIGUSR1/SIGUSR2 handlers. (SIGRTMIN is a
  // runtime value on glibc/musl, so it is only ever used at runtime, never in a
  // constant context.)
  #ifndef VGC_SUSPEND_SIGNAL
    #define VGC_SUSPEND_SIGNAL (SIGRTMIN + 6)
  #endif

  #define VGC_LINUX_MAXTH 128 // >= caches[64]; one slot per simultaneously-parked thread
  typedef struct {
      volatile uint32_t tid;     // target kernel tid (0 = free slot)
      volatile uint32_t acked;   // handler has captured regs and is parked
      volatile uint32_t release; // collector asks the handler to leave
      volatile uintptr_t sp;     // interrupted stack pointer (from ucontext)
      uintptr_t regs[32];        // interrupted general registers (conservative roots)
      volatile int nregs;
  } vgc_lin_susp;
  static vgc_lin_susp vgc_lin_slots[VGC_LINUX_MAXTH];

  static inline uint32_t vgc_lin_gettid(void) {
      return (uint32_t)syscall(SYS_gettid); // async-signal-safe
  }

  // Async-signal-safe: only syscall(gettid), volatile loads/stores, register copy,
  // and sched_yield while parking. No malloc / no locks / no stdio.
  static void vgc_suspend_handler(int sig, siginfo_t* si, void* uctx) {
      (void)sig; (void)si;
      uint32_t me = vgc_lin_gettid();
      vgc_lin_susp* s = 0;
      for (int i = 0; i < VGC_LINUX_MAXTH; i++) {
          if (__atomic_load_n(&vgc_lin_slots[i].tid, __ATOMIC_ACQUIRE) == me) { s = &vgc_lin_slots[i]; break; }
      }
      if (s == 0) return; // spurious / not a target of this cycle
      ucontext_t* uc = (ucontext_t*)uctx;
      int c = 0;
    #if defined(__aarch64__)
      for (int i = 0; i < 31 && c < 32; i++) s->regs[c++] = (uintptr_t)uc->uc_mcontext.regs[i];
      s->sp = (uintptr_t)uc->uc_mcontext.sp;
    #elif defined(__x86_64__)
      for (int i = 0; i < NGREG && c < 32; i++) s->regs[c++] = (uintptr_t)uc->uc_mcontext.gregs[i];
      s->sp = (uintptr_t)uc->uc_mcontext.gregs[REG_RSP];
    #else
      s->sp = (uintptr_t)__builtin_frame_address(0); // best-effort fallback
    #endif
      s->nregs = c;
      __atomic_store_n(&s->acked, 1, __ATOMIC_RELEASE); // settle/ack: SP+regs now trustworthy
      while (__atomic_load_n(&s->release, __ATOMIC_ACQUIRE) == 0) { sched_yield(); }
      __atomic_store_n(&s->acked, 0, __ATOMIC_RELEASE); // confirm departure before slot reuse
  }

  static pthread_once_t _vgc_sig_once = PTHREAD_ONCE_INIT;
  static void _vgc_sig_install(void) {
      struct sigaction sa;
      memset(&sa, 0, sizeof(sa));
      sa.sa_sigaction = vgc_suspend_handler;
      sa.sa_flags = SA_SIGINFO | SA_RESTART; // SA_RESTART so V's blocking syscalls resume after the park
      sigfillset(&sa.sa_mask); // no nested signals while parking
      sigaction(VGC_SUSPEND_SIGNAL, &sa, 0);
  }

  // Called at every thread registration. Installs the process-wide handler once,
  // ensures the suspend signal is UNBLOCKED in this thread (new threads inherit
  // the creator's mask), and returns this thread's tid as the "port".
  static inline uint32_t vgc_thread_self_port(void) {
      pthread_once(&_vgc_sig_once, _vgc_sig_install);
      sigset_t set;
      sigemptyset(&set);
      sigaddset(&set, VGC_SUSPEND_SIGNAL);
      pthread_sigmask(SIG_UNBLOCK, &set, 0);
      return vgc_lin_gettid();
  }

  static inline vgc_lin_susp* vgc_lin_find(uint32_t t) {
      for (int i = 0; i < VGC_LINUX_MAXTH; i++) {
          if (__atomic_load_n(&vgc_lin_slots[i].tid, __ATOMIC_ACQUIRE) == t) return &vgc_lin_slots[i];
      }
      return 0;
  }

  // Suspend = claim a slot, signal the target, and SPIN UNTIL ACK (the settle).
  // Only one collector runs the suspend loop at a time (STW gc_phase guard), so
  // slot claiming is single-threaded against other suspends; the handler reads it
  // concurrently, guarded by atomics.
  static inline void vgc_suspend_thread(uint32_t t) {
      if (t == 0) return;
      vgc_lin_susp* s = 0;
      for (int i = 0; i < VGC_LINUX_MAXTH; i++) {
          if (__atomic_load_n(&vgc_lin_slots[i].tid, __ATOMIC_ACQUIRE) == 0) { s = &vgc_lin_slots[i]; break; }
      }
      if (s == 0) return; // table full (should not happen: MAXTH >= caches)
      s->acked = 0; s->release = 0; s->sp = 0; s->nregs = 0;
      __atomic_store_n(&s->tid, t, __ATOMIC_RELEASE); // publish key before signaling
      if (syscall(SYS_tgkill, getpid(), (int)t, VGC_SUSPEND_SIGNAL) != 0) {
          __atomic_store_n(&s->tid, 0, __ATOMIC_RELEASE); // target gone: release the slot
          return;
      }
      for (int i = 0; i < 200000; i++) {
          if (__atomic_load_n(&s->acked, __ATOMIC_ACQUIRE) != 0) return; // settled
          sched_yield();
      }
      // Backstop expired (signal lost / thread dying): leave acked==0 so
      // vgc_thread_regs returns 0 and the collector safely skips this thread,
      // matching darwin's thread_get_state failure path.
  }

  static inline void vgc_resume_thread(uint32_t t) {
      vgc_lin_susp* s = vgc_lin_find(t);
      if (s == 0) return;
      __atomic_store_n(&s->release, 1, __ATOMIC_RELEASE);
      for (int i = 0; i < 200000; i++) { // wait for the handler to leave before freeing
          if (__atomic_load_n(&s->acked, __ATOMIC_ACQUIRE) == 0) break;
          sched_yield();
      }
      __atomic_store_n(&s->tid, 0, __ATOMIC_RELEASE); // free the slot
  }

  // Read the SP + registers the handler captured at suspend. The thread has been
  // parked since suspend, so the captured frame is frozen — this is even tighter
  // than darwin's re-read (no chance of an advancing frame). Returns 0 if the
  // thread never acked (skipped safely by the collector).
  static inline int vgc_thread_regs(uint32_t t, uintptr_t* sp_out, uintptr_t* regs, int max) {
      vgc_lin_susp* s = vgc_lin_find(t);
      if (s == 0 || __atomic_load_n(&s->acked, __ATOMIC_ACQUIRE) == 0) return 0;
      *sp_out = s->sp;
      int n = s->nregs < max ? s->nregs : max;
      for (int i = 0; i < n; i++) regs[i] = s->regs[i];
      return n;
  }
#else
  // Other platforms (Windows/BSD): signal/mach STW not yet ported. Return 0 so the
  // collector detects "no OS-suspend available" and falls back safely.
  static inline uint32_t vgc_thread_self_port(void) { return 0; }
  static inline void vgc_suspend_thread(uint32_t t) { (void)t; }
  static inline void vgc_resume_thread(uint32_t t) { (void)t; }
  static inline int vgc_thread_regs(uint32_t t, uintptr_t* sp_out, uintptr_t* regs, int max) {
      (void)t; (void)sp_out; (void)regs; (void)max; return 0;
  }
#endif

// ============================================================
// Minimal stderr write helpers — always available. Used by the loud
// span-registry-overflow abort (vgc_say below); NOT diagnostic scaffolding.
// ============================================================
static void vgc__ws(const char* s) { size_t n = 0; while (s[n]) n++; (void)!write(2, s, n); }
static void vgc__wh(uint64_t v) {
    char b[18]; b[0] = '0'; b[1] = 'x';
    for (int i = 0; i < 16; i++) { int d = (int)((v >> ((15 - i) * 4)) & 0xf); b[2 + i] = d < 10 ? '0' + d : 'a' + d - 10; }
    (void)!write(2, b, 18);
}
static inline void vgc_say(uint64_t tag, uint64_t v) { // loud one-line stderr note
    vgc__ws("[vgc tag="); vgc__wh(tag); vgc__ws(" v="); vgc__wh(v); vgc__ws("]\n");
}

// Mark-closure verifier report (DEBUG, -d vgc_verify only): a MARKED (live)
// object holds a pointer to an ALLOCATED-but-UNMARKED object that sweep is about
// to reclaim — i.e. the mark did not reach closure. `kind`: 0 = referrer is a
// SCAN span (a definite mark-fixpoint / work-queue-drop bug), 1 = referrer is a
// NOSCAN span (candidate noscan misclassification; possible conservative false
// positive since noscan bytes are not real pointers — confirm by size). Sizes
// name the CX type. Async-signal-safe / non-allocating (no GC re-entry).
static inline void vgc_verify_report(uint64_t kind, uint64_t referrer_addr, uint64_t referrer_size,
                                     uint64_t off, uint64_t referent_addr, uint64_t referent_size) {
    vgc__ws("[vgc-verify ");
    vgc__ws(kind ? "NOSCAN" : "SCAN  ");
    vgc__ws(" referrer="); vgc__wh(referrer_addr);
    vgc__ws(" rsz="); vgc__wh(referrer_size);
    vgc__ws(" off="); vgc__wh(off);
    vgc__ws(" -> referent="); vgc__wh(referent_addr);
    vgc__ws(" tsz="); vgc__wh(referent_size);
    // Dump the referrer's words (the world is stopped — safe to read) so the
    // struct layout can be matched to a V/CX type by eye. Capped at 8 words.
    vgc__ws(" words=[");
    uint64_t nw = referrer_size / 8; if (nw > 8) nw = 8;
    for (uint64_t i = 0; i < nw; i++) {
        if (i) vgc__ws(",");
        vgc__wh(((uint64_t*)(uintptr_t)referrer_addr)[i]);
    }
    vgc__ws("]]\n");
}

// Root-finder report: a root (pointer to an allocated-but-unmarked object) was
// found outside the GC heap. `in_stack`: the holding address falls inside a
// registered thread's [stack_lo,stack_hi] (the mark's stack scan should have
// covered it — a stack-range/scan bug) vs EXTERNAL (libc heap / anon mmap /
// TLS / a data segment vgc_data_segments misses — memory the GC never scans).
static inline void vgc_rootfind_report(uint64_t referrer, int in_stack, uint64_t target, uint64_t tsz, uint64_t kind) {
    vgc__ws("[vgc-rootfind ");
    vgc__ws(in_stack ? "STACKMISS" : "EXTERNAL ");
    const char* kn = kind == 1 ? "[stack]" : kind == 2 ? "[heap] " : kind == 3 ? "file   " : "anon   ";
    vgc__ws(" region="); vgc__ws(kn);
    vgc__ws(" root@="); vgc__wh(referrer);
    vgc__ws(" -> target="); vgc__wh(target);
    vgc__ws(" tsz="); vgc__wh(tsz);
    vgc__ws(" twords=[");
    uint64_t nw = tsz / 8; if (nw > 6) nw = 6;
    for (uint64_t i = 0; i < nw; i++) { if (i) vgc__ws(","); vgc__wh(((uint64_t*)(uintptr_t)target)[i]); }
    vgc__ws("]]\n");
}

// ============================================================
// Root-finder (DEBUG, -d vgc_verify, Linux) — locate the MISSED root.
// ============================================================
// When the mark-closure verifier is clean yet a live object is still being
// swept, the object is reachable only from a root the collector's root scan
// misses. This enumerates every writable region in /proc/self/maps and (via the
// V callback vgc_rootfind_region) scans it for pointers to allocated-but-
// UNMARKED GC objects, telling us WHERE the missed root physically lives — a
// thread stack the mark's range didn't cover, the libc heap, or an anon mmap the
// GC never scans. Single read() into a static buffer: no malloc (STW-suspended
// threads may hold the libc malloc lock). Runs in-STW, so it cannot shift the
// mutator/collector timing the residual depends on.
#if defined(__linux__)
  #include <fcntl.h>
  extern void vgc_rootfind_region(uintptr_t lo, uintptr_t hi, int kind); // V callback; kind 0=anon 1=[stack] 2=[heap] 3=file
  static inline uintptr_t vgc__hex(const char* p, const char** end) {
      uintptr_t v = 0;
      while (1) {
          char c = *p; int d;
          if (c >= '0' && c <= '9') d = c - '0';
          else if (c >= 'a' && c <= 'f') d = c - 'a' + 10;
          else if (c >= 'A' && c <= 'F') d = c - 'A' + 10;
          else break;
          v = (v << 4) | (uintptr_t)d; p++;
      }
      *end = p; return v;
  }
  static inline void vgc_rootfind_enumerate(uintptr_t arena_lo, uintptr_t arena_hi) {
      int fd = open("/proc/self/maps", O_RDONLY);
      if (fd < 0) return;
      static char buf[1 << 20];
      ssize_t n = 0, total = 0;
      while (total < (ssize_t)sizeof(buf) - 1 && (n = read(fd, buf + total, sizeof(buf) - 1 - total)) > 0) total += n;
      close(fd);
      buf[total > 0 ? total : 0] = 0;
      const char* p = buf;
      while (*p) {
          const char* e;
          uintptr_t lo = vgc__hex(p, &e);
          if (*e != '-') { while (*p && *p != '\n') p++; if (*p) p++; continue; }
          e++;
          uintptr_t hi = vgc__hex(e, &e);
          // perms: " rwxp"
          const char* perms = e + 1; // skip space
          int readable = perms[0] == 'r', writable = perms[1] == 'w';
          // classify the line's pathname (last field) before advancing
          const char* ln = p;                       // start of this line
          const char* nl = ln; while (*nl && *nl != '\n') nl++; // end of line
          int kind = 0;                              // anon
          // find pathname: last token; [stack]/[heap] markers, or a '/'-path = file
          for (const char* q = ln; q < nl; q++) {
              if (q[0] == '[' && q[1] == 's' && q[2] == 't') { kind = 1; break; } // [stack]
              if (q[0] == '[' && q[1] == 'h' && q[2] == 'e') { kind = 2; break; } // [heap]
              if (*q == '/') { kind = 3; break; }    // file-backed
          }
          // advance to next line
          p = (*nl) ? nl + 1 : nl;
          if (!readable || !writable || hi <= lo) continue;
          // skip the GC arena (its objects are the transitive case, already checked)
          if (lo >= arena_lo && hi <= arena_hi) continue;
          // Cost control: a root to a live structure lives in a SMALL holder — the
          // exe's .data (file-backed) / .bss (small anon) / libc [heap], or TLS.
          // Skip [stack] (kind 1; STACKMISS already proven empty by the live-range
          // classifier) and skip any anon region > 2 MiB (thread stacks ~8 MiB,
          // arena chunks huge) so the scan stays cheap enough to run every cycle
          // under churn without starving the mutators. .bss is a small anon region,
          // so it survives this cap.
          if (kind == 1) continue;
          if (kind == 0 && (hi - lo) > (uintptr_t)2 * 1024 * 1024) continue;
          vgc_rootfind_region(lo, hi, kind);
      }
  }
#else
  static inline void vgc_rootfind_enumerate(uintptr_t arena_lo, uintptr_t arena_hi) { (void)arena_lo; (void)arena_hi; }
#endif

#ifdef VGC_DIAG
// ============================================================
// OPTIONAL diagnostic trace ring + async-signal-safe crash dump (correlates GC
// cycles with thread lifecycle). OFF by default; enable with `-cflags -DVGC_DIAG`.
// ============================================================
#include <signal.h>
typedef struct { uint32_t seq; int ev; int slot; uint64_t a; uint64_t b; uint32_t port; } vgc_tev;
#define VGC_TRACE_N 16384
static vgc_tev vgc_trace_ring[VGC_TRACE_N];
static volatile uint32_t vgc_trace_seq = 0;
static inline void vgc_trace(int ev, int slot, uint64_t a, uint64_t b) {
    uint32_t i = (uint32_t)__sync_fetch_and_add(&vgc_trace_seq, 1);
    vgc_tev* e = &vgc_trace_ring[i & (VGC_TRACE_N - 1)];
    e->seq = i; e->ev = ev; e->slot = slot; e->a = a; e->b = b;
    e->port = vgc_thread_self_port();
}
static const char* vgc_ev_name(int ev) {
    switch (ev) {
        case 1: return "REG    "; case 2: return "BAR_IN "; case 3: return "BAR_OUT";
        case 4: return "EXIT   "; case 5: return "GC_BEG "; case 6: return "SUSP?  ";
        case 7: return "SUSP!  "; case 8: return "SCAN   "; case 9: return "SWEEP0 ";
        case 10: return "SWEEP1 "; case 11: return "RESUME "; case 12: return "GC_END "; case 20: return "PACE   "; case 21: return "MAYBEGC";
        case 99: return "INITDONE"; default: return "?      ";
    }
}
static void vgc_trace_dump(void) {
    vgc__ws("\n=== VGC TRACE (oldest->newest) ===\n");
    uint32_t end = vgc_trace_seq;
    uint32_t start = end > VGC_TRACE_N ? end - VGC_TRACE_N : 0;
    for (uint32_t i = start; i < end; i++) {
        vgc_tev* e = &vgc_trace_ring[i & (VGC_TRACE_N - 1)];
        vgc__ws("#"); vgc__wh(e->seq); vgc__ws(" port="); vgc__wh(e->port);
        vgc__ws(" "); vgc__ws(vgc_ev_name(e->ev));
        vgc__ws(" slot="); vgc__wh((uint64_t)(uint32_t)e->slot);
        vgc__ws(" a="); vgc__wh(e->a); vgc__ws(" b="); vgc__wh(e->b); vgc__ws("\n");
    }
    vgc__ws("=== END VGC TRACE ===\n");
}
static volatile uint32_t vgc_dumping = 0;
static void vgc_crash_handler(int sig) {
    if (__sync_val_compare_and_swap(&vgc_dumping, 0, 1) != 0) { for (;;) {} } // only first faulter dumps; others park
    vgc__ws("\n!!! VGC CRASH sig="); vgc__wh((uint64_t)sig);
    vgc__ws(" faulting_port="); vgc__wh(vgc_thread_self_port()); vgc__ws("\n");
    vgc_trace_dump();
    signal(sig, SIG_DFL);
    raise(sig);
}
static inline void vgc_trace_init(void) {
    signal(SIGSEGV, vgc_crash_handler);
    signal(SIGBUS, vgc_crash_handler);
}
#else
// Diagnostics compiled out: zero-overhead no-ops the optimizer elides.
static inline void vgc_trace(int ev, int slot, uint64_t a, uint64_t b) { (void)ev; (void)slot; (void)a; (void)b; }
static inline void vgc_trace_init(void) {}
#endif // VGC_DIAG

#endif // VGC_PLATFORM_H
