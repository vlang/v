// vgc_platform.h - Platform abstractions for V Garbage Collector
// Translated from Go's runtime GC (golang/go src/runtime/mgc*.go, malloc.go, mheap.go)

#ifndef VGC_PLATFORM_H
#define VGC_PLATFORM_H

#include <stdint.h>
#include <string.h>
#include <stdio.h>

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
#if defined(_MSC_VER)
  #include <intrin.h>
  static inline void* vgc_get_sp(void) { return _AddressOfReturnAddress(); }
#else
  static inline void* vgc_get_sp(void) { return __builtin_frame_address(0); }
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
#define VGC_ADDR_SHIFT 30  // 1GB chunks
#define VGC_ADDR_MAP_SIZE 4096

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

#endif // VGC_PLATFORM_H
