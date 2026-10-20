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

#if defined(__TINYC__) && defined(__APPLE__)
  // TinyCC's Mach-O output has no working thread-local storage: writing a `__thread` variable
  // crashes. Keep the index in a pthread key instead, stored as idx + 1 so that a thread
  // without a value reads -1.
  // The key also tells vgc when the thread exits (see vgc_watch_thread_exit).
  #include <pthread.h>
  static void vgc_thread_exit_dispatch(void* value);
  static pthread_key_t _vgc_cache_idx_key;
  static pthread_once_t _vgc_cache_idx_once = PTHREAD_ONCE_INIT;
  static void vgc_cache_idx_key_create(void) { pthread_key_create(&_vgc_cache_idx_key, vgc_thread_exit_dispatch); }
  static inline int vgc_get_cache_idx(void) {
      pthread_once(&_vgc_cache_idx_once, vgc_cache_idx_key_create);
      return (int)(intptr_t)pthread_getspecific(_vgc_cache_idx_key) - 1;
  }
  static inline void vgc_set_cache_idx(int idx) {
      pthread_once(&_vgc_cache_idx_once, vgc_cache_idx_key_create);
      pthread_setspecific(_vgc_cache_idx_key, (void*)(intptr_t)(idx + 1));
  }
#else
// Stored as idx + 1 in a zero-initialized variable, so that a thread without a value reads -1.
// Keep it free of a non-zero initializer: TinyCC's ELF linker (the bundled tcc 85ba3ae8, fixed
// upstream by 128e7893) places .tdata after .bss in the same PT_LOAD segment, so the loader
// reads the TLS init image from zero-filled memory. Every thread then started with index 0
// (the main thread's cache), and the .tdata bytes overwrote the start of .bss instead.
static VGC_TLS int _vgc_cache_idx_plus1;

static inline int vgc_get_cache_idx(void) { return _vgc_cache_idx_plus1 - 1; }
static inline void vgc_set_cache_idx(int idx) { _vgc_cache_idx_plus1 = idx + 1; }
#endif

// ============================================================
// Atomic operations
// ============================================================
#if defined(__TINYC__)
  // TCC does not implement the legacy __sync_* family: those degrade into
  // plain function calls and then fail at link time with
  // "unresolved reference to '__sync_add_and_fetch'" etc.
  // Use the __atomic_* builtins instead, which lower to fixed-width helper
  // functions provided by libtcc1.a (same approach as vlib/builtin/prealloc_atomics.h).
  extern uint32_t __atomic_load_4(uint32_t* ptr, int order);
  extern uint64_t __atomic_load_8(uint64_t* ptr, int order);
  extern void __atomic_store_4(uint32_t* ptr, uint32_t val, int order);
  extern void __atomic_store_8(uint64_t* ptr, uint64_t val, int order);
  extern uint32_t __atomic_exchange_4(uint32_t* ptr, uint32_t val, int order);
  extern _Bool __atomic_compare_exchange_4(uint32_t* ptr, uint32_t* expected, uint32_t desired, int success_order, int failure_order);
  #define vgc_atomic_load_u32(ptr) ((uint32_t)__atomic_load_4((uint32_t*)(ptr), 5))
  #define vgc_atomic_store_u32(ptr, val) __atomic_store_4((uint32_t*)(ptr), (uint32_t)(val), 5)
  #define vgc_atomic_load_u64(ptr) ((uint64_t)__atomic_load_8((uint64_t*)(ptr), 5))
  #define vgc_atomic_store_u64(ptr, val) __atomic_store_8((uint64_t*)(ptr), (uint64_t)(val), 5)
  #define vgc_atomic_add_u64(ptr, val) __atomic_add_fetch((volatile uint64_t*)(ptr), (uint64_t)(val), 5)
  #define vgc_atomic_sub_u64(ptr, val) __atomic_sub_fetch((volatile uint64_t*)(ptr), (uint64_t)(val), 5)
  #define vgc_atomic_add_u32(ptr, val) __atomic_add_fetch((volatile uint32_t*)(ptr), (uint32_t)(val), 5)
  #define vgc_atomic_cas_u32(ptr, expected, desired) __atomic_compare_exchange_4((uint32_t*)(ptr), (uint32_t*)(expected), (uint32_t)(desired), 5, 5)
  #define vgc_atomic_exchange_u32(ptr, val) ((uint32_t)__atomic_exchange_4((uint32_t*)(ptr), (uint32_t)(val), 5))
  #if defined(__x86_64__) || defined(__i386__)
    #define vgc_atomic_fence() __asm__ __volatile__("mfence" ::: "memory")
  #elif defined(__aarch64__)
    #define vgc_atomic_fence() __asm__ __volatile__("dmb ish" ::: "memory")
  #elif defined(__arm__)
    #define vgc_atomic_fence() __asm__ __volatile__("dmb ish" ::: "memory")
  #elif defined(__riscv)
    #define vgc_atomic_fence() __asm__ __volatile__("fence rw,rw" ::: "memory")
  #else
    #define vgc_atomic_fence() __asm__ __volatile__("" ::: "memory")
  #endif
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
#if defined(ALL_PROCESSOR_GROUPS)
      DWORD count = GetActiveProcessorCount(ALL_PROCESSOR_GROUPS);
      return count > 0 ? (int)count : 1;
#else
      SYSTEM_INFO sysinfo;
      GetSystemInfo(&sysinfo);
      return sysinfo.dwNumberOfProcessors > 0 ? (int)sysinfo.dwNumberOfProcessors : 1;
#endif
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
  #if !defined(__ANDROID__) && !defined(__USE_GNU) && !defined(__cplusplus)
    // glibc and musl declare pthread_getattr_np only with _GNU_SOURCE, which has to be defined
    // before the first libc header. V's C output includes <pthread.h> long before this header,
    // so declare it here; repeating a declaration is fine in C.
    int pthread_getattr_np(pthread_t thread, pthread_attr_t* attr);
  #endif
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
// Registers as roots
// ============================================================
#include <setjmp.h>
typedef void (*vgc_spill_fn)(void*);
static void* volatile vgc_spill_sink;

// Calls fn(arg) after saving the callee-saved registers in a jmp_buf in this frame. A
// conservative scan of the stack from fn's frame upwards then also sees pointers that the
// callers only kept in registers.
static void vgc_call_with_registers_spilled(vgc_spill_fn fn, void* arg) {
    jmp_buf regs;
    if (setjmp(regs) == 0) {
        vgc_spill_sink = (void*)&regs;
        fn(arg);
    }
    // The store after the call keeps fn() from becoming a tail call that would drop this frame.
    vgc_spill_sink = NULL;
}

// ============================================================
// Data segments (globals and consts) as roots
// vgc_data_ranges stores the writable segments of the executable in lo/hi and returns how many
// there are, or 0 when the platform has no way to find them.
// ============================================================
#if defined(__APPLE__)
  #include <mach-o/dyld.h>
  #include <mach-o/loader.h>
  static int vgc_data_ranges(uintptr_t* lo, uintptr_t* hi, int max) {
      // Image 0 is the main executable.
      const struct mach_header* mh = _dyld_get_image_header(0);
      if (mh == NULL || mh->magic != MH_MAGIC_64) return 0;
      intptr_t slide = _dyld_get_image_vmaddr_slide(0);
      const uint8_t* cmd = (const uint8_t*)mh + sizeof(struct mach_header_64);
      int n = 0;
      for (uint32_t i = 0; i < mh->ncmds && n < max; i++) {
          const struct load_command* lc = (const struct load_command*)cmd;
          if (lc->cmd == LC_SEGMENT_64) {
              const struct segment_command_64* seg = (const struct segment_command_64*)cmd;
              if ((seg->initprot & VM_PROT_WRITE) != 0 && seg->vmsize != 0) {
                  lo[n] = (uintptr_t)(seg->vmaddr + slide);
                  hi[n] = lo[n] + (uintptr_t)seg->vmsize;
                  n++;
              }
          }
          cmd += lc->cmdsize;
      }
      return n;
  }
#elif defined(__linux__) || defined(__ANDROID__) || defined(__FreeBSD__) || defined(__DragonFly__) || defined(__NetBSD__) || defined(__OpenBSD__)
  #include <link.h>
  #if defined(__GLIBC__) && !defined(__USE_GNU)
    // glibc declares dl_iterate_phdr and struct dl_phdr_info only with _GNU_SOURCE, which V's
    // C output does not define before its first libc header. Declare the function, and read the
    // leading fields of the struct through a copy of its layout.
    struct dl_phdr_info;
    extern int dl_iterate_phdr(int (*callback)(struct dl_phdr_info*, size_t, void*), void* data);
    struct vgc_dl_phdr_info {
        ElfW(Addr) dlpi_addr;
        const char* dlpi_name;
        const ElfW(Phdr)* dlpi_phdr;
        ElfW(Half) dlpi_phnum;
    };
    #define VGC_DL_PHDR_INFO struct vgc_dl_phdr_info
  #else
    #define VGC_DL_PHDR_INFO struct dl_phdr_info
  #endif
  typedef struct { uintptr_t* lo; uintptr_t* hi; int max; int n; } vgc_data_ranges_acc;
  static int vgc_data_ranges_cb(struct dl_phdr_info* info_arg, size_t size, void* data) {
      (void)size;
      VGC_DL_PHDR_INFO* info = (VGC_DL_PHDR_INFO*)(void*)info_arg;
      vgc_data_ranges_acc* acc = (vgc_data_ranges_acc*)data;
      for (int i = 0; i < (int)info->dlpi_phnum && acc->n < acc->max; i++) {
          if (info->dlpi_phdr[i].p_type == PT_LOAD && (info->dlpi_phdr[i].p_flags & PF_W) != 0) {
              uintptr_t start = (uintptr_t)info->dlpi_addr + (uintptr_t)info->dlpi_phdr[i].p_vaddr;
              acc->lo[acc->n] = start;
              acc->hi[acc->n] = start + (uintptr_t)info->dlpi_phdr[i].p_memsz;
              acc->n++;
          }
      }
      // The first object is the main program, which holds all of V's globals.
      return 1;
  }
  static int vgc_data_ranges(uintptr_t* lo, uintptr_t* hi, int max) {
      vgc_data_ranges_acc acc = { lo, hi, max, 0 };
      dl_iterate_phdr(vgc_data_ranges_cb, &acc);
      return acc.n;
  }
#elif defined(_WIN32)
  static int vgc_data_ranges(uintptr_t* lo, uintptr_t* hi, int max) {
      uint8_t* base = (uint8_t*)GetModuleHandleW(NULL);
      if (base == NULL) return 0;
      IMAGE_NT_HEADERS* nt = (IMAGE_NT_HEADERS*)(base + ((IMAGE_DOS_HEADER*)base)->e_lfanew);
      IMAGE_SECTION_HEADER* sec = IMAGE_FIRST_SECTION(nt);
      int n = 0;
      for (int i = 0; i < (int)nt->FileHeader.NumberOfSections && n < max; i++) {
          if ((sec[i].Characteristics & IMAGE_SCN_MEM_WRITE) != 0 && sec[i].Misc.VirtualSize != 0) {
              lo[n] = (uintptr_t)(base + sec[i].VirtualAddress);
              hi[n] = lo[n] + (uintptr_t)sec[i].Misc.VirtualSize;
              n++;
          }
      }
      return n;
  }
#else
  static int vgc_data_ranges(uintptr_t* lo, uintptr_t* hi, int max) {
      (void)lo;
      (void)hi;
      (void)max;
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
    // The lookup tables are filled so that entry i holds the class for a request of
    // (i+1)*8 bytes (resp. 1024+(i+1)*128). The bucket index for `size` is therefore
    // ceil(size/8)-1 == (size-1)>>3, not ceil(size/8). Using (size+7)>>3 read one
    // bucket too high and handed out a size class one larger than necessary.
    if (size <= 1024) {
        return vgc_s2c8[(size - 1) >> 3];
    }
    if (size <= 32768) {
        return vgc_s2c128[(size - 1025) >> 7];
    }
    return 0; // large allocation
}

// ============================================================
// Pause / yield for spinlocks
// ============================================================
#if defined(__x86_64__) || defined(__i386__) || defined(_M_X64) || defined(_M_IX86)
  #define vgc_cpu_pause() __asm__ __volatile__("pause")
#elif (defined(__aarch64__) || defined(_M_ARM64)) && !defined(__TINYC__)
  #define vgc_cpu_pause() __asm__ __volatile__("yield")
#else
  // TinyCC's arm64 assembler has no `yield`. The hint is optional: vgc_mutex_lock falls back to
  // vgc_thread_yield() after a few rounds anyway.
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

static inline int vgc_mutex_trylock(volatile uint32_t* lock) {
    uint32_t expected = 0;
    return vgc_atomic_cas_u32(lock, &expected, 1) ? 1 : 0;
}

static inline void vgc_mutex_unlock(volatile uint32_t* lock) {
    vgc_atomic_store_u32(lock, 0);
}

// ============================================================
// Thread exit
// After vgc_watch_thread_exit(idx), the calling thread calls the hook set with
// vgc_set_thread_exit_hook when it exits, with (void*)(idx + 1). vgc then releases the
// thread's cache slot and stops scanning its stack, which the system unmaps. The index is
// passed in, because the thread-local variables may already be gone at that point.
// ============================================================
typedef void (*vgc_thread_exit_fn)(void*);
static vgc_thread_exit_fn vgc_thread_exit_hook;
static void vgc_set_thread_exit_hook(vgc_thread_exit_fn fn) { vgc_thread_exit_hook = fn; }
static void vgc_thread_exit_dispatch(void* value) {
    if (value != NULL && vgc_thread_exit_hook != NULL) vgc_thread_exit_hook(value);
}
#ifdef _WIN32
  static DWORD vgc_thread_exit_fls = FLS_OUT_OF_INDEXES;
  static void WINAPI vgc_thread_exit_fls_cb(void* value) { vgc_thread_exit_dispatch(value); }
  static void vgc_watch_thread_exit(int idx) {
      if (vgc_thread_exit_fls == FLS_OUT_OF_INDEXES) vgc_thread_exit_fls = FlsAlloc(vgc_thread_exit_fls_cb);
      if (vgc_thread_exit_fls != FLS_OUT_OF_INDEXES) FlsSetValue(vgc_thread_exit_fls, (void*)(intptr_t)(idx + 1));
  }
#elif defined(__TINYC__) && defined(__APPLE__)
  // The pthread key that holds the cache index calls vgc_thread_exit_dispatch.
  static void vgc_watch_thread_exit(int idx) { (void)idx; }
#else
  #include <pthread.h>
  static pthread_key_t vgc_thread_exit_key;
  static pthread_once_t vgc_thread_exit_once = PTHREAD_ONCE_INIT;
  static void vgc_thread_exit_key_create(void) { pthread_key_create(&vgc_thread_exit_key, vgc_thread_exit_dispatch); }
  static void vgc_watch_thread_exit(int idx) {
      pthread_once(&vgc_thread_exit_once, vgc_thread_exit_key_create);
      pthread_setspecific(vgc_thread_exit_key, (void*)(intptr_t)(idx + 1));
  }
#endif

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
