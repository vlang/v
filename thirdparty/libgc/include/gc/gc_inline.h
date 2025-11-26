/*
 * Copyright 1988, 1989 Hans-J. Boehm, Alan J. Demers
 * Copyright (c) 1991-1995 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 2005 Hewlett-Packard Development Company, L.P.
 * Copyright (c) 2008-2025 Ivan Maidanski
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose, provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 */

#ifndef GC_INLINE_H
#define GC_INLINE_H

/*
 * *Warning*: Note that for these routines, it is the client`s responsibility
 * to add the extra byte at the end to deal with one-past-the-end pointers.
 * In the standard collector configuration, the collector assumes that such
 * a byte has been added, and hence does not trace the last "pointer-sized"
 * word in the resulting object.  This is not an issue if
 * `GC_get_all_interior_pointers()` returns zero or if
 * `GC_get_dont_add_byte_at_end()` returns 1.  This interface is most useful
 * for compilers that generate C code.  It is also used internally for
 * thread-local allocation.  A manual use is hereby discouraged.
 * Multi-threaded clients should include `atomic_ops.h` file (or similar)
 * before this header.  There is no debugging variant of this allocation API.
 */

#include "gc.h"
#include "gc_tiny_fl.h"

#if GC_GNUC_PREREQ(3, 0) || defined(__clang__)
/* Equivalent to `(expr)`, but predict that usually `expr == outcome`. */
#  define GC_EXPECT(expr, outcome) __builtin_expect(expr, outcome)
#else
#  define GC_EXPECT(expr, outcome) (expr)
#endif

#ifndef GC_ASSERT
#  ifdef NDEBUG
#    define GC_ASSERT(expr) (void)0
#  else
#    include <assert.h>
#    define GC_ASSERT(expr) assert(expr)
#  endif
#endif

#ifndef GC_PREFETCH_FOR_WRITE
#  if (GC_GNUC_PREREQ(3, 0) || defined(__clang__)) \
      && !defined(GC_NO_PREFETCH_FOR_WRITE)
#    define GC_PREFETCH_FOR_WRITE(x) __builtin_prefetch((x), 1 /* write */)
#  elif defined(_MSC_VER) && !defined(GC_NO_PREFETCH_FOR_WRITE)         \
      && (defined(_M_IX86) || defined(_M_X64)) && !defined(_CHPE_ONLY_) \
      && (_MSC_VER >= 1900 /* VS 2015+ */)
#    include <intrin.h>
#    define GC_PREFETCH_FOR_WRITE(x) _m_prefetchw(x)
/* TODO: Support also `_M_ARM` (`__prefetchw`). */
#  else
#    define GC_PREFETCH_FOR_WRITE(x) (void)0
#  endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* Object kinds (exposed to public). */
#define GC_I_PTRFREE 0
#define GC_I_NORMAL 1

/**
 * Determine if the collector has been configured not to pad the
 * allocated objects even in the all-interior-pointers mode.
 * Meaningful only if `GC_get_all_interior_pointers()` returns 1.
 */
GC_API int GC_CALL GC_get_dont_add_byte_at_end(void);

/**
 * Return a list of one or more objects of the indicated size, linked
 * through the first pointer in each object.  This has the advantage
 * that it acquires the allocator lock only once, and may greatly
 * reduce time wasted contending for the allocator lock.  Typical usage
 * would be in a thread that requires many items of the same size.
 * It would keep its own free list in a thread-local storage, and call
 * `GC_malloc_many()` or friends to replenish it.  (We do not round up
 * object sizes, since a call indicates the intention to consume many
 * objects of exactly this size.)  We assume that the size is nonzero
 * and a multiple of `GC_GRANULE_BYTES`, and that the size already
 * includes the value returned by `GC_get_all_interior_pointers()`
 * (unless `GC_get_dont_add_byte_at_end()` returns a nonzero value).
 * We return the free-list by assigning it to `*result`, since it is
 * not safe to return a linked list of pointer-free objects, since the
 * collector would not retain the entire list if it were invoked just
 * as we were returning; the client must make sure that `*result` is
 * traced even if objects are pointer-free.  Note also that the client
 * should usually clear the link field.
 */
GC_API void GC_CALL GC_generic_malloc_many(size_t /* `lb_adjusted` */,
                                           int /* `kind` */,
                                           void ** /* `result` */);

/**
 * A generalized variant of `GC_malloc` and `GC_malloc_atomic`.
 * Uses appropriately the thread-local (if available) or the global
 * free-list of the specified `kind`.
 */
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void *GC_CALL
    GC_malloc_kind(size_t /* `lb` */, int /* `kind` */);

#ifdef GC_THREADS
/* Same as `GC_malloc_kind` but uses only the global free-list. */
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void *GC_CALL
    GC_malloc_kind_global(size_t /* `lb` */, int /* `kind` */);
#else
#  define GC_malloc_kind_global GC_malloc_kind
#endif

/*
 * An internal macro to update the free-list pointer atomically
 * (if the AO primitives are available) to avoid race with the marker.
 */
#if !defined(GC_THREADS) || !defined(AO_HAVE_store)
#  define GC_FAST_M_AO_STORE(my_fl, next) (void)(*(my_fl) = (next))
#elif defined(__SIZEOF_POINTER__) && (__SIZEOF_POINTER__ > __SIZEOF_SIZE_T__)
/*
 * Directly use the gcc atomic intrinsic as the size of a pointer is
 * bigger than that of `AO_t`.
 */
#  define GC_FAST_M_AO_STORE(my_fl, next) \
    __atomic_store_n(my_fl, next, __ATOMIC_RELAXED)
#else
#  define GC_FAST_M_AO_STORE(my_fl, next) \
    AO_store((volatile AO_t *)(my_fl), (size_t)(next))
#endif

/**
 * The ultimately general inline allocation macro.  Allocate an object
 * of size `lg` (in granules), putting the resulting pointer in `result`.
 * `tiny_fl` is a "tiny" free-list array, which will be used first,
 * if the size is appropriate.  If `lg` argument is too large, then we
 * allocate with `default_expr` instead.  If we need to refill the free
 * list, we use `GC_generic_malloc_many()` with the indicated kind `k`.
 * `tiny_fl` should be an array of `GC_TINY_FREELISTS` `void` pointers.
 * If `num_direct` is nonzero, and the individual free-list pointers are
 * initialized to `(void *)1`, then we allocate `num_direct` granules
 * directly using `GC_generic_malloc_many()` before putting multiple
 * objects into the `tiny_fl` entry.  If `num_direct` is zero, then the
 * free lists may also be initialized to `NULL`.  Note that we use the
 * zeroth free list to hold objects of 1 granule in size that are used to
 * satisfy size 0 allocation requests.  We rely on much of this hopefully
 * getting optimized away in the case of `num_direct` is 0.  Particularly,
 * if `lg` argument is constant, this should generate a small amount of code.
 */
#define GC_FAST_MALLOC_GRANS(result, lg, tiny_fl, num_direct, k,         \
                             default_expr, init)                         \
  do {                                                                   \
    if (GC_EXPECT((lg) >= GC_TINY_FREELISTS, 0)) {                       \
      result = (default_expr);                                           \
    } else {                                                             \
      void **my_fl = (tiny_fl) + (lg);                                   \
      void *my_entry = *my_fl;                                           \
      void *next;                                                        \
                                                                         \
      for (;;) {                                                         \
        if (GC_EXPECT((GC_word)(GC_uintptr_t)my_entry                    \
                          > (num_direct) + GC_TINY_FREELISTS + 1,        \
                      1)) {                                              \
          next = *(void **)(my_entry);                                   \
          result = my_entry;                                             \
          GC_FAST_M_AO_STORE(my_fl, next);                               \
          init;                                                          \
          GC_PREFETCH_FOR_WRITE(next);                                   \
          if ((k) != GC_I_PTRFREE) {                                     \
            GC_end_stubborn_change(my_fl);                               \
            GC_reachable_here(next);                                     \
          }                                                              \
          GC_ASSERT(GC_size(result) >= GC_RAW_BYTES_FROM_INDEX(lg));     \
          GC_ASSERT((k) == GC_I_PTRFREE                                  \
                    || 0 /* `NULL` */ == ((void **)result)[1]);          \
          break;                                                         \
        }                                                                \
        /* Entry contains counter or `NULL`. */                          \
        if ((GC_signed_word)(GC_uintptr_t)my_entry                       \
                    - (GC_signed_word)(num_direct)                       \
                <= 0 /*< `(GC_uintptr_t)my_entry <= num_direct` */       \
            && my_entry != 0 /* NULL */) {                               \
          /* Small counter value, not `NULL`. */                         \
          GC_FAST_M_AO_STORE(my_fl, (char *)my_entry + (lg) + 1);        \
          result = (default_expr);                                       \
          break;                                                         \
        } else {                                                         \
          /* Large counter or `NULL`. */                                 \
          size_t lb_adj = GC_RAW_BYTES_FROM_INDEX(0 == (lg) ? 1 : (lg)); \
                                                                         \
          GC_generic_malloc_many(lb_adj, k, my_fl);                      \
          my_entry = *my_fl;                                             \
          if (0 /* `NULL` */ == my_entry) {                              \
            result = (*GC_get_oom_fn())(lb_adj);                         \
            break;                                                       \
          }                                                              \
        }                                                                \
      }                                                                  \
    }                                                                    \
  } while (0)

/**
 * Allocate `n` "pointer-sized" words.  The allocation size is rounded
 * up to a granule size.  The pointer is stored to `result`.
 * Should not be used unless `GC_get_all_interior_pointers()` returns zero
 * or if `GC_get_dont_add_byte_at_end()` returns 1.  Does not acquire
 * the allocator lock.  The caller is responsible for supplying
 * a cleared `tiny_fl` free-list array.  For single-threaded applications,
 * this may be a global array.
 */
#define GC_MALLOC_WORDS_KIND(result, n, tiny_fl, k, init)                \
  do {                                                                   \
    size_t lg = GC_PTRS_TO_WHOLE_GRANULES(n);                            \
                                                                         \
    GC_FAST_MALLOC_GRANS(result, lg, tiny_fl, 0 /* `num_direct` */, k,   \
                         GC_malloc_kind(GC_RAW_BYTES_FROM_INDEX(lg), k), \
                         init);                                          \
  } while (0)

#define GC_MALLOC_WORDS(result, n, tiny_fl)             \
  GC_MALLOC_WORDS_KIND(result, n, tiny_fl, GC_I_NORMAL, \
                       (void)(*(void **)(result) = 0 /* `NULL` */))

#define GC_MALLOC_ATOMIC_WORDS(result, n, tiny_fl) \
  GC_MALLOC_WORDS_KIND(result, n, tiny_fl, GC_I_PTRFREE, (void)0)

/** Allocate a two-pointer initialized object. */
#define GC_CONS(result, first, second, tiny_fl)                     \
  do {                                                              \
    void *l = (void *)(first);                                      \
    void *r = (void *)(second);                                     \
    GC_MALLOC_WORDS_KIND(result, 2, tiny_fl, GC_I_NORMAL, (void)0); \
    if ((result) != 0 /* `NULL` */) {                               \
      *(void **)(result) = l;                                       \
      GC_ptr_store_and_dirty((void **)(result) + 1, r);             \
      GC_reachable_here(l);                                         \
    }                                                               \
  } while (0)

/**
 * Print address of each object in the free list for the given `kind`
 * and size `lg` (in granules).  The caller should hold the allocator
 * lock at least in the reader mode.  Defined only if the library has
 * been compiled without `NO_DEBUGGING` macro defined.
 */
GC_API void GC_CALL GC_print_free_list(int /* `kind` */, size_t /* `lg` */);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* !GC_INLINE_H */
