/*
 * Copyright 1988, 1989 Hans-J. Boehm, Alan J. Demers
 * Copyright (c) 1991-1995 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 2005 Hewlett-Packard Development Company, L.P.
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose,  provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 */

#ifndef GC_INLINE_H
#define GC_INLINE_H

/* WARNING:                                                             */
/* Note that for these routines, it is the clients responsibility to    */
/* add the extra byte at the end to deal with one-past-the-end pointers.*/
/* In the standard collector configuration, the collector assumes that  */
/* such a byte has been added, and hence does not trace the last word   */
/* in the resulting object.                                             */
/* This is not an issue if the collector is compiled with               */
/* DONT_ADD_BYTE_AT_END, or if GC_all_interior_pointers is not set.     */
/* This interface is most useful for compilers that generate C.         */
/* It is also used internally for thread-local allocation.              */
/* Manual use is hereby discouraged.                                    */

#include "gc.h"
#include "gc_tiny_fl.h"

#if GC_GNUC_PREREQ(3, 0)
# define GC_EXPECT(expr, outcome) __builtin_expect(expr,outcome)
  /* Equivalent to (expr), but predict that usually (expr)==outcome. */
#else
# define GC_EXPECT(expr, outcome) (expr)
#endif

#ifndef GC_ASSERT
# ifdef NDEBUG
#   define GC_ASSERT(expr) /* empty */
# else
#   include <assert.h>
#   define GC_ASSERT(expr) assert(expr)
# endif
#endif

#ifdef __cplusplus
  extern "C" {
#endif

#ifndef GC_PREFETCH_FOR_WRITE
# if GC_GNUC_PREREQ(3, 0) && !defined(GC_NO_PREFETCH_FOR_WRITE)
#   define GC_PREFETCH_FOR_WRITE(x) __builtin_prefetch((x), 1)
# else
#   define GC_PREFETCH_FOR_WRITE(x) (void)0
# endif
#endif

/* Object kinds; must match PTRFREE, NORMAL in gc_priv.h.       */
#define GC_I_PTRFREE 0
#define GC_I_NORMAL 1

/* Store a pointer to a list of newly allocated objects of kind k and   */
/* size lb in *result.  The caller must make sure that *result is       */
/* traced even if objects are ptrfree.                                  */
GC_API void GC_CALL GC_generic_malloc_many(size_t /* lb */, int /* k */,
                                           void ** /* result */);

/* Generalized version of GC_malloc and GC_malloc_atomic.               */
/* Uses appropriately the thread-local (if available) or the global     */
/* free-list of the specified kind.                                     */
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
        GC_malloc_kind(size_t /* lb */, int /* k */);

#ifdef GC_THREADS
  /* Same as above but uses only the global free-list.  */
  GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
        GC_malloc_kind_global(size_t /* lb */, int /* k */);
#else
# define GC_malloc_kind_global GC_malloc_kind
#endif

/* An internal macro to update the free list pointer atomically (if     */
/* the AO primitives are available) to avoid race with the marker.      */
#if defined(GC_THREADS) && defined(AO_HAVE_store)
# define GC_FAST_M_AO_STORE(my_fl, next) \
                AO_store((volatile AO_t *)(my_fl), (AO_t)(next))
#else
# define GC_FAST_M_AO_STORE(my_fl, next) (void)(*(my_fl) = (next))
#endif

/* The ultimately general inline allocation macro.  Allocate an object  */
/* of size granules, putting the resulting pointer in result.  Tiny_fl  */
/* is a "tiny" free list array, which will be used first, if the size   */
/* is appropriate.  If granules is too large, we allocate with          */
/* default_expr instead.  If we need to refill the free list, we use    */
/* GC_generic_malloc_many with the indicated kind.                      */
/* Tiny_fl should be an array of GC_TINY_FREELISTS void * pointers.     */
/* If num_direct is nonzero, and the individual free list pointers      */
/* are initialized to (void *)1, then we allocate num_direct granules   */
/* directly using generic_malloc before putting multiple objects into   */
/* the tiny_fl entry.  If num_direct is zero, then the free lists may   */
/* also be initialized to (void *)0.                                    */
/* Note that we use the zeroth free list to hold objects 1 granule in   */
/* size that are used to satisfy size 0 allocation requests.            */
/* We rely on much of this hopefully getting optimized away in the      */
/* num_direct = 0 case.                                                 */
/* Particularly if granules is constant, this should generate a small   */
/* amount of code.                                                      */
# define GC_FAST_MALLOC_GRANS(result,granules,tiny_fl,num_direct, \
                              kind,default_expr,init) \
  do { \
    if (GC_EXPECT((granules) >= GC_TINY_FREELISTS,0)) { \
        result = (default_expr); \
    } else { \
        void **my_fl = (tiny_fl) + (granules); \
        void *my_entry=*my_fl; \
        void *next; \
    \
        for (;;) { \
            if (GC_EXPECT((GC_word)my_entry \
                          > (num_direct) + GC_TINY_FREELISTS + 1, 1)) { \
                next = *(void **)(my_entry); \
                result = (void *)my_entry; \
                GC_FAST_M_AO_STORE(my_fl, next); \
                init; \
                GC_PREFETCH_FOR_WRITE(next); \
                if ((kind) != GC_I_PTRFREE) { \
                    GC_end_stubborn_change(my_fl); \
                    GC_reachable_here(next); \
                } \
                GC_ASSERT(GC_size(result) >= (granules)*GC_GRANULE_BYTES); \
                GC_ASSERT((kind) == GC_I_PTRFREE \
                          || ((GC_word *)result)[1] == 0); \
                break; \
            } \
            /* Entry contains counter or NULL */ \
            if ((GC_signed_word)my_entry - (GC_signed_word)(num_direct) <= 0 \
                    /* (GC_word)my_entry <= (num_direct) */ \
                    && my_entry != 0 /* NULL */) { \
                /* Small counter value, not NULL */ \
                GC_FAST_M_AO_STORE(my_fl, (char *)my_entry \
                                          + (granules) + 1); \
                result = (default_expr); \
                break; \
            } else { \
                /* Large counter or NULL */ \
                GC_generic_malloc_many(((granules) == 0? GC_GRANULE_BYTES : \
                                        GC_RAW_BYTES_FROM_INDEX(granules)), \
                                       kind, my_fl); \
                my_entry = *my_fl; \
                if (my_entry == 0) { \
                    result = (*GC_get_oom_fn())((granules)*GC_GRANULE_BYTES); \
                    break; \
                } \
            } \
        } \
    } \
  } while (0)

# define GC_WORDS_TO_WHOLE_GRANULES(n) \
        GC_WORDS_TO_GRANULES((n) + GC_GRANULE_WORDS - 1)

/* Allocate n words (NOT BYTES).  X is made to point to the result.     */
/* This should really only be used if GC_all_interior_pointers is       */
/* not set, or DONT_ADD_BYTE_AT_END is set.  See above.                 */
/* Does not acquire lock.  The caller is responsible for supplying      */
/* a cleared tiny_fl free list array.  For single-threaded              */
/* applications, this may be a global array.                            */
# define GC_MALLOC_WORDS_KIND(result,n,tiny_fl,kind,init) \
    do { \
      size_t granules = GC_WORDS_TO_WHOLE_GRANULES(n); \
      GC_FAST_MALLOC_GRANS(result, granules, tiny_fl, 0, kind, \
                           GC_malloc_kind(granules*GC_GRANULE_BYTES, kind), \
                           init); \
    } while (0)

# define GC_MALLOC_WORDS(result,n,tiny_fl) \
        GC_MALLOC_WORDS_KIND(result, n, tiny_fl, GC_I_NORMAL, \
                             *(void **)(result) = 0)

# define GC_MALLOC_ATOMIC_WORDS(result,n,tiny_fl) \
        GC_MALLOC_WORDS_KIND(result, n, tiny_fl, GC_I_PTRFREE, (void)0)

/* And once more for two word initialized objects: */
# define GC_CONS(result, first, second, tiny_fl) \
    do { \
      void *l = (void *)(first); \
      void *r = (void *)(second); \
      GC_MALLOC_WORDS_KIND(result, 2, tiny_fl, GC_I_NORMAL, (void)0); \
      if ((result) != 0 /* NULL */) { \
        *(void **)(result) = l; \
        GC_PTR_STORE_AND_DIRTY((void **)(result) + 1, r); \
        GC_reachable_here(l); \
      } \
    } while (0)

GC_API void GC_CALL GC_print_free_list(int /* kind */,
                                       size_t /* sz_in_granules */);

#ifdef __cplusplus
  } /* extern "C" */
#endif

#endif /* !GC_INLINE_H */
