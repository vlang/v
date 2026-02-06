/*
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1996-1999 by Silicon Graphics.  All rights reserved.
 * Copyright (c) 1999-2003 by Hewlett-Packard Company. All rights reserved.
 *
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose, provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 *
 */

#if (AO_GNUC_PREREQ(12, 0) || AO_CLANG_PREREQ(13, 0)) \
    && !defined(AO_DISABLE_GCC_ATOMICS)
  /* Probably, it could be enabled for earlier compiler versions as well. */

# include "generic.h"

#else /* AO_DISABLE_GCC_ATOMICS */

#include "../all_atomic_load_store.h"

/* Real SPARC code uses TSO:                            */
#include "../ordered_except_wr.h"

/* Test_and_set location is just a byte.                */
#include "../test_and_set_t_is_char.h"

AO_INLINE AO_TS_VAL_t
AO_test_and_set_full(volatile AO_TS_t *addr) {
   AO_TS_VAL_t oldval;

   __asm__ __volatile__("ldstub %1,%0"
                        : "=r"(oldval), "=m"(*addr)
                        : "m"(*addr) : "memory");
   return oldval;
}
#define AO_HAVE_test_and_set_full

#ifndef AO_NO_SPARC_V9

# ifndef AO_GENERALIZE_ASM_BOOL_CAS
/* Returns nonzero if the comparison succeeded. */
AO_INLINE int
AO_compare_and_swap_full(volatile AO_t *addr, AO_t old, AO_t new_val) {
  __asm__ __volatile__ ("membar #StoreLoad | #LoadLoad\n\t"
#                       if defined(__arch64__)
                          "casx [%1],%2,%0\n\t"
#                       else
                          "cas [%1],%2,%0\n\t" /* 32-bit version */
#                       endif
                        "membar #StoreLoad | #StoreStore\n\t"
                        : "+r" (new_val)
                        : "r" (addr), "r" (old)
                        : "memory");
  return new_val == old;
}
#   define AO_HAVE_compare_and_swap_full
# endif /* !AO_GENERALIZE_ASM_BOOL_CAS */

AO_INLINE AO_t
AO_fetch_compare_and_swap_full(volatile AO_t *addr, AO_t old, AO_t new_val) {
  __asm__ __volatile__ ("membar #StoreLoad | #LoadLoad\n\t"
#                       if defined(__arch64__)
                          "casx [%1],%2,%0\n\t"
#                       else
                          "cas [%1],%2,%0\n\t" /* 32-bit version */
#                       endif
                        "membar #StoreLoad | #StoreStore\n\t"
                        : "+r" (new_val)
                        : "r" (addr), "r" (old)
                        : "memory");
  return new_val;
}
#define AO_HAVE_fetch_compare_and_swap_full

#endif /* !AO_NO_SPARC_V9 */

/* TODO: Extend this for SPARC v8 and v9 (V8 also has swap, V9 has CAS, */
/* there are barriers like membar #LoadStore, CASA (32-bit) and         */
/* CASXA (64-bit) instructions added in V9).                            */

#endif /* AO_DISABLE_GCC_ATOMICS */
