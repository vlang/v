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

/* The cas instruction causes an emulation trap for the */
/* 060 with a misaligned pointer, so let's avoid this.  */
#undef AO_t
typedef unsigned long AO_t __attribute__((__aligned__(4)));

/* FIXME.  Very incomplete.  */
#include "../all_aligned_atomic_load_store.h"

/* Are there any m68k multiprocessors still around?     */
/* AFAIK, Alliants were sequentially consistent.        */
#include "../ordered.h"

#include "../test_and_set_t_is_char.h"

AO_INLINE AO_TS_VAL_t
AO_test_and_set_full(volatile AO_TS_t *addr) {
  AO_TS_t oldval;

  /* The value at addr is semi-phony.   */
  /* 'tas' sets bit 7 while the return  */
  /* value pretends all bits were set,  */
  /* which at least matches AO_TS_SET.  */
  __asm__ __volatile__(
                "tas %1; sne %0"
                : "=d" (oldval), "=m" (*addr)
                : "m" (*addr)
                : "memory");
  /* This cast works due to the above.  */
  return (AO_TS_VAL_t)oldval;
}
#define AO_HAVE_test_and_set_full

/* Returns nonzero if the comparison succeeded. */
AO_INLINE int
AO_compare_and_swap_full(volatile AO_t *addr,
                         AO_t old, AO_t new_val)
{
  char result;

  __asm__ __volatile__(
                "cas.l %3,%4,%1; seq %0"
                : "=d" (result), "=m" (*addr)
                : "m" (*addr), "d" (old), "d" (new_val)
                : "memory");
  return -result;
}
#define AO_HAVE_compare_and_swap_full

/* TODO: implement AO_fetch_compare_and_swap.   */

#define AO_T_IS_INT
