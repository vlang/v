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
 * for any purpose,  provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 *
 */

#include "../loadstore/atomic_load.h"
#include "../loadstore/atomic_store.h"

#include "../test_and_set_t_is_ao_t.h"

#define AO_NO_DD_ORDERING
        /* Data dependence does not imply read ordering.        */

AO_INLINE void
AO_nop_full(void)
{
  __asm__ __volatile__("mb" : : : "memory");
}
#define AO_HAVE_nop_full

AO_INLINE void
AO_nop_write(void)
{
  __asm__ __volatile__("wmb" : : : "memory");
}
#define AO_HAVE_nop_write

/* mb should be used for AO_nop_read().  That's the default.    */

/* TODO: implement AO_fetch_and_add explicitly. */

/* We believe that ldq_l ... stq_c does not imply any memory barrier.   */
AO_INLINE int
AO_compare_and_swap(volatile AO_t *addr,
                    AO_t old, AO_t new_val)
{
  unsigned long was_equal;
  unsigned long temp;

  __asm__ __volatile__(
                     "1:     ldq_l %0,%1\n"
                     "       cmpeq %0,%4,%2\n"
                     "       mov %3,%0\n"
                     "       beq %2,2f\n"
                     "       stq_c %0,%1\n"
                     "       beq %0,1b\n"
                     "2:\n"
                     : "=&r" (temp), "+m" (*addr), "=&r" (was_equal)
                     : "r" (new_val), "Ir" (old)
                     :"memory");
  return (int)was_equal;
}
#define AO_HAVE_compare_and_swap

/* TODO: implement AO_fetch_compare_and_swap */
