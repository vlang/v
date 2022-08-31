/*
 * Copyright (c) 2009 by Takashi YOSHII. All rights reserved.
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
 */

#include "../all_atomic_load_store.h"
#include "../ordered.h"

/* sh has tas.b(byte) only */
#include "../test_and_set_t_is_char.h"

AO_INLINE AO_TS_VAL_t
AO_test_and_set_full(volatile AO_TS_t *addr)
{
  int oldval;
  __asm__ __volatile__(
        "tas.b @%1; movt %0"
        : "=r" (oldval)
        : "r" (addr)
        : "t", "memory");
  return oldval? AO_TS_CLEAR : AO_TS_SET;
}
#define AO_HAVE_test_and_set_full

/* TODO: Very incomplete.       */
