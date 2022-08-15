/*
 * Copyright (C) 2009 Bradley Smith <brad@brad-smith.co.uk>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */

#include "../all_atomic_load_store.h"

#include "../ordered.h" /* There are no multiprocessor implementations. */

#include "../test_and_set_t_is_ao_t.h"

#ifndef AO_PREFER_GENERALIZED
  AO_INLINE AO_TS_VAL_t
  AO_test_and_set_full(volatile AO_TS_t *addr)
  {
        register long ret;

        __asm__ __volatile__(
                "xchg %[oldval], %[mem], %[newval]"
                : [oldval] "=&r"(ret)
                : [mem] "r"(addr), [newval] "r"(1)
                : "memory");

        return (AO_TS_VAL_t)ret;
  }
# define AO_HAVE_test_and_set_full
#endif /* !AO_PREFER_GENERALIZED */

AO_INLINE int
AO_compare_and_swap_full(volatile AO_t *addr, AO_t old, AO_t new_val)
{
       register long ret;

       __asm__ __volatile__(
               "1: ssrf    5\n"
               "   ld.w    %[res], %[mem]\n"
               "   eor     %[res], %[oldval]\n"
               "   brne    2f\n"
               "   stcond  %[mem], %[newval]\n"
               "   brne    1b\n"
               "2:\n"
               : [res] "=&r"(ret), [mem] "=m"(*addr)
               : "m"(*addr), [newval] "r"(new_val), [oldval] "r"(old)
               : "cc", "memory");

       return (int)ret;
}
#define AO_HAVE_compare_and_swap_full

/* TODO: implement AO_fetch_compare_and_swap.   */

#define AO_T_IS_INT
