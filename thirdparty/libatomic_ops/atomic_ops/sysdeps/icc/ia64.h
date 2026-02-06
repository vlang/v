/*
 * Copyright (c) 2003 by Hewlett-Packard Company.  All rights reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

/*
 * This file specifies Itanimum primitives for use with the Intel (ecc)
 * compiler.  We use intrinsics instead of the inline assembly code in the
 * gcc file.
 */

#include "../all_atomic_load_store.h"

#include "../test_and_set_t_is_char.h"

#include <ia64intrin.h>

/* The acquire release semantics of volatile can be turned off.  And volatile   */
/* operations in icc9 don't imply ordering with respect to other nonvolatile    */
/* operations.                                                                  */

#define AO_INTEL_PTR_t void *

AO_INLINE AO_t
AO_load_acquire(const volatile AO_t *p)
{
  return (AO_t)(__ld8_acq((AO_INTEL_PTR_t)p));
}
#define AO_HAVE_load_acquire

AO_INLINE void
AO_store_release(volatile AO_t *p, AO_t val)
{
  __st8_rel((AO_INTEL_PTR_t)p, (__int64)val);
}
#define AO_HAVE_store_release

AO_INLINE unsigned char
AO_char_load_acquire(const volatile unsigned char *p)
{
  /* A normal volatile load generates an ld.acq         */
  return (__ld1_acq((AO_INTEL_PTR_t)p));
}
#define AO_HAVE_char_load_acquire

AO_INLINE void
AO_char_store_release(volatile unsigned char *p, unsigned char val)
{
  __st1_rel((AO_INTEL_PTR_t)p, val);
}
#define AO_HAVE_char_store_release

AO_INLINE unsigned short
AO_short_load_acquire(const volatile unsigned short *p)
{
  /* A normal volatile load generates an ld.acq         */
  return (__ld2_acq((AO_INTEL_PTR_t)p));
}
#define AO_HAVE_short_load_acquire

AO_INLINE void
AO_short_store_release(volatile unsigned short *p, unsigned short val)
{
  __st2_rel((AO_INTEL_PTR_t)p, val);
}
#define AO_HAVE_short_store_release

AO_INLINE unsigned int
AO_int_load_acquire(const volatile unsigned int *p)
{
  /* A normal volatile load generates an ld.acq         */
  return (__ld4_acq((AO_INTEL_PTR_t)p));
}
#define AO_HAVE_int_load_acquire

AO_INLINE void
AO_int_store_release(volatile unsigned int *p, unsigned int val)
{
  __st4_rel((AO_INTEL_PTR_t)p, val);
}
#define AO_HAVE_int_store_release

AO_INLINE void
AO_nop_full(void)
{
  __mf();
}
#define AO_HAVE_nop_full

#ifndef AO_PREFER_GENERALIZED
AO_INLINE AO_t
AO_fetch_and_add1_acquire(volatile AO_t *p)
{
  return __fetchadd8_acq((unsigned __int64 *)p, 1);
}
#define AO_HAVE_fetch_and_add1_acquire

AO_INLINE AO_t
AO_fetch_and_add1_release(volatile AO_t *p)
{
  return __fetchadd8_rel((unsigned __int64 *)p, 1);
}
#define AO_HAVE_fetch_and_add1_release

AO_INLINE AO_t
AO_fetch_and_sub1_acquire(volatile AO_t *p)
{
  return __fetchadd8_acq((unsigned __int64 *)p, -1);
}
#define AO_HAVE_fetch_and_sub1_acquire

AO_INLINE AO_t
AO_fetch_and_sub1_release(volatile AO_t *p)
{
  return __fetchadd8_rel((unsigned __int64 *)p, -1);
}
#define AO_HAVE_fetch_and_sub1_release
#endif /* !AO_PREFER_GENERALIZED */

AO_INLINE AO_t
AO_fetch_compare_and_swap_acquire(volatile AO_t *addr, AO_t old_val,
                                  AO_t new_val)
{
  return _InterlockedCompareExchange64_acq(addr, new_val, old_val);
}
#define AO_HAVE_fetch_compare_and_swap_acquire

AO_INLINE AO_t
AO_fetch_compare_and_swap_release(volatile AO_t *addr, AO_t old_val,
                                  AO_t new_val)
{
  return _InterlockedCompareExchange64_rel(addr, new_val, old_val);
}
#define AO_HAVE_fetch_compare_and_swap_release

AO_INLINE unsigned char
AO_char_fetch_compare_and_swap_acquire(volatile unsigned char *addr,
                                       unsigned char old_val,
                                       unsigned char new_val)
{
  return _InterlockedCompareExchange8_acq(addr, new_val, old_val);
}
#define AO_HAVE_char_fetch_compare_and_swap_acquire

AO_INLINE unsigned char
AO_char_fetch_compare_and_swap_release(volatile unsigned char *addr,
                                       unsigned char old_val,
                                       unsigned char new_val)
{
  return _InterlockedCompareExchange8_rel(addr, new_val, old_val);
}
#define AO_HAVE_char_fetch_compare_and_swap_release

AO_INLINE unsigned short
AO_short_fetch_compare_and_swap_acquire(volatile unsigned short *addr,
                                        unsigned short old_val,
                                        unsigned short new_val)
{
  return _InterlockedCompareExchange16_acq(addr, new_val, old_val);
}
#define AO_HAVE_short_fetch_compare_and_swap_acquire

AO_INLINE unsigned short
AO_short_fetch_compare_and_swap_release(volatile unsigned short *addr,
                                        unsigned short old_val,
                                        unsigned short new_val)
{
  return _InterlockedCompareExchange16_rel(addr, new_val, old_val);
}
#define AO_HAVE_short_fetch_compare_and_swap_release

AO_INLINE unsigned int
AO_int_fetch_compare_and_swap_acquire(volatile unsigned int *addr,
                                      unsigned int old_val,
                                      unsigned int new_val)
{
  return _InterlockedCompareExchange_acq(addr, new_val, old_val);
}
#define AO_HAVE_int_fetch_compare_and_swap_acquire

AO_INLINE unsigned int
AO_int_fetch_compare_and_swap_release(volatile unsigned int *addr,
                                      unsigned int old_val,
                                      unsigned int new_val)
{
  return _InterlockedCompareExchange_rel(addr, new_val, old_val);
}
#define AO_HAVE_int_fetch_compare_and_swap_release

#undef AO_INTEL_PTR_t
