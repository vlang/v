/*
 * Copyright (c) 2003-2011 Hewlett-Packard Development Company, L.P.
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

#include "../all_aligned_atomic_load_store.h"

/* Real X86 implementations appear                                      */
/* to enforce ordering between memory operations, EXCEPT that a later   */
/* read can pass earlier writes, presumably due to the visible          */
/* presence of store buffers.                                           */
/* We ignore the fact that the official specs                           */
/* seem to be much weaker (and arguably too weak to be usable).         */

#include "../ordered_except_wr.h"

#ifdef AO_ASM_X64_AVAILABLE
# include "../test_and_set_t_is_char.h"
#else
# include "../test_and_set_t_is_ao_t.h"
#endif

/* Assume _MSC_VER >= 1400 */
#include <intrin.h>

#pragma intrinsic (_InterlockedCompareExchange)
#pragma intrinsic (_InterlockedCompareExchange64)

#ifndef AO_PREFER_GENERALIZED

# pragma intrinsic (_InterlockedIncrement)
# pragma intrinsic (_InterlockedIncrement64)
# pragma intrinsic (_InterlockedDecrement)
# pragma intrinsic (_InterlockedDecrement64)
# pragma intrinsic (_InterlockedExchangeAdd)
# pragma intrinsic (_InterlockedExchangeAdd64)

AO_INLINE AO_t
AO_fetch_and_add_full (volatile AO_t *p, AO_t incr)
{
  return _InterlockedExchangeAdd64((__int64 volatile *)p, incr);
}
#define AO_HAVE_fetch_and_add_full

AO_INLINE AO_t
AO_fetch_and_add1_full (volatile AO_t *p)
{
  return _InterlockedIncrement64((__int64 volatile *)p) - 1;
}
#define AO_HAVE_fetch_and_add1_full

AO_INLINE AO_t
AO_fetch_and_sub1_full (volatile AO_t *p)
{
  return _InterlockedDecrement64((__int64 volatile *)p) + 1;
}
#define AO_HAVE_fetch_and_sub1_full
#endif /* !AO_PREFER_GENERALIZED */

AO_INLINE AO_t
AO_fetch_compare_and_swap_full(volatile AO_t *addr, AO_t old_val,
                               AO_t new_val)
{
  return (AO_t)_InterlockedCompareExchange64((__int64 volatile *)addr,
                                             new_val, old_val);
}
#define AO_HAVE_fetch_compare_and_swap_full

AO_INLINE unsigned int
AO_int_fetch_compare_and_swap_full(volatile unsigned int *addr,
                                   unsigned int old_val, unsigned int new_val)
{
  return _InterlockedCompareExchange((long volatile *)addr, new_val, old_val);
}
#define AO_HAVE_int_fetch_compare_and_swap_full

#ifndef AO_PREFER_GENERALIZED
AO_INLINE unsigned int
AO_int_fetch_and_add_full(volatile unsigned int *p, unsigned int incr)
{
  return _InterlockedExchangeAdd((long volatile *)p, incr);
}
#define AO_HAVE_int_fetch_and_add_full

  AO_INLINE unsigned int
  AO_int_fetch_and_add1_full(volatile unsigned int *p)
  {
    return _InterlockedIncrement((long volatile *)p) - 1;
  }
# define AO_HAVE_int_fetch_and_add1_full

  AO_INLINE unsigned int
  AO_int_fetch_and_sub1_full(volatile unsigned int *p)
  {
    return _InterlockedDecrement((long volatile *)p) + 1;
  }
# define AO_HAVE_int_fetch_and_sub1_full
#endif /* !AO_PREFER_GENERALIZED */

#if _MSC_VER > 1400
# pragma intrinsic (_InterlockedAnd8)
# pragma intrinsic (_InterlockedCompareExchange16)
# pragma intrinsic (_InterlockedOr8)
# pragma intrinsic (_InterlockedXor8)

  AO_INLINE void
  AO_char_and_full(volatile unsigned char *p, unsigned char value)
  {
    _InterlockedAnd8((char volatile *)p, value);
  }
# define AO_HAVE_char_and_full

  AO_INLINE void
  AO_char_or_full(volatile unsigned char *p, unsigned char value)
  {
    _InterlockedOr8((char volatile *)p, value);
  }
# define AO_HAVE_char_or_full

  AO_INLINE void
  AO_char_xor_full(volatile unsigned char *p, unsigned char value)
  {
    _InterlockedXor8((char volatile *)p, value);
  }
# define AO_HAVE_char_xor_full

  AO_INLINE unsigned short
  AO_short_fetch_compare_and_swap_full(volatile unsigned short *addr,
                                       unsigned short old_val,
                                       unsigned short new_val)
  {
    return _InterlockedCompareExchange16((short volatile *)addr,
                                         new_val, old_val);
  }
# define AO_HAVE_short_fetch_compare_and_swap_full

# ifndef AO_PREFER_GENERALIZED
#   pragma intrinsic (_InterlockedIncrement16)
#   pragma intrinsic (_InterlockedDecrement16)

    AO_INLINE unsigned short
    AO_short_fetch_and_add1_full(volatile unsigned short *p)
    {
      return _InterlockedIncrement16((short volatile *)p) - 1;
    }
#   define AO_HAVE_short_fetch_and_add1_full

    AO_INLINE unsigned short
    AO_short_fetch_and_sub1_full(volatile unsigned short *p)
    {
      return _InterlockedDecrement16((short volatile *)p) + 1;
    }
#   define AO_HAVE_short_fetch_and_sub1_full
# endif /* !AO_PREFER_GENERALIZED */
#endif /* _MSC_VER > 1400 */

#if _MSC_VER >= 1800 /* Visual Studio 2013+ */

# pragma intrinsic (_InterlockedCompareExchange8)

  AO_INLINE unsigned char
  AO_char_fetch_compare_and_swap_full(volatile unsigned char *addr,
                                      unsigned char old_val,
                                      unsigned char new_val)
  {
    return _InterlockedCompareExchange8((char volatile *)addr,
                                        new_val, old_val);
  }
# define AO_HAVE_char_fetch_compare_and_swap_full

# ifndef AO_PREFER_GENERALIZED
#   pragma intrinsic (_InterlockedExchangeAdd16)
#   pragma intrinsic (_InterlockedExchangeAdd8)

    AO_INLINE unsigned char
    AO_char_fetch_and_add_full(volatile unsigned char *p, unsigned char incr)
    {
      return _InterlockedExchangeAdd8((char volatile *)p, incr);
    }
#   define AO_HAVE_char_fetch_and_add_full

    AO_INLINE unsigned short
    AO_short_fetch_and_add_full(volatile unsigned short *p,
                                unsigned short incr)
    {
      return _InterlockedExchangeAdd16((short volatile *)p, incr);
    }
#   define AO_HAVE_short_fetch_and_add_full
# endif /* !AO_PREFER_GENERALIZED */

#elif defined(AO_ASM_X64_AVAILABLE)

  AO_INLINE unsigned char
  AO_char_fetch_and_add_full(volatile unsigned char *p, unsigned char incr)
  {
    __asm
    {
      mov al, incr
      mov rbx, p
      lock xadd byte ptr [rbx], al
    }
  }
# define AO_HAVE_char_fetch_and_add_full

  AO_INLINE unsigned short
  AO_short_fetch_and_add_full(volatile unsigned short *p, unsigned short incr)
  {
    __asm
    {
      mov ax, incr
      mov rbx, p
      lock xadd word ptr [rbx], ax
    }
  }
# define AO_HAVE_short_fetch_and_add_full

#endif /* _MSC_VER < 1800 && AO_ASM_X64_AVAILABLE */

#ifdef AO_ASM_X64_AVAILABLE

/* As far as we can tell, the lfence and sfence instructions are not    */
/* currently needed or useful for cached memory accesses.               */

  AO_INLINE void
  AO_nop_full(void)
  {
    /* Note: "mfence" (SSE2) is supported on all x86_64/amd64 chips.    */
    __asm { mfence }
  }
# define AO_HAVE_nop_full

  AO_INLINE AO_TS_VAL_t
  AO_test_and_set_full(volatile AO_TS_t *addr)
  {
    __asm
    {
        mov     rax,AO_TS_SET           ;
        mov     rbx,addr                ;
        xchg    byte ptr [rbx],al       ;
    }
  }
# define AO_HAVE_test_and_set_full

#endif /* AO_ASM_X64_AVAILABLE */

#ifdef AO_CMPXCHG16B_AVAILABLE
/* AO_compare_double_and_swap_double_full needs implementation for Win64.
 * Also see ../gcc/x86.h for partial old Opteron workaround.
 */

# if _MSC_VER >= 1500

#   include "../standard_ao_double_t.h"

#   pragma intrinsic (_InterlockedCompareExchange128)

AO_INLINE int
AO_compare_double_and_swap_double_full(volatile AO_double_t *addr,
                                       AO_t old_val1, AO_t old_val2,
                                       AO_t new_val1, AO_t new_val2)
{
   __int64 comparandResult[2];

   AO_ASSERT_ADDR_ALIGNED(addr);
   comparandResult[0] = old_val1; /* low */
   comparandResult[1] = old_val2; /* high */
   return _InterlockedCompareExchange128((volatile __int64 *)addr,
                new_val2 /* high */, new_val1 /* low */, comparandResult);
}
#   define AO_HAVE_compare_double_and_swap_double_full

# elif defined(AO_ASM_X64_AVAILABLE)

#   include "../standard_ao_double_t.h"

    /* If there is no intrinsic _InterlockedCompareExchange128 then we  */
    /* need basically what's given below.                               */
AO_INLINE int
AO_compare_double_and_swap_double_full(volatile AO_double_t *addr,
                                       AO_t old_val1, AO_t old_val2,
                                       AO_t new_val1, AO_t new_val2)
{
        __asm
        {
                mov     rdx,QWORD PTR [old_val2]        ;
                mov     rax,QWORD PTR [old_val1]        ;
                mov     rcx,QWORD PTR [new_val2]        ;
                mov     rbx,QWORD PTR [new_val1]        ;
                lock cmpxchg16b [addr]                  ;
                setz    rax                             ;
        }
}
#   define AO_HAVE_compare_double_and_swap_double_full
# endif /* AO_ASM_X64_AVAILABLE && (_MSC_VER < 1500) */

#endif /* AO_CMPXCHG16B_AVAILABLE */
