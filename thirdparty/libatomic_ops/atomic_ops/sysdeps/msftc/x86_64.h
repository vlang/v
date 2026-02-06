/*
 * Copyright (c) 2003-2011 Hewlett-Packard Development Company, L.P.
 * Copyright (c) 2009-2021 Ivan Maidanski
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

#ifndef AO_ASSUME_WINDOWS98
  /* CAS is always available */
# define AO_ASSUME_WINDOWS98
#endif
#ifndef AO_USE_INTERLOCKED_INTRINSICS
# define AO_USE_INTERLOCKED_INTRINSICS
#endif
#include "common32_defs.h"

#ifdef AO_ASM_X64_AVAILABLE

#if _MSC_VER < 1800
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
#endif /* _MSC_VER < 1800 */

  /* As far as we can tell, the lfence and sfence instructions are not  */
  /* currently needed or useful for cached memory accesses.             */

  AO_INLINE void
  AO_nop_full(void)
  {
    /* Note: "mfence" (SSE2) is supported on all x86_64/amd64 chips.    */
    __asm { mfence }
  }
# define AO_HAVE_nop_full

# ifndef AO_HAVE_test_and_set_full
#   include "../test_and_set_t_is_char.h"

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
#   define AO_HAVE_test_and_set_full
# endif

#endif /* AO_ASM_X64_AVAILABLE */

#ifndef AO_HAVE_test_and_set_full
# include "../test_and_set_t_is_ao_t.h"
  /* AO_test_and_set_full() is emulated using word-wide CAS.    */
#endif

#ifdef AO_CMPXCHG16B_AVAILABLE

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
                                            new_val2 /* high */,
                                            new_val1 /* low */,
                                            comparandResult);
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
