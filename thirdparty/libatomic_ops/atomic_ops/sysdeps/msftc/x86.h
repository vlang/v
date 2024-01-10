/*
 * Copyright (c) 2003 Hewlett-Packard Development Company, L.P.
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

/* If AO_ASSUME_VISTA is defined, we assume Windows Server 2003, Vista  */
/* or later.                                                            */

#include "../all_aligned_atomic_load_store.h"

#include "../test_and_set_t_is_char.h"

#if !defined(AO_ASSUME_WINDOWS98) \
    && (defined(AO_ASSUME_VISTA) || _MSC_VER >= 1400)
   /* Visual Studio 2005 (MS VC++ 8.0) discontinued support of Windows 95. */
# define AO_ASSUME_WINDOWS98
#endif

#ifndef AO_USE_INTERLOCKED_INTRINSICS
  /* _Interlocked primitives (Inc, Dec, Xchg, Add) are always available */
# define AO_USE_INTERLOCKED_INTRINSICS
#endif
#include "common32_defs.h"

/* As far as we can tell, the lfence and sfence instructions are not    */
/* currently needed or useful for cached memory accesses.               */

/* Unfortunately mfence doesn't exist everywhere.               */
/* IsProcessorFeaturePresent(PF_COMPARE_EXCHANGE128) is         */
/* probably a conservative test for it?                         */

#if defined(AO_USE_PENTIUM4_INSTRS)

AO_INLINE void
AO_nop_full(void)
{
  __asm { mfence }
}
#define AO_HAVE_nop_full

#else

/* We could use the cpuid instruction.  But that seems to be slower     */
/* than the default implementation based on test_and_set_full.  Thus    */
/* we omit that bit of misinformation here.                             */

#endif

#if !defined(AO_NO_ASM_XADD) && !defined(AO_HAVE_char_fetch_and_add_full)
  AO_INLINE unsigned char
  AO_char_fetch_and_add_full(volatile unsigned char *p, unsigned char incr)
  {
    __asm
    {
      mov al, incr
      mov ebx, p
      lock xadd byte ptr [ebx], al
    }
    /* Ignore possible "missing return value" warning here.     */
  }
# define AO_HAVE_char_fetch_and_add_full

  AO_INLINE unsigned short
  AO_short_fetch_and_add_full(volatile unsigned short *p, unsigned short incr)
  {
    __asm
    {
      mov ax, incr
      mov ebx, p
      lock xadd word ptr [ebx], ax
    }
    /* Ignore possible "missing return value" warning here.     */
  }
# define AO_HAVE_short_fetch_and_add_full
#endif /* !AO_NO_ASM_XADD */

AO_INLINE AO_TS_VAL_t
AO_test_and_set_full(volatile AO_TS_t *addr)
{
    __asm
    {
        mov     eax,0xff                ; /* AO_TS_SET */
        mov     ebx,addr                ;
        xchg    byte ptr [ebx],al       ;
    }
    /* Ignore possible "missing return value" warning here. */
}
#define AO_HAVE_test_and_set_full

#if defined(_WIN64) && !defined(CPPCHECK)
# error wrong architecture
#endif

#ifdef AO_ASSUME_VISTA
# include "../standard_ao_double_t.h"

  /* Reading or writing a quadword aligned on a 64-bit boundary is      */
  /* always carried out atomically (requires at least a Pentium).       */
# define AO_ACCESS_double_CHECK_ALIGNED
# include "../loadstore/double_atomic_load_store.h"

  /* Whenever we run on a Pentium class machine, we have that certain   */
  /* function.                                                          */
# pragma intrinsic (_InterlockedCompareExchange64)

  /* Returns nonzero if the comparison succeeded.       */
  AO_INLINE int
  AO_double_compare_and_swap_full(volatile AO_double_t *addr,
                                  AO_double_t old_val, AO_double_t new_val)
  {
    AO_ASSERT_ADDR_ALIGNED(addr);
    return (double_ptr_storage)_InterlockedCompareExchange64(
                                        (__int64 volatile *)addr,
                                        new_val.AO_whole /* exchange */,
                                        old_val.AO_whole) == old_val.AO_whole;
  }
# define AO_HAVE_double_compare_and_swap_full
#endif /* AO_ASSUME_VISTA */

#define AO_T_IS_INT

/* Real X86 implementations, except for some old WinChips, appear       */
/* to enforce ordering between memory operations, EXCEPT that a later   */
/* read can pass earlier writes, presumably due to the visible          */
/* presence of store buffers.                                           */
/* We ignore both the WinChips, and the fact that the official specs    */
/* seem to be much weaker (and arguably too weak to be usable).         */
#include "../ordered_except_wr.h"
