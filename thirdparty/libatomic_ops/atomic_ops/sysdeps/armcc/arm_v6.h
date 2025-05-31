/*
 * Copyright (c) 2007 by NEC LE-IT:               All rights reserved.
 * A transcription of ARMv6 atomic operations for the ARM Realview Toolchain.
 * This code works with armcc from RVDS 3.1
 * This is based on work in gcc/arm.h by
 *   Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 *   Copyright (c) 1996-1999 by Silicon Graphics.  All rights reserved.
 *   Copyright (c) 1999-2003 by Hewlett-Packard Company. All rights reserved.
 *
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

#include "../test_and_set_t_is_ao_t.h" /* Probably suboptimal */

#if __TARGET_ARCH_ARM < 6
# if !defined(CPPCHECK)
#   error Do not use with ARM instruction sets lower than v6
# endif
#else

#define AO_ACCESS_CHECK_ALIGNED
#define AO_ACCESS_short_CHECK_ALIGNED
#define AO_ACCESS_int_CHECK_ALIGNED
#include "../all_atomic_only_load.h"

#include "../standard_ao_double_t.h"

/* NEC LE-IT: ARMv6 is the first architecture providing support for simple LL/SC
 * A data memory barrier must be raised via CP15 command (see documentation).
 *
 * ARMv7 is compatible to ARMv6 but has a simpler command for issuing a
 * memory barrier (DMB). Raising it via CP15 should still work as told me by the
 * support engineers. If it turns out to be much quicker than we should implement
 * custom code for ARMv7 using the asm { dmb } command.
 *
 * If only a single processor is used, we can define AO_UNIPROCESSOR
 * and do not need to access CP15 for ensuring a DMB at all.
*/

AO_INLINE void
AO_nop_full(void)
{
# ifndef AO_UNIPROCESSOR
    unsigned int dest=0;
    /* Issue a data memory barrier (keeps ordering of memory transactions  */
    /* before and after this operation).                                   */
    __asm {
            mcr p15,0,dest,c7,c10,5
            };
# else
    AO_compiler_barrier();
# endif
}
#define AO_HAVE_nop_full

/* NEC LE-IT: atomic "store" - according to ARM documentation this is
 * the only safe way to set variables also used in LL/SC environment.
 * A direct write won't be recognized by the LL/SC construct in other CPUs.
 *
 * HB: Based on subsequent discussion, I think it would be OK to use an
 * ordinary store here if we knew that interrupt handlers always cleared
 * the reservation.  They should, but there is some doubt that this is
 * currently always the case for e.g. Linux.
*/
AO_INLINE void AO_store(volatile AO_t *addr, AO_t value)
{
        unsigned long tmp;

retry:
__asm {
        ldrex   tmp, [addr]
        strex   tmp, value, [addr]
        teq     tmp, #0
        bne     retry
        };
}
#define AO_HAVE_store

/* NEC LE-IT: replace the SWAP as recommended by ARM:

   "Applies to: ARM11 Cores
        Though the SWP instruction will still work with ARM V6 cores, it is recommended
        to use the new V6 synchronization instructions. The SWP instruction produces
        locked read and write accesses which are atomic, i.e. another operation cannot
        be done between these locked accesses which ties up external bus (AHB,AXI)
        bandwidth and can increase worst case interrupt latencies. LDREX,STREX are
        more flexible, other instructions can be done between the LDREX and STREX accesses.
   "
*/
#ifndef AO_PREFER_GENERALIZED
AO_INLINE AO_TS_VAL_t
AO_test_and_set(volatile AO_TS_t *addr) {
        AO_TS_VAL_t oldval;
        unsigned long tmp;
        unsigned long one = 1;
retry:
__asm {
        ldrex   oldval, [addr]
        strex   tmp, one, [addr]
        teq     tmp, #0
        bne     retry
        }

        return oldval;
}
#define AO_HAVE_test_and_set

AO_INLINE AO_t
AO_fetch_and_add(volatile AO_t *p, AO_t incr)
{
        unsigned long tmp,tmp2;
        AO_t result;

retry:
__asm {
        ldrex   result, [p]
        add     tmp, incr, result
        strex   tmp2, tmp, [p]
        teq     tmp2, #0
        bne     retry
        }

        return result;
}
#define AO_HAVE_fetch_and_add

AO_INLINE AO_t
AO_fetch_and_add1(volatile AO_t *p)
{
        unsigned long tmp,tmp2;
        AO_t result;

retry:
__asm {
        ldrex   result, [p]
        add     tmp, result, #1
        strex   tmp2, tmp, [p]
        teq     tmp2, #0
        bne     retry
        }

        return result;
}
#define AO_HAVE_fetch_and_add1

AO_INLINE AO_t
AO_fetch_and_sub1(volatile AO_t *p)
{
        unsigned long tmp,tmp2;
        AO_t result;

retry:
__asm {
        ldrex   result, [p]
        sub     tmp, result, #1
        strex   tmp2, tmp, [p]
        teq     tmp2, #0
        bne     retry
        }

        return result;
}
#define AO_HAVE_fetch_and_sub1
#endif /* !AO_PREFER_GENERALIZED */

#ifndef AO_GENERALIZE_ASM_BOOL_CAS
  /* Returns nonzero if the comparison succeeded.       */
  AO_INLINE int
  AO_compare_and_swap(volatile AO_t *addr, AO_t old_val, AO_t new_val)
  {
    AO_t result, tmp;

  retry:
    __asm__ {
      mov     result, #2
      ldrex   tmp, [addr]
      teq     tmp, old_val
#     ifdef __thumb__
        it      eq
#     endif
      strexeq result, new_val, [addr]
      teq     result, #1
      beq     retry
    }
    return !(result&2);
  }
# define AO_HAVE_compare_and_swap
#endif /* !AO_GENERALIZE_ASM_BOOL_CAS */

AO_INLINE AO_t
AO_fetch_compare_and_swap(volatile AO_t *addr, AO_t old_val, AO_t new_val)
{
         AO_t fetched_val, tmp;

retry:
__asm__ {
        mov     tmp, #2
        ldrex   fetched_val, [addr]
        teq     fetched_val, old_val
#     ifdef __thumb__
        it      eq
#     endif
        strexeq tmp, new_val, [addr]
        teq     tmp, #1
        beq     retry
        }
        return fetched_val;
}
#define AO_HAVE_fetch_compare_and_swap

/* helper functions for the Realview compiler: LDREXD is not usable
 * with inline assembler, so use the "embedded" assembler as
 * suggested by ARM Dev. support (June 2008). */
__asm inline double_ptr_storage AO_load_ex(const volatile AO_double_t *addr) {
        LDREXD r0,r1,[r0]
}

__asm inline int AO_store_ex(AO_t val1, AO_t val2, volatile AO_double_t *addr) {
        STREXD r3,r0,r1,[r2]
        MOV    r0,r3
}

AO_INLINE AO_double_t
AO_double_load(const volatile AO_double_t *addr)
{
  AO_double_t result;

  result.AO_whole = AO_load_ex(addr);
  return result;
}
#define AO_HAVE_double_load

AO_INLINE int
AO_compare_double_and_swap_double(volatile AO_double_t *addr,
                                  AO_t old_val1, AO_t old_val2,
                                  AO_t new_val1, AO_t new_val2)
{
        double_ptr_storage old_val =
                        ((double_ptr_storage)old_val2 << 32) | old_val1;
        double_ptr_storage tmp;
        int result;

        while(1) {
                tmp = AO_load_ex(addr);
                if(tmp != old_val)      return 0;
                result = AO_store_ex(new_val1, new_val2, addr);
                if(!result)     return 1;
        }
}
#define AO_HAVE_compare_double_and_swap_double

#endif /* __TARGET_ARCH_ARM >= 6 */

#define AO_T_IS_INT
