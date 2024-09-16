/*
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1996-1999 by Silicon Graphics.  All rights reserved.
 * Copyright (c) 1999-2003 by Hewlett-Packard Company. All rights reserved.
 * Copyright (c) 2008-2017 Ivan Maidanski
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

#if (AO_GNUC_PREREQ(4, 8) || AO_CLANG_PREREQ(3, 5)) \
    && !defined(AO_DISABLE_GCC_ATOMICS)
  /* Probably, it could be enabled even for earlier gcc/clang versions. */
# define AO_GCC_ATOMIC_TEST_AND_SET
#endif

#ifdef __native_client__
  /* Mask instruction should immediately precede access instruction.    */
# define AO_MASK_PTR(reg) "       bical " reg ", " reg ", #0xc0000000\n"
# define AO_BR_ALIGN "       .align 4\n"
#else
# define AO_MASK_PTR(reg) /* empty */
# define AO_BR_ALIGN /* empty */
#endif

#if defined(__thumb__) && !defined(__thumb2__)
  /* Thumb One mode does not have ARM "mcr", "swp" and some load/store  */
  /* instructions, so we temporarily switch to ARM mode and go back     */
  /* afterwards (clobbering "r3" register).                             */
# define AO_THUMB_GO_ARM \
           "       adr     r3, 4f\n" \
           "       bx      r3\n" \
           "      .align\n" \
           "      .arm\n" \
           AO_BR_ALIGN \
           "4:\n"
# define AO_THUMB_RESTORE_MODE \
           "       adr     r3, 5f + 1\n" \
           "       bx      r3\n" \
           "       .thumb\n" \
           AO_BR_ALIGN \
           "5:\n"
# define AO_THUMB_SWITCH_CLOBBERS "r3",
#else
# define AO_THUMB_GO_ARM /* empty */
# define AO_THUMB_RESTORE_MODE /* empty */
# define AO_THUMB_SWITCH_CLOBBERS /* empty */
#endif /* !__thumb__ */

/* NEC LE-IT: gcc has no way to easily check the arm architecture       */
/* but it defines only one (or several) of __ARM_ARCH_x__ to be true.   */
#if !defined(__ARM_ARCH_2__) && !defined(__ARM_ARCH_3__) \
    && !defined(__ARM_ARCH_3M__) && !defined(__ARM_ARCH_4__) \
    && !defined(__ARM_ARCH_4T__) \
    && ((!defined(__ARM_ARCH_5__) && !defined(__ARM_ARCH_5E__) \
         && !defined(__ARM_ARCH_5T__) && !defined(__ARM_ARCH_5TE__) \
         && !defined(__ARM_ARCH_5TEJ__) && !defined(__ARM_ARCH_6M__)) \
        || defined(__ARM_ARCH_7__) || defined(__ARM_ARCH_7A__) \
        || defined(__ARM_ARCH_8A__))
# define AO_ARM_HAVE_LDREX
# if !defined(__ARM_ARCH_6__) && !defined(__ARM_ARCH_6J__) \
     && !defined(__ARM_ARCH_6T2__)
    /* LDREXB/STREXB and LDREXH/STREXH are present in ARMv6K/Z+.        */
#   define AO_ARM_HAVE_LDREXBH
# endif
# if !defined(__ARM_ARCH_6__) && !defined(__ARM_ARCH_6J__) \
     && !defined(__ARM_ARCH_6T2__) && !defined(__ARM_ARCH_6Z__) \
     && !defined(__ARM_ARCH_6ZT2__)
#   if !defined(__ARM_ARCH_6K__) && !defined(__ARM_ARCH_6KZ__) \
       && !defined(__ARM_ARCH_6ZK__)
      /* DMB is present in ARMv6M and ARMv7+.   */
#     define AO_ARM_HAVE_DMB
#   endif
#   if (!defined(__thumb__) \
        || (defined(__thumb2__) && !defined(__ARM_ARCH_7__) \
            && !defined(__ARM_ARCH_7M__) && !defined(__ARM_ARCH_7EM__))) \
       && (!defined(__clang__) || AO_CLANG_PREREQ(3, 3))
      /* LDREXD/STREXD present in ARMv6K/M+ (see gas/config/tc-arm.c).  */
      /* In the Thumb mode, this works only starting from ARMv7 (except */
      /* for the base and 'M' models).  Clang3.2 (and earlier) does not */
      /* allocate register pairs for LDREXD/STREXD properly (besides,   */
      /* Clang3.1 does not support "%H<r>" operand specification).      */
#     define AO_ARM_HAVE_LDREXD
#   endif /* !thumb || ARMv7A || ARMv7R+ */
# endif /* ARMv7+ */
#endif /* ARMv6+ */

#if !defined(__ARM_ARCH_2__) && !defined(__ARM_ARCH_6M__) \
    && !defined(__ARM_ARCH_8A__) && !defined(__thumb2__)
# define AO_ARM_HAVE_SWP
                /* Note: ARMv6M is excluded due to no ARM mode support. */
                /* Also, SWP is obsoleted for ARMv8+.                   */
#endif /* !__thumb2__ */

#if !defined(AO_UNIPROCESSOR) && defined(AO_ARM_HAVE_DMB) \
    && !defined(AO_PREFER_BUILTIN_ATOMICS)
  AO_INLINE void
  AO_nop_write(void)
  {
    /* AO_THUMB_GO_ARM is empty. */
    /* This will target the system domain and thus be overly            */
    /* conservative as the CPUs (even in case of big.LITTLE SoC) will   */
    /* occupy the inner shareable domain.                               */
    /* The plain variant (dmb st) is theoretically slower, and should   */
    /* not be needed.  That said, with limited experimentation, a CPU   */
    /* implementation for which it actually matters has not been found  */
    /* yet, though they should already exist.                           */
    /* Anyway, note that the "st" and "ishst" barriers are actually     */
    /* quite weak and, as the libatomic_ops documentation states,       */
    /* usually not what you really want.                                */
    __asm__ __volatile__("dmb ishst" : : : "memory");
  }
# define AO_HAVE_nop_write
#endif /* AO_ARM_HAVE_DMB */

#ifndef AO_GCC_ATOMIC_TEST_AND_SET

#ifdef AO_UNIPROCESSOR
  /* If only a single processor (core) is used, AO_UNIPROCESSOR could   */
  /* be defined by the client to avoid unnecessary memory barrier.      */
  AO_INLINE void
  AO_nop_full(void)
  {
    AO_compiler_barrier();
  }
# define AO_HAVE_nop_full

#elif defined(AO_ARM_HAVE_DMB)
  /* ARMv7 is compatible to ARMv6 but has a simpler command for issuing */
  /* a memory barrier (DMB).  Raising it via CP15 should still work     */
  /* (but slightly less efficient because it requires the use of        */
  /* a general-purpose register).                                       */
  AO_INLINE void
  AO_nop_full(void)
  {
    /* AO_THUMB_GO_ARM is empty. */
    __asm__ __volatile__("dmb" : : : "memory");
  }
# define AO_HAVE_nop_full

#elif defined(AO_ARM_HAVE_LDREX)
  /* ARMv6 is the first architecture providing support for a simple     */
  /* LL/SC.  A data memory barrier must be raised via CP15 command.     */
  AO_INLINE void
  AO_nop_full(void)
  {
    unsigned dest = 0;

    /* Issue a data memory barrier (keeps ordering of memory    */
    /* transactions before and after this operation).           */
    __asm__ __volatile__("@AO_nop_full\n"
      AO_THUMB_GO_ARM
      "       mcr p15,0,%0,c7,c10,5\n"
      AO_THUMB_RESTORE_MODE
      : "=&r"(dest)
      : /* empty */
      : AO_THUMB_SWITCH_CLOBBERS "memory");
  }
# define AO_HAVE_nop_full

#else
  /* AO_nop_full() is emulated using AO_test_and_set_full().    */
#endif /* !AO_UNIPROCESSOR && !AO_ARM_HAVE_LDREX */

#endif /* !AO_GCC_ATOMIC_TEST_AND_SET */

#ifdef AO_ARM_HAVE_LDREX

  /* "ARM Architecture Reference Manual" (chapter A3.5.3) says that the */
  /* single-copy atomic processor accesses are all byte accesses, all   */
  /* halfword accesses to halfword-aligned locations, all word accesses */
  /* to word-aligned locations.                                         */
  /* There is only a single concern related to AO store operations:     */
  /* a direct write (by STR[B/H] instruction) will not be recognized    */
  /* by the LL/SC construct on the same CPU (i.e., according to ARM     */
  /* documentation, e.g., see CortexA8 TRM reference, point 8.5,        */
  /* atomic "store" (using LDREX/STREX[B/H]) is the only safe way to    */
  /* set variables also used in LL/SC environment).                     */
  /* This is only a problem if interrupt handlers do not clear the      */
  /* reservation (by CLREX instruction or a dummy STREX one), as they   */
  /* almost certainly should (e.g., see restore_user_regs defined in    */
  /* arch/arm/kernel/entry-header.S of Linux.  Nonetheless, there is    */
  /* a doubt this was properly implemented in some ancient OS releases. */
# ifdef AO_BROKEN_TASKSWITCH_CLREX

#   define AO_SKIPATOMIC_store
#   define AO_SKIPATOMIC_store_release
#   define AO_SKIPATOMIC_char_store
#   define AO_SKIPATOMIC_char_store_release
#   define AO_SKIPATOMIC_short_store
#   define AO_SKIPATOMIC_short_store_release
#   define AO_SKIPATOMIC_int_store
#   define AO_SKIPATOMIC_int_store_release

#   ifndef AO_PREFER_BUILTIN_ATOMICS

    AO_INLINE void AO_store(volatile AO_t *addr, AO_t value)
    {
      int flag;

      __asm__ __volatile__("@AO_store\n"
        AO_THUMB_GO_ARM
        AO_BR_ALIGN
        "1: " AO_MASK_PTR("%2")
        "       ldrex %0, [%2]\n"
        AO_MASK_PTR("%2")
        "       strex %0, %3, [%2]\n"
        "       teq %0, #0\n"
        "       bne 1b\n"
        AO_THUMB_RESTORE_MODE
        : "=&r" (flag), "+m" (*addr)
        : "r" (addr), "r" (value)
        : AO_THUMB_SWITCH_CLOBBERS "cc");
    }
#   define AO_HAVE_store

#   ifdef AO_ARM_HAVE_LDREXBH
      AO_INLINE void AO_char_store(volatile unsigned char *addr,
                                   unsigned char value)
      {
        int flag;

        __asm__ __volatile__("@AO_char_store\n"
          AO_THUMB_GO_ARM
          AO_BR_ALIGN
          "1: " AO_MASK_PTR("%2")
          "       ldrexb %0, [%2]\n"
          AO_MASK_PTR("%2")
          "       strexb %0, %3, [%2]\n"
          "       teq    %0, #0\n"
          "       bne 1b\n"
          AO_THUMB_RESTORE_MODE
          : "=&r" (flag), "+m" (*addr)
          : "r" (addr), "r" (value)
          : AO_THUMB_SWITCH_CLOBBERS "cc");
      }
#     define AO_HAVE_char_store

      AO_INLINE void AO_short_store(volatile unsigned short *addr,
                                    unsigned short value)
      {
        int flag;

        __asm__ __volatile__("@AO_short_store\n"
          AO_THUMB_GO_ARM
          AO_BR_ALIGN
          "1: " AO_MASK_PTR("%2")
          "       ldrexh %0, [%2]\n"
          AO_MASK_PTR("%2")
          "       strexh %0, %3, [%2]\n"
          "       teq    %0, #0\n"
          "       bne 1b\n"
          AO_THUMB_RESTORE_MODE
          : "=&r" (flag), "+m" (*addr)
          : "r" (addr), "r" (value)
          : AO_THUMB_SWITCH_CLOBBERS "cc");
      }
#     define AO_HAVE_short_store
#   endif /* AO_ARM_HAVE_LDREXBH */

#   endif /* !AO_PREFER_BUILTIN_ATOMICS */

# elif !defined(AO_GCC_ATOMIC_TEST_AND_SET)
#   include "../loadstore/atomic_store.h"
    /* AO_int_store is defined in ao_t_is_int.h.    */
# endif /* !AO_BROKEN_TASKSWITCH_CLREX */

#endif /* AO_ARM_HAVE_LDREX */

#ifndef AO_GCC_ATOMIC_TEST_AND_SET

# include "../test_and_set_t_is_ao_t.h" /* Probably suboptimal  */

#ifdef AO_ARM_HAVE_LDREX

  /* AO_t/char/short/int load is simple reading.                */
  /* Unaligned accesses are not guaranteed to be atomic.        */
# define AO_ACCESS_CHECK_ALIGNED
# define AO_ACCESS_short_CHECK_ALIGNED
# define AO_ACCESS_int_CHECK_ALIGNED
# include "../all_atomic_only_load.h"

# ifndef AO_HAVE_char_store
#   include "../loadstore/char_atomic_store.h"
#   include "../loadstore/short_atomic_store.h"
# endif

/* NEC LE-IT: replace the SWAP as recommended by ARM:
   "Applies to: ARM11 Cores
      Though the SWP instruction will still work with ARM V6 cores, it is
      recommended to use the new V6 synchronization instructions.  The SWP
      instruction produces 'locked' read and write accesses which are atomic,
      i.e. another operation cannot be done between these locked accesses which
      ties up external bus (AHB, AXI) bandwidth and can increase worst case
      interrupt latencies. LDREX, STREX are more flexible, other instructions
      can be done between the LDREX and STREX accesses."
*/
#ifndef AO_PREFER_GENERALIZED
#if !defined(AO_FORCE_USE_SWP) || !defined(AO_ARM_HAVE_SWP)
  /* But, on the other hand, there could be a considerable performance  */
  /* degradation in case of a race.  Eg., test_atomic.c executing       */
  /* test_and_set test on a dual-core ARMv7 processor using LDREX/STREX */
  /* showed around 35 times lower performance than that using SWP.      */
  /* To force use of SWP instruction, use -D AO_FORCE_USE_SWP option    */
  /* (the latter is ignored if SWP instruction is unsupported).         */
  AO_INLINE AO_TS_VAL_t
  AO_test_and_set(volatile AO_TS_t *addr)
  {
    AO_TS_VAL_t oldval;
    int flag;

    __asm__ __volatile__("@AO_test_and_set\n"
      AO_THUMB_GO_ARM
      AO_BR_ALIGN
      "1: " AO_MASK_PTR("%3")
      "       ldrex   %0, [%3]\n"
      AO_MASK_PTR("%3")
      "       strex   %1, %4, [%3]\n"
      "       teq     %1, #0\n"
      "       bne     1b\n"
      AO_THUMB_RESTORE_MODE
      : "=&r"(oldval), "=&r"(flag), "+m"(*addr)
      : "r"(addr), "r"(1)
      : AO_THUMB_SWITCH_CLOBBERS "cc");
    return oldval;
  }
# define AO_HAVE_test_and_set
#endif /* !AO_FORCE_USE_SWP */

AO_INLINE AO_t
AO_fetch_and_add(volatile AO_t *p, AO_t incr)
{
  AO_t result, tmp;
  int flag;

  __asm__ __volatile__("@AO_fetch_and_add\n"
    AO_THUMB_GO_ARM
    AO_BR_ALIGN
    "1: " AO_MASK_PTR("%5")
    "       ldrex   %0, [%5]\n"         /* get original         */
    "       add     %2, %0, %4\n"       /* sum up in incr       */
    AO_MASK_PTR("%5")
    "       strex   %1, %2, [%5]\n"     /* store them           */
    "       teq     %1, #0\n"
    "       bne     1b\n"
    AO_THUMB_RESTORE_MODE
    : "=&r"(result), "=&r"(flag), "=&r"(tmp), "+m"(*p) /* 0..3 */
    : "r"(incr), "r"(p)                                /* 4..5 */
    : AO_THUMB_SWITCH_CLOBBERS "cc");
  return result;
}
#define AO_HAVE_fetch_and_add

AO_INLINE AO_t
AO_fetch_and_add1(volatile AO_t *p)
{
  AO_t result, tmp;
  int flag;

  __asm__ __volatile__("@AO_fetch_and_add1\n"
    AO_THUMB_GO_ARM
    AO_BR_ALIGN
    "1: " AO_MASK_PTR("%4")
    "       ldrex   %0, [%4]\n"         /* get original */
    "       add     %1, %0, #1\n"       /* increment */
    AO_MASK_PTR("%4")
    "       strex   %2, %1, [%4]\n"     /* store them */
    "       teq     %2, #0\n"
    "       bne     1b\n"
    AO_THUMB_RESTORE_MODE
    : "=&r"(result), "=&r"(tmp), "=&r"(flag), "+m"(*p)
    : "r"(p)
    : AO_THUMB_SWITCH_CLOBBERS "cc");
  return result;
}
#define AO_HAVE_fetch_and_add1

AO_INLINE AO_t
AO_fetch_and_sub1(volatile AO_t *p)
{
  AO_t result, tmp;
  int flag;

  __asm__ __volatile__("@AO_fetch_and_sub1\n"
    AO_THUMB_GO_ARM
    AO_BR_ALIGN
    "1: " AO_MASK_PTR("%4")
    "       ldrex   %0, [%4]\n"         /* get original */
    "       sub     %1, %0, #1\n"       /* decrement */
    AO_MASK_PTR("%4")
    "       strex   %2, %1, [%4]\n"     /* store them */
    "       teq     %2, #0\n"
    "       bne     1b\n"
    AO_THUMB_RESTORE_MODE
    : "=&r"(result), "=&r"(tmp), "=&r"(flag), "+m"(*p)
    : "r"(p)
    : AO_THUMB_SWITCH_CLOBBERS "cc");
  return result;
}
#define AO_HAVE_fetch_and_sub1

AO_INLINE void
AO_and(volatile AO_t *p, AO_t value)
{
  AO_t tmp, result;

  __asm__ __volatile__("@AO_and\n"
    AO_THUMB_GO_ARM
    AO_BR_ALIGN
    "1: " AO_MASK_PTR("%4")
    "       ldrex   %0, [%4]\n"
    "       and     %1, %0, %3\n"
    AO_MASK_PTR("%4")
    "       strex   %0, %1, [%4]\n"
    "       teq     %0, #0\n"
    "       bne     1b\n"
    AO_THUMB_RESTORE_MODE
    : "=&r" (tmp), "=&r" (result), "+m" (*p)
    : "r" (value), "r" (p)
    : AO_THUMB_SWITCH_CLOBBERS "cc");
}
#define AO_HAVE_and

AO_INLINE void
AO_or(volatile AO_t *p, AO_t value)
{
  AO_t tmp, result;

  __asm__ __volatile__("@AO_or\n"
    AO_THUMB_GO_ARM
    AO_BR_ALIGN
    "1: " AO_MASK_PTR("%4")
    "       ldrex   %0, [%4]\n"
    "       orr     %1, %0, %3\n"
    AO_MASK_PTR("%4")
    "       strex   %0, %1, [%4]\n"
    "       teq     %0, #0\n"
    "       bne     1b\n"
    AO_THUMB_RESTORE_MODE
    : "=&r" (tmp), "=&r" (result), "+m" (*p)
    : "r" (value), "r" (p)
    : AO_THUMB_SWITCH_CLOBBERS "cc");
}
#define AO_HAVE_or

AO_INLINE void
AO_xor(volatile AO_t *p, AO_t value)
{
  AO_t tmp, result;

  __asm__ __volatile__("@AO_xor\n"
    AO_THUMB_GO_ARM
    AO_BR_ALIGN
    "1: " AO_MASK_PTR("%4")
    "       ldrex   %0, [%4]\n"
    "       eor     %1, %0, %3\n"
    AO_MASK_PTR("%4")
    "       strex   %0, %1, [%4]\n"
    "       teq     %0, #0\n"
    "       bne     1b\n"
    AO_THUMB_RESTORE_MODE
    : "=&r" (tmp), "=&r" (result), "+m" (*p)
    : "r" (value), "r" (p)
    : AO_THUMB_SWITCH_CLOBBERS "cc");
}
#define AO_HAVE_xor
#endif /* !AO_PREFER_GENERALIZED */

#ifdef AO_ARM_HAVE_LDREXBH
  AO_INLINE unsigned char
  AO_char_fetch_and_add(volatile unsigned char *p, unsigned char incr)
  {
    unsigned result, tmp;
    int flag;

    __asm__ __volatile__("@AO_char_fetch_and_add\n"
      AO_THUMB_GO_ARM
      AO_BR_ALIGN
      "1: " AO_MASK_PTR("%5")
      "       ldrexb  %0, [%5]\n"
      "       add     %2, %0, %4\n"
      AO_MASK_PTR("%5")
      "       strexb  %1, %2, [%5]\n"
      "       teq     %1, #0\n"
      "       bne     1b\n"
      AO_THUMB_RESTORE_MODE
      : "=&r" (result), "=&r" (flag), "=&r" (tmp), "+m" (*p)
      : "r" ((unsigned)incr), "r" (p)
      : AO_THUMB_SWITCH_CLOBBERS "cc");
    return (unsigned char)result;
  }
# define AO_HAVE_char_fetch_and_add

  AO_INLINE unsigned short
  AO_short_fetch_and_add(volatile unsigned short *p, unsigned short incr)
  {
    unsigned result, tmp;
    int flag;

    __asm__ __volatile__("@AO_short_fetch_and_add\n"
      AO_THUMB_GO_ARM
      AO_BR_ALIGN
      "1: " AO_MASK_PTR("%5")
      "       ldrexh  %0, [%5]\n"
      "       add     %2, %0, %4\n"
      AO_MASK_PTR("%5")
      "       strexh  %1, %2, [%5]\n"
      "       teq     %1, #0\n"
      "       bne     1b\n"
      AO_THUMB_RESTORE_MODE
      : "=&r" (result), "=&r" (flag), "=&r" (tmp), "+m" (*p)
      : "r" ((unsigned)incr), "r" (p)
      : AO_THUMB_SWITCH_CLOBBERS "cc");
    return (unsigned short)result;
  }
# define AO_HAVE_short_fetch_and_add
#endif /* AO_ARM_HAVE_LDREXBH */

#ifndef AO_GENERALIZE_ASM_BOOL_CAS
  /* Returns nonzero if the comparison succeeded.       */
  AO_INLINE int
  AO_compare_and_swap(volatile AO_t *addr, AO_t old_val, AO_t new_val)
  {
    AO_t result, tmp;

    __asm__ __volatile__("@AO_compare_and_swap\n"
      AO_THUMB_GO_ARM
      AO_BR_ALIGN
      "1:     mov     %0, #2\n"         /* store a flag */
      AO_MASK_PTR("%3")
      "       ldrex   %1, [%3]\n"       /* get original */
      "       teq     %1, %4\n"         /* see if match */
      AO_MASK_PTR("%3")
#     ifdef __thumb2__
        /* TODO: Eliminate warning: it blocks containing wide Thumb */
        /* instructions are deprecated in ARMv8.                    */
        "       it      eq\n"
#     endif
      "       strexeq %0, %5, [%3]\n"   /* store new one if matched */
      "       teq     %0, #1\n"
      "       beq     1b\n"             /* if update failed, repeat */
      AO_THUMB_RESTORE_MODE
      : "=&r"(result), "=&r"(tmp), "+m"(*addr)
      : "r"(addr), "r"(old_val), "r"(new_val)
      : AO_THUMB_SWITCH_CLOBBERS "cc");
    return !(result&2); /* if succeeded then return 1 else 0 */
  }
# define AO_HAVE_compare_and_swap
#endif /* !AO_GENERALIZE_ASM_BOOL_CAS */

AO_INLINE AO_t
AO_fetch_compare_and_swap(volatile AO_t *addr, AO_t old_val, AO_t new_val)
{
  AO_t fetched_val;
  int flag;

  __asm__ __volatile__("@AO_fetch_compare_and_swap\n"
    AO_THUMB_GO_ARM
    AO_BR_ALIGN
    "1:     mov     %0, #2\n"           /* store a flag */
    AO_MASK_PTR("%3")
    "       ldrex   %1, [%3]\n"         /* get original */
    "       teq     %1, %4\n"           /* see if match */
    AO_MASK_PTR("%3")
#   ifdef __thumb2__
      "       it      eq\n"
#   endif
    "       strexeq %0, %5, [%3]\n"     /* store new one if matched */
    "       teq     %0, #1\n"
    "       beq     1b\n"               /* if update failed, repeat */
    AO_THUMB_RESTORE_MODE
    : "=&r"(flag), "=&r"(fetched_val), "+m"(*addr)
    : "r"(addr), "r"(old_val), "r"(new_val)
    : AO_THUMB_SWITCH_CLOBBERS "cc");
  return fetched_val;
}
#define AO_HAVE_fetch_compare_and_swap

#ifdef AO_ARM_HAVE_LDREXD
# include "../standard_ao_double_t.h"

  /* "ARM Architecture Reference Manual ARMv7-A/R edition" (chapter     */
  /* A3.5.3) says that memory accesses caused by LDREXD and STREXD      */
  /* instructions to doubleword-aligned locations are single-copy       */
  /* atomic; accesses to 64-bit elements by other instructions might    */
  /* not be single-copy atomic as they are executed as a sequence of    */
  /* 32-bit accesses.                                                   */
  AO_INLINE AO_double_t
  AO_double_load(const volatile AO_double_t *addr)
  {
    AO_double_t result;

    /* AO_THUMB_GO_ARM is empty. */
    __asm__ __volatile__("@AO_double_load\n"
      AO_MASK_PTR("%1")
      "       ldrexd  %0, %H0, [%1]"
      : "=&r" (result.AO_whole)
      : "r" (addr)
      /* : no clobber */);
    return result;
  }
# define AO_HAVE_double_load

  AO_INLINE void
  AO_double_store(volatile AO_double_t *addr, AO_double_t new_val)
  {
    AO_double_t old_val;
    int status;

    do {
      /* AO_THUMB_GO_ARM is empty. */
      __asm__ __volatile__("@AO_double_store\n"
        AO_MASK_PTR("%3")
        "       ldrexd  %0, %H0, [%3]\n"
        AO_MASK_PTR("%3")
        "       strexd  %1, %4, %H4, [%3]"
        : "=&r" (old_val.AO_whole), "=&r" (status), "+m" (*addr)
        : "r" (addr), "r" (new_val.AO_whole)
        : "cc");
    } while (AO_EXPECT_FALSE(status));
  }
# define AO_HAVE_double_store

  AO_INLINE int
  AO_double_compare_and_swap(volatile AO_double_t *addr,
                             AO_double_t old_val, AO_double_t new_val)
  {
    double_ptr_storage tmp;
    int result = 1;

    do {
      /* AO_THUMB_GO_ARM is empty. */
      __asm__ __volatile__("@AO_double_compare_and_swap\n"
        AO_MASK_PTR("%1")
        "       ldrexd  %0, %H0, [%1]\n" /* get original to r1 & r2 */
        : "=&r"(tmp)
        : "r"(addr)
        /* : no clobber */);
      if (tmp != old_val.AO_whole)
        break;
      __asm__ __volatile__(
        AO_MASK_PTR("%2")
        "       strexd  %0, %3, %H3, [%2]\n" /* store new one if matched */
        : "=&r"(result), "+m"(*addr)
        : "r" (addr), "r" (new_val.AO_whole)
        : "cc");
    } while (AO_EXPECT_FALSE(result));
    return !result;   /* if succeeded then return 1 else 0 */
  }
# define AO_HAVE_double_compare_and_swap
#endif /* AO_ARM_HAVE_LDREXD */

#else
/* pre ARMv6 architectures ... */

/* I found a slide set that, if I read it correctly, claims that        */
/* Loads followed by either a Load or Store are ordered, but nothing    */
/* else is.                                                             */
/* It appears that SWP is the only simple memory barrier.               */
#include "../all_aligned_atomic_load_store.h"

/* The code should run correctly on a multi-core ARMv6+ as well.        */

#endif /* !AO_ARM_HAVE_LDREX */

#if !defined(AO_HAVE_test_and_set_full) && !defined(AO_HAVE_test_and_set) \
    && defined (AO_ARM_HAVE_SWP) && (!defined(AO_PREFER_GENERALIZED) \
                                || !defined(AO_HAVE_fetch_compare_and_swap))
  AO_INLINE AO_TS_VAL_t
  AO_test_and_set_full(volatile AO_TS_t *addr)
  {
    AO_TS_VAL_t oldval;
    /* SWP on ARM is very similar to XCHG on x86.                       */
    /* The first operand is the result, the second the value            */
    /* to be stored.  Both registers must be different from addr.       */
    /* Make the address operand an early clobber output so it           */
    /* doesn't overlap with the other operands.  The early clobber      */
    /* on oldval is necessary to prevent the compiler allocating        */
    /* them to the same register if they are both unused.               */

    __asm__ __volatile__("@AO_test_and_set_full\n"
      AO_THUMB_GO_ARM
      AO_MASK_PTR("%3")
      "       swp %0, %2, [%3]\n"
                /* Ignore GCC "SWP is deprecated for this architecture" */
                /* warning here (for ARMv6+).                           */
      AO_THUMB_RESTORE_MODE
      : "=&r"(oldval), "=&r"(addr)
      : "r"(1), "1"(addr)
      : AO_THUMB_SWITCH_CLOBBERS "memory");
    return oldval;
  }
# define AO_HAVE_test_and_set_full
#endif /* !AO_HAVE_test_and_set[_full] && AO_ARM_HAVE_SWP */

#define AO_T_IS_INT

#else /* AO_GCC_ATOMIC_TEST_AND_SET */

# if defined(__clang__) && !defined(AO_ARM_HAVE_LDREX)
    /* As of clang-3.8, it cannot compile __atomic_and/or/xor_fetch     */
    /* library calls yet for pre ARMv6.                                 */
#   define AO_SKIPATOMIC_ANY_and_ANY
#   define AO_SKIPATOMIC_ANY_or_ANY
#   define AO_SKIPATOMIC_ANY_xor_ANY
# endif

# ifdef AO_ARM_HAVE_LDREXD
#   include "../standard_ao_double_t.h"
# endif
# include "generic.h"

#endif /* AO_GCC_ATOMIC_TEST_AND_SET */

#undef AO_ARM_HAVE_DMB
#undef AO_ARM_HAVE_LDREX
#undef AO_ARM_HAVE_LDREXBH
#undef AO_ARM_HAVE_LDREXD
#undef AO_ARM_HAVE_SWP
#undef AO_BR_ALIGN
#undef AO_MASK_PTR
#undef AO_SKIPATOMIC_ANY_and_ANY
#undef AO_SKIPATOMIC_ANY_or_ANY
#undef AO_SKIPATOMIC_ANY_xor_ANY
#undef AO_SKIPATOMIC_char_store
#undef AO_SKIPATOMIC_char_store_release
#undef AO_SKIPATOMIC_int_store
#undef AO_SKIPATOMIC_int_store_release
#undef AO_SKIPATOMIC_short_store
#undef AO_SKIPATOMIC_short_store_release
#undef AO_SKIPATOMIC_store
#undef AO_SKIPATOMIC_store_release
#undef AO_THUMB_GO_ARM
#undef AO_THUMB_RESTORE_MODE
#undef AO_THUMB_SWITCH_CLOBBERS
