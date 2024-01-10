/*
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1996-1999 by Silicon Graphics.  All rights reserved.
 * Copyright (c) 1999-2003 by Hewlett-Packard Company. All rights reserved.
 * Copyright (c) 2008-2018 Ivan Maidanski
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
 * Some of the machine specific code was borrowed from our GC distribution.
 */

#if (AO_GNUC_PREREQ(4, 8) || AO_CLANG_PREREQ(3, 4)) \
    && !defined(__INTEL_COMPILER) /* TODO: test and enable icc */ \
    && !defined(AO_DISABLE_GCC_ATOMICS)
# define AO_GCC_ATOMIC_TEST_AND_SET

# if defined(__APPLE_CC__)
    /* OS X 10.7 clang-425 lacks __GCC_HAVE_SYNC_COMPARE_AND_SWAP_n     */
    /* predefined macro (unlike e.g. OS X 10.11 clang-703).             */
#   define AO_GCC_FORCE_HAVE_CAS

#   ifdef __x86_64__
#     if !AO_CLANG_PREREQ(9, 0) /* < Apple clang-900 */
        /* Older Apple clang (e.g., clang-600 based on LLVM 3.5svn) had */
        /* some bug in the double word CAS implementation for x64.      */
#       define AO_SKIPATOMIC_double_compare_and_swap_ANY
#     endif

#   elif defined(__MACH__)
      /* OS X 10.8 lacks __atomic_load/store symbols for arch i386      */
      /* (even with a non-Apple clang).                                 */
#     ifndef MAC_OS_X_VERSION_MIN_REQUIRED
        /* Include this header just to import the version macro. */
#       include <AvailabilityMacros.h>
#     endif
#     if MAC_OS_X_VERSION_MIN_REQUIRED < 1090 /* MAC_OS_X_VERSION_10_9  */
#       define AO_SKIPATOMIC_DOUBLE_LOAD_STORE_ANY
#     endif
#   endif /* __i386__ */

# elif defined(__clang__)
#   if !defined(__x86_64__)
#     if !defined(AO_PREFER_BUILTIN_ATOMICS) && !defined(__CYGWIN__) \
         && !AO_CLANG_PREREQ(5, 0)
        /* At least clang-3.8/i686 (from NDK r11c) required to specify  */
        /* -latomic in case of a double-word atomic operation use.      */
#       define AO_SKIPATOMIC_double_compare_and_swap_ANY
#       define AO_SKIPATOMIC_DOUBLE_LOAD_STORE_ANY
#     endif /* !AO_PREFER_BUILTIN_ATOMICS */

#   elif !defined(__ILP32__)
#     if (!AO_CLANG_PREREQ(3, 5) && !defined(AO_PREFER_BUILTIN_ATOMICS)) \
         || (!AO_CLANG_PREREQ(4, 0) && defined(AO_ADDRESS_SANITIZER)) \
         || defined(AO_THREAD_SANITIZER)
        /* clang-3.4/x64 required -latomic.  clang-3.9/x64 seems to     */
        /* pass double-wide arguments to atomic operations incorrectly  */
        /* in case of ASan/TSan.                                        */
        /* TODO: As of clang-4.0, lock-free test_stack fails if TSan.   */
#       define AO_SKIPATOMIC_double_compare_and_swap_ANY
#       define AO_SKIPATOMIC_DOUBLE_LOAD_STORE_ANY
#     endif
#   endif /* __x86_64__ */

# elif AO_GNUC_PREREQ(7, 0) && !defined(AO_PREFER_BUILTIN_ATOMICS) \
       && !defined(AO_THREAD_SANITIZER) && !defined(__MINGW32__)
    /* gcc-7.x/x64 (gcc-7.2, at least) requires -latomic flag in case   */
    /* of double-word atomic operations use (but not in case of TSan).  */
    /* TODO: Revise it for the future gcc-7 releases. */
#   define AO_SKIPATOMIC_double_compare_and_swap_ANY
#   define AO_SKIPATOMIC_DOUBLE_LOAD_STORE_ANY
# endif /* __GNUC__ && !__clang__ */

# ifdef AO_SKIPATOMIC_DOUBLE_LOAD_STORE_ANY
#   define AO_SKIPATOMIC_double_load
#   define AO_SKIPATOMIC_double_load_acquire
#   define AO_SKIPATOMIC_double_store
#   define AO_SKIPATOMIC_double_store_release
#   undef AO_SKIPATOMIC_DOUBLE_LOAD_STORE_ANY
# endif

#else /* AO_DISABLE_GCC_ATOMICS */

/* The following really assume we have a 486 or better.  Unfortunately  */
/* gcc doesn't define a suitable feature test macro based on command    */
/* line options.                                                        */
/* We should perhaps test dynamically.                                  */

#include "../all_aligned_atomic_load_store.h"

#include "../test_and_set_t_is_char.h"

#if defined(__SSE2__) && !defined(AO_USE_PENTIUM4_INSTRS)
  /* "mfence" is a part of SSE2 set (introduced on Intel Pentium 4).    */
# define AO_USE_PENTIUM4_INSTRS
#endif

#if defined(AO_USE_PENTIUM4_INSTRS)
  AO_INLINE void
  AO_nop_full(void)
  {
    __asm__ __volatile__("mfence" : : : "memory");
  }
# define AO_HAVE_nop_full

#else
  /* We could use the cpuid instruction.  But that seems to be slower   */
  /* than the default implementation based on test_and_set_full.  Thus  */
  /* we omit that bit of misinformation here.                           */
#endif /* !AO_USE_PENTIUM4_INSTRS */

/* As far as we can tell, the lfence and sfence instructions are not    */
/* currently needed or useful for cached memory accesses.               */

/* Really only works for 486 and later */
#ifndef AO_PREFER_GENERALIZED
  AO_INLINE AO_t
  AO_fetch_and_add_full (volatile AO_t *p, AO_t incr)
  {
    AO_t result;

    __asm__ __volatile__ ("lock; xadd %0, %1"
                        : "=r" (result), "+m" (*p)
                        : "0" (incr)
                        : "memory");
    return result;
  }
# define AO_HAVE_fetch_and_add_full
#endif /* !AO_PREFER_GENERALIZED */

AO_INLINE unsigned char
AO_char_fetch_and_add_full (volatile unsigned char *p, unsigned char incr)
{
  unsigned char result;

  __asm__ __volatile__ ("lock; xaddb %0, %1"
                        : "=q" (result), "+m" (*p)
                        : "0" (incr)
                        : "memory");
  return result;
}
#define AO_HAVE_char_fetch_and_add_full

AO_INLINE unsigned short
AO_short_fetch_and_add_full (volatile unsigned short *p, unsigned short incr)
{
  unsigned short result;

  __asm__ __volatile__ ("lock; xaddw %0, %1"
                        : "=r" (result), "+m" (*p)
                        : "0" (incr)
                        : "memory");
  return result;
}
#define AO_HAVE_short_fetch_and_add_full

#ifndef AO_PREFER_GENERALIZED
  AO_INLINE void
  AO_and_full (volatile AO_t *p, AO_t value)
  {
    __asm__ __volatile__ ("lock; and %1, %0"
                        : "+m" (*p)
                        : "r" (value)
                        : "memory");
  }
# define AO_HAVE_and_full

  AO_INLINE void
  AO_or_full (volatile AO_t *p, AO_t value)
  {
    __asm__ __volatile__ ("lock; or %1, %0"
                        : "+m" (*p)
                        : "r" (value)
                        : "memory");
  }
# define AO_HAVE_or_full

  AO_INLINE void
  AO_xor_full (volatile AO_t *p, AO_t value)
  {
    __asm__ __volatile__ ("lock; xor %1, %0"
                        : "+m" (*p)
                        : "r" (value)
                        : "memory");
  }
# define AO_HAVE_xor_full

  /* AO_store_full could be implemented directly using "xchg" but it    */
  /* could be generalized efficiently as an ordinary store accomplished */
  /* with AO_nop_full ("mfence" instruction).                           */

AO_INLINE void
AO_char_and_full (volatile unsigned char *p, unsigned char value)
{
  __asm__ __volatile__ ("lock; andb %1, %0"
                        : "+m" (*p)
                        : "r" (value)
                        : "memory");
}
#define AO_HAVE_char_and_full

AO_INLINE void
AO_char_or_full (volatile unsigned char *p, unsigned char value)
{
  __asm__ __volatile__ ("lock; orb %1, %0"
                        : "+m" (*p)
                        : "r" (value)
                        : "memory");
}
#define AO_HAVE_char_or_full

AO_INLINE void
AO_char_xor_full (volatile unsigned char *p, unsigned char value)
{
  __asm__ __volatile__ ("lock; xorb %1, %0"
                        : "+m" (*p)
                        : "r" (value)
                        : "memory");
}
#define AO_HAVE_char_xor_full

AO_INLINE void
AO_short_and_full (volatile unsigned short *p, unsigned short value)
{
  __asm__ __volatile__ ("lock; andw %1, %0"
                        : "+m" (*p)
                        : "r" (value)
                        : "memory");
}
#define AO_HAVE_short_and_full

AO_INLINE void
AO_short_or_full (volatile unsigned short *p, unsigned short value)
{
  __asm__ __volatile__ ("lock; orw %1, %0"
                        : "+m" (*p)
                        : "r" (value)
                        : "memory");
}
#define AO_HAVE_short_or_full

AO_INLINE void
AO_short_xor_full (volatile unsigned short *p, unsigned short value)
{
  __asm__ __volatile__ ("lock; xorw %1, %0"
                        : "+m" (*p)
                        : "r" (value)
                        : "memory");
}
#define AO_HAVE_short_xor_full
#endif /* !AO_PREFER_GENERALIZED */

AO_INLINE AO_TS_VAL_t
AO_test_and_set_full(volatile AO_TS_t *addr)
{
  unsigned char oldval;
  /* Note: the "xchg" instruction does not need a "lock" prefix */
  __asm__ __volatile__ ("xchgb %0, %1"
                        : "=q" (oldval), "+m" (*addr)
                        : "0" ((unsigned char)0xff)
                        : "memory");
  return (AO_TS_VAL_t)oldval;
}
#define AO_HAVE_test_and_set_full

#ifndef AO_GENERALIZE_ASM_BOOL_CAS
  /* Returns nonzero if the comparison succeeded.       */
  AO_INLINE int
  AO_compare_and_swap_full(volatile AO_t *addr, AO_t old, AO_t new_val)
  {
#   ifdef AO_USE_SYNC_CAS_BUILTIN
      return (int)__sync_bool_compare_and_swap(addr, old, new_val
                                               /* empty protection list */);
                /* Note: an empty list of variables protected by the    */
                /* memory barrier should mean all globally accessible   */
                /* variables are protected.                             */
#   else
      char result;
#     if defined(__GCC_ASM_FLAG_OUTPUTS__)
        AO_t dummy;

        __asm__ __volatile__ ("lock; cmpxchg %3, %0"
                        : "+m" (*addr), "=@ccz" (result), "=a" (dummy)
                        : "r" (new_val), "a" (old)
                        : "memory");
#     else
        __asm__ __volatile__ ("lock; cmpxchg %2, %0; setz %1"
                        : "+m" (*addr), "=a" (result)
                        : "r" (new_val), "a" (old)
                        : "memory");
#     endif
      return (int)result;
#   endif
  }
# define AO_HAVE_compare_and_swap_full
#endif /* !AO_GENERALIZE_ASM_BOOL_CAS */

AO_INLINE AO_t
AO_fetch_compare_and_swap_full(volatile AO_t *addr, AO_t old_val,
                               AO_t new_val)
{
# ifdef AO_USE_SYNC_CAS_BUILTIN
    return __sync_val_compare_and_swap(addr, old_val, new_val
                                       /* empty protection list */);
# else
    AO_t fetched_val;
    __asm__ __volatile__ ("lock; cmpxchg %3, %1"
                        : "=a" (fetched_val), "+m" (*addr)
                        : "a" (old_val), "r" (new_val)
                        : "memory");
    return fetched_val;
# endif
}
#define AO_HAVE_fetch_compare_and_swap_full

  AO_INLINE unsigned char
  AO_char_fetch_compare_and_swap_full(volatile unsigned char *addr,
                                      unsigned char old_val,
                                      unsigned char new_val)
  {
#   ifdef AO_USE_SYNC_CAS_BUILTIN
      return __sync_val_compare_and_swap(addr, old_val, new_val
                                         /* empty protection list */);
#   else
      unsigned char fetched_val;

      __asm__ __volatile__ ("lock; cmpxchgb %3, %1"
                            : "=a" (fetched_val), "+m" (*addr)
                            : "a" (old_val), "q" (new_val)
                            : "memory");
      return fetched_val;
#   endif
  }
# define AO_HAVE_char_fetch_compare_and_swap_full

  AO_INLINE unsigned short
  AO_short_fetch_compare_and_swap_full(volatile unsigned short *addr,
                                       unsigned short old_val,
                                       unsigned short new_val)
  {
#   ifdef AO_USE_SYNC_CAS_BUILTIN
      return __sync_val_compare_and_swap(addr, old_val, new_val
                                         /* empty protection list */);
#   else
      unsigned short fetched_val;

      __asm__ __volatile__ ("lock; cmpxchgw %3, %1"
                            : "=a" (fetched_val), "+m" (*addr)
                            : "a" (old_val), "r" (new_val)
                            : "memory");
      return fetched_val;
#   endif
  }
# define AO_HAVE_short_fetch_compare_and_swap_full

# if defined(__x86_64__) && !defined(__ILP32__)
    AO_INLINE unsigned int
    AO_int_fetch_compare_and_swap_full(volatile unsigned int *addr,
                                       unsigned int old_val,
                                       unsigned int new_val)
    {
#     ifdef AO_USE_SYNC_CAS_BUILTIN
        return __sync_val_compare_and_swap(addr, old_val, new_val
                                           /* empty protection list */);
#     else
        unsigned int fetched_val;

        __asm__ __volatile__ ("lock; cmpxchgl %3, %1"
                            : "=a" (fetched_val), "+m" (*addr)
                            : "a" (old_val), "r" (new_val)
                            : "memory");
        return fetched_val;
#     endif
    }
#   define AO_HAVE_int_fetch_compare_and_swap_full

#   ifndef AO_PREFER_GENERALIZED
    AO_INLINE unsigned int
    AO_int_fetch_and_add_full (volatile unsigned int *p, unsigned int incr)
    {
      unsigned int result;

      __asm__ __volatile__ ("lock; xaddl %0, %1"
                            : "=r" (result), "+m" (*p)
                            : "0" (incr)
                            : "memory");
      return result;
    }
#   define AO_HAVE_int_fetch_and_add_full

    AO_INLINE void
    AO_int_and_full (volatile unsigned int *p, unsigned int value)
    {
      __asm__ __volatile__ ("lock; andl %1, %0"
                            : "+m" (*p)
                            : "r" (value)
                            : "memory");
    }
#   define AO_HAVE_int_and_full

    AO_INLINE void
    AO_int_or_full (volatile unsigned int *p, unsigned int value)
    {
      __asm__ __volatile__ ("lock; orl %1, %0"
                            : "+m" (*p)
                            : "r" (value)
                            : "memory");
    }
#   define AO_HAVE_int_or_full

    AO_INLINE void
    AO_int_xor_full (volatile unsigned int *p, unsigned int value)
    {
      __asm__ __volatile__ ("lock; xorl %1, %0"
                            : "+m" (*p)
                            : "r" (value)
                            : "memory");
    }
#   define AO_HAVE_int_xor_full
#   endif /* !AO_PREFER_GENERALIZED */

# else
#   define AO_T_IS_INT
# endif /* !x86_64 || ILP32 */

  /* Real X86 implementations, except for some old 32-bit WinChips,     */
  /* appear to enforce ordering between memory operations, EXCEPT that  */
  /* a later read can pass earlier writes, presumably due to the        */
  /* visible presence of store buffers.                                 */
  /* We ignore both the WinChips and the fact that the official specs   */
  /* seem to be much weaker (and arguably too weak to be usable).       */
# include "../ordered_except_wr.h"

#endif /* AO_DISABLE_GCC_ATOMICS */

#if defined(AO_GCC_ATOMIC_TEST_AND_SET) \
    && !defined(AO_SKIPATOMIC_double_compare_and_swap_ANY)

# if defined(__ILP32__) || !defined(__x86_64__) /* 32-bit AO_t */ \
     || defined(__GCC_HAVE_SYNC_COMPARE_AND_SWAP_16) /* 64-bit AO_t */
#   include "../standard_ao_double_t.h"
# endif

#elif !defined(__x86_64__) && (!defined(AO_USE_SYNC_CAS_BUILTIN) \
                               || defined(AO_GCC_ATOMIC_TEST_AND_SET))
# include "../standard_ao_double_t.h"

  /* Reading or writing a quadword aligned on a 64-bit boundary is      */
  /* always carried out atomically on at least a Pentium according to   */
  /* Chapter 8.1.1 of Volume 3A Part 1 of Intel processor manuals.      */
# ifndef AO_PREFER_GENERALIZED
#   define AO_ACCESS_double_CHECK_ALIGNED
#   include "../loadstore/double_atomic_load_store.h"
# endif

  /* Returns nonzero if the comparison succeeded.       */
  /* Really requires at least a Pentium.                */
  AO_INLINE int
  AO_compare_double_and_swap_double_full(volatile AO_double_t *addr,
                                         AO_t old_val1, AO_t old_val2,
                                         AO_t new_val1, AO_t new_val2)
  {
    char result;
#   if defined(__PIC__) && !(AO_GNUC_PREREQ(5, 1) || AO_CLANG_PREREQ(4, 0))
      AO_t saved_ebx;
      AO_t dummy;

      /* The following applies to an ancient GCC (and, probably, it was   */
      /* never needed for Clang):                                         */
      /* If PIC is turned on, we cannot use ebx as it is reserved for the */
      /* GOT pointer.  We should save and restore ebx.  The proposed      */
      /* solution is not so efficient as the older alternatives using     */
      /* push ebx or edi as new_val1 (w/o clobbering edi and temporary    */
      /* local variable usage) but it is more portable (it works even if  */
      /* ebx is not used as GOT pointer, and it works for the buggy GCC   */
      /* releases that incorrectly evaluate memory operands offset in the */
      /* inline assembly after push).                                     */
#     ifdef __OPTIMIZE__
        __asm__ __volatile__("mov %%ebx, %2\n\t" /* save ebx */
                             "lea %0, %%edi\n\t" /* in case addr is in ebx */
                             "mov %7, %%ebx\n\t" /* load new_val1 */
                             "lock; cmpxchg8b (%%edi)\n\t"
                             "mov %2, %%ebx\n\t" /* restore ebx */
                             "setz %1"
                        : "+m" (*addr), "=a" (result),
                          "=m" (saved_ebx), "=d" (dummy)
                        : "d" (old_val2), "a" (old_val1),
                          "c" (new_val2), "m" (new_val1)
                        : "%edi", "memory");
#     else
        /* A less-efficient code manually preserving edi if GCC invoked */
        /* with -O0 option (otherwise it fails while finding a register */
        /* in class 'GENERAL_REGS').                                    */
        AO_t saved_edi;
        __asm__ __volatile__("mov %%edi, %3\n\t" /* save edi */
                             "mov %%ebx, %2\n\t" /* save ebx */
                             "lea %0, %%edi\n\t" /* in case addr is in ebx */
                             "mov %8, %%ebx\n\t" /* load new_val1 */
                             "lock; cmpxchg8b (%%edi)\n\t"
                             "mov %2, %%ebx\n\t" /* restore ebx */
                             "mov %3, %%edi\n\t" /* restore edi */
                             "setz %1"
                        : "+m" (*addr), "=a" (result),
                          "=m" (saved_ebx), "=m" (saved_edi), "=d" (dummy)
                        : "d" (old_val2), "a" (old_val1),
                          "c" (new_val2), "m" (new_val1)
                        : "memory");
#     endif
#   else
      /* For non-PIC mode, this operation could be simplified (and be   */
      /* faster) by using ebx as new_val1.  Reuse of the PIC hard       */
      /* register, instead of using a fixed register, is implemented    */
      /* in Clang and GCC 5.1+, at least. (Older GCC refused to compile */
      /* such code for PIC mode).                                       */
#     if defined(__GCC_ASM_FLAG_OUTPUTS__)
        __asm__ __volatile__ ("lock; cmpxchg8b %0"
                        : "+m" (*addr), "=@ccz" (result),
                          "+d" (old_val2), "+a" (old_val1)
                        : "c" (new_val2), "b" (new_val1)
                        : "memory");
#     else
        AO_t dummy; /* an output for clobbered edx */

        __asm__ __volatile__ ("lock; cmpxchg8b %0; setz %1"
                        : "+m" (*addr), "=a" (result), "=d" (dummy)
                        : "d" (old_val2), "a" (old_val1),
                          "c" (new_val2), "b" (new_val1)
                        : "memory");
#     endif
#   endif
    return (int) result;
  }
# define AO_HAVE_compare_double_and_swap_double_full

#elif defined(__ILP32__) || !defined(__x86_64__)
# include "../standard_ao_double_t.h"

  /* Reading or writing a quadword aligned on a 64-bit boundary is      */
  /* always carried out atomically (requires at least a Pentium).       */
# ifndef AO_PREFER_GENERALIZED
#   define AO_ACCESS_double_CHECK_ALIGNED
#   include "../loadstore/double_atomic_load_store.h"
# endif

  /* X32 has native support for 64-bit integer operations (AO_double_t  */
  /* is a 64-bit integer and we could use 64-bit cmpxchg).              */
  /* This primitive is used by compare_double_and_swap_double_full.     */
  AO_INLINE int
  AO_double_compare_and_swap_full(volatile AO_double_t *addr,
                                  AO_double_t old_val, AO_double_t new_val)
  {
    /* It is safe to use __sync CAS built-in here.      */
    return __sync_bool_compare_and_swap(&addr->AO_whole,
                                        old_val.AO_whole, new_val.AO_whole
                                        /* empty protection list */);
  }
# define AO_HAVE_double_compare_and_swap_full

#elif defined(AO_CMPXCHG16B_AVAILABLE) \
      || (defined(__GCC_HAVE_SYNC_COMPARE_AND_SWAP_16) \
          && !defined(AO_THREAD_SANITIZER))
# include "../standard_ao_double_t.h"

  /* The Intel and AMD Architecture Programmer Manuals state roughly    */
  /* the following:                                                     */
  /* - CMPXCHG16B (with a LOCK prefix) can be used to perform 16-byte   */
  /* atomic accesses in 64-bit mode (with certain alignment             */
  /* restrictions);                                                     */
  /* - SSE instructions that access data larger than a quadword (like   */
  /* MOVDQA) may be implemented using multiple memory accesses;         */
  /* - LOCK prefix causes an invalid-opcode exception when used with    */
  /* 128-bit media (SSE) instructions.                                  */
  /* Thus, currently, the only way to implement lock-free double_load   */
  /* and double_store on x86_64 is to use CMPXCHG16B (if available).    */

  /* NEC LE-IT: older AMD Opterons are missing this instruction.        */
  /* On these machines SIGILL will be thrown.                           */
  /* Define AO_WEAK_DOUBLE_CAS_EMULATION to have an emulated (lock      */
  /* based) version available.                                          */
  /* HB: Changed this to not define either by default.  There are       */
  /* enough machines and tool chains around on which cmpxchg16b         */
  /* doesn't work.  And the emulation is unsafe by our usual rules.     */
  /* However both are clearly useful in certain cases.                  */

  AO_INLINE int
  AO_compare_double_and_swap_double_full(volatile AO_double_t *addr,
                                         AO_t old_val1, AO_t old_val2,
                                         AO_t new_val1, AO_t new_val2)
  {
    char result;

#   if defined(__GCC_ASM_FLAG_OUTPUTS__)
      __asm__ __volatile__("lock; cmpxchg16b %0"
                        : "+m" (*addr), "=@ccz" (result),
                          "+d" (old_val2), "+a" (old_val1)
                        : "c" (new_val2), "b" (new_val1)
                        : "memory");
#   else
      AO_t dummy; /* an output for clobbered rdx */

      __asm__ __volatile__("lock; cmpxchg16b %0; setz %1"
                        : "+m" (*addr), "=a" (result), "=d" (dummy)
                        : "d" (old_val2), "a" (old_val1),
                          "c" (new_val2), "b" (new_val1)
                        : "memory");
#   endif
    return (int) result;
  }
# define AO_HAVE_compare_double_and_swap_double_full

#elif defined(AO_WEAK_DOUBLE_CAS_EMULATION)
# include "../standard_ao_double_t.h"

# ifdef __cplusplus
    extern "C" {
# endif

  /* This one provides spinlock based emulation of CAS implemented in   */
  /* atomic_ops.c.  We probably do not want to do this here, since it   */
  /* is not atomic with respect to other kinds of updates of *addr.     */
  /* On the other hand, this may be a useful facility on occasion.      */
  int AO_compare_double_and_swap_double_emulation(
                                        volatile AO_double_t *addr,
                                        AO_t old_val1, AO_t old_val2,
                                        AO_t new_val1, AO_t new_val2);

# ifdef __cplusplus
    } /* extern "C" */
# endif

  AO_INLINE int
  AO_compare_double_and_swap_double_full(volatile AO_double_t *addr,
                                         AO_t old_val1, AO_t old_val2,
                                         AO_t new_val1, AO_t new_val2)
  {
    return AO_compare_double_and_swap_double_emulation(addr,
                                old_val1, old_val2, new_val1, new_val2);
  }
# define AO_HAVE_compare_double_and_swap_double_full
#endif /* x86_64 && !ILP32 && CAS_EMULATION && !AO_CMPXCHG16B_AVAILABLE */

#ifdef AO_GCC_ATOMIC_TEST_AND_SET
# include "generic.h"
#endif

#undef AO_GCC_FORCE_HAVE_CAS
#undef AO_SKIPATOMIC_double_compare_and_swap_ANY
#undef AO_SKIPATOMIC_double_load
#undef AO_SKIPATOMIC_double_load_acquire
#undef AO_SKIPATOMIC_double_store
#undef AO_SKIPATOMIC_double_store_release
