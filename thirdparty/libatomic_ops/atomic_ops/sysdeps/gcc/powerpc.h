/*
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1996-1999 by Silicon Graphics.  All rights reserved.
 * Copyright (c) 2003-2011 Hewlett-Packard Development Company, L.P.
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

/* Memory model documented at http://www-106.ibm.com/developerworks/    */
/* eserver/articles/archguide.html and (clearer)                        */
/* http://www-106.ibm.com/developerworks/eserver/articles/powerpc.html. */
/* There appears to be no implicit ordering between any kind of         */
/* independent memory references.                                       */

/* TODO: Implement double-wide operations if available. */

#if (AO_GNUC_PREREQ(4, 8) || AO_CLANG_PREREQ(3, 8)) \
    && !defined(AO_DISABLE_GCC_ATOMICS)
  /* Probably, it could be enabled even for earlier gcc/clang versions. */

  /* TODO: As of clang-3.8.1, it emits lwsync in AO_load_acquire        */
  /* (i.e., the code is less efficient than the one given below).       */

# include "generic.h"

#else /* AO_DISABLE_GCC_ATOMICS */

/* Architecture enforces some ordering based on control dependence.     */
/* I don't know if that could help.                                     */
/* Data-dependent loads are always ordered.                             */
/* Based on the above references, eieio is intended for use on          */
/* uncached memory, which we don't support.  It does not order loads    */
/* from cached memory.                                                  */

#include "../all_aligned_atomic_load_store.h"

#include "../test_and_set_t_is_ao_t.h"
        /* There seems to be no byte equivalent of lwarx, so this       */
        /* may really be what we want, at least in the 32-bit case.     */

AO_INLINE void
AO_nop_full(void)
{
  __asm__ __volatile__("sync" : : : "memory");
}
#define AO_HAVE_nop_full

/* lwsync apparently works for everything but a StoreLoad barrier.      */
AO_INLINE void
AO_lwsync(void)
{
#ifdef __NO_LWSYNC__
  __asm__ __volatile__("sync" : : : "memory");
#else
  __asm__ __volatile__("lwsync" : : : "memory");
#endif
}

#define AO_nop_write() AO_lwsync()
#define AO_HAVE_nop_write

#define AO_nop_read() AO_lwsync()
#define AO_HAVE_nop_read

#if defined(__powerpc64__) || defined(__ppc64__) || defined(__64BIT__)
  /* ppc64 uses ld not lwz */
# define AO_PPC_LD      "ld"
# define AO_PPC_LxARX   "ldarx"
# define AO_PPC_CMPx    "cmpd"
# define AO_PPC_STxCXd  "stdcx."
# define AO_PPC_LOAD_CLOBBER "cr0"
#else
# define AO_PPC_LD      "lwz"
# define AO_PPC_LxARX   "lwarx"
# define AO_PPC_CMPx    "cmpw"
# define AO_PPC_STxCXd  "stwcx."
# define AO_PPC_LOAD_CLOBBER "cc"
        /* FIXME: We should get gcc to allocate one of the condition    */
        /* registers.  I always got "impossible constraint" when I      */
        /* tried the "y" constraint.                                    */
# define AO_T_IS_INT
#endif

#ifdef _AIX
  /* Labels are not supported on AIX.                   */
  /* ppc64 has same size of instructions as 32-bit one. */
# define AO_PPC_L(label) /* empty */
# define AO_PPC_BR_A(labelBF, addr) addr
#else
# define AO_PPC_L(label) label ": "
# define AO_PPC_BR_A(labelBF, addr) labelBF
#endif

/* We explicitly specify load_acquire, since it is important, and can   */
/* be implemented relatively cheaply.  It could be implemented          */
/* with an ordinary load followed by a lwsync.  But the general wisdom  */
/* seems to be that a data dependent branch followed by an isync is     */
/* cheaper.  And the documentation is fairly explicit that this also    */
/* has acquire semantics.                                               */
AO_INLINE AO_t
AO_load_acquire(const volatile AO_t *addr)
{
  AO_t result;

  __asm__ __volatile__ (
    AO_PPC_LD "%U1%X1 %0,%1\n"
    "cmpw %0,%0\n"
    "bne- " AO_PPC_BR_A("1f", "$+4") "\n"
    AO_PPC_L("1") "isync\n"
    : "=r" (result)
    : "m"(*addr) : "memory", AO_PPC_LOAD_CLOBBER);
  return result;
}
#define AO_HAVE_load_acquire

/* We explicitly specify store_release, since it relies         */
/* on the fact that lwsync is also a LoadStore barrier.         */
AO_INLINE void
AO_store_release(volatile AO_t *addr, AO_t value)
{
  AO_lwsync();
  *addr = value;
}
#define AO_HAVE_store_release

#ifndef AO_PREFER_GENERALIZED
/* This is similar to the code in the garbage collector.  Deleting      */
/* this and having it synthesized from compare_and_swap would probably  */
/* only cost us a load immediate instruction.                           */
AO_INLINE AO_TS_VAL_t
AO_test_and_set(volatile AO_TS_t *addr) {
  /* TODO: And we should be using smaller objects anyway.       */
  AO_t oldval;
  AO_t temp = 1; /* locked value */

  __asm__ __volatile__(
               AO_PPC_L("1") AO_PPC_LxARX " %0,0,%1\n"
                                                /* load and reserve     */
               AO_PPC_CMPx "i %0, 0\n"          /* if load is           */
               "bne " AO_PPC_BR_A("2f", "$+12") "\n"
                                    /* non-zero, return already set     */
               AO_PPC_STxCXd " %2,0,%1\n"   /* else store conditional   */
               "bne- " AO_PPC_BR_A("1b", "$-16") "\n"
                                    /* retry if lost reservation        */
               AO_PPC_L("2") "\n"   /* oldval is zero if we set         */
              : "=&r"(oldval)
              : "r"(addr), "r"(temp)
              : "memory", "cr0");
  return (AO_TS_VAL_t)oldval;
}
#define AO_HAVE_test_and_set

AO_INLINE AO_TS_VAL_t
AO_test_and_set_acquire(volatile AO_TS_t *addr) {
  AO_TS_VAL_t result = AO_test_and_set(addr);
  AO_lwsync();
  return result;
}
#define AO_HAVE_test_and_set_acquire

AO_INLINE AO_TS_VAL_t
AO_test_and_set_release(volatile AO_TS_t *addr) {
  AO_lwsync();
  return AO_test_and_set(addr);
}
#define AO_HAVE_test_and_set_release

AO_INLINE AO_TS_VAL_t
AO_test_and_set_full(volatile AO_TS_t *addr) {
  AO_TS_VAL_t result;
  AO_lwsync();
  result = AO_test_and_set(addr);
  AO_lwsync();
  return result;
}
#define AO_HAVE_test_and_set_full
#endif /* !AO_PREFER_GENERALIZED */

#ifndef AO_GENERALIZE_ASM_BOOL_CAS

  AO_INLINE int
  AO_compare_and_swap(volatile AO_t *addr, AO_t old, AO_t new_val)
  {
    AO_t oldval;
    int result = 0;

    __asm__ __volatile__(
        AO_PPC_L("1") AO_PPC_LxARX " %0,0,%2\n" /* load and reserve */
        AO_PPC_CMPx " %0, %4\n" /* if load is not equal to      */
        "bne " AO_PPC_BR_A("2f", "$+16") "\n"   /*   old, fail  */
        AO_PPC_STxCXd " %3,0,%2\n"  /* else store conditional   */
        "bne- " AO_PPC_BR_A("1b", "$-16") "\n"
                                /* retry if lost reservation    */
        "li %1,1\n"             /* result = 1;                  */
        AO_PPC_L("2") "\n"
        : "=&r"(oldval), "=&r"(result)
        : "r"(addr), "r"(new_val), "r"(old), "1"(result)
        : "memory", "cr0");
    return result;
  }
# define AO_HAVE_compare_and_swap

  AO_INLINE int
  AO_compare_and_swap_acquire(volatile AO_t *addr, AO_t old, AO_t new_val)
  {
    int result = AO_compare_and_swap(addr, old, new_val);
    AO_lwsync();
    return result;
  }
# define AO_HAVE_compare_and_swap_acquire

  AO_INLINE int
  AO_compare_and_swap_release(volatile AO_t *addr, AO_t old, AO_t new_val)
  {
    AO_lwsync();
    return AO_compare_and_swap(addr, old, new_val);
  }
# define AO_HAVE_compare_and_swap_release

  AO_INLINE int
  AO_compare_and_swap_full(volatile AO_t *addr, AO_t old, AO_t new_val)
  {
    int result;
    AO_lwsync();
    result = AO_compare_and_swap(addr, old, new_val);
    if (result)
      AO_lwsync();
    return result;
  }
# define AO_HAVE_compare_and_swap_full

#endif /* !AO_GENERALIZE_ASM_BOOL_CAS */

AO_INLINE AO_t
AO_fetch_compare_and_swap(volatile AO_t *addr, AO_t old_val, AO_t new_val)
{
  AO_t fetched_val;

  __asm__ __volatile__(
      AO_PPC_L("1") AO_PPC_LxARX " %0,0,%1\n" /* load and reserve */
      AO_PPC_CMPx " %0, %3\n"   /* if load is not equal to      */
      "bne " AO_PPC_BR_A("2f", "$+12") "\n" /*   old_val, fail  */
      AO_PPC_STxCXd " %2,0,%1\n"    /* else store conditional   */
      "bne- " AO_PPC_BR_A("1b", "$-16") "\n"
                                /* retry if lost reservation    */
      AO_PPC_L("2") "\n"
      : "=&r"(fetched_val)
      : "r"(addr), "r"(new_val), "r"(old_val)
      : "memory", "cr0");
  return fetched_val;
}
#define AO_HAVE_fetch_compare_and_swap

AO_INLINE AO_t
AO_fetch_compare_and_swap_acquire(volatile AO_t *addr, AO_t old_val,
                                  AO_t new_val)
{
  AO_t result = AO_fetch_compare_and_swap(addr, old_val, new_val);
  AO_lwsync();
  return result;
}
#define AO_HAVE_fetch_compare_and_swap_acquire

AO_INLINE AO_t
AO_fetch_compare_and_swap_release(volatile AO_t *addr, AO_t old_val,
                                  AO_t new_val)
{
  AO_lwsync();
  return AO_fetch_compare_and_swap(addr, old_val, new_val);
}
#define AO_HAVE_fetch_compare_and_swap_release

AO_INLINE AO_t
AO_fetch_compare_and_swap_full(volatile AO_t *addr, AO_t old_val,
                               AO_t new_val)
{
  AO_t result;
  AO_lwsync();
  result = AO_fetch_compare_and_swap(addr, old_val, new_val);
  if (result == old_val)
    AO_lwsync();
  return result;
}
#define AO_HAVE_fetch_compare_and_swap_full

#ifndef AO_PREFER_GENERALIZED
AO_INLINE AO_t
AO_fetch_and_add(volatile AO_t *addr, AO_t incr) {
  AO_t oldval;
  AO_t newval;

  __asm__ __volatile__(
               AO_PPC_L("1") AO_PPC_LxARX " %0,0,%2\n" /* load and reserve */
               "add %1,%0,%3\n"                 /* increment            */
               AO_PPC_STxCXd " %1,0,%2\n"       /* store conditional    */
               "bne- " AO_PPC_BR_A("1b", "$-12") "\n"
                                    /* retry if lost reservation        */
              : "=&r"(oldval), "=&r"(newval)
              : "r"(addr), "r"(incr)
              : "memory", "cr0");
  return oldval;
}
#define AO_HAVE_fetch_and_add

AO_INLINE AO_t
AO_fetch_and_add_acquire(volatile AO_t *addr, AO_t incr) {
  AO_t result = AO_fetch_and_add(addr, incr);
  AO_lwsync();
  return result;
}
#define AO_HAVE_fetch_and_add_acquire

AO_INLINE AO_t
AO_fetch_and_add_release(volatile AO_t *addr, AO_t incr) {
  AO_lwsync();
  return AO_fetch_and_add(addr, incr);
}
#define AO_HAVE_fetch_and_add_release

AO_INLINE AO_t
AO_fetch_and_add_full(volatile AO_t *addr, AO_t incr) {
  AO_t result;
  AO_lwsync();
  result = AO_fetch_and_add(addr, incr);
  AO_lwsync();
  return result;
}
#define AO_HAVE_fetch_and_add_full
#endif /* !AO_PREFER_GENERALIZED */

#undef AO_PPC_BR_A
#undef AO_PPC_CMPx
#undef AO_PPC_L
#undef AO_PPC_LD
#undef AO_PPC_LOAD_CLOBBER
#undef AO_PPC_LxARX
#undef AO_PPC_STxCXd

#endif /* AO_DISABLE_GCC_ATOMICS */
