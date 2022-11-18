
/* Memory model documented at http://www-106.ibm.com/developerworks/    */
/* eserver/articles/archguide.html and (clearer)                        */
/* http://www-106.ibm.com/developerworks/eserver/articles/powerpc.html. */
/* There appears to be no implicit ordering between any kind of         */
/* independent memory references.                                       */
/* Architecture enforces some ordering based on control dependence.     */
/* I don't know if that could help.                                     */
/* Data-dependent loads are always ordered.                             */
/* Based on the above references, eieio is intended for use on          */
/* uncached memory, which we don't support.  It does not order loads    */
/* from cached memory.                                                  */
/* Thanks to Maged Michael, Doug Lea, and Roger Hoover for helping to   */
/* track some of this down and correcting my misunderstandings. -HB     */

#include "../all_aligned_atomic_load_store.h"

#include "../test_and_set_t_is_ao_t.h"

void AO_sync(void);
#pragma mc_func AO_sync { "7c0004ac" }

#ifdef __NO_LWSYNC__
# define AO_lwsync AO_sync
#else
  void AO_lwsync(void);
#pragma mc_func AO_lwsync { "7c2004ac" }
#endif

#define AO_nop_write() AO_lwsync()
#define AO_HAVE_nop_write

#define AO_nop_read() AO_lwsync()
#define AO_HAVE_nop_read

/* We explicitly specify load_acquire and store_release, since these    */
/* rely on the fact that lwsync is also a LoadStore barrier.            */
AO_INLINE AO_t
AO_load_acquire(const volatile AO_t *addr)
{
  AO_t result = *addr;
  AO_lwsync();
  return result;
}
#define AO_HAVE_load_acquire

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
#if defined(__powerpc64__) || defined(__ppc64__) || defined(__64BIT__)
/* Completely untested.  And we should be using smaller objects anyway. */
  unsigned long oldval;
  unsigned long temp = 1; /* locked value */

  __asm__ __volatile__(
               "1:ldarx %0,0,%1\n"   /* load and reserve               */
               "cmpdi %0, 0\n"       /* if load is                     */
               "bne 2f\n"            /*   non-zero, return already set */
               "stdcx. %2,0,%1\n"    /* else store conditional         */
               "bne- 1b\n"           /* retry if lost reservation      */
               "2:\n"                /* oldval is zero if we set       */
              : "=&r"(oldval)
              : "r"(addr), "r"(temp)
              : "memory", "cr0");
#else
  int oldval;
  int temp = 1; /* locked value */

  __asm__ __volatile__(
               "1:lwarx %0,0,%1\n"   /* load and reserve               */
               "cmpwi %0, 0\n"       /* if load is                     */
               "bne 2f\n"            /*   non-zero, return already set */
               "stwcx. %2,0,%1\n"    /* else store conditional         */
               "bne- 1b\n"           /* retry if lost reservation      */
               "2:\n"                /* oldval is zero if we set       */
              : "=&r"(oldval)
              : "r"(addr), "r"(temp)
              : "memory", "cr0");
#endif
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

AO_INLINE AO_t
AO_fetch_compare_and_swap(volatile AO_t *addr, AO_t old_val, AO_t new_val)
{
  AO_t fetched_val;
# if defined(__powerpc64__) || defined(__ppc64__) || defined(__64BIT__)
    __asm__ __volatile__(
      "1:ldarx %0,0,%1\n"       /* load and reserve             */
      "cmpd %0, %3\n"           /* if load is not equal to      */
      "bne 2f\n"                /*   old_val, fail              */
      "stdcx. %2,0,%1\n"        /* else store conditional       */
      "bne- 1b\n"               /* retry if lost reservation    */
      "2:\n"
      : "=&r"(fetched_val)
      : "r"(addr), "r"(new_val), "r"(old_val)
      : "memory", "cr0");
# else
    __asm__ __volatile__(
      "1:lwarx %0,0,%1\n"       /* load and reserve             */
      "cmpw %0, %3\n"           /* if load is not equal to      */
      "bne 2f\n"                /*   old_val, fail              */
      "stwcx. %2,0,%1\n"        /* else store conditional       */
      "bne- 1b\n"               /* retry if lost reservation    */
      "2:\n"
      : "=&r"(fetched_val)
      : "r"(addr), "r"(new_val), "r"(old_val)
      : "memory", "cr0");
# endif
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
  AO_lwsync();
  return result;
}
#define AO_HAVE_fetch_compare_and_swap_full

/* TODO: Implement AO_fetch_and_add, AO_and/or/xor directly.    */
