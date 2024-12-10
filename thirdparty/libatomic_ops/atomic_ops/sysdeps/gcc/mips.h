/*
 * Copyright (c) 2005,2007  Thiemo Seufer <ths@networkno.de>
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose, provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 */

/*
 * FIXME:  This should probably make finer distinctions.  SGI MIPS is
 * much more strongly ordered, and in fact closer to sequentially
 * consistent.  This is really aimed at modern embedded implementations.
 */

/* Data dependence does not imply read ordering.  */
#define AO_NO_DD_ORDERING

/* #include "../standard_ao_double_t.h" */
/* TODO: Implement double-wide operations if available. */

#if (AO_GNUC_PREREQ(4, 9) || AO_CLANG_PREREQ(3, 5)) \
    && !defined(AO_DISABLE_GCC_ATOMICS)
  /* Probably, it could be enabled even for earlier gcc/clang versions. */

  /* As of clang-3.6/mips[64], __GCC_HAVE_SYNC_COMPARE_AND_SWAP_n missing. */
# if defined(__clang__)
#   define AO_GCC_FORCE_HAVE_CAS
# endif

# include "generic.h"

#else /* AO_DISABLE_GCC_ATOMICS */

# include "../test_and_set_t_is_ao_t.h"
# include "../all_aligned_atomic_load_store.h"

# if !defined(_ABI64) || _MIPS_SIM != _ABI64
#   define AO_T_IS_INT
#   if __mips_isa_rev >= 6
      /* Encoding of ll/sc in mips rel6 differs from that of mips2/3. */
#     define AO_MIPS_SET_ISA  ""
#   else
#     define AO_MIPS_SET_ISA  "       .set mips2\n"
#   endif
#   define AO_MIPS_LL_1(args) "       ll " args "\n"
#   define AO_MIPS_SC(args)   "       sc " args "\n"
# else
#   if __mips_isa_rev >= 6
#     define AO_MIPS_SET_ISA  ""
#   else
#     define AO_MIPS_SET_ISA  "       .set mips3\n"
#   endif
#   define AO_MIPS_LL_1(args) "       lld " args "\n"
#   define AO_MIPS_SC(args)   "       scd " args "\n"
# endif /* _MIPS_SIM == _ABI64 */

#ifdef AO_ICE9A1_LLSC_WAR
  /* ICE9 rev A1 chip (used in very few systems) is reported to */
  /* have a low-frequency bug that causes LL to fail.           */
  /* To workaround, just issue the second 'LL'.                 */
# define AO_MIPS_LL(args) AO_MIPS_LL_1(args) AO_MIPS_LL_1(args)
#else
# define AO_MIPS_LL(args) AO_MIPS_LL_1(args)
#endif

AO_INLINE void
AO_nop_full(void)
{
  __asm__ __volatile__(
      "       .set push\n"
      AO_MIPS_SET_ISA
      "       .set noreorder\n"
      "       .set nomacro\n"
      "       sync\n"
      "       .set pop"
      : : : "memory");
}
#define AO_HAVE_nop_full

#ifndef AO_PREFER_GENERALIZED
AO_INLINE AO_t
AO_fetch_and_add(volatile AO_t *addr, AO_t incr)
{
  register int result;
  register int temp;

  __asm__ __volatile__(
      "       .set push\n"
      AO_MIPS_SET_ISA
      "       .set noreorder\n"
      "       .set nomacro\n"
      "1: "
      AO_MIPS_LL("%0, %2")
      "       addu %1, %0, %3\n"
      AO_MIPS_SC("%1, %2")
      "       beqz %1, 1b\n"
      "       nop\n"
      "       .set pop"
      : "=&r" (result), "=&r" (temp), "+m" (*addr)
      : "Ir" (incr)
      : "memory");
  return (AO_t)result;
}
#define AO_HAVE_fetch_and_add

AO_INLINE AO_TS_VAL_t
AO_test_and_set(volatile AO_TS_t *addr)
{
  register int oldval;
  register int temp;

  __asm__ __volatile__(
      "       .set push\n"
      AO_MIPS_SET_ISA
      "       .set noreorder\n"
      "       .set nomacro\n"
      "1: "
      AO_MIPS_LL("%0, %2")
      "       move %1, %3\n"
      AO_MIPS_SC("%1, %2")
      "       beqz %1, 1b\n"
      "       nop\n"
      "       .set pop"
      : "=&r" (oldval), "=&r" (temp), "+m" (*addr)
      : "r" (1)
      : "memory");
  return (AO_TS_VAL_t)oldval;
}
#define AO_HAVE_test_and_set

  /* TODO: Implement AO_and/or/xor primitives directly. */
#endif /* !AO_PREFER_GENERALIZED */

#ifndef AO_GENERALIZE_ASM_BOOL_CAS
  AO_INLINE int
  AO_compare_and_swap(volatile AO_t *addr, AO_t old, AO_t new_val)
  {
    register int was_equal = 0;
    register int temp;

    __asm__ __volatile__(
        "       .set push\n"
        AO_MIPS_SET_ISA
        "       .set noreorder\n"
        "       .set nomacro\n"
        "1: "
        AO_MIPS_LL("%0, %1")
        "       bne     %0, %4, 2f\n"
        "       move    %0, %3\n"
        AO_MIPS_SC("%0, %1")
        "       .set pop\n"
        "       beqz    %0, 1b\n"
        "       li      %2, 1\n"
        "2:"
        : "=&r" (temp), "+m" (*addr), "+r" (was_equal)
        : "r" (new_val), "r" (old)
        : "memory");
    return was_equal;
  }
# define AO_HAVE_compare_and_swap
#endif /* !AO_GENERALIZE_ASM_BOOL_CAS */

AO_INLINE AO_t
AO_fetch_compare_and_swap(volatile AO_t *addr, AO_t old, AO_t new_val)
{
  register int fetched_val;
  register int temp;

  __asm__ __volatile__(
      "       .set push\n"
      AO_MIPS_SET_ISA
      "       .set noreorder\n"
      "       .set nomacro\n"
      "1: "
      AO_MIPS_LL("%0, %2")
      "       bne  %0, %4, 2f\n"
      "       move %1, %3\n"
      AO_MIPS_SC("%1, %2")
      "       beqz %1, 1b\n"
      "       nop\n"
      "       .set pop\n"
      "2:"
      : "=&r" (fetched_val), "=&r" (temp), "+m" (*addr)
      : "r" (new_val), "Jr" (old)
      : "memory");
  return (AO_t)fetched_val;
}
#define AO_HAVE_fetch_compare_and_swap

#endif /* AO_DISABLE_GCC_ATOMICS */

/* CAS primitives with acquire, release and full semantics are  */
/* generated automatically (and AO_int_... primitives are       */
/* defined properly after the first generalization pass).       */

#undef AO_GCC_FORCE_HAVE_CAS
#undef AO_MIPS_LL
#undef AO_MIPS_LL_1
#undef AO_MIPS_SC
#undef AO_MIPS_SET_ISA
