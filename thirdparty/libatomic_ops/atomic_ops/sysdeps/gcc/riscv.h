/*
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose, provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 */

#if defined(__clang__) || defined(AO_PREFER_BUILTIN_ATOMICS)
  /* All __GCC_HAVE_SYNC_COMPARE_AND_SWAP_n macros are still missing.   */
  /* The operations are lock-free even for the types smaller than word. */
# define AO_GCC_FORCE_HAVE_CAS
#else

  /* As of gcc-7.5, CAS and arithmetic atomic operations for char and   */
  /* short are supported by the compiler but require -latomic flag.     */
# if !defined(__GCC_HAVE_SYNC_COMPARE_AND_SWAP_1)
#   define AO_NO_char_ARITHM
# endif
# if !defined(__GCC_HAVE_SYNC_COMPARE_AND_SWAP_2)
#   define AO_NO_short_ARITHM
# endif
#endif /* !__clang__ */

#if defined(__riscv_zacas) && __riscv_xlen == 64 && !defined(AO_NO_DOUBLE_CAS)
  /* TODO: Support also rv32, i.e. use amocas.w.        */

# define AO_SKIPATOMIC_double_load
# define AO_SKIPATOMIC_double_load_acquire
# define AO_SKIPATOMIC_double_store
# define AO_SKIPATOMIC_double_store_release

# include "../standard_ao_double_t.h"

  AO_INLINE int
  AO_double_compare_and_swap(volatile AO_double_t *addr,
                             AO_double_t old_val, AO_double_t new_val)
  {
    AO_double_t expected = old_val;

    __asm__ __volatile__("amocas.q %0, %z2, %1"
                         : "+rJ" (expected.AO_whole), "+A" (*addr)
                         : "rJ" (new_val.AO_whole));
    return expected.AO_whole == old_val.AO_whole;
  }
# define AO_HAVE_double_compare_and_swap

  AO_INLINE int
  AO_double_compare_and_swap_acquire(volatile AO_double_t *addr,
                                     AO_double_t old_val, AO_double_t new_val)
  {
    AO_double_t expected = old_val;

    __asm__ __volatile__("amocas.q.aq %0, %z2, %1"
                         : "+rJ" (expected.AO_whole), "+A" (*addr)
                         : "rJ" (new_val.AO_whole));
    return expected.AO_whole == old_val.AO_whole;
  }
# define AO_HAVE_double_compare_and_swap_acquire

  AO_INLINE int
  AO_double_compare_and_swap_release(volatile AO_double_t *addr,
                                     AO_double_t old_val, AO_double_t new_val)
  {
    AO_double_t expected = old_val;

    __asm__ __volatile__("amocas.q.rl %0, %z2, %1"
                         : "+rJ" (expected.AO_whole), "+A" (*addr)
                         : "rJ" (new_val.AO_whole));
    return expected.AO_whole == old_val.AO_whole;
  }
# define AO_HAVE_double_compare_and_swap_release

  AO_INLINE int
  AO_double_compare_and_swap_full(volatile AO_double_t *addr,
                                  AO_double_t old_val, AO_double_t new_val)
  {
    AO_double_t expected = old_val;

    __asm__ __volatile__("amocas.q.aqrl %0, %z2, %1"
                         : "+rJ" (expected.AO_whole), "+A" (*addr)
                         : "rJ" (new_val.AO_whole));
    return expected.AO_whole == old_val.AO_whole;
  }
# define AO_HAVE_double_compare_and_swap_full

#endif /* __riscv_zacas */

#include "generic.h"

#undef AO_GCC_FORCE_HAVE_CAS
#undef AO_NO_char_ARITHM
#undef AO_NO_short_ARITHM
#undef AO_SKIPATOMIC_double_load
#undef AO_SKIPATOMIC_double_load_acquire
#undef AO_SKIPATOMIC_double_store
#undef AO_SKIPATOMIC_double_store_release
