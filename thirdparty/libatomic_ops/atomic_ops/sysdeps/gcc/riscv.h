/*
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose,  provided the above notices are retained on all copies.
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

#include "generic.h"

#undef AO_GCC_FORCE_HAVE_CAS
#undef AO_NO_char_ARITHM
#undef AO_NO_short_ARITHM
