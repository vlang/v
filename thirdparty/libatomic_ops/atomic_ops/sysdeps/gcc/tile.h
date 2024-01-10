/*
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED. ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose, provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 */

/* Minimal support for tile.    */

#if (AO_GNUC_PREREQ(4, 8) || AO_CLANG_PREREQ(3, 4)) \
    && !defined(AO_DISABLE_GCC_ATOMICS)

# include "generic.h"

#else /* AO_DISABLE_GCC_ATOMICS */

# include "../all_atomic_load_store.h"

# include "../test_and_set_t_is_ao_t.h"

  AO_INLINE void
  AO_nop_full(void)
  {
    __sync_synchronize();
  }
# define AO_HAVE_nop_full

  AO_INLINE AO_t
  AO_fetch_and_add_full(volatile AO_t *p, AO_t incr)
  {
    return __sync_fetch_and_add(p, incr);
  }
# define AO_HAVE_fetch_and_add_full

  AO_INLINE AO_t
  AO_fetch_compare_and_swap_full(volatile AO_t *addr, AO_t old_val,
                                 AO_t new_val)
  {
    return __sync_val_compare_and_swap(addr, old_val, new_val
                                       /* empty protection list */);
  }
# define AO_HAVE_fetch_compare_and_swap_full

#endif /* AO_DISABLE_GCC_ATOMICS */
