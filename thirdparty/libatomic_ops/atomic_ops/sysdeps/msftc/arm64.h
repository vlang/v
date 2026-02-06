/*
 * Copyright (c) 2003 Hewlett-Packard Development Company, L.P.
 * Copyright (c) 2021 Ivan Maidanski
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

#include "../all_aligned_atomic_load_store.h"

#ifndef AO_ASSUME_WINDOWS98
# define AO_ASSUME_WINDOWS98
#endif
#ifndef AO_USE_INTERLOCKED_INTRINSICS
# define AO_USE_INTERLOCKED_INTRINSICS
#endif
#include "common32_defs.h"

#ifndef AO_HAVE_test_and_set_full
# include "../test_and_set_t_is_ao_t.h"
  /* AO_test_and_set_full() is emulated using word-wide CAS.    */
#endif

#ifndef AO_NO_DOUBLE_CAS

# include "../standard_ao_double_t.h"

# pragma intrinsic (_InterlockedCompareExchange128)
# pragma intrinsic (_InterlockedCompareExchange128_acq)
# pragma intrinsic (_InterlockedCompareExchange128_nf)
# pragma intrinsic (_InterlockedCompareExchange128_rel)

  AO_INLINE int
  AO_compare_double_and_swap_double(volatile AO_double_t *addr,
                                    AO_t old_val1, AO_t old_val2,
                                    AO_t new_val1, AO_t new_val2)
  {
    __int64 comparandResult[2];

    AO_ASSERT_ADDR_ALIGNED(addr);
    comparandResult[0] = old_val1; /* low */
    comparandResult[1] = old_val2; /* high */
    return _InterlockedCompareExchange128_nf((volatile __int64 *)addr,
                                             new_val2 /* high */,
                                             new_val1 /* low */,
                                             comparandResult);
  }
# define AO_HAVE_compare_double_and_swap_double

  AO_INLINE int
  AO_compare_double_and_swap_double_acquire(volatile AO_double_t *addr,
                                            AO_t old_val1, AO_t old_val2,
                                            AO_t new_val1, AO_t new_val2)
  {
    __int64 comparandResult[2];

    AO_ASSERT_ADDR_ALIGNED(addr);
    comparandResult[0] = old_val1; /* low */
    comparandResult[1] = old_val2; /* high */
    return _InterlockedCompareExchange128_acq((volatile __int64 *)addr,
                                              new_val2 /* high */,
                                              new_val1 /* low */,
                                              comparandResult);
  }
# define AO_HAVE_compare_double_and_swap_double_acquire

  AO_INLINE int
  AO_compare_double_and_swap_double_release(volatile AO_double_t *addr,
                                            AO_t old_val1, AO_t old_val2,
                                            AO_t new_val1, AO_t new_val2)
  {
    __int64 comparandResult[2];

    AO_ASSERT_ADDR_ALIGNED(addr);
    comparandResult[0] = old_val1; /* low */
    comparandResult[1] = old_val2; /* high */
    return _InterlockedCompareExchange128_rel((volatile __int64 *)addr,
                                              new_val2 /* high */,
                                              new_val1 /* low */,
                                              comparandResult);
  }
# define AO_HAVE_compare_double_and_swap_double_release

  AO_INLINE int
  AO_compare_double_and_swap_double_full(volatile AO_double_t *addr,
                                         AO_t old_val1, AO_t old_val2,
                                         AO_t new_val1, AO_t new_val2)
  {
    __int64 comparandResult[2];

    AO_ASSERT_ADDR_ALIGNED(addr);
    comparandResult[0] = old_val1; /* low */
    comparandResult[1] = old_val2; /* high */
    return _InterlockedCompareExchange128((volatile __int64 *)addr,
                                          new_val2 /* high */,
                                          new_val1 /* low */,
                                          comparandResult);
  }
# define AO_HAVE_compare_double_and_swap_double_full

#endif /* !AO_NO_DOUBLE_CAS */
