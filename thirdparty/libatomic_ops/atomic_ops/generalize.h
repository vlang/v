/*
 * Copyright (c) 2003-2011 Hewlett-Packard Development Company, L.P.
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

/*
 * Generalize atomic operations for atomic_ops.h.
 * Should not be included directly.
 *
 * We make no attempt to define useless operations, such as
 * AO_nop_acquire
 * AO_nop_release
 *
 * We have also so far neglected to define some others, which
 * do not appear likely to be useful, e.g. stores with acquire
 * or read barriers.
 *
 * This file is sometimes included twice by atomic_ops.h.
 * All definitions include explicit checks that we are not replacing
 * an earlier definition.  In general, more desirable expansions
 * appear earlier so that we are more likely to use them.
 *
 * We only make safe generalizations, except that by default we define
 * the ...dd_acquire_read operations to be equivalent to those without
 * a barrier.  On platforms for which this is unsafe, the platform-specific
 * file must define AO_NO_DD_ORDERING.
 */

#ifndef AO_ATOMIC_OPS_H
# error This file should not be included directly.
#endif

/* Generate test_and_set_full, if necessary and possible.       */
#if !defined(AO_HAVE_test_and_set) && !defined(AO_HAVE_test_and_set_release) \
    && !defined(AO_HAVE_test_and_set_acquire) \
    && !defined(AO_HAVE_test_and_set_read) \
    && !defined(AO_HAVE_test_and_set_full)

  /* Emulate AO_compare_and_swap() via AO_fetch_compare_and_swap().     */
# if defined(AO_HAVE_fetch_compare_and_swap) \
     && !defined(AO_HAVE_compare_and_swap)
    AO_INLINE int
    AO_compare_and_swap(volatile AO_t *addr, AO_t old_val, AO_t new_val)
    {
      return AO_fetch_compare_and_swap(addr, old_val, new_val) == old_val;
    }
#   define AO_HAVE_compare_and_swap
# endif

# if defined(AO_HAVE_fetch_compare_and_swap_full) \
     && !defined(AO_HAVE_compare_and_swap_full)
    AO_INLINE int
    AO_compare_and_swap_full(volatile AO_t *addr, AO_t old_val, AO_t new_val)
    {
      return AO_fetch_compare_and_swap_full(addr, old_val, new_val)
               == old_val;
    }
#   define AO_HAVE_compare_and_swap_full
# endif

# if defined(AO_HAVE_fetch_compare_and_swap_acquire) \
     && !defined(AO_HAVE_compare_and_swap_acquire)
    AO_INLINE int
    AO_compare_and_swap_acquire(volatile AO_t *addr, AO_t old_val,
                                AO_t new_val)
    {
      return AO_fetch_compare_and_swap_acquire(addr, old_val, new_val)
               == old_val;
    }
#   define AO_HAVE_compare_and_swap_acquire
# endif

# if defined(AO_HAVE_fetch_compare_and_swap_release) \
     && !defined(AO_HAVE_compare_and_swap_release)
    AO_INLINE int
    AO_compare_and_swap_release(volatile AO_t *addr, AO_t old_val,
                                AO_t new_val)
    {
      return AO_fetch_compare_and_swap_release(addr, old_val, new_val)
               == old_val;
    }
#   define AO_HAVE_compare_and_swap_release
# endif

# if defined(AO_CHAR_TS_T)
#   define AO_TS_COMPARE_AND_SWAP_FULL(a,o,n) \
                                AO_char_compare_and_swap_full(a,o,n)
#   define AO_TS_COMPARE_AND_SWAP_ACQUIRE(a,o,n) \
                                AO_char_compare_and_swap_acquire(a,o,n)
#   define AO_TS_COMPARE_AND_SWAP_RELEASE(a,o,n) \
                                AO_char_compare_and_swap_release(a,o,n)
#   define AO_TS_COMPARE_AND_SWAP(a,o,n) AO_char_compare_and_swap(a,o,n)
# endif

# if defined(AO_AO_TS_T)
#   define AO_TS_COMPARE_AND_SWAP_FULL(a,o,n) AO_compare_and_swap_full(a,o,n)
#   define AO_TS_COMPARE_AND_SWAP_ACQUIRE(a,o,n) \
                                AO_compare_and_swap_acquire(a,o,n)
#   define AO_TS_COMPARE_AND_SWAP_RELEASE(a,o,n) \
                                AO_compare_and_swap_release(a,o,n)
#   define AO_TS_COMPARE_AND_SWAP(a,o,n) AO_compare_and_swap(a,o,n)
# endif

# if (defined(AO_AO_TS_T) && defined(AO_HAVE_compare_and_swap_full)) \
     || (defined(AO_CHAR_TS_T) && defined(AO_HAVE_char_compare_and_swap_full))
    AO_INLINE AO_TS_VAL_t
    AO_test_and_set_full(volatile AO_TS_t *addr)
    {
      if (AO_TS_COMPARE_AND_SWAP_FULL(addr, AO_TS_CLEAR, AO_TS_SET))
        return AO_TS_CLEAR;
      else
        return AO_TS_SET;
    }
#   define AO_HAVE_test_and_set_full
# endif /* AO_HAVE_compare_and_swap_full */

# if (defined(AO_AO_TS_T) && defined(AO_HAVE_compare_and_swap_acquire)) \
     || (defined(AO_CHAR_TS_T) \
         && defined(AO_HAVE_char_compare_and_swap_acquire))
    AO_INLINE AO_TS_VAL_t
    AO_test_and_set_acquire(volatile AO_TS_t *addr)
    {
      if (AO_TS_COMPARE_AND_SWAP_ACQUIRE(addr, AO_TS_CLEAR, AO_TS_SET))
        return AO_TS_CLEAR;
      else
        return AO_TS_SET;
    }
#   define AO_HAVE_test_and_set_acquire
# endif /* AO_HAVE_compare_and_swap_acquire */

# if (defined(AO_AO_TS_T) && defined(AO_HAVE_compare_and_swap_release)) \
     || (defined(AO_CHAR_TS_T) \
         && defined(AO_HAVE_char_compare_and_swap_release))
    AO_INLINE AO_TS_VAL_t
    AO_test_and_set_release(volatile AO_TS_t *addr)
    {
      if (AO_TS_COMPARE_AND_SWAP_RELEASE(addr, AO_TS_CLEAR, AO_TS_SET))
        return AO_TS_CLEAR;
      else
        return AO_TS_SET;
    }
#   define AO_HAVE_test_and_set_release
# endif /* AO_HAVE_compare_and_swap_release */

# if (defined(AO_AO_TS_T) && defined(AO_HAVE_compare_and_swap)) \
     || (defined(AO_CHAR_TS_T) && defined(AO_HAVE_char_compare_and_swap))
    AO_INLINE AO_TS_VAL_t
    AO_test_and_set(volatile AO_TS_t *addr)
    {
      if (AO_TS_COMPARE_AND_SWAP(addr, AO_TS_CLEAR, AO_TS_SET))
        return AO_TS_CLEAR;
      else
        return AO_TS_SET;
    }
#   define AO_HAVE_test_and_set
# endif /* AO_HAVE_compare_and_swap */
#endif /* No prior test and set */

/* Nop */
#if !defined(AO_HAVE_nop)
  AO_INLINE void AO_nop(void) {}
# define AO_HAVE_nop
#endif

#if defined(AO_HAVE_test_and_set_full) && !defined(AO_HAVE_nop_full)
  AO_INLINE void
  AO_nop_full(void)
  {
    AO_TS_t dummy = AO_TS_INITIALIZER;
    AO_test_and_set_full(&dummy);
  }
# define AO_HAVE_nop_full
#endif

#if defined(AO_HAVE_nop_acquire) && !defined(CPPCHECK)
# error AO_nop_acquire is useless: do not define.
#endif

#if defined(AO_HAVE_nop_release) && !defined(CPPCHECK)
# error AO_nop_release is useless: do not define.
#endif

#if defined(AO_HAVE_nop_full) && !defined(AO_HAVE_nop_read)
# define AO_nop_read() AO_nop_full()
# define AO_HAVE_nop_read
#endif

#if defined(AO_HAVE_nop_full) && !defined(AO_HAVE_nop_write)
# define AO_nop_write() AO_nop_full()
# define AO_HAVE_nop_write
#endif

/* Test_and_set */
#if defined(AO_HAVE_test_and_set) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_test_and_set_release)
# define AO_test_and_set_release(addr) (AO_nop_full(), AO_test_and_set(addr))
# define AO_HAVE_test_and_set_release
#endif

#if defined(AO_HAVE_test_and_set) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_test_and_set_acquire)
  AO_INLINE AO_TS_VAL_t
  AO_test_and_set_acquire(volatile AO_TS_t *addr)
  {
    AO_TS_VAL_t result = AO_test_and_set(addr);
    AO_nop_full();
    return result;
  }
# define AO_HAVE_test_and_set_acquire
#endif

#if defined(AO_HAVE_test_and_set_full)
# if !defined(AO_HAVE_test_and_set_release)
#   define AO_test_and_set_release(addr) AO_test_and_set_full(addr)
#   define AO_HAVE_test_and_set_release
# endif
# if !defined(AO_HAVE_test_and_set_acquire)
#   define AO_test_and_set_acquire(addr) AO_test_and_set_full(addr)
#   define AO_HAVE_test_and_set_acquire
# endif
# if !defined(AO_HAVE_test_and_set_write)
#   define AO_test_and_set_write(addr) AO_test_and_set_full(addr)
#   define AO_HAVE_test_and_set_write
# endif
# if !defined(AO_HAVE_test_and_set_read)
#   define AO_test_and_set_read(addr) AO_test_and_set_full(addr)
#   define AO_HAVE_test_and_set_read
# endif
#endif /* AO_HAVE_test_and_set_full */

#if !defined(AO_HAVE_test_and_set) && defined(AO_HAVE_test_and_set_release)
# define AO_test_and_set(addr) AO_test_and_set_release(addr)
# define AO_HAVE_test_and_set
#endif
#if !defined(AO_HAVE_test_and_set) && defined(AO_HAVE_test_and_set_acquire)
# define AO_test_and_set(addr) AO_test_and_set_acquire(addr)
# define AO_HAVE_test_and_set
#endif
#if !defined(AO_HAVE_test_and_set) && defined(AO_HAVE_test_and_set_write)
# define AO_test_and_set(addr) AO_test_and_set_write(addr)
# define AO_HAVE_test_and_set
#endif
#if !defined(AO_HAVE_test_and_set) && defined(AO_HAVE_test_and_set_read)
# define AO_test_and_set(addr) AO_test_and_set_read(addr)
# define AO_HAVE_test_and_set
#endif

#if defined(AO_HAVE_test_and_set_acquire) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_test_and_set_full)
# define AO_test_and_set_full(addr) \
                        (AO_nop_full(), AO_test_and_set_acquire(addr))
# define AO_HAVE_test_and_set_full
#endif

#if !defined(AO_HAVE_test_and_set_release_write) \
    && defined(AO_HAVE_test_and_set_write)
# define AO_test_and_set_release_write(addr) AO_test_and_set_write(addr)
# define AO_HAVE_test_and_set_release_write
#endif
#if !defined(AO_HAVE_test_and_set_release_write) \
    && defined(AO_HAVE_test_and_set_release)
# define AO_test_and_set_release_write(addr) AO_test_and_set_release(addr)
# define AO_HAVE_test_and_set_release_write
#endif
#if !defined(AO_HAVE_test_and_set_acquire_read) \
    && defined(AO_HAVE_test_and_set_read)
# define AO_test_and_set_acquire_read(addr) AO_test_and_set_read(addr)
# define AO_HAVE_test_and_set_acquire_read
#endif
#if !defined(AO_HAVE_test_and_set_acquire_read) \
    && defined(AO_HAVE_test_and_set_acquire)
# define AO_test_and_set_acquire_read(addr) AO_test_and_set_acquire(addr)
# define AO_HAVE_test_and_set_acquire_read
#endif

#ifdef AO_NO_DD_ORDERING
# if defined(AO_HAVE_test_and_set_acquire_read)
#   define AO_test_and_set_dd_acquire_read(addr) \
                                        AO_test_and_set_acquire_read(addr)
#   define AO_HAVE_test_and_set_dd_acquire_read
# endif
#else
# if defined(AO_HAVE_test_and_set)
#   define AO_test_and_set_dd_acquire_read(addr) AO_test_and_set(addr)
#   define AO_HAVE_test_and_set_dd_acquire_read
# endif
#endif /* !AO_NO_DD_ORDERING */

#include "generalize-small.h"

#include "generalize-arithm.h"

/* Compare_double_and_swap_double based on double_compare_and_swap.     */
#ifdef AO_HAVE_DOUBLE_PTR_STORAGE
# if defined(AO_HAVE_double_compare_and_swap) \
     && !defined(AO_HAVE_compare_double_and_swap_double)
   AO_INLINE int
   AO_compare_double_and_swap_double(volatile AO_double_t *addr,
                                     AO_t old_val1, AO_t old_val2,
                                     AO_t new_val1, AO_t new_val2)
   {
     AO_double_t old_w;
     AO_double_t new_w;
     old_w.AO_val1 = old_val1;
     old_w.AO_val2 = old_val2;
     new_w.AO_val1 = new_val1;
     new_w.AO_val2 = new_val2;
     return AO_double_compare_and_swap(addr, old_w, new_w);
   }
#  define AO_HAVE_compare_double_and_swap_double
# endif
# if defined(AO_HAVE_double_compare_and_swap_acquire) \
     && !defined(AO_HAVE_compare_double_and_swap_double_acquire)
    AO_INLINE int
    AO_compare_double_and_swap_double_acquire(volatile AO_double_t *addr,
                                              AO_t old_val1, AO_t old_val2,
                                              AO_t new_val1, AO_t new_val2)
    {
      AO_double_t old_w;
      AO_double_t new_w;
      old_w.AO_val1 = old_val1;
      old_w.AO_val2 = old_val2;
      new_w.AO_val1 = new_val1;
      new_w.AO_val2 = new_val2;
      return AO_double_compare_and_swap_acquire(addr, old_w, new_w);
    }
#   define AO_HAVE_compare_double_and_swap_double_acquire
# endif
# if defined(AO_HAVE_double_compare_and_swap_release) \
     && !defined(AO_HAVE_compare_double_and_swap_double_release)
    AO_INLINE int
    AO_compare_double_and_swap_double_release(volatile AO_double_t *addr,
                                              AO_t old_val1, AO_t old_val2,
                                              AO_t new_val1, AO_t new_val2)
    {
      AO_double_t old_w;
      AO_double_t new_w;
      old_w.AO_val1 = old_val1;
      old_w.AO_val2 = old_val2;
      new_w.AO_val1 = new_val1;
      new_w.AO_val2 = new_val2;
      return AO_double_compare_and_swap_release(addr, old_w, new_w);
    }
#   define AO_HAVE_compare_double_and_swap_double_release
# endif
# if defined(AO_HAVE_double_compare_and_swap_full) \
     && !defined(AO_HAVE_compare_double_and_swap_double_full)
    AO_INLINE int
    AO_compare_double_and_swap_double_full(volatile AO_double_t *addr,
                                           AO_t old_val1, AO_t old_val2,
                                           AO_t new_val1, AO_t new_val2)
    {
      AO_double_t old_w;
      AO_double_t new_w;
      old_w.AO_val1 = old_val1;
      old_w.AO_val2 = old_val2;
      new_w.AO_val1 = new_val1;
      new_w.AO_val2 = new_val2;
      return AO_double_compare_and_swap_full(addr, old_w, new_w);
    }
#   define AO_HAVE_compare_double_and_swap_double_full
# endif
#endif /* AO_HAVE_DOUBLE_PTR_STORAGE */

/* Compare_double_and_swap_double */
#if defined(AO_HAVE_compare_double_and_swap_double) \
    && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_compare_double_and_swap_double_acquire)
  AO_INLINE int
  AO_compare_double_and_swap_double_acquire(volatile AO_double_t *addr,
                                            AO_t o1, AO_t o2,
                                            AO_t n1, AO_t n2)
  {
    int result = AO_compare_double_and_swap_double(addr, o1, o2, n1, n2);
    AO_nop_full();
    return result;
  }
# define AO_HAVE_compare_double_and_swap_double_acquire
#endif
#if defined(AO_HAVE_compare_double_and_swap_double) \
    && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_compare_double_and_swap_double_release)
# define AO_compare_double_and_swap_double_release(addr,o1,o2,n1,n2) \
      (AO_nop_full(), AO_compare_double_and_swap_double(addr,o1,o2,n1,n2))
# define AO_HAVE_compare_double_and_swap_double_release
#endif
#if defined(AO_HAVE_compare_double_and_swap_double_full)
# if !defined(AO_HAVE_compare_double_and_swap_double_release)
#   define AO_compare_double_and_swap_double_release(addr,o1,o2,n1,n2) \
                AO_compare_double_and_swap_double_full(addr,o1,o2,n1,n2)
#   define AO_HAVE_compare_double_and_swap_double_release
# endif
# if !defined(AO_HAVE_compare_double_and_swap_double_acquire)
#   define AO_compare_double_and_swap_double_acquire(addr,o1,o2,n1,n2) \
                AO_compare_double_and_swap_double_full(addr,o1,o2,n1,n2)
#   define AO_HAVE_compare_double_and_swap_double_acquire
# endif
# if !defined(AO_HAVE_compare_double_and_swap_double_write)
#   define AO_compare_double_and_swap_double_write(addr,o1,o2,n1,n2) \
                AO_compare_double_and_swap_double_full(addr,o1,o2,n1,n2)
#   define AO_HAVE_compare_double_and_swap_double_write
# endif
# if !defined(AO_HAVE_compare_double_and_swap_double_read)
#   define AO_compare_double_and_swap_double_read(addr,o1,o2,n1,n2) \
                AO_compare_double_and_swap_double_full(addr,o1,o2,n1,n2)
#   define AO_HAVE_compare_double_and_swap_double_read
# endif
#endif /* AO_HAVE_compare_double_and_swap_double_full */

#if !defined(AO_HAVE_compare_double_and_swap_double) \
    && defined(AO_HAVE_compare_double_and_swap_double_release)
# define AO_compare_double_and_swap_double(addr,o1,o2,n1,n2) \
                AO_compare_double_and_swap_double_release(addr,o1,o2,n1,n2)
# define AO_HAVE_compare_double_and_swap_double
#endif
#if !defined(AO_HAVE_compare_double_and_swap_double) \
    && defined(AO_HAVE_compare_double_and_swap_double_acquire)
# define AO_compare_double_and_swap_double(addr,o1,o2,n1,n2) \
                AO_compare_double_and_swap_double_acquire(addr,o1,o2,n1,n2)
# define AO_HAVE_compare_double_and_swap_double
#endif
#if !defined(AO_HAVE_compare_double_and_swap_double) \
    && defined(AO_HAVE_compare_double_and_swap_double_write)
# define AO_compare_double_and_swap_double(addr,o1,o2,n1,n2) \
                AO_compare_double_and_swap_double_write(addr,o1,o2,n1,n2)
# define AO_HAVE_compare_double_and_swap_double
#endif
#if !defined(AO_HAVE_compare_double_and_swap_double) \
    && defined(AO_HAVE_compare_double_and_swap_double_read)
# define AO_compare_double_and_swap_double(addr,o1,o2,n1,n2) \
                AO_compare_double_and_swap_double_read(addr,o1,o2,n1,n2)
# define AO_HAVE_compare_double_and_swap_double
#endif

#if defined(AO_HAVE_compare_double_and_swap_double_acquire) \
    && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_compare_double_and_swap_double_full)
# define AO_compare_double_and_swap_double_full(addr,o1,o2,n1,n2) \
                (AO_nop_full(), \
                 AO_compare_double_and_swap_double_acquire(addr,o1,o2,n1,n2))
# define AO_HAVE_compare_double_and_swap_double_full
#endif

#if !defined(AO_HAVE_compare_double_and_swap_double_release_write) \
    && defined(AO_HAVE_compare_double_and_swap_double_write)
# define AO_compare_double_and_swap_double_release_write(addr,o1,o2,n1,n2) \
                AO_compare_double_and_swap_double_write(addr,o1,o2,n1,n2)
# define AO_HAVE_compare_double_and_swap_double_release_write
#endif
#if !defined(AO_HAVE_compare_double_and_swap_double_release_write) \
    && defined(AO_HAVE_compare_double_and_swap_double_release)
# define AO_compare_double_and_swap_double_release_write(addr,o1,o2,n1,n2) \
                AO_compare_double_and_swap_double_release(addr,o1,o2,n1,n2)
# define AO_HAVE_compare_double_and_swap_double_release_write
#endif
#if !defined(AO_HAVE_compare_double_and_swap_double_acquire_read) \
    && defined(AO_HAVE_compare_double_and_swap_double_read)
# define AO_compare_double_and_swap_double_acquire_read(addr,o1,o2,n1,n2) \
                AO_compare_double_and_swap_double_read(addr,o1,o2,n1,n2)
# define AO_HAVE_compare_double_and_swap_double_acquire_read
#endif
#if !defined(AO_HAVE_compare_double_and_swap_double_acquire_read) \
    && defined(AO_HAVE_compare_double_and_swap_double_acquire)
# define AO_compare_double_and_swap_double_acquire_read(addr,o1,o2,n1,n2) \
                AO_compare_double_and_swap_double_acquire(addr,o1,o2,n1,n2)
# define AO_HAVE_compare_double_and_swap_double_acquire_read
#endif

#ifdef AO_NO_DD_ORDERING
# if defined(AO_HAVE_compare_double_and_swap_double_acquire_read)
#   define AO_compare_double_and_swap_double_dd_acquire_read(addr,o1,o2,n1,n2) \
             AO_compare_double_and_swap_double_acquire_read(addr,o1,o2,n1,n2)
#   define AO_HAVE_compare_double_and_swap_double_dd_acquire_read
# endif
#else
# if defined(AO_HAVE_compare_double_and_swap_double)
#   define AO_compare_double_and_swap_double_dd_acquire_read(addr,o1,o2,n1,n2) \
                        AO_compare_double_and_swap_double(addr,o1,o2,n1,n2)
#   define AO_HAVE_compare_double_and_swap_double_dd_acquire_read
# endif
#endif /* !AO_NO_DD_ORDERING */

/* Compare_and_swap_double */
#if defined(AO_HAVE_compare_and_swap_double) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_compare_and_swap_double_acquire)
  AO_INLINE int
  AO_compare_and_swap_double_acquire(volatile AO_double_t *addr,
                                            AO_t o1,
                                            AO_t n1, AO_t n2)
  {
    int result = AO_compare_and_swap_double(addr, o1, n1, n2);
    AO_nop_full();
    return result;
  }
# define AO_HAVE_compare_and_swap_double_acquire
#endif
#if defined(AO_HAVE_compare_and_swap_double) \
    && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_compare_and_swap_double_release)
# define AO_compare_and_swap_double_release(addr,o1,n1,n2) \
                (AO_nop_full(), AO_compare_and_swap_double(addr,o1,n1,n2))
# define AO_HAVE_compare_and_swap_double_release
#endif
#if defined(AO_HAVE_compare_and_swap_double_full)
# if !defined(AO_HAVE_compare_and_swap_double_release)
#   define AO_compare_and_swap_double_release(addr,o1,n1,n2) \
                                AO_compare_and_swap_double_full(addr,o1,n1,n2)
#   define AO_HAVE_compare_and_swap_double_release
# endif
# if !defined(AO_HAVE_compare_and_swap_double_acquire)
#   define AO_compare_and_swap_double_acquire(addr,o1,n1,n2) \
                                AO_compare_and_swap_double_full(addr,o1,n1,n2)
#   define AO_HAVE_compare_and_swap_double_acquire
# endif
# if !defined(AO_HAVE_compare_and_swap_double_write)
#   define AO_compare_and_swap_double_write(addr,o1,n1,n2) \
                                AO_compare_and_swap_double_full(addr,o1,n1,n2)
#   define AO_HAVE_compare_and_swap_double_write
# endif
# if !defined(AO_HAVE_compare_and_swap_double_read)
#   define AO_compare_and_swap_double_read(addr,o1,n1,n2) \
                                AO_compare_and_swap_double_full(addr,o1,n1,n2)
#   define AO_HAVE_compare_and_swap_double_read
# endif
#endif /* AO_HAVE_compare_and_swap_double_full */

#if !defined(AO_HAVE_compare_and_swap_double) \
    && defined(AO_HAVE_compare_and_swap_double_release)
# define AO_compare_and_swap_double(addr,o1,n1,n2) \
                        AO_compare_and_swap_double_release(addr,o1,n1,n2)
# define AO_HAVE_compare_and_swap_double
#endif
#if !defined(AO_HAVE_compare_and_swap_double) \
    && defined(AO_HAVE_compare_and_swap_double_acquire)
# define AO_compare_and_swap_double(addr,o1,n1,n2) \
                        AO_compare_and_swap_double_acquire(addr,o1,n1,n2)
# define AO_HAVE_compare_and_swap_double
#endif
#if !defined(AO_HAVE_compare_and_swap_double) \
    && defined(AO_HAVE_compare_and_swap_double_write)
# define AO_compare_and_swap_double(addr,o1,n1,n2) \
                        AO_compare_and_swap_double_write(addr,o1,n1,n2)
# define AO_HAVE_compare_and_swap_double
#endif
#if !defined(AO_HAVE_compare_and_swap_double) \
    && defined(AO_HAVE_compare_and_swap_double_read)
# define AO_compare_and_swap_double(addr,o1,n1,n2) \
                        AO_compare_and_swap_double_read(addr,o1,n1,n2)
# define AO_HAVE_compare_and_swap_double
#endif

#if defined(AO_HAVE_compare_and_swap_double_acquire) \
    && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_compare_and_swap_double_full)
# define AO_compare_and_swap_double_full(addr,o1,n1,n2) \
        (AO_nop_full(), AO_compare_and_swap_double_acquire(addr,o1,n1,n2))
# define AO_HAVE_compare_and_swap_double_full
#endif

#if !defined(AO_HAVE_compare_and_swap_double_release_write) \
    && defined(AO_HAVE_compare_and_swap_double_write)
# define AO_compare_and_swap_double_release_write(addr,o1,n1,n2) \
                        AO_compare_and_swap_double_write(addr,o1,n1,n2)
# define AO_HAVE_compare_and_swap_double_release_write
#endif
#if !defined(AO_HAVE_compare_and_swap_double_release_write) \
    && defined(AO_HAVE_compare_and_swap_double_release)
# define AO_compare_and_swap_double_release_write(addr,o1,n1,n2) \
                        AO_compare_and_swap_double_release(addr,o1,n1,n2)
# define AO_HAVE_compare_and_swap_double_release_write
#endif
#if !defined(AO_HAVE_compare_and_swap_double_acquire_read) \
    && defined(AO_HAVE_compare_and_swap_double_read)
# define AO_compare_and_swap_double_acquire_read(addr,o1,n1,n2) \
                        AO_compare_and_swap_double_read(addr,o1,n1,n2)
# define AO_HAVE_compare_and_swap_double_acquire_read
#endif
#if !defined(AO_HAVE_compare_and_swap_double_acquire_read) \
    && defined(AO_HAVE_compare_and_swap_double_acquire)
# define AO_compare_and_swap_double_acquire_read(addr,o1,n1,n2) \
                        AO_compare_and_swap_double_acquire(addr,o1,n1,n2)
# define AO_HAVE_compare_and_swap_double_acquire_read
#endif

#ifdef AO_NO_DD_ORDERING
# if defined(AO_HAVE_compare_and_swap_double_acquire_read)
#   define AO_compare_and_swap_double_dd_acquire_read(addr,o1,n1,n2) \
                        AO_compare_and_swap_double_acquire_read(addr,o1,n1,n2)
#   define AO_HAVE_compare_and_swap_double_dd_acquire_read
# endif
#else
# if defined(AO_HAVE_compare_and_swap_double)
#   define AO_compare_and_swap_double_dd_acquire_read(addr,o1,n1,n2) \
                        AO_compare_and_swap_double(addr,o1,n1,n2)
#   define AO_HAVE_compare_and_swap_double_dd_acquire_read
# endif
#endif

/* Convenience functions for AO_double compare-and-swap which types and */
/* reads easier in code.                                                */
#if defined(AO_HAVE_compare_double_and_swap_double) \
    && !defined(AO_HAVE_double_compare_and_swap)
  AO_INLINE int
  AO_double_compare_and_swap(volatile AO_double_t *addr,
                             AO_double_t old_val, AO_double_t new_val)
  {
    return AO_compare_double_and_swap_double(addr,
                                        old_val.AO_val1, old_val.AO_val2,
                                        new_val.AO_val1, new_val.AO_val2);
  }
# define AO_HAVE_double_compare_and_swap
#endif
#if defined(AO_HAVE_compare_double_and_swap_double_release) \
    && !defined(AO_HAVE_double_compare_and_swap_release)
  AO_INLINE int
  AO_double_compare_and_swap_release(volatile AO_double_t *addr,
                                     AO_double_t old_val, AO_double_t new_val)
  {
    return AO_compare_double_and_swap_double_release(addr,
                                          old_val.AO_val1, old_val.AO_val2,
                                          new_val.AO_val1, new_val.AO_val2);
  }
# define AO_HAVE_double_compare_and_swap_release
#endif
#if defined(AO_HAVE_compare_double_and_swap_double_acquire) \
    && !defined(AO_HAVE_double_compare_and_swap_acquire)
  AO_INLINE int
  AO_double_compare_and_swap_acquire(volatile AO_double_t *addr,
                                     AO_double_t old_val, AO_double_t new_val)
  {
    return AO_compare_double_and_swap_double_acquire(addr,
                                          old_val.AO_val1, old_val.AO_val2,
                                          new_val.AO_val1, new_val.AO_val2);
  }
# define AO_HAVE_double_compare_and_swap_acquire
#endif
#if defined(AO_HAVE_compare_double_and_swap_double_read) \
    && !defined(AO_HAVE_double_compare_and_swap_read)
  AO_INLINE int
  AO_double_compare_and_swap_read(volatile AO_double_t *addr,
                                  AO_double_t old_val, AO_double_t new_val)
  {
    return AO_compare_double_and_swap_double_read(addr,
                                          old_val.AO_val1, old_val.AO_val2,
                                          new_val.AO_val1, new_val.AO_val2);
  }
# define AO_HAVE_double_compare_and_swap_read
#endif
#if defined(AO_HAVE_compare_double_and_swap_double_write) \
    && !defined(AO_HAVE_double_compare_and_swap_write)
  AO_INLINE int
  AO_double_compare_and_swap_write(volatile AO_double_t *addr,
                                   AO_double_t old_val, AO_double_t new_val)
  {
    return AO_compare_double_and_swap_double_write(addr,
                                          old_val.AO_val1, old_val.AO_val2,
                                          new_val.AO_val1, new_val.AO_val2);
  }
# define AO_HAVE_double_compare_and_swap_write
#endif
#if defined(AO_HAVE_compare_double_and_swap_double_release_write) \
    && !defined(AO_HAVE_double_compare_and_swap_release_write)
  AO_INLINE int
  AO_double_compare_and_swap_release_write(volatile AO_double_t *addr,
                                AO_double_t old_val, AO_double_t new_val)
  {
    return AO_compare_double_and_swap_double_release_write(addr,
                                          old_val.AO_val1, old_val.AO_val2,
                                          new_val.AO_val1, new_val.AO_val2);
  }
# define AO_HAVE_double_compare_and_swap_release_write
#endif
#if defined(AO_HAVE_compare_double_and_swap_double_acquire_read) \
    && !defined(AO_HAVE_double_compare_and_swap_acquire_read)
  AO_INLINE int
  AO_double_compare_and_swap_acquire_read(volatile AO_double_t *addr,
                                AO_double_t old_val, AO_double_t new_val)
  {
    return AO_compare_double_and_swap_double_acquire_read(addr,
                                          old_val.AO_val1, old_val.AO_val2,
                                          new_val.AO_val1, new_val.AO_val2);
  }
# define AO_HAVE_double_compare_and_swap_acquire_read
#endif
#if defined(AO_HAVE_compare_double_and_swap_double_full) \
    && !defined(AO_HAVE_double_compare_and_swap_full)
  AO_INLINE int
  AO_double_compare_and_swap_full(volatile AO_double_t *addr,
                                  AO_double_t old_val, AO_double_t new_val)
  {
    return AO_compare_double_and_swap_double_full(addr,
                                          old_val.AO_val1, old_val.AO_val2,
                                          new_val.AO_val1, new_val.AO_val2);
  }
# define AO_HAVE_double_compare_and_swap_full
#endif

#ifndef AO_HAVE_double_compare_and_swap_dd_acquire_read
  /* Duplicated from generalize-small because double CAS might be       */
  /* defined after the include.                                         */
# ifdef AO_NO_DD_ORDERING
#   if defined(AO_HAVE_double_compare_and_swap_acquire_read)
#     define AO_double_compare_and_swap_dd_acquire_read(addr, old, new_val) \
                AO_double_compare_and_swap_acquire_read(addr, old, new_val)
#     define AO_HAVE_double_compare_and_swap_dd_acquire_read
#   endif
# elif defined(AO_HAVE_double_compare_and_swap)
#   define AO_double_compare_and_swap_dd_acquire_read(addr, old, new_val) \
                AO_double_compare_and_swap(addr, old, new_val)
#   define AO_HAVE_double_compare_and_swap_dd_acquire_read
# endif /* !AO_NO_DD_ORDERING */
#endif
