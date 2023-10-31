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

/* char_fetch_compare_and_swap */
#if defined(AO_HAVE_char_fetch_compare_and_swap) \
    && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_char_fetch_compare_and_swap_acquire)
  AO_INLINE unsigned/**/char
  AO_char_fetch_compare_and_swap_acquire(volatile unsigned/**/char *addr,
                                          unsigned/**/char old_val, unsigned/**/char new_val)
  {
    unsigned/**/char result = AO_char_fetch_compare_and_swap(addr, old_val, new_val);
    AO_nop_full();
    return result;
  }
# define AO_HAVE_char_fetch_compare_and_swap_acquire
#endif
#if defined(AO_HAVE_char_fetch_compare_and_swap) \
    && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_char_fetch_compare_and_swap_release)
# define AO_char_fetch_compare_and_swap_release(addr, old_val, new_val) \
                (AO_nop_full(), \
                 AO_char_fetch_compare_and_swap(addr, old_val, new_val))
# define AO_HAVE_char_fetch_compare_and_swap_release
#endif
#if defined(AO_HAVE_char_fetch_compare_and_swap_full)
# if !defined(AO_HAVE_char_fetch_compare_and_swap_release)
#   define AO_char_fetch_compare_and_swap_release(addr, old_val, new_val) \
                AO_char_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define AO_HAVE_char_fetch_compare_and_swap_release
# endif
# if !defined(AO_HAVE_char_fetch_compare_and_swap_acquire)
#   define AO_char_fetch_compare_and_swap_acquire(addr, old_val, new_val) \
                AO_char_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define AO_HAVE_char_fetch_compare_and_swap_acquire
# endif
# if !defined(AO_HAVE_char_fetch_compare_and_swap_write)
#   define AO_char_fetch_compare_and_swap_write(addr, old_val, new_val) \
                AO_char_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define AO_HAVE_char_fetch_compare_and_swap_write
# endif
# if !defined(AO_HAVE_char_fetch_compare_and_swap_read)
#   define AO_char_fetch_compare_and_swap_read(addr, old_val, new_val) \
                AO_char_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define AO_HAVE_char_fetch_compare_and_swap_read
# endif
#endif /* AO_HAVE_char_fetch_compare_and_swap_full */

#if !defined(AO_HAVE_char_fetch_compare_and_swap) \
    && defined(AO_HAVE_char_fetch_compare_and_swap_release)
# define AO_char_fetch_compare_and_swap(addr, old_val, new_val) \
            AO_char_fetch_compare_and_swap_release(addr, old_val, new_val)
# define AO_HAVE_char_fetch_compare_and_swap
#endif
#if !defined(AO_HAVE_char_fetch_compare_and_swap) \
    && defined(AO_HAVE_char_fetch_compare_and_swap_acquire)
# define AO_char_fetch_compare_and_swap(addr, old_val, new_val) \
            AO_char_fetch_compare_and_swap_acquire(addr, old_val, new_val)
# define AO_HAVE_char_fetch_compare_and_swap
#endif
#if !defined(AO_HAVE_char_fetch_compare_and_swap) \
    && defined(AO_HAVE_char_fetch_compare_and_swap_write)
# define AO_char_fetch_compare_and_swap(addr, old_val, new_val) \
                AO_char_fetch_compare_and_swap_write(addr, old_val, new_val)
# define AO_HAVE_char_fetch_compare_and_swap
#endif
#if !defined(AO_HAVE_char_fetch_compare_and_swap) \
    && defined(AO_HAVE_char_fetch_compare_and_swap_read)
# define AO_char_fetch_compare_and_swap(addr, old_val, new_val) \
                AO_char_fetch_compare_and_swap_read(addr, old_val, new_val)
# define AO_HAVE_char_fetch_compare_and_swap
#endif

#if defined(AO_HAVE_char_fetch_compare_and_swap_acquire) \
    && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_char_fetch_compare_and_swap_full)
# define AO_char_fetch_compare_and_swap_full(addr, old_val, new_val) \
            (AO_nop_full(), \
             AO_char_fetch_compare_and_swap_acquire(addr, old_val, new_val))
# define AO_HAVE_char_fetch_compare_and_swap_full
#endif

#if !defined(AO_HAVE_char_fetch_compare_and_swap_release_write) \
    && defined(AO_HAVE_char_fetch_compare_and_swap_write)
# define AO_char_fetch_compare_and_swap_release_write(addr,old_val,new_val) \
                AO_char_fetch_compare_and_swap_write(addr, old_val, new_val)
# define AO_HAVE_char_fetch_compare_and_swap_release_write
#endif
#if !defined(AO_HAVE_char_fetch_compare_and_swap_release_write) \
    && defined(AO_HAVE_char_fetch_compare_and_swap_release)
# define AO_char_fetch_compare_and_swap_release_write(addr,old_val,new_val) \
            AO_char_fetch_compare_and_swap_release(addr, old_val, new_val)
# define AO_HAVE_char_fetch_compare_and_swap_release_write
#endif
#if !defined(AO_HAVE_char_fetch_compare_and_swap_acquire_read) \
    && defined(AO_HAVE_char_fetch_compare_and_swap_read)
# define AO_char_fetch_compare_and_swap_acquire_read(addr,old_val,new_val) \
                AO_char_fetch_compare_and_swap_read(addr, old_val, new_val)
# define AO_HAVE_char_fetch_compare_and_swap_acquire_read
#endif
#if !defined(AO_HAVE_char_fetch_compare_and_swap_acquire_read) \
    && defined(AO_HAVE_char_fetch_compare_and_swap_acquire)
# define AO_char_fetch_compare_and_swap_acquire_read(addr,old_val,new_val) \
            AO_char_fetch_compare_and_swap_acquire(addr, old_val, new_val)
# define AO_HAVE_char_fetch_compare_and_swap_acquire_read
#endif

#ifdef AO_NO_DD_ORDERING
# if defined(AO_HAVE_char_fetch_compare_and_swap_acquire_read)
#   define AO_char_fetch_compare_and_swap_dd_acquire_read(addr,old_val,new_val) \
        AO_char_fetch_compare_and_swap_acquire_read(addr, old_val, new_val)
#   define AO_HAVE_char_fetch_compare_and_swap_dd_acquire_read
# endif
#else
# if defined(AO_HAVE_char_fetch_compare_and_swap)
#   define AO_char_fetch_compare_and_swap_dd_acquire_read(addr,old_val,new_val) \
                AO_char_fetch_compare_and_swap(addr, old_val, new_val)
#   define AO_HAVE_char_fetch_compare_and_swap_dd_acquire_read
# endif
#endif /* !AO_NO_DD_ORDERING */

/* char_compare_and_swap */
#if defined(AO_HAVE_char_compare_and_swap) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_char_compare_and_swap_acquire)
  AO_INLINE int
  AO_char_compare_and_swap_acquire(volatile unsigned/**/char *addr, unsigned/**/char old,
                                    unsigned/**/char new_val)
  {
    int result = AO_char_compare_and_swap(addr, old, new_val);
    AO_nop_full();
    return result;
  }
# define AO_HAVE_char_compare_and_swap_acquire
#endif
#if defined(AO_HAVE_char_compare_and_swap) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_char_compare_and_swap_release)
# define AO_char_compare_and_swap_release(addr, old, new_val) \
                (AO_nop_full(), AO_char_compare_and_swap(addr, old, new_val))
# define AO_HAVE_char_compare_and_swap_release
#endif
#if defined(AO_HAVE_char_compare_and_swap_full)
# if !defined(AO_HAVE_char_compare_and_swap_release)
#   define AO_char_compare_and_swap_release(addr, old, new_val) \
                AO_char_compare_and_swap_full(addr, old, new_val)
#   define AO_HAVE_char_compare_and_swap_release
# endif
# if !defined(AO_HAVE_char_compare_and_swap_acquire)
#   define AO_char_compare_and_swap_acquire(addr, old, new_val) \
                AO_char_compare_and_swap_full(addr, old, new_val)
#   define AO_HAVE_char_compare_and_swap_acquire
# endif
# if !defined(AO_HAVE_char_compare_and_swap_write)
#   define AO_char_compare_and_swap_write(addr, old, new_val) \
                AO_char_compare_and_swap_full(addr, old, new_val)
#   define AO_HAVE_char_compare_and_swap_write
# endif
# if !defined(AO_HAVE_char_compare_and_swap_read)
#   define AO_char_compare_and_swap_read(addr, old, new_val) \
                AO_char_compare_and_swap_full(addr, old, new_val)
#   define AO_HAVE_char_compare_and_swap_read
# endif
#endif /* AO_HAVE_char_compare_and_swap_full */

#if !defined(AO_HAVE_char_compare_and_swap) \
    && defined(AO_HAVE_char_compare_and_swap_release)
# define AO_char_compare_and_swap(addr, old, new_val) \
                AO_char_compare_and_swap_release(addr, old, new_val)
# define AO_HAVE_char_compare_and_swap
#endif
#if !defined(AO_HAVE_char_compare_and_swap) \
    && defined(AO_HAVE_char_compare_and_swap_acquire)
# define AO_char_compare_and_swap(addr, old, new_val) \
                AO_char_compare_and_swap_acquire(addr, old, new_val)
# define AO_HAVE_char_compare_and_swap
#endif
#if !defined(AO_HAVE_char_compare_and_swap) \
    && defined(AO_HAVE_char_compare_and_swap_write)
# define AO_char_compare_and_swap(addr, old, new_val) \
                AO_char_compare_and_swap_write(addr, old, new_val)
# define AO_HAVE_char_compare_and_swap
#endif
#if !defined(AO_HAVE_char_compare_and_swap) \
    && defined(AO_HAVE_char_compare_and_swap_read)
# define AO_char_compare_and_swap(addr, old, new_val) \
                AO_char_compare_and_swap_read(addr, old, new_val)
# define AO_HAVE_char_compare_and_swap
#endif

#if defined(AO_HAVE_char_compare_and_swap_acquire) \
    && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_char_compare_and_swap_full)
# define AO_char_compare_and_swap_full(addr, old, new_val) \
                (AO_nop_full(), \
                 AO_char_compare_and_swap_acquire(addr, old, new_val))
# define AO_HAVE_char_compare_and_swap_full
#endif

#if !defined(AO_HAVE_char_compare_and_swap_release_write) \
    && defined(AO_HAVE_char_compare_and_swap_write)
# define AO_char_compare_and_swap_release_write(addr, old, new_val) \
                AO_char_compare_and_swap_write(addr, old, new_val)
# define AO_HAVE_char_compare_and_swap_release_write
#endif
#if !defined(AO_HAVE_char_compare_and_swap_release_write) \
    && defined(AO_HAVE_char_compare_and_swap_release)
# define AO_char_compare_and_swap_release_write(addr, old, new_val) \
                AO_char_compare_and_swap_release(addr, old, new_val)
# define AO_HAVE_char_compare_and_swap_release_write
#endif
#if !defined(AO_HAVE_char_compare_and_swap_acquire_read) \
    && defined(AO_HAVE_char_compare_and_swap_read)
# define AO_char_compare_and_swap_acquire_read(addr, old, new_val) \
                AO_char_compare_and_swap_read(addr, old, new_val)
# define AO_HAVE_char_compare_and_swap_acquire_read
#endif
#if !defined(AO_HAVE_char_compare_and_swap_acquire_read) \
    && defined(AO_HAVE_char_compare_and_swap_acquire)
# define AO_char_compare_and_swap_acquire_read(addr, old, new_val) \
                AO_char_compare_and_swap_acquire(addr, old, new_val)
# define AO_HAVE_char_compare_and_swap_acquire_read
#endif

#ifdef AO_NO_DD_ORDERING
# if defined(AO_HAVE_char_compare_and_swap_acquire_read)
#   define AO_char_compare_and_swap_dd_acquire_read(addr, old, new_val) \
                AO_char_compare_and_swap_acquire_read(addr, old, new_val)
#   define AO_HAVE_char_compare_and_swap_dd_acquire_read
# endif
#else
# if defined(AO_HAVE_char_compare_and_swap)
#   define AO_char_compare_and_swap_dd_acquire_read(addr, old, new_val) \
                AO_char_compare_and_swap(addr, old, new_val)
#   define AO_HAVE_char_compare_and_swap_dd_acquire_read
# endif
#endif /* !AO_NO_DD_ORDERING */

/* char_load */
#if defined(AO_HAVE_char_load_full) && !defined(AO_HAVE_char_load_acquire)
# define AO_char_load_acquire(addr) AO_char_load_full(addr)
# define AO_HAVE_char_load_acquire
#endif

#if defined(AO_HAVE_char_load_acquire) && !defined(AO_HAVE_char_load)
# define AO_char_load(addr) AO_char_load_acquire(addr)
# define AO_HAVE_char_load
#endif

#if defined(AO_HAVE_char_load_full) && !defined(AO_HAVE_char_load_read)
# define AO_char_load_read(addr) AO_char_load_full(addr)
# define AO_HAVE_char_load_read
#endif

#if !defined(AO_HAVE_char_load_acquire_read) \
    && defined(AO_HAVE_char_load_acquire)
# define AO_char_load_acquire_read(addr) AO_char_load_acquire(addr)
# define AO_HAVE_char_load_acquire_read
#endif

#if defined(AO_HAVE_char_load) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_char_load_acquire)
  AO_INLINE unsigned/**/char
  AO_char_load_acquire(const volatile unsigned/**/char *addr)
  {
    unsigned/**/char result = AO_char_load(addr);

    /* Acquire barrier would be useless, since the load could be delayed    */
    /* beyond it.                                                           */
    AO_nop_full();
    return result;
  }
# define AO_HAVE_char_load_acquire
#endif

#if defined(AO_HAVE_char_load) && defined(AO_HAVE_nop_read) \
    && !defined(AO_HAVE_char_load_read)
  AO_INLINE unsigned/**/char
  AO_char_load_read(const volatile unsigned/**/char *addr)
  {
    unsigned/**/char result = AO_char_load(addr);

    AO_nop_read();
    return result;
  }
# define AO_HAVE_char_load_read
#endif

#if defined(AO_HAVE_char_load_acquire) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_char_load_full)
# define AO_char_load_full(addr) (AO_nop_full(), AO_char_load_acquire(addr))
# define AO_HAVE_char_load_full
#endif

#if defined(AO_HAVE_char_compare_and_swap_read) \
    && !defined(AO_HAVE_char_load_read)
# define AO_char_CAS_BASED_LOAD_READ
  AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE unsigned/**/char
  AO_char_load_read(const volatile unsigned/**/char *addr)
  {
    unsigned/**/char result;

    do {
      result = *(const unsigned/**/char *)addr;
    } while (AO_EXPECT_FALSE(!AO_char_compare_and_swap_read(
                                                (volatile unsigned/**/char *)addr,
                                                result, result)));
    return result;
  }
# define AO_HAVE_char_load_read
#endif

#if !defined(AO_HAVE_char_load_acquire_read) \
    && defined(AO_HAVE_char_load_read)
# define AO_char_load_acquire_read(addr) AO_char_load_read(addr)
# define AO_HAVE_char_load_acquire_read
#endif

#if defined(AO_HAVE_char_load_acquire_read) && !defined(AO_HAVE_char_load) \
    && (!defined(AO_char_CAS_BASED_LOAD_READ) \
        || !defined(AO_HAVE_char_compare_and_swap))
# define AO_char_load(addr) AO_char_load_acquire_read(addr)
# define AO_HAVE_char_load
#endif

#if defined(AO_HAVE_char_compare_and_swap_full) \
    && !defined(AO_HAVE_char_load_full)
  AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE unsigned/**/char
  AO_char_load_full(const volatile unsigned/**/char *addr)
  {
    unsigned/**/char result;

    do {
      result = *(const unsigned/**/char *)addr;
    } while (AO_EXPECT_FALSE(!AO_char_compare_and_swap_full(
                                                (volatile unsigned/**/char *)addr,
                                                result, result)));
    return result;
  }
# define AO_HAVE_char_load_full
#endif

#if defined(AO_HAVE_char_compare_and_swap_acquire) \
    && !defined(AO_HAVE_char_load_acquire)
  AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE unsigned/**/char
  AO_char_load_acquire(const volatile unsigned/**/char *addr)
  {
    unsigned/**/char result;

    do {
      result = *(const unsigned/**/char *)addr;
    } while (AO_EXPECT_FALSE(!AO_char_compare_and_swap_acquire(
                                                (volatile unsigned/**/char *)addr,
                                                result, result)));
    return result;
  }
# define AO_HAVE_char_load_acquire
#endif

#if defined(AO_HAVE_char_compare_and_swap) && !defined(AO_HAVE_char_load)
  AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE unsigned/**/char
  AO_char_load(const volatile unsigned/**/char *addr)
  {
    unsigned/**/char result;

    do {
      result = *(const unsigned/**/char *)addr;
    } while (AO_EXPECT_FALSE(!AO_char_compare_and_swap(
                                                (volatile unsigned/**/char *)addr,
                                                result, result)));
    return result;
  }
# define AO_HAVE_char_load
#endif

#ifdef AO_NO_DD_ORDERING
# if defined(AO_HAVE_char_load_acquire_read)
#   define AO_char_load_dd_acquire_read(addr) \
                                AO_char_load_acquire_read(addr)
#   define AO_HAVE_char_load_dd_acquire_read
# endif
#else
# if defined(AO_HAVE_char_load)
#   define AO_char_load_dd_acquire_read(addr) AO_char_load(addr)
#   define AO_HAVE_char_load_dd_acquire_read
# endif
#endif /* !AO_NO_DD_ORDERING */

/* char_store */
#if defined(AO_HAVE_char_store_full) && !defined(AO_HAVE_char_store_release)
# define AO_char_store_release(addr, val) AO_char_store_full(addr, val)
# define AO_HAVE_char_store_release
#endif

#if defined(AO_HAVE_char_store_release) && !defined(AO_HAVE_char_store)
# define AO_char_store(addr, val) AO_char_store_release(addr, val)
# define AO_HAVE_char_store
#endif

#if defined(AO_HAVE_char_store_full) && !defined(AO_HAVE_char_store_write)
# define AO_char_store_write(addr, val) AO_char_store_full(addr, val)
# define AO_HAVE_char_store_write
#endif

#if defined(AO_HAVE_char_store_release) \
    && !defined(AO_HAVE_char_store_release_write)
# define AO_char_store_release_write(addr, val) \
                                AO_char_store_release(addr, val)
# define AO_HAVE_char_store_release_write
#endif

#if defined(AO_HAVE_char_store_write) && !defined(AO_HAVE_char_store)
# define AO_char_store(addr, val) AO_char_store_write(addr, val)
# define AO_HAVE_char_store
#endif

#if defined(AO_HAVE_char_store) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_char_store_release)
# define AO_char_store_release(addr, val) \
                                (AO_nop_full(), AO_char_store(addr, val))
# define AO_HAVE_char_store_release
#endif

#if defined(AO_HAVE_char_store) && defined(AO_HAVE_nop_write) \
    && !defined(AO_HAVE_char_store_write)
# define AO_char_store_write(addr, val) \
                                (AO_nop_write(), AO_char_store(addr, val))
# define AO_HAVE_char_store_write
#endif

#if defined(AO_HAVE_char_compare_and_swap_write) \
    && !defined(AO_HAVE_char_store_write)
  AO_ATTR_NO_SANITIZE_MEMORY AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE void
  AO_char_store_write(volatile unsigned/**/char *addr, unsigned/**/char new_val)
  {
    unsigned/**/char old_val;

    do {
      old_val = *(unsigned/**/char *)addr;
    } while (AO_EXPECT_FALSE(!AO_char_compare_and_swap_write(addr, old_val,
                                                              new_val)));
  }
# define AO_HAVE_char_store_write
#endif

#if defined(AO_HAVE_char_store_write) \
    && !defined(AO_HAVE_char_store_release_write)
# define AO_char_store_release_write(addr, val) \
                                AO_char_store_write(addr, val)
# define AO_HAVE_char_store_release_write
#endif

#if defined(AO_HAVE_char_store_release) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_char_store_full)
# define AO_char_store_full(addr, val) \
                                (AO_char_store_release(addr, val), \
                                 AO_nop_full())
# define AO_HAVE_char_store_full
#endif

#if defined(AO_HAVE_char_compare_and_swap) && !defined(AO_HAVE_char_store)
  AO_ATTR_NO_SANITIZE_MEMORY AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE void
  AO_char_store(volatile unsigned/**/char *addr, unsigned/**/char new_val)
  {
    unsigned/**/char old_val;

    do {
      old_val = *(unsigned/**/char *)addr;
    } while (AO_EXPECT_FALSE(!AO_char_compare_and_swap(addr,
                                                        old_val, new_val)));
  }
# define AO_HAVE_char_store
#endif

#if defined(AO_HAVE_char_compare_and_swap_release) \
    && !defined(AO_HAVE_char_store_release)
  AO_ATTR_NO_SANITIZE_MEMORY AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE void
  AO_char_store_release(volatile unsigned/**/char *addr, unsigned/**/char new_val)
  {
    unsigned/**/char old_val;

    do {
      old_val = *(unsigned/**/char *)addr;
    } while (AO_EXPECT_FALSE(!AO_char_compare_and_swap_release(addr, old_val,
                                                                new_val)));
  }
# define AO_HAVE_char_store_release
#endif

#if defined(AO_HAVE_char_compare_and_swap_full) \
    && !defined(AO_HAVE_char_store_full)
  AO_ATTR_NO_SANITIZE_MEMORY AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE void
  AO_char_store_full(volatile unsigned/**/char *addr, unsigned/**/char new_val)
  {
    unsigned/**/char old_val;

    do {
      old_val = *(unsigned/**/char *)addr;
    } while (AO_EXPECT_FALSE(!AO_char_compare_and_swap_full(addr, old_val,
                                                             new_val)));
  }
# define AO_HAVE_char_store_full
#endif
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

/* short_fetch_compare_and_swap */
#if defined(AO_HAVE_short_fetch_compare_and_swap) \
    && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_short_fetch_compare_and_swap_acquire)
  AO_INLINE unsigned/**/short
  AO_short_fetch_compare_and_swap_acquire(volatile unsigned/**/short *addr,
                                          unsigned/**/short old_val, unsigned/**/short new_val)
  {
    unsigned/**/short result = AO_short_fetch_compare_and_swap(addr, old_val, new_val);
    AO_nop_full();
    return result;
  }
# define AO_HAVE_short_fetch_compare_and_swap_acquire
#endif
#if defined(AO_HAVE_short_fetch_compare_and_swap) \
    && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_short_fetch_compare_and_swap_release)
# define AO_short_fetch_compare_and_swap_release(addr, old_val, new_val) \
                (AO_nop_full(), \
                 AO_short_fetch_compare_and_swap(addr, old_val, new_val))
# define AO_HAVE_short_fetch_compare_and_swap_release
#endif
#if defined(AO_HAVE_short_fetch_compare_and_swap_full)
# if !defined(AO_HAVE_short_fetch_compare_and_swap_release)
#   define AO_short_fetch_compare_and_swap_release(addr, old_val, new_val) \
                AO_short_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define AO_HAVE_short_fetch_compare_and_swap_release
# endif
# if !defined(AO_HAVE_short_fetch_compare_and_swap_acquire)
#   define AO_short_fetch_compare_and_swap_acquire(addr, old_val, new_val) \
                AO_short_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define AO_HAVE_short_fetch_compare_and_swap_acquire
# endif
# if !defined(AO_HAVE_short_fetch_compare_and_swap_write)
#   define AO_short_fetch_compare_and_swap_write(addr, old_val, new_val) \
                AO_short_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define AO_HAVE_short_fetch_compare_and_swap_write
# endif
# if !defined(AO_HAVE_short_fetch_compare_and_swap_read)
#   define AO_short_fetch_compare_and_swap_read(addr, old_val, new_val) \
                AO_short_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define AO_HAVE_short_fetch_compare_and_swap_read
# endif
#endif /* AO_HAVE_short_fetch_compare_and_swap_full */

#if !defined(AO_HAVE_short_fetch_compare_and_swap) \
    && defined(AO_HAVE_short_fetch_compare_and_swap_release)
# define AO_short_fetch_compare_and_swap(addr, old_val, new_val) \
            AO_short_fetch_compare_and_swap_release(addr, old_val, new_val)
# define AO_HAVE_short_fetch_compare_and_swap
#endif
#if !defined(AO_HAVE_short_fetch_compare_and_swap) \
    && defined(AO_HAVE_short_fetch_compare_and_swap_acquire)
# define AO_short_fetch_compare_and_swap(addr, old_val, new_val) \
            AO_short_fetch_compare_and_swap_acquire(addr, old_val, new_val)
# define AO_HAVE_short_fetch_compare_and_swap
#endif
#if !defined(AO_HAVE_short_fetch_compare_and_swap) \
    && defined(AO_HAVE_short_fetch_compare_and_swap_write)
# define AO_short_fetch_compare_and_swap(addr, old_val, new_val) \
                AO_short_fetch_compare_and_swap_write(addr, old_val, new_val)
# define AO_HAVE_short_fetch_compare_and_swap
#endif
#if !defined(AO_HAVE_short_fetch_compare_and_swap) \
    && defined(AO_HAVE_short_fetch_compare_and_swap_read)
# define AO_short_fetch_compare_and_swap(addr, old_val, new_val) \
                AO_short_fetch_compare_and_swap_read(addr, old_val, new_val)
# define AO_HAVE_short_fetch_compare_and_swap
#endif

#if defined(AO_HAVE_short_fetch_compare_and_swap_acquire) \
    && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_short_fetch_compare_and_swap_full)
# define AO_short_fetch_compare_and_swap_full(addr, old_val, new_val) \
            (AO_nop_full(), \
             AO_short_fetch_compare_and_swap_acquire(addr, old_val, new_val))
# define AO_HAVE_short_fetch_compare_and_swap_full
#endif

#if !defined(AO_HAVE_short_fetch_compare_and_swap_release_write) \
    && defined(AO_HAVE_short_fetch_compare_and_swap_write)
# define AO_short_fetch_compare_and_swap_release_write(addr,old_val,new_val) \
                AO_short_fetch_compare_and_swap_write(addr, old_val, new_val)
# define AO_HAVE_short_fetch_compare_and_swap_release_write
#endif
#if !defined(AO_HAVE_short_fetch_compare_and_swap_release_write) \
    && defined(AO_HAVE_short_fetch_compare_and_swap_release)
# define AO_short_fetch_compare_and_swap_release_write(addr,old_val,new_val) \
            AO_short_fetch_compare_and_swap_release(addr, old_val, new_val)
# define AO_HAVE_short_fetch_compare_and_swap_release_write
#endif
#if !defined(AO_HAVE_short_fetch_compare_and_swap_acquire_read) \
    && defined(AO_HAVE_short_fetch_compare_and_swap_read)
# define AO_short_fetch_compare_and_swap_acquire_read(addr,old_val,new_val) \
                AO_short_fetch_compare_and_swap_read(addr, old_val, new_val)
# define AO_HAVE_short_fetch_compare_and_swap_acquire_read
#endif
#if !defined(AO_HAVE_short_fetch_compare_and_swap_acquire_read) \
    && defined(AO_HAVE_short_fetch_compare_and_swap_acquire)
# define AO_short_fetch_compare_and_swap_acquire_read(addr,old_val,new_val) \
            AO_short_fetch_compare_and_swap_acquire(addr, old_val, new_val)
# define AO_HAVE_short_fetch_compare_and_swap_acquire_read
#endif

#ifdef AO_NO_DD_ORDERING
# if defined(AO_HAVE_short_fetch_compare_and_swap_acquire_read)
#   define AO_short_fetch_compare_and_swap_dd_acquire_read(addr,old_val,new_val) \
        AO_short_fetch_compare_and_swap_acquire_read(addr, old_val, new_val)
#   define AO_HAVE_short_fetch_compare_and_swap_dd_acquire_read
# endif
#else
# if defined(AO_HAVE_short_fetch_compare_and_swap)
#   define AO_short_fetch_compare_and_swap_dd_acquire_read(addr,old_val,new_val) \
                AO_short_fetch_compare_and_swap(addr, old_val, new_val)
#   define AO_HAVE_short_fetch_compare_and_swap_dd_acquire_read
# endif
#endif /* !AO_NO_DD_ORDERING */

/* short_compare_and_swap */
#if defined(AO_HAVE_short_compare_and_swap) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_short_compare_and_swap_acquire)
  AO_INLINE int
  AO_short_compare_and_swap_acquire(volatile unsigned/**/short *addr, unsigned/**/short old,
                                    unsigned/**/short new_val)
  {
    int result = AO_short_compare_and_swap(addr, old, new_val);
    AO_nop_full();
    return result;
  }
# define AO_HAVE_short_compare_and_swap_acquire
#endif
#if defined(AO_HAVE_short_compare_and_swap) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_short_compare_and_swap_release)
# define AO_short_compare_and_swap_release(addr, old, new_val) \
                (AO_nop_full(), AO_short_compare_and_swap(addr, old, new_val))
# define AO_HAVE_short_compare_and_swap_release
#endif
#if defined(AO_HAVE_short_compare_and_swap_full)
# if !defined(AO_HAVE_short_compare_and_swap_release)
#   define AO_short_compare_and_swap_release(addr, old, new_val) \
                AO_short_compare_and_swap_full(addr, old, new_val)
#   define AO_HAVE_short_compare_and_swap_release
# endif
# if !defined(AO_HAVE_short_compare_and_swap_acquire)
#   define AO_short_compare_and_swap_acquire(addr, old, new_val) \
                AO_short_compare_and_swap_full(addr, old, new_val)
#   define AO_HAVE_short_compare_and_swap_acquire
# endif
# if !defined(AO_HAVE_short_compare_and_swap_write)
#   define AO_short_compare_and_swap_write(addr, old, new_val) \
                AO_short_compare_and_swap_full(addr, old, new_val)
#   define AO_HAVE_short_compare_and_swap_write
# endif
# if !defined(AO_HAVE_short_compare_and_swap_read)
#   define AO_short_compare_and_swap_read(addr, old, new_val) \
                AO_short_compare_and_swap_full(addr, old, new_val)
#   define AO_HAVE_short_compare_and_swap_read
# endif
#endif /* AO_HAVE_short_compare_and_swap_full */

#if !defined(AO_HAVE_short_compare_and_swap) \
    && defined(AO_HAVE_short_compare_and_swap_release)
# define AO_short_compare_and_swap(addr, old, new_val) \
                AO_short_compare_and_swap_release(addr, old, new_val)
# define AO_HAVE_short_compare_and_swap
#endif
#if !defined(AO_HAVE_short_compare_and_swap) \
    && defined(AO_HAVE_short_compare_and_swap_acquire)
# define AO_short_compare_and_swap(addr, old, new_val) \
                AO_short_compare_and_swap_acquire(addr, old, new_val)
# define AO_HAVE_short_compare_and_swap
#endif
#if !defined(AO_HAVE_short_compare_and_swap) \
    && defined(AO_HAVE_short_compare_and_swap_write)
# define AO_short_compare_and_swap(addr, old, new_val) \
                AO_short_compare_and_swap_write(addr, old, new_val)
# define AO_HAVE_short_compare_and_swap
#endif
#if !defined(AO_HAVE_short_compare_and_swap) \
    && defined(AO_HAVE_short_compare_and_swap_read)
# define AO_short_compare_and_swap(addr, old, new_val) \
                AO_short_compare_and_swap_read(addr, old, new_val)
# define AO_HAVE_short_compare_and_swap
#endif

#if defined(AO_HAVE_short_compare_and_swap_acquire) \
    && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_short_compare_and_swap_full)
# define AO_short_compare_and_swap_full(addr, old, new_val) \
                (AO_nop_full(), \
                 AO_short_compare_and_swap_acquire(addr, old, new_val))
# define AO_HAVE_short_compare_and_swap_full
#endif

#if !defined(AO_HAVE_short_compare_and_swap_release_write) \
    && defined(AO_HAVE_short_compare_and_swap_write)
# define AO_short_compare_and_swap_release_write(addr, old, new_val) \
                AO_short_compare_and_swap_write(addr, old, new_val)
# define AO_HAVE_short_compare_and_swap_release_write
#endif
#if !defined(AO_HAVE_short_compare_and_swap_release_write) \
    && defined(AO_HAVE_short_compare_and_swap_release)
# define AO_short_compare_and_swap_release_write(addr, old, new_val) \
                AO_short_compare_and_swap_release(addr, old, new_val)
# define AO_HAVE_short_compare_and_swap_release_write
#endif
#if !defined(AO_HAVE_short_compare_and_swap_acquire_read) \
    && defined(AO_HAVE_short_compare_and_swap_read)
# define AO_short_compare_and_swap_acquire_read(addr, old, new_val) \
                AO_short_compare_and_swap_read(addr, old, new_val)
# define AO_HAVE_short_compare_and_swap_acquire_read
#endif
#if !defined(AO_HAVE_short_compare_and_swap_acquire_read) \
    && defined(AO_HAVE_short_compare_and_swap_acquire)
# define AO_short_compare_and_swap_acquire_read(addr, old, new_val) \
                AO_short_compare_and_swap_acquire(addr, old, new_val)
# define AO_HAVE_short_compare_and_swap_acquire_read
#endif

#ifdef AO_NO_DD_ORDERING
# if defined(AO_HAVE_short_compare_and_swap_acquire_read)
#   define AO_short_compare_and_swap_dd_acquire_read(addr, old, new_val) \
                AO_short_compare_and_swap_acquire_read(addr, old, new_val)
#   define AO_HAVE_short_compare_and_swap_dd_acquire_read
# endif
#else
# if defined(AO_HAVE_short_compare_and_swap)
#   define AO_short_compare_and_swap_dd_acquire_read(addr, old, new_val) \
                AO_short_compare_and_swap(addr, old, new_val)
#   define AO_HAVE_short_compare_and_swap_dd_acquire_read
# endif
#endif /* !AO_NO_DD_ORDERING */

/* short_load */
#if defined(AO_HAVE_short_load_full) && !defined(AO_HAVE_short_load_acquire)
# define AO_short_load_acquire(addr) AO_short_load_full(addr)
# define AO_HAVE_short_load_acquire
#endif

#if defined(AO_HAVE_short_load_acquire) && !defined(AO_HAVE_short_load)
# define AO_short_load(addr) AO_short_load_acquire(addr)
# define AO_HAVE_short_load
#endif

#if defined(AO_HAVE_short_load_full) && !defined(AO_HAVE_short_load_read)
# define AO_short_load_read(addr) AO_short_load_full(addr)
# define AO_HAVE_short_load_read
#endif

#if !defined(AO_HAVE_short_load_acquire_read) \
    && defined(AO_HAVE_short_load_acquire)
# define AO_short_load_acquire_read(addr) AO_short_load_acquire(addr)
# define AO_HAVE_short_load_acquire_read
#endif

#if defined(AO_HAVE_short_load) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_short_load_acquire)
  AO_INLINE unsigned/**/short
  AO_short_load_acquire(const volatile unsigned/**/short *addr)
  {
    unsigned/**/short result = AO_short_load(addr);

    /* Acquire barrier would be useless, since the load could be delayed    */
    /* beyond it.                                                           */
    AO_nop_full();
    return result;
  }
# define AO_HAVE_short_load_acquire
#endif

#if defined(AO_HAVE_short_load) && defined(AO_HAVE_nop_read) \
    && !defined(AO_HAVE_short_load_read)
  AO_INLINE unsigned/**/short
  AO_short_load_read(const volatile unsigned/**/short *addr)
  {
    unsigned/**/short result = AO_short_load(addr);

    AO_nop_read();
    return result;
  }
# define AO_HAVE_short_load_read
#endif

#if defined(AO_HAVE_short_load_acquire) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_short_load_full)
# define AO_short_load_full(addr) (AO_nop_full(), AO_short_load_acquire(addr))
# define AO_HAVE_short_load_full
#endif

#if defined(AO_HAVE_short_compare_and_swap_read) \
    && !defined(AO_HAVE_short_load_read)
# define AO_short_CAS_BASED_LOAD_READ
  AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE unsigned/**/short
  AO_short_load_read(const volatile unsigned/**/short *addr)
  {
    unsigned/**/short result;

    do {
      result = *(const unsigned/**/short *)addr;
    } while (AO_EXPECT_FALSE(!AO_short_compare_and_swap_read(
                                                (volatile unsigned/**/short *)addr,
                                                result, result)));
    return result;
  }
# define AO_HAVE_short_load_read
#endif

#if !defined(AO_HAVE_short_load_acquire_read) \
    && defined(AO_HAVE_short_load_read)
# define AO_short_load_acquire_read(addr) AO_short_load_read(addr)
# define AO_HAVE_short_load_acquire_read
#endif

#if defined(AO_HAVE_short_load_acquire_read) && !defined(AO_HAVE_short_load) \
    && (!defined(AO_short_CAS_BASED_LOAD_READ) \
        || !defined(AO_HAVE_short_compare_and_swap))
# define AO_short_load(addr) AO_short_load_acquire_read(addr)
# define AO_HAVE_short_load
#endif

#if defined(AO_HAVE_short_compare_and_swap_full) \
    && !defined(AO_HAVE_short_load_full)
  AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE unsigned/**/short
  AO_short_load_full(const volatile unsigned/**/short *addr)
  {
    unsigned/**/short result;

    do {
      result = *(const unsigned/**/short *)addr;
    } while (AO_EXPECT_FALSE(!AO_short_compare_and_swap_full(
                                                (volatile unsigned/**/short *)addr,
                                                result, result)));
    return result;
  }
# define AO_HAVE_short_load_full
#endif

#if defined(AO_HAVE_short_compare_and_swap_acquire) \
    && !defined(AO_HAVE_short_load_acquire)
  AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE unsigned/**/short
  AO_short_load_acquire(const volatile unsigned/**/short *addr)
  {
    unsigned/**/short result;

    do {
      result = *(const unsigned/**/short *)addr;
    } while (AO_EXPECT_FALSE(!AO_short_compare_and_swap_acquire(
                                                (volatile unsigned/**/short *)addr,
                                                result, result)));
    return result;
  }
# define AO_HAVE_short_load_acquire
#endif

#if defined(AO_HAVE_short_compare_and_swap) && !defined(AO_HAVE_short_load)
  AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE unsigned/**/short
  AO_short_load(const volatile unsigned/**/short *addr)
  {
    unsigned/**/short result;

    do {
      result = *(const unsigned/**/short *)addr;
    } while (AO_EXPECT_FALSE(!AO_short_compare_and_swap(
                                                (volatile unsigned/**/short *)addr,
                                                result, result)));
    return result;
  }
# define AO_HAVE_short_load
#endif

#ifdef AO_NO_DD_ORDERING
# if defined(AO_HAVE_short_load_acquire_read)
#   define AO_short_load_dd_acquire_read(addr) \
                                AO_short_load_acquire_read(addr)
#   define AO_HAVE_short_load_dd_acquire_read
# endif
#else
# if defined(AO_HAVE_short_load)
#   define AO_short_load_dd_acquire_read(addr) AO_short_load(addr)
#   define AO_HAVE_short_load_dd_acquire_read
# endif
#endif /* !AO_NO_DD_ORDERING */

/* short_store */
#if defined(AO_HAVE_short_store_full) && !defined(AO_HAVE_short_store_release)
# define AO_short_store_release(addr, val) AO_short_store_full(addr, val)
# define AO_HAVE_short_store_release
#endif

#if defined(AO_HAVE_short_store_release) && !defined(AO_HAVE_short_store)
# define AO_short_store(addr, val) AO_short_store_release(addr, val)
# define AO_HAVE_short_store
#endif

#if defined(AO_HAVE_short_store_full) && !defined(AO_HAVE_short_store_write)
# define AO_short_store_write(addr, val) AO_short_store_full(addr, val)
# define AO_HAVE_short_store_write
#endif

#if defined(AO_HAVE_short_store_release) \
    && !defined(AO_HAVE_short_store_release_write)
# define AO_short_store_release_write(addr, val) \
                                AO_short_store_release(addr, val)
# define AO_HAVE_short_store_release_write
#endif

#if defined(AO_HAVE_short_store_write) && !defined(AO_HAVE_short_store)
# define AO_short_store(addr, val) AO_short_store_write(addr, val)
# define AO_HAVE_short_store
#endif

#if defined(AO_HAVE_short_store) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_short_store_release)
# define AO_short_store_release(addr, val) \
                                (AO_nop_full(), AO_short_store(addr, val))
# define AO_HAVE_short_store_release
#endif

#if defined(AO_HAVE_short_store) && defined(AO_HAVE_nop_write) \
    && !defined(AO_HAVE_short_store_write)
# define AO_short_store_write(addr, val) \
                                (AO_nop_write(), AO_short_store(addr, val))
# define AO_HAVE_short_store_write
#endif

#if defined(AO_HAVE_short_compare_and_swap_write) \
    && !defined(AO_HAVE_short_store_write)
  AO_ATTR_NO_SANITIZE_MEMORY AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE void
  AO_short_store_write(volatile unsigned/**/short *addr, unsigned/**/short new_val)
  {
    unsigned/**/short old_val;

    do {
      old_val = *(unsigned/**/short *)addr;
    } while (AO_EXPECT_FALSE(!AO_short_compare_and_swap_write(addr, old_val,
                                                              new_val)));
  }
# define AO_HAVE_short_store_write
#endif

#if defined(AO_HAVE_short_store_write) \
    && !defined(AO_HAVE_short_store_release_write)
# define AO_short_store_release_write(addr, val) \
                                AO_short_store_write(addr, val)
# define AO_HAVE_short_store_release_write
#endif

#if defined(AO_HAVE_short_store_release) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_short_store_full)
# define AO_short_store_full(addr, val) \
                                (AO_short_store_release(addr, val), \
                                 AO_nop_full())
# define AO_HAVE_short_store_full
#endif

#if defined(AO_HAVE_short_compare_and_swap) && !defined(AO_HAVE_short_store)
  AO_ATTR_NO_SANITIZE_MEMORY AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE void
  AO_short_store(volatile unsigned/**/short *addr, unsigned/**/short new_val)
  {
    unsigned/**/short old_val;

    do {
      old_val = *(unsigned/**/short *)addr;
    } while (AO_EXPECT_FALSE(!AO_short_compare_and_swap(addr,
                                                        old_val, new_val)));
  }
# define AO_HAVE_short_store
#endif

#if defined(AO_HAVE_short_compare_and_swap_release) \
    && !defined(AO_HAVE_short_store_release)
  AO_ATTR_NO_SANITIZE_MEMORY AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE void
  AO_short_store_release(volatile unsigned/**/short *addr, unsigned/**/short new_val)
  {
    unsigned/**/short old_val;

    do {
      old_val = *(unsigned/**/short *)addr;
    } while (AO_EXPECT_FALSE(!AO_short_compare_and_swap_release(addr, old_val,
                                                                new_val)));
  }
# define AO_HAVE_short_store_release
#endif

#if defined(AO_HAVE_short_compare_and_swap_full) \
    && !defined(AO_HAVE_short_store_full)
  AO_ATTR_NO_SANITIZE_MEMORY AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE void
  AO_short_store_full(volatile unsigned/**/short *addr, unsigned/**/short new_val)
  {
    unsigned/**/short old_val;

    do {
      old_val = *(unsigned/**/short *)addr;
    } while (AO_EXPECT_FALSE(!AO_short_compare_and_swap_full(addr, old_val,
                                                             new_val)));
  }
# define AO_HAVE_short_store_full
#endif
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

/* int_fetch_compare_and_swap */
#if defined(AO_HAVE_int_fetch_compare_and_swap) \
    && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_int_fetch_compare_and_swap_acquire)
  AO_INLINE unsigned
  AO_int_fetch_compare_and_swap_acquire(volatile unsigned *addr,
                                          unsigned old_val, unsigned new_val)
  {
    unsigned result = AO_int_fetch_compare_and_swap(addr, old_val, new_val);
    AO_nop_full();
    return result;
  }
# define AO_HAVE_int_fetch_compare_and_swap_acquire
#endif
#if defined(AO_HAVE_int_fetch_compare_and_swap) \
    && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_int_fetch_compare_and_swap_release)
# define AO_int_fetch_compare_and_swap_release(addr, old_val, new_val) \
                (AO_nop_full(), \
                 AO_int_fetch_compare_and_swap(addr, old_val, new_val))
# define AO_HAVE_int_fetch_compare_and_swap_release
#endif
#if defined(AO_HAVE_int_fetch_compare_and_swap_full)
# if !defined(AO_HAVE_int_fetch_compare_and_swap_release)
#   define AO_int_fetch_compare_and_swap_release(addr, old_val, new_val) \
                AO_int_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define AO_HAVE_int_fetch_compare_and_swap_release
# endif
# if !defined(AO_HAVE_int_fetch_compare_and_swap_acquire)
#   define AO_int_fetch_compare_and_swap_acquire(addr, old_val, new_val) \
                AO_int_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define AO_HAVE_int_fetch_compare_and_swap_acquire
# endif
# if !defined(AO_HAVE_int_fetch_compare_and_swap_write)
#   define AO_int_fetch_compare_and_swap_write(addr, old_val, new_val) \
                AO_int_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define AO_HAVE_int_fetch_compare_and_swap_write
# endif
# if !defined(AO_HAVE_int_fetch_compare_and_swap_read)
#   define AO_int_fetch_compare_and_swap_read(addr, old_val, new_val) \
                AO_int_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define AO_HAVE_int_fetch_compare_and_swap_read
# endif
#endif /* AO_HAVE_int_fetch_compare_and_swap_full */

#if !defined(AO_HAVE_int_fetch_compare_and_swap) \
    && defined(AO_HAVE_int_fetch_compare_and_swap_release)
# define AO_int_fetch_compare_and_swap(addr, old_val, new_val) \
            AO_int_fetch_compare_and_swap_release(addr, old_val, new_val)
# define AO_HAVE_int_fetch_compare_and_swap
#endif
#if !defined(AO_HAVE_int_fetch_compare_and_swap) \
    && defined(AO_HAVE_int_fetch_compare_and_swap_acquire)
# define AO_int_fetch_compare_and_swap(addr, old_val, new_val) \
            AO_int_fetch_compare_and_swap_acquire(addr, old_val, new_val)
# define AO_HAVE_int_fetch_compare_and_swap
#endif
#if !defined(AO_HAVE_int_fetch_compare_and_swap) \
    && defined(AO_HAVE_int_fetch_compare_and_swap_write)
# define AO_int_fetch_compare_and_swap(addr, old_val, new_val) \
                AO_int_fetch_compare_and_swap_write(addr, old_val, new_val)
# define AO_HAVE_int_fetch_compare_and_swap
#endif
#if !defined(AO_HAVE_int_fetch_compare_and_swap) \
    && defined(AO_HAVE_int_fetch_compare_and_swap_read)
# define AO_int_fetch_compare_and_swap(addr, old_val, new_val) \
                AO_int_fetch_compare_and_swap_read(addr, old_val, new_val)
# define AO_HAVE_int_fetch_compare_and_swap
#endif

#if defined(AO_HAVE_int_fetch_compare_and_swap_acquire) \
    && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_int_fetch_compare_and_swap_full)
# define AO_int_fetch_compare_and_swap_full(addr, old_val, new_val) \
            (AO_nop_full(), \
             AO_int_fetch_compare_and_swap_acquire(addr, old_val, new_val))
# define AO_HAVE_int_fetch_compare_and_swap_full
#endif

#if !defined(AO_HAVE_int_fetch_compare_and_swap_release_write) \
    && defined(AO_HAVE_int_fetch_compare_and_swap_write)
# define AO_int_fetch_compare_and_swap_release_write(addr,old_val,new_val) \
                AO_int_fetch_compare_and_swap_write(addr, old_val, new_val)
# define AO_HAVE_int_fetch_compare_and_swap_release_write
#endif
#if !defined(AO_HAVE_int_fetch_compare_and_swap_release_write) \
    && defined(AO_HAVE_int_fetch_compare_and_swap_release)
# define AO_int_fetch_compare_and_swap_release_write(addr,old_val,new_val) \
            AO_int_fetch_compare_and_swap_release(addr, old_val, new_val)
# define AO_HAVE_int_fetch_compare_and_swap_release_write
#endif
#if !defined(AO_HAVE_int_fetch_compare_and_swap_acquire_read) \
    && defined(AO_HAVE_int_fetch_compare_and_swap_read)
# define AO_int_fetch_compare_and_swap_acquire_read(addr,old_val,new_val) \
                AO_int_fetch_compare_and_swap_read(addr, old_val, new_val)
# define AO_HAVE_int_fetch_compare_and_swap_acquire_read
#endif
#if !defined(AO_HAVE_int_fetch_compare_and_swap_acquire_read) \
    && defined(AO_HAVE_int_fetch_compare_and_swap_acquire)
# define AO_int_fetch_compare_and_swap_acquire_read(addr,old_val,new_val) \
            AO_int_fetch_compare_and_swap_acquire(addr, old_val, new_val)
# define AO_HAVE_int_fetch_compare_and_swap_acquire_read
#endif

#ifdef AO_NO_DD_ORDERING
# if defined(AO_HAVE_int_fetch_compare_and_swap_acquire_read)
#   define AO_int_fetch_compare_and_swap_dd_acquire_read(addr,old_val,new_val) \
        AO_int_fetch_compare_and_swap_acquire_read(addr, old_val, new_val)
#   define AO_HAVE_int_fetch_compare_and_swap_dd_acquire_read
# endif
#else
# if defined(AO_HAVE_int_fetch_compare_and_swap)
#   define AO_int_fetch_compare_and_swap_dd_acquire_read(addr,old_val,new_val) \
                AO_int_fetch_compare_and_swap(addr, old_val, new_val)
#   define AO_HAVE_int_fetch_compare_and_swap_dd_acquire_read
# endif
#endif /* !AO_NO_DD_ORDERING */

/* int_compare_and_swap */
#if defined(AO_HAVE_int_compare_and_swap) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_int_compare_and_swap_acquire)
  AO_INLINE int
  AO_int_compare_and_swap_acquire(volatile unsigned *addr, unsigned old,
                                    unsigned new_val)
  {
    int result = AO_int_compare_and_swap(addr, old, new_val);
    AO_nop_full();
    return result;
  }
# define AO_HAVE_int_compare_and_swap_acquire
#endif
#if defined(AO_HAVE_int_compare_and_swap) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_int_compare_and_swap_release)
# define AO_int_compare_and_swap_release(addr, old, new_val) \
                (AO_nop_full(), AO_int_compare_and_swap(addr, old, new_val))
# define AO_HAVE_int_compare_and_swap_release
#endif
#if defined(AO_HAVE_int_compare_and_swap_full)
# if !defined(AO_HAVE_int_compare_and_swap_release)
#   define AO_int_compare_and_swap_release(addr, old, new_val) \
                AO_int_compare_and_swap_full(addr, old, new_val)
#   define AO_HAVE_int_compare_and_swap_release
# endif
# if !defined(AO_HAVE_int_compare_and_swap_acquire)
#   define AO_int_compare_and_swap_acquire(addr, old, new_val) \
                AO_int_compare_and_swap_full(addr, old, new_val)
#   define AO_HAVE_int_compare_and_swap_acquire
# endif
# if !defined(AO_HAVE_int_compare_and_swap_write)
#   define AO_int_compare_and_swap_write(addr, old, new_val) \
                AO_int_compare_and_swap_full(addr, old, new_val)
#   define AO_HAVE_int_compare_and_swap_write
# endif
# if !defined(AO_HAVE_int_compare_and_swap_read)
#   define AO_int_compare_and_swap_read(addr, old, new_val) \
                AO_int_compare_and_swap_full(addr, old, new_val)
#   define AO_HAVE_int_compare_and_swap_read
# endif
#endif /* AO_HAVE_int_compare_and_swap_full */

#if !defined(AO_HAVE_int_compare_and_swap) \
    && defined(AO_HAVE_int_compare_and_swap_release)
# define AO_int_compare_and_swap(addr, old, new_val) \
                AO_int_compare_and_swap_release(addr, old, new_val)
# define AO_HAVE_int_compare_and_swap
#endif
#if !defined(AO_HAVE_int_compare_and_swap) \
    && defined(AO_HAVE_int_compare_and_swap_acquire)
# define AO_int_compare_and_swap(addr, old, new_val) \
                AO_int_compare_and_swap_acquire(addr, old, new_val)
# define AO_HAVE_int_compare_and_swap
#endif
#if !defined(AO_HAVE_int_compare_and_swap) \
    && defined(AO_HAVE_int_compare_and_swap_write)
# define AO_int_compare_and_swap(addr, old, new_val) \
                AO_int_compare_and_swap_write(addr, old, new_val)
# define AO_HAVE_int_compare_and_swap
#endif
#if !defined(AO_HAVE_int_compare_and_swap) \
    && defined(AO_HAVE_int_compare_and_swap_read)
# define AO_int_compare_and_swap(addr, old, new_val) \
                AO_int_compare_and_swap_read(addr, old, new_val)
# define AO_HAVE_int_compare_and_swap
#endif

#if defined(AO_HAVE_int_compare_and_swap_acquire) \
    && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_int_compare_and_swap_full)
# define AO_int_compare_and_swap_full(addr, old, new_val) \
                (AO_nop_full(), \
                 AO_int_compare_and_swap_acquire(addr, old, new_val))
# define AO_HAVE_int_compare_and_swap_full
#endif

#if !defined(AO_HAVE_int_compare_and_swap_release_write) \
    && defined(AO_HAVE_int_compare_and_swap_write)
# define AO_int_compare_and_swap_release_write(addr, old, new_val) \
                AO_int_compare_and_swap_write(addr, old, new_val)
# define AO_HAVE_int_compare_and_swap_release_write
#endif
#if !defined(AO_HAVE_int_compare_and_swap_release_write) \
    && defined(AO_HAVE_int_compare_and_swap_release)
# define AO_int_compare_and_swap_release_write(addr, old, new_val) \
                AO_int_compare_and_swap_release(addr, old, new_val)
# define AO_HAVE_int_compare_and_swap_release_write
#endif
#if !defined(AO_HAVE_int_compare_and_swap_acquire_read) \
    && defined(AO_HAVE_int_compare_and_swap_read)
# define AO_int_compare_and_swap_acquire_read(addr, old, new_val) \
                AO_int_compare_and_swap_read(addr, old, new_val)
# define AO_HAVE_int_compare_and_swap_acquire_read
#endif
#if !defined(AO_HAVE_int_compare_and_swap_acquire_read) \
    && defined(AO_HAVE_int_compare_and_swap_acquire)
# define AO_int_compare_and_swap_acquire_read(addr, old, new_val) \
                AO_int_compare_and_swap_acquire(addr, old, new_val)
# define AO_HAVE_int_compare_and_swap_acquire_read
#endif

#ifdef AO_NO_DD_ORDERING
# if defined(AO_HAVE_int_compare_and_swap_acquire_read)
#   define AO_int_compare_and_swap_dd_acquire_read(addr, old, new_val) \
                AO_int_compare_and_swap_acquire_read(addr, old, new_val)
#   define AO_HAVE_int_compare_and_swap_dd_acquire_read
# endif
#else
# if defined(AO_HAVE_int_compare_and_swap)
#   define AO_int_compare_and_swap_dd_acquire_read(addr, old, new_val) \
                AO_int_compare_and_swap(addr, old, new_val)
#   define AO_HAVE_int_compare_and_swap_dd_acquire_read
# endif
#endif /* !AO_NO_DD_ORDERING */

/* int_load */
#if defined(AO_HAVE_int_load_full) && !defined(AO_HAVE_int_load_acquire)
# define AO_int_load_acquire(addr) AO_int_load_full(addr)
# define AO_HAVE_int_load_acquire
#endif

#if defined(AO_HAVE_int_load_acquire) && !defined(AO_HAVE_int_load)
# define AO_int_load(addr) AO_int_load_acquire(addr)
# define AO_HAVE_int_load
#endif

#if defined(AO_HAVE_int_load_full) && !defined(AO_HAVE_int_load_read)
# define AO_int_load_read(addr) AO_int_load_full(addr)
# define AO_HAVE_int_load_read
#endif

#if !defined(AO_HAVE_int_load_acquire_read) \
    && defined(AO_HAVE_int_load_acquire)
# define AO_int_load_acquire_read(addr) AO_int_load_acquire(addr)
# define AO_HAVE_int_load_acquire_read
#endif

#if defined(AO_HAVE_int_load) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_int_load_acquire)
  AO_INLINE unsigned
  AO_int_load_acquire(const volatile unsigned *addr)
  {
    unsigned result = AO_int_load(addr);

    /* Acquire barrier would be useless, since the load could be delayed    */
    /* beyond it.                                                           */
    AO_nop_full();
    return result;
  }
# define AO_HAVE_int_load_acquire
#endif

#if defined(AO_HAVE_int_load) && defined(AO_HAVE_nop_read) \
    && !defined(AO_HAVE_int_load_read)
  AO_INLINE unsigned
  AO_int_load_read(const volatile unsigned *addr)
  {
    unsigned result = AO_int_load(addr);

    AO_nop_read();
    return result;
  }
# define AO_HAVE_int_load_read
#endif

#if defined(AO_HAVE_int_load_acquire) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_int_load_full)
# define AO_int_load_full(addr) (AO_nop_full(), AO_int_load_acquire(addr))
# define AO_HAVE_int_load_full
#endif

#if defined(AO_HAVE_int_compare_and_swap_read) \
    && !defined(AO_HAVE_int_load_read)
# define AO_int_CAS_BASED_LOAD_READ
  AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE unsigned
  AO_int_load_read(const volatile unsigned *addr)
  {
    unsigned result;

    do {
      result = *(const unsigned *)addr;
    } while (AO_EXPECT_FALSE(!AO_int_compare_and_swap_read(
                                                (volatile unsigned *)addr,
                                                result, result)));
    return result;
  }
# define AO_HAVE_int_load_read
#endif

#if !defined(AO_HAVE_int_load_acquire_read) \
    && defined(AO_HAVE_int_load_read)
# define AO_int_load_acquire_read(addr) AO_int_load_read(addr)
# define AO_HAVE_int_load_acquire_read
#endif

#if defined(AO_HAVE_int_load_acquire_read) && !defined(AO_HAVE_int_load) \
    && (!defined(AO_int_CAS_BASED_LOAD_READ) \
        || !defined(AO_HAVE_int_compare_and_swap))
# define AO_int_load(addr) AO_int_load_acquire_read(addr)
# define AO_HAVE_int_load
#endif

#if defined(AO_HAVE_int_compare_and_swap_full) \
    && !defined(AO_HAVE_int_load_full)
  AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE unsigned
  AO_int_load_full(const volatile unsigned *addr)
  {
    unsigned result;

    do {
      result = *(const unsigned *)addr;
    } while (AO_EXPECT_FALSE(!AO_int_compare_and_swap_full(
                                                (volatile unsigned *)addr,
                                                result, result)));
    return result;
  }
# define AO_HAVE_int_load_full
#endif

#if defined(AO_HAVE_int_compare_and_swap_acquire) \
    && !defined(AO_HAVE_int_load_acquire)
  AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE unsigned
  AO_int_load_acquire(const volatile unsigned *addr)
  {
    unsigned result;

    do {
      result = *(const unsigned *)addr;
    } while (AO_EXPECT_FALSE(!AO_int_compare_and_swap_acquire(
                                                (volatile unsigned *)addr,
                                                result, result)));
    return result;
  }
# define AO_HAVE_int_load_acquire
#endif

#if defined(AO_HAVE_int_compare_and_swap) && !defined(AO_HAVE_int_load)
  AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE unsigned
  AO_int_load(const volatile unsigned *addr)
  {
    unsigned result;

    do {
      result = *(const unsigned *)addr;
    } while (AO_EXPECT_FALSE(!AO_int_compare_and_swap(
                                                (volatile unsigned *)addr,
                                                result, result)));
    return result;
  }
# define AO_HAVE_int_load
#endif

#ifdef AO_NO_DD_ORDERING
# if defined(AO_HAVE_int_load_acquire_read)
#   define AO_int_load_dd_acquire_read(addr) \
                                AO_int_load_acquire_read(addr)
#   define AO_HAVE_int_load_dd_acquire_read
# endif
#else
# if defined(AO_HAVE_int_load)
#   define AO_int_load_dd_acquire_read(addr) AO_int_load(addr)
#   define AO_HAVE_int_load_dd_acquire_read
# endif
#endif /* !AO_NO_DD_ORDERING */

/* int_store */
#if defined(AO_HAVE_int_store_full) && !defined(AO_HAVE_int_store_release)
# define AO_int_store_release(addr, val) AO_int_store_full(addr, val)
# define AO_HAVE_int_store_release
#endif

#if defined(AO_HAVE_int_store_release) && !defined(AO_HAVE_int_store)
# define AO_int_store(addr, val) AO_int_store_release(addr, val)
# define AO_HAVE_int_store
#endif

#if defined(AO_HAVE_int_store_full) && !defined(AO_HAVE_int_store_write)
# define AO_int_store_write(addr, val) AO_int_store_full(addr, val)
# define AO_HAVE_int_store_write
#endif

#if defined(AO_HAVE_int_store_release) \
    && !defined(AO_HAVE_int_store_release_write)
# define AO_int_store_release_write(addr, val) \
                                AO_int_store_release(addr, val)
# define AO_HAVE_int_store_release_write
#endif

#if defined(AO_HAVE_int_store_write) && !defined(AO_HAVE_int_store)
# define AO_int_store(addr, val) AO_int_store_write(addr, val)
# define AO_HAVE_int_store
#endif

#if defined(AO_HAVE_int_store) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_int_store_release)
# define AO_int_store_release(addr, val) \
                                (AO_nop_full(), AO_int_store(addr, val))
# define AO_HAVE_int_store_release
#endif

#if defined(AO_HAVE_int_store) && defined(AO_HAVE_nop_write) \
    && !defined(AO_HAVE_int_store_write)
# define AO_int_store_write(addr, val) \
                                (AO_nop_write(), AO_int_store(addr, val))
# define AO_HAVE_int_store_write
#endif

#if defined(AO_HAVE_int_compare_and_swap_write) \
    && !defined(AO_HAVE_int_store_write)
  AO_ATTR_NO_SANITIZE_MEMORY AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE void
  AO_int_store_write(volatile unsigned *addr, unsigned new_val)
  {
    unsigned old_val;

    do {
      old_val = *(unsigned *)addr;
    } while (AO_EXPECT_FALSE(!AO_int_compare_and_swap_write(addr, old_val,
                                                              new_val)));
  }
# define AO_HAVE_int_store_write
#endif

#if defined(AO_HAVE_int_store_write) \
    && !defined(AO_HAVE_int_store_release_write)
# define AO_int_store_release_write(addr, val) \
                                AO_int_store_write(addr, val)
# define AO_HAVE_int_store_release_write
#endif

#if defined(AO_HAVE_int_store_release) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_int_store_full)
# define AO_int_store_full(addr, val) \
                                (AO_int_store_release(addr, val), \
                                 AO_nop_full())
# define AO_HAVE_int_store_full
#endif

#if defined(AO_HAVE_int_compare_and_swap) && !defined(AO_HAVE_int_store)
  AO_ATTR_NO_SANITIZE_MEMORY AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE void
  AO_int_store(volatile unsigned *addr, unsigned new_val)
  {
    unsigned old_val;

    do {
      old_val = *(unsigned *)addr;
    } while (AO_EXPECT_FALSE(!AO_int_compare_and_swap(addr,
                                                        old_val, new_val)));
  }
# define AO_HAVE_int_store
#endif

#if defined(AO_HAVE_int_compare_and_swap_release) \
    && !defined(AO_HAVE_int_store_release)
  AO_ATTR_NO_SANITIZE_MEMORY AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE void
  AO_int_store_release(volatile unsigned *addr, unsigned new_val)
  {
    unsigned old_val;

    do {
      old_val = *(unsigned *)addr;
    } while (AO_EXPECT_FALSE(!AO_int_compare_and_swap_release(addr, old_val,
                                                                new_val)));
  }
# define AO_HAVE_int_store_release
#endif

#if defined(AO_HAVE_int_compare_and_swap_full) \
    && !defined(AO_HAVE_int_store_full)
  AO_ATTR_NO_SANITIZE_MEMORY AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE void
  AO_int_store_full(volatile unsigned *addr, unsigned new_val)
  {
    unsigned old_val;

    do {
      old_val = *(unsigned *)addr;
    } while (AO_EXPECT_FALSE(!AO_int_compare_and_swap_full(addr, old_val,
                                                             new_val)));
  }
# define AO_HAVE_int_store_full
#endif
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

/* fetch_compare_and_swap */
#if defined(AO_HAVE_fetch_compare_and_swap) \
    && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_fetch_compare_and_swap_acquire)
  AO_INLINE AO_t
  AO_fetch_compare_and_swap_acquire(volatile AO_t *addr,
                                          AO_t old_val, AO_t new_val)
  {
    AO_t result = AO_fetch_compare_and_swap(addr, old_val, new_val);
    AO_nop_full();
    return result;
  }
# define AO_HAVE_fetch_compare_and_swap_acquire
#endif
#if defined(AO_HAVE_fetch_compare_and_swap) \
    && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_fetch_compare_and_swap_release)
# define AO_fetch_compare_and_swap_release(addr, old_val, new_val) \
                (AO_nop_full(), \
                 AO_fetch_compare_and_swap(addr, old_val, new_val))
# define AO_HAVE_fetch_compare_and_swap_release
#endif
#if defined(AO_HAVE_fetch_compare_and_swap_full)
# if !defined(AO_HAVE_fetch_compare_and_swap_release)
#   define AO_fetch_compare_and_swap_release(addr, old_val, new_val) \
                AO_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define AO_HAVE_fetch_compare_and_swap_release
# endif
# if !defined(AO_HAVE_fetch_compare_and_swap_acquire)
#   define AO_fetch_compare_and_swap_acquire(addr, old_val, new_val) \
                AO_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define AO_HAVE_fetch_compare_and_swap_acquire
# endif
# if !defined(AO_HAVE_fetch_compare_and_swap_write)
#   define AO_fetch_compare_and_swap_write(addr, old_val, new_val) \
                AO_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define AO_HAVE_fetch_compare_and_swap_write
# endif
# if !defined(AO_HAVE_fetch_compare_and_swap_read)
#   define AO_fetch_compare_and_swap_read(addr, old_val, new_val) \
                AO_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define AO_HAVE_fetch_compare_and_swap_read
# endif
#endif /* AO_HAVE_fetch_compare_and_swap_full */

#if !defined(AO_HAVE_fetch_compare_and_swap) \
    && defined(AO_HAVE_fetch_compare_and_swap_release)
# define AO_fetch_compare_and_swap(addr, old_val, new_val) \
            AO_fetch_compare_and_swap_release(addr, old_val, new_val)
# define AO_HAVE_fetch_compare_and_swap
#endif
#if !defined(AO_HAVE_fetch_compare_and_swap) \
    && defined(AO_HAVE_fetch_compare_and_swap_acquire)
# define AO_fetch_compare_and_swap(addr, old_val, new_val) \
            AO_fetch_compare_and_swap_acquire(addr, old_val, new_val)
# define AO_HAVE_fetch_compare_and_swap
#endif
#if !defined(AO_HAVE_fetch_compare_and_swap) \
    && defined(AO_HAVE_fetch_compare_and_swap_write)
# define AO_fetch_compare_and_swap(addr, old_val, new_val) \
                AO_fetch_compare_and_swap_write(addr, old_val, new_val)
# define AO_HAVE_fetch_compare_and_swap
#endif
#if !defined(AO_HAVE_fetch_compare_and_swap) \
    && defined(AO_HAVE_fetch_compare_and_swap_read)
# define AO_fetch_compare_and_swap(addr, old_val, new_val) \
                AO_fetch_compare_and_swap_read(addr, old_val, new_val)
# define AO_HAVE_fetch_compare_and_swap
#endif

#if defined(AO_HAVE_fetch_compare_and_swap_acquire) \
    && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_fetch_compare_and_swap_full)
# define AO_fetch_compare_and_swap_full(addr, old_val, new_val) \
            (AO_nop_full(), \
             AO_fetch_compare_and_swap_acquire(addr, old_val, new_val))
# define AO_HAVE_fetch_compare_and_swap_full
#endif

#if !defined(AO_HAVE_fetch_compare_and_swap_release_write) \
    && defined(AO_HAVE_fetch_compare_and_swap_write)
# define AO_fetch_compare_and_swap_release_write(addr,old_val,new_val) \
                AO_fetch_compare_and_swap_write(addr, old_val, new_val)
# define AO_HAVE_fetch_compare_and_swap_release_write
#endif
#if !defined(AO_HAVE_fetch_compare_and_swap_release_write) \
    && defined(AO_HAVE_fetch_compare_and_swap_release)
# define AO_fetch_compare_and_swap_release_write(addr,old_val,new_val) \
            AO_fetch_compare_and_swap_release(addr, old_val, new_val)
# define AO_HAVE_fetch_compare_and_swap_release_write
#endif
#if !defined(AO_HAVE_fetch_compare_and_swap_acquire_read) \
    && defined(AO_HAVE_fetch_compare_and_swap_read)
# define AO_fetch_compare_and_swap_acquire_read(addr,old_val,new_val) \
                AO_fetch_compare_and_swap_read(addr, old_val, new_val)
# define AO_HAVE_fetch_compare_and_swap_acquire_read
#endif
#if !defined(AO_HAVE_fetch_compare_and_swap_acquire_read) \
    && defined(AO_HAVE_fetch_compare_and_swap_acquire)
# define AO_fetch_compare_and_swap_acquire_read(addr,old_val,new_val) \
            AO_fetch_compare_and_swap_acquire(addr, old_val, new_val)
# define AO_HAVE_fetch_compare_and_swap_acquire_read
#endif

#ifdef AO_NO_DD_ORDERING
# if defined(AO_HAVE_fetch_compare_and_swap_acquire_read)
#   define AO_fetch_compare_and_swap_dd_acquire_read(addr,old_val,new_val) \
        AO_fetch_compare_and_swap_acquire_read(addr, old_val, new_val)
#   define AO_HAVE_fetch_compare_and_swap_dd_acquire_read
# endif
#else
# if defined(AO_HAVE_fetch_compare_and_swap)
#   define AO_fetch_compare_and_swap_dd_acquire_read(addr,old_val,new_val) \
                AO_fetch_compare_and_swap(addr, old_val, new_val)
#   define AO_HAVE_fetch_compare_and_swap_dd_acquire_read
# endif
#endif /* !AO_NO_DD_ORDERING */

/* compare_and_swap */
#if defined(AO_HAVE_compare_and_swap) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_compare_and_swap_acquire)
  AO_INLINE int
  AO_compare_and_swap_acquire(volatile AO_t *addr, AO_t old,
                                    AO_t new_val)
  {
    int result = AO_compare_and_swap(addr, old, new_val);
    AO_nop_full();
    return result;
  }
# define AO_HAVE_compare_and_swap_acquire
#endif
#if defined(AO_HAVE_compare_and_swap) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_compare_and_swap_release)
# define AO_compare_and_swap_release(addr, old, new_val) \
                (AO_nop_full(), AO_compare_and_swap(addr, old, new_val))
# define AO_HAVE_compare_and_swap_release
#endif
#if defined(AO_HAVE_compare_and_swap_full)
# if !defined(AO_HAVE_compare_and_swap_release)
#   define AO_compare_and_swap_release(addr, old, new_val) \
                AO_compare_and_swap_full(addr, old, new_val)
#   define AO_HAVE_compare_and_swap_release
# endif
# if !defined(AO_HAVE_compare_and_swap_acquire)
#   define AO_compare_and_swap_acquire(addr, old, new_val) \
                AO_compare_and_swap_full(addr, old, new_val)
#   define AO_HAVE_compare_and_swap_acquire
# endif
# if !defined(AO_HAVE_compare_and_swap_write)
#   define AO_compare_and_swap_write(addr, old, new_val) \
                AO_compare_and_swap_full(addr, old, new_val)
#   define AO_HAVE_compare_and_swap_write
# endif
# if !defined(AO_HAVE_compare_and_swap_read)
#   define AO_compare_and_swap_read(addr, old, new_val) \
                AO_compare_and_swap_full(addr, old, new_val)
#   define AO_HAVE_compare_and_swap_read
# endif
#endif /* AO_HAVE_compare_and_swap_full */

#if !defined(AO_HAVE_compare_and_swap) \
    && defined(AO_HAVE_compare_and_swap_release)
# define AO_compare_and_swap(addr, old, new_val) \
                AO_compare_and_swap_release(addr, old, new_val)
# define AO_HAVE_compare_and_swap
#endif
#if !defined(AO_HAVE_compare_and_swap) \
    && defined(AO_HAVE_compare_and_swap_acquire)
# define AO_compare_and_swap(addr, old, new_val) \
                AO_compare_and_swap_acquire(addr, old, new_val)
# define AO_HAVE_compare_and_swap
#endif
#if !defined(AO_HAVE_compare_and_swap) \
    && defined(AO_HAVE_compare_and_swap_write)
# define AO_compare_and_swap(addr, old, new_val) \
                AO_compare_and_swap_write(addr, old, new_val)
# define AO_HAVE_compare_and_swap
#endif
#if !defined(AO_HAVE_compare_and_swap) \
    && defined(AO_HAVE_compare_and_swap_read)
# define AO_compare_and_swap(addr, old, new_val) \
                AO_compare_and_swap_read(addr, old, new_val)
# define AO_HAVE_compare_and_swap
#endif

#if defined(AO_HAVE_compare_and_swap_acquire) \
    && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_compare_and_swap_full)
# define AO_compare_and_swap_full(addr, old, new_val) \
                (AO_nop_full(), \
                 AO_compare_and_swap_acquire(addr, old, new_val))
# define AO_HAVE_compare_and_swap_full
#endif

#if !defined(AO_HAVE_compare_and_swap_release_write) \
    && defined(AO_HAVE_compare_and_swap_write)
# define AO_compare_and_swap_release_write(addr, old, new_val) \
                AO_compare_and_swap_write(addr, old, new_val)
# define AO_HAVE_compare_and_swap_release_write
#endif
#if !defined(AO_HAVE_compare_and_swap_release_write) \
    && defined(AO_HAVE_compare_and_swap_release)
# define AO_compare_and_swap_release_write(addr, old, new_val) \
                AO_compare_and_swap_release(addr, old, new_val)
# define AO_HAVE_compare_and_swap_release_write
#endif
#if !defined(AO_HAVE_compare_and_swap_acquire_read) \
    && defined(AO_HAVE_compare_and_swap_read)
# define AO_compare_and_swap_acquire_read(addr, old, new_val) \
                AO_compare_and_swap_read(addr, old, new_val)
# define AO_HAVE_compare_and_swap_acquire_read
#endif
#if !defined(AO_HAVE_compare_and_swap_acquire_read) \
    && defined(AO_HAVE_compare_and_swap_acquire)
# define AO_compare_and_swap_acquire_read(addr, old, new_val) \
                AO_compare_and_swap_acquire(addr, old, new_val)
# define AO_HAVE_compare_and_swap_acquire_read
#endif

#ifdef AO_NO_DD_ORDERING
# if defined(AO_HAVE_compare_and_swap_acquire_read)
#   define AO_compare_and_swap_dd_acquire_read(addr, old, new_val) \
                AO_compare_and_swap_acquire_read(addr, old, new_val)
#   define AO_HAVE_compare_and_swap_dd_acquire_read
# endif
#else
# if defined(AO_HAVE_compare_and_swap)
#   define AO_compare_and_swap_dd_acquire_read(addr, old, new_val) \
                AO_compare_and_swap(addr, old, new_val)
#   define AO_HAVE_compare_and_swap_dd_acquire_read
# endif
#endif /* !AO_NO_DD_ORDERING */

/* load */
#if defined(AO_HAVE_load_full) && !defined(AO_HAVE_load_acquire)
# define AO_load_acquire(addr) AO_load_full(addr)
# define AO_HAVE_load_acquire
#endif

#if defined(AO_HAVE_load_acquire) && !defined(AO_HAVE_load)
# define AO_load(addr) AO_load_acquire(addr)
# define AO_HAVE_load
#endif

#if defined(AO_HAVE_load_full) && !defined(AO_HAVE_load_read)
# define AO_load_read(addr) AO_load_full(addr)
# define AO_HAVE_load_read
#endif

#if !defined(AO_HAVE_load_acquire_read) \
    && defined(AO_HAVE_load_acquire)
# define AO_load_acquire_read(addr) AO_load_acquire(addr)
# define AO_HAVE_load_acquire_read
#endif

#if defined(AO_HAVE_load) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_load_acquire)
  AO_INLINE AO_t
  AO_load_acquire(const volatile AO_t *addr)
  {
    AO_t result = AO_load(addr);

    /* Acquire barrier would be useless, since the load could be delayed    */
    /* beyond it.                                                           */
    AO_nop_full();
    return result;
  }
# define AO_HAVE_load_acquire
#endif

#if defined(AO_HAVE_load) && defined(AO_HAVE_nop_read) \
    && !defined(AO_HAVE_load_read)
  AO_INLINE AO_t
  AO_load_read(const volatile AO_t *addr)
  {
    AO_t result = AO_load(addr);

    AO_nop_read();
    return result;
  }
# define AO_HAVE_load_read
#endif

#if defined(AO_HAVE_load_acquire) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_load_full)
# define AO_load_full(addr) (AO_nop_full(), AO_load_acquire(addr))
# define AO_HAVE_load_full
#endif

#if defined(AO_HAVE_compare_and_swap_read) \
    && !defined(AO_HAVE_load_read)
# define AO_CAS_BASED_LOAD_READ
  AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE AO_t
  AO_load_read(const volatile AO_t *addr)
  {
    AO_t result;

    do {
      result = *(const AO_t *)addr;
    } while (AO_EXPECT_FALSE(!AO_compare_and_swap_read(
                                                (volatile AO_t *)addr,
                                                result, result)));
    return result;
  }
# define AO_HAVE_load_read
#endif

#if !defined(AO_HAVE_load_acquire_read) \
    && defined(AO_HAVE_load_read)
# define AO_load_acquire_read(addr) AO_load_read(addr)
# define AO_HAVE_load_acquire_read
#endif

#if defined(AO_HAVE_load_acquire_read) && !defined(AO_HAVE_load) \
    && (!defined(AO_CAS_BASED_LOAD_READ) \
        || !defined(AO_HAVE_compare_and_swap))
# define AO_load(addr) AO_load_acquire_read(addr)
# define AO_HAVE_load
#endif

#if defined(AO_HAVE_compare_and_swap_full) \
    && !defined(AO_HAVE_load_full)
  AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE AO_t
  AO_load_full(const volatile AO_t *addr)
  {
    AO_t result;

    do {
      result = *(const AO_t *)addr;
    } while (AO_EXPECT_FALSE(!AO_compare_and_swap_full(
                                                (volatile AO_t *)addr,
                                                result, result)));
    return result;
  }
# define AO_HAVE_load_full
#endif

#if defined(AO_HAVE_compare_and_swap_acquire) \
    && !defined(AO_HAVE_load_acquire)
  AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE AO_t
  AO_load_acquire(const volatile AO_t *addr)
  {
    AO_t result;

    do {
      result = *(const AO_t *)addr;
    } while (AO_EXPECT_FALSE(!AO_compare_and_swap_acquire(
                                                (volatile AO_t *)addr,
                                                result, result)));
    return result;
  }
# define AO_HAVE_load_acquire
#endif

#if defined(AO_HAVE_compare_and_swap) && !defined(AO_HAVE_load)
  AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE AO_t
  AO_load(const volatile AO_t *addr)
  {
    AO_t result;

    do {
      result = *(const AO_t *)addr;
    } while (AO_EXPECT_FALSE(!AO_compare_and_swap(
                                                (volatile AO_t *)addr,
                                                result, result)));
    return result;
  }
# define AO_HAVE_load
#endif

#ifdef AO_NO_DD_ORDERING
# if defined(AO_HAVE_load_acquire_read)
#   define AO_load_dd_acquire_read(addr) \
                                AO_load_acquire_read(addr)
#   define AO_HAVE_load_dd_acquire_read
# endif
#else
# if defined(AO_HAVE_load)
#   define AO_load_dd_acquire_read(addr) AO_load(addr)
#   define AO_HAVE_load_dd_acquire_read
# endif
#endif /* !AO_NO_DD_ORDERING */

/* store */
#if defined(AO_HAVE_store_full) && !defined(AO_HAVE_store_release)
# define AO_store_release(addr, val) AO_store_full(addr, val)
# define AO_HAVE_store_release
#endif

#if defined(AO_HAVE_store_release) && !defined(AO_HAVE_store)
# define AO_store(addr, val) AO_store_release(addr, val)
# define AO_HAVE_store
#endif

#if defined(AO_HAVE_store_full) && !defined(AO_HAVE_store_write)
# define AO_store_write(addr, val) AO_store_full(addr, val)
# define AO_HAVE_store_write
#endif

#if defined(AO_HAVE_store_release) \
    && !defined(AO_HAVE_store_release_write)
# define AO_store_release_write(addr, val) \
                                AO_store_release(addr, val)
# define AO_HAVE_store_release_write
#endif

#if defined(AO_HAVE_store_write) && !defined(AO_HAVE_store)
# define AO_store(addr, val) AO_store_write(addr, val)
# define AO_HAVE_store
#endif

#if defined(AO_HAVE_store) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_store_release)
# define AO_store_release(addr, val) \
                                (AO_nop_full(), AO_store(addr, val))
# define AO_HAVE_store_release
#endif

#if defined(AO_HAVE_store) && defined(AO_HAVE_nop_write) \
    && !defined(AO_HAVE_store_write)
# define AO_store_write(addr, val) \
                                (AO_nop_write(), AO_store(addr, val))
# define AO_HAVE_store_write
#endif

#if defined(AO_HAVE_compare_and_swap_write) \
    && !defined(AO_HAVE_store_write)
  AO_ATTR_NO_SANITIZE_MEMORY AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE void
  AO_store_write(volatile AO_t *addr, AO_t new_val)
  {
    AO_t old_val;

    do {
      old_val = *(AO_t *)addr;
    } while (AO_EXPECT_FALSE(!AO_compare_and_swap_write(addr, old_val,
                                                              new_val)));
  }
# define AO_HAVE_store_write
#endif

#if defined(AO_HAVE_store_write) \
    && !defined(AO_HAVE_store_release_write)
# define AO_store_release_write(addr, val) \
                                AO_store_write(addr, val)
# define AO_HAVE_store_release_write
#endif

#if defined(AO_HAVE_store_release) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_store_full)
# define AO_store_full(addr, val) \
                                (AO_store_release(addr, val), \
                                 AO_nop_full())
# define AO_HAVE_store_full
#endif

#if defined(AO_HAVE_compare_and_swap) && !defined(AO_HAVE_store)
  AO_ATTR_NO_SANITIZE_MEMORY AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE void
  AO_store(volatile AO_t *addr, AO_t new_val)
  {
    AO_t old_val;

    do {
      old_val = *(AO_t *)addr;
    } while (AO_EXPECT_FALSE(!AO_compare_and_swap(addr,
                                                        old_val, new_val)));
  }
# define AO_HAVE_store
#endif

#if defined(AO_HAVE_compare_and_swap_release) \
    && !defined(AO_HAVE_store_release)
  AO_ATTR_NO_SANITIZE_MEMORY AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE void
  AO_store_release(volatile AO_t *addr, AO_t new_val)
  {
    AO_t old_val;

    do {
      old_val = *(AO_t *)addr;
    } while (AO_EXPECT_FALSE(!AO_compare_and_swap_release(addr, old_val,
                                                                new_val)));
  }
# define AO_HAVE_store_release
#endif

#if defined(AO_HAVE_compare_and_swap_full) \
    && !defined(AO_HAVE_store_full)
  AO_ATTR_NO_SANITIZE_MEMORY AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE void
  AO_store_full(volatile AO_t *addr, AO_t new_val)
  {
    AO_t old_val;

    do {
      old_val = *(AO_t *)addr;
    } while (AO_EXPECT_FALSE(!AO_compare_and_swap_full(addr, old_val,
                                                             new_val)));
  }
# define AO_HAVE_store_full
#endif
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

/* double_fetch_compare_and_swap */
#if defined(AO_HAVE_double_fetch_compare_and_swap) \
    && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_double_fetch_compare_and_swap_acquire)
  AO_INLINE AO_double_t
  AO_double_fetch_compare_and_swap_acquire(volatile AO_double_t *addr,
                                          AO_double_t old_val, AO_double_t new_val)
  {
    AO_double_t result = AO_double_fetch_compare_and_swap(addr, old_val, new_val);
    AO_nop_full();
    return result;
  }
# define AO_HAVE_double_fetch_compare_and_swap_acquire
#endif
#if defined(AO_HAVE_double_fetch_compare_and_swap) \
    && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_double_fetch_compare_and_swap_release)
# define AO_double_fetch_compare_and_swap_release(addr, old_val, new_val) \
                (AO_nop_full(), \
                 AO_double_fetch_compare_and_swap(addr, old_val, new_val))
# define AO_HAVE_double_fetch_compare_and_swap_release
#endif
#if defined(AO_HAVE_double_fetch_compare_and_swap_full)
# if !defined(AO_HAVE_double_fetch_compare_and_swap_release)
#   define AO_double_fetch_compare_and_swap_release(addr, old_val, new_val) \
                AO_double_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define AO_HAVE_double_fetch_compare_and_swap_release
# endif
# if !defined(AO_HAVE_double_fetch_compare_and_swap_acquire)
#   define AO_double_fetch_compare_and_swap_acquire(addr, old_val, new_val) \
                AO_double_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define AO_HAVE_double_fetch_compare_and_swap_acquire
# endif
# if !defined(AO_HAVE_double_fetch_compare_and_swap_write)
#   define AO_double_fetch_compare_and_swap_write(addr, old_val, new_val) \
                AO_double_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define AO_HAVE_double_fetch_compare_and_swap_write
# endif
# if !defined(AO_HAVE_double_fetch_compare_and_swap_read)
#   define AO_double_fetch_compare_and_swap_read(addr, old_val, new_val) \
                AO_double_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define AO_HAVE_double_fetch_compare_and_swap_read
# endif
#endif /* AO_HAVE_double_fetch_compare_and_swap_full */

#if !defined(AO_HAVE_double_fetch_compare_and_swap) \
    && defined(AO_HAVE_double_fetch_compare_and_swap_release)
# define AO_double_fetch_compare_and_swap(addr, old_val, new_val) \
            AO_double_fetch_compare_and_swap_release(addr, old_val, new_val)
# define AO_HAVE_double_fetch_compare_and_swap
#endif
#if !defined(AO_HAVE_double_fetch_compare_and_swap) \
    && defined(AO_HAVE_double_fetch_compare_and_swap_acquire)
# define AO_double_fetch_compare_and_swap(addr, old_val, new_val) \
            AO_double_fetch_compare_and_swap_acquire(addr, old_val, new_val)
# define AO_HAVE_double_fetch_compare_and_swap
#endif
#if !defined(AO_HAVE_double_fetch_compare_and_swap) \
    && defined(AO_HAVE_double_fetch_compare_and_swap_write)
# define AO_double_fetch_compare_and_swap(addr, old_val, new_val) \
                AO_double_fetch_compare_and_swap_write(addr, old_val, new_val)
# define AO_HAVE_double_fetch_compare_and_swap
#endif
#if !defined(AO_HAVE_double_fetch_compare_and_swap) \
    && defined(AO_HAVE_double_fetch_compare_and_swap_read)
# define AO_double_fetch_compare_and_swap(addr, old_val, new_val) \
                AO_double_fetch_compare_and_swap_read(addr, old_val, new_val)
# define AO_HAVE_double_fetch_compare_and_swap
#endif

#if defined(AO_HAVE_double_fetch_compare_and_swap_acquire) \
    && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_double_fetch_compare_and_swap_full)
# define AO_double_fetch_compare_and_swap_full(addr, old_val, new_val) \
            (AO_nop_full(), \
             AO_double_fetch_compare_and_swap_acquire(addr, old_val, new_val))
# define AO_HAVE_double_fetch_compare_and_swap_full
#endif

#if !defined(AO_HAVE_double_fetch_compare_and_swap_release_write) \
    && defined(AO_HAVE_double_fetch_compare_and_swap_write)
# define AO_double_fetch_compare_and_swap_release_write(addr,old_val,new_val) \
                AO_double_fetch_compare_and_swap_write(addr, old_val, new_val)
# define AO_HAVE_double_fetch_compare_and_swap_release_write
#endif
#if !defined(AO_HAVE_double_fetch_compare_and_swap_release_write) \
    && defined(AO_HAVE_double_fetch_compare_and_swap_release)
# define AO_double_fetch_compare_and_swap_release_write(addr,old_val,new_val) \
            AO_double_fetch_compare_and_swap_release(addr, old_val, new_val)
# define AO_HAVE_double_fetch_compare_and_swap_release_write
#endif
#if !defined(AO_HAVE_double_fetch_compare_and_swap_acquire_read) \
    && defined(AO_HAVE_double_fetch_compare_and_swap_read)
# define AO_double_fetch_compare_and_swap_acquire_read(addr,old_val,new_val) \
                AO_double_fetch_compare_and_swap_read(addr, old_val, new_val)
# define AO_HAVE_double_fetch_compare_and_swap_acquire_read
#endif
#if !defined(AO_HAVE_double_fetch_compare_and_swap_acquire_read) \
    && defined(AO_HAVE_double_fetch_compare_and_swap_acquire)
# define AO_double_fetch_compare_and_swap_acquire_read(addr,old_val,new_val) \
            AO_double_fetch_compare_and_swap_acquire(addr, old_val, new_val)
# define AO_HAVE_double_fetch_compare_and_swap_acquire_read
#endif

#ifdef AO_NO_DD_ORDERING
# if defined(AO_HAVE_double_fetch_compare_and_swap_acquire_read)
#   define AO_double_fetch_compare_and_swap_dd_acquire_read(addr,old_val,new_val) \
        AO_double_fetch_compare_and_swap_acquire_read(addr, old_val, new_val)
#   define AO_HAVE_double_fetch_compare_and_swap_dd_acquire_read
# endif
#else
# if defined(AO_HAVE_double_fetch_compare_and_swap)
#   define AO_double_fetch_compare_and_swap_dd_acquire_read(addr,old_val,new_val) \
                AO_double_fetch_compare_and_swap(addr, old_val, new_val)
#   define AO_HAVE_double_fetch_compare_and_swap_dd_acquire_read
# endif
#endif /* !AO_NO_DD_ORDERING */

/* double_compare_and_swap */
#if defined(AO_HAVE_double_compare_and_swap) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_double_compare_and_swap_acquire)
  AO_INLINE int
  AO_double_compare_and_swap_acquire(volatile AO_double_t *addr, AO_double_t old,
                                    AO_double_t new_val)
  {
    int result = AO_double_compare_and_swap(addr, old, new_val);
    AO_nop_full();
    return result;
  }
# define AO_HAVE_double_compare_and_swap_acquire
#endif
#if defined(AO_HAVE_double_compare_and_swap) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_double_compare_and_swap_release)
# define AO_double_compare_and_swap_release(addr, old, new_val) \
                (AO_nop_full(), AO_double_compare_and_swap(addr, old, new_val))
# define AO_HAVE_double_compare_and_swap_release
#endif
#if defined(AO_HAVE_double_compare_and_swap_full)
# if !defined(AO_HAVE_double_compare_and_swap_release)
#   define AO_double_compare_and_swap_release(addr, old, new_val) \
                AO_double_compare_and_swap_full(addr, old, new_val)
#   define AO_HAVE_double_compare_and_swap_release
# endif
# if !defined(AO_HAVE_double_compare_and_swap_acquire)
#   define AO_double_compare_and_swap_acquire(addr, old, new_val) \
                AO_double_compare_and_swap_full(addr, old, new_val)
#   define AO_HAVE_double_compare_and_swap_acquire
# endif
# if !defined(AO_HAVE_double_compare_and_swap_write)
#   define AO_double_compare_and_swap_write(addr, old, new_val) \
                AO_double_compare_and_swap_full(addr, old, new_val)
#   define AO_HAVE_double_compare_and_swap_write
# endif
# if !defined(AO_HAVE_double_compare_and_swap_read)
#   define AO_double_compare_and_swap_read(addr, old, new_val) \
                AO_double_compare_and_swap_full(addr, old, new_val)
#   define AO_HAVE_double_compare_and_swap_read
# endif
#endif /* AO_HAVE_double_compare_and_swap_full */

#if !defined(AO_HAVE_double_compare_and_swap) \
    && defined(AO_HAVE_double_compare_and_swap_release)
# define AO_double_compare_and_swap(addr, old, new_val) \
                AO_double_compare_and_swap_release(addr, old, new_val)
# define AO_HAVE_double_compare_and_swap
#endif
#if !defined(AO_HAVE_double_compare_and_swap) \
    && defined(AO_HAVE_double_compare_and_swap_acquire)
# define AO_double_compare_and_swap(addr, old, new_val) \
                AO_double_compare_and_swap_acquire(addr, old, new_val)
# define AO_HAVE_double_compare_and_swap
#endif
#if !defined(AO_HAVE_double_compare_and_swap) \
    && defined(AO_HAVE_double_compare_and_swap_write)
# define AO_double_compare_and_swap(addr, old, new_val) \
                AO_double_compare_and_swap_write(addr, old, new_val)
# define AO_HAVE_double_compare_and_swap
#endif
#if !defined(AO_HAVE_double_compare_and_swap) \
    && defined(AO_HAVE_double_compare_and_swap_read)
# define AO_double_compare_and_swap(addr, old, new_val) \
                AO_double_compare_and_swap_read(addr, old, new_val)
# define AO_HAVE_double_compare_and_swap
#endif

#if defined(AO_HAVE_double_compare_and_swap_acquire) \
    && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_double_compare_and_swap_full)
# define AO_double_compare_and_swap_full(addr, old, new_val) \
                (AO_nop_full(), \
                 AO_double_compare_and_swap_acquire(addr, old, new_val))
# define AO_HAVE_double_compare_and_swap_full
#endif

#if !defined(AO_HAVE_double_compare_and_swap_release_write) \
    && defined(AO_HAVE_double_compare_and_swap_write)
# define AO_double_compare_and_swap_release_write(addr, old, new_val) \
                AO_double_compare_and_swap_write(addr, old, new_val)
# define AO_HAVE_double_compare_and_swap_release_write
#endif
#if !defined(AO_HAVE_double_compare_and_swap_release_write) \
    && defined(AO_HAVE_double_compare_and_swap_release)
# define AO_double_compare_and_swap_release_write(addr, old, new_val) \
                AO_double_compare_and_swap_release(addr, old, new_val)
# define AO_HAVE_double_compare_and_swap_release_write
#endif
#if !defined(AO_HAVE_double_compare_and_swap_acquire_read) \
    && defined(AO_HAVE_double_compare_and_swap_read)
# define AO_double_compare_and_swap_acquire_read(addr, old, new_val) \
                AO_double_compare_and_swap_read(addr, old, new_val)
# define AO_HAVE_double_compare_and_swap_acquire_read
#endif
#if !defined(AO_HAVE_double_compare_and_swap_acquire_read) \
    && defined(AO_HAVE_double_compare_and_swap_acquire)
# define AO_double_compare_and_swap_acquire_read(addr, old, new_val) \
                AO_double_compare_and_swap_acquire(addr, old, new_val)
# define AO_HAVE_double_compare_and_swap_acquire_read
#endif

#ifdef AO_NO_DD_ORDERING
# if defined(AO_HAVE_double_compare_and_swap_acquire_read)
#   define AO_double_compare_and_swap_dd_acquire_read(addr, old, new_val) \
                AO_double_compare_and_swap_acquire_read(addr, old, new_val)
#   define AO_HAVE_double_compare_and_swap_dd_acquire_read
# endif
#else
# if defined(AO_HAVE_double_compare_and_swap)
#   define AO_double_compare_and_swap_dd_acquire_read(addr, old, new_val) \
                AO_double_compare_and_swap(addr, old, new_val)
#   define AO_HAVE_double_compare_and_swap_dd_acquire_read
# endif
#endif /* !AO_NO_DD_ORDERING */

/* double_load */
#if defined(AO_HAVE_double_load_full) && !defined(AO_HAVE_double_load_acquire)
# define AO_double_load_acquire(addr) AO_double_load_full(addr)
# define AO_HAVE_double_load_acquire
#endif

#if defined(AO_HAVE_double_load_acquire) && !defined(AO_HAVE_double_load)
# define AO_double_load(addr) AO_double_load_acquire(addr)
# define AO_HAVE_double_load
#endif

#if defined(AO_HAVE_double_load_full) && !defined(AO_HAVE_double_load_read)
# define AO_double_load_read(addr) AO_double_load_full(addr)
# define AO_HAVE_double_load_read
#endif

#if !defined(AO_HAVE_double_load_acquire_read) \
    && defined(AO_HAVE_double_load_acquire)
# define AO_double_load_acquire_read(addr) AO_double_load_acquire(addr)
# define AO_HAVE_double_load_acquire_read
#endif

#if defined(AO_HAVE_double_load) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_double_load_acquire)
  AO_INLINE AO_double_t
  AO_double_load_acquire(const volatile AO_double_t *addr)
  {
    AO_double_t result = AO_double_load(addr);

    /* Acquire barrier would be useless, since the load could be delayed    */
    /* beyond it.                                                           */
    AO_nop_full();
    return result;
  }
# define AO_HAVE_double_load_acquire
#endif

#if defined(AO_HAVE_double_load) && defined(AO_HAVE_nop_read) \
    && !defined(AO_HAVE_double_load_read)
  AO_INLINE AO_double_t
  AO_double_load_read(const volatile AO_double_t *addr)
  {
    AO_double_t result = AO_double_load(addr);

    AO_nop_read();
    return result;
  }
# define AO_HAVE_double_load_read
#endif

#if defined(AO_HAVE_double_load_acquire) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_double_load_full)
# define AO_double_load_full(addr) (AO_nop_full(), AO_double_load_acquire(addr))
# define AO_HAVE_double_load_full
#endif

#if defined(AO_HAVE_double_compare_and_swap_read) \
    && !defined(AO_HAVE_double_load_read)
# define AO_double_CAS_BASED_LOAD_READ
  AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE AO_double_t
  AO_double_load_read(const volatile AO_double_t *addr)
  {
    AO_double_t result;

    do {
      result = *(const AO_double_t *)addr;
    } while (AO_EXPECT_FALSE(!AO_double_compare_and_swap_read(
                                                (volatile AO_double_t *)addr,
                                                result, result)));
    return result;
  }
# define AO_HAVE_double_load_read
#endif

#if !defined(AO_HAVE_double_load_acquire_read) \
    && defined(AO_HAVE_double_load_read)
# define AO_double_load_acquire_read(addr) AO_double_load_read(addr)
# define AO_HAVE_double_load_acquire_read
#endif

#if defined(AO_HAVE_double_load_acquire_read) && !defined(AO_HAVE_double_load) \
    && (!defined(AO_double_CAS_BASED_LOAD_READ) \
        || !defined(AO_HAVE_double_compare_and_swap))
# define AO_double_load(addr) AO_double_load_acquire_read(addr)
# define AO_HAVE_double_load
#endif

#if defined(AO_HAVE_double_compare_and_swap_full) \
    && !defined(AO_HAVE_double_load_full)
  AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE AO_double_t
  AO_double_load_full(const volatile AO_double_t *addr)
  {
    AO_double_t result;

    do {
      result = *(const AO_double_t *)addr;
    } while (AO_EXPECT_FALSE(!AO_double_compare_and_swap_full(
                                                (volatile AO_double_t *)addr,
                                                result, result)));
    return result;
  }
# define AO_HAVE_double_load_full
#endif

#if defined(AO_HAVE_double_compare_and_swap_acquire) \
    && !defined(AO_HAVE_double_load_acquire)
  AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE AO_double_t
  AO_double_load_acquire(const volatile AO_double_t *addr)
  {
    AO_double_t result;

    do {
      result = *(const AO_double_t *)addr;
    } while (AO_EXPECT_FALSE(!AO_double_compare_and_swap_acquire(
                                                (volatile AO_double_t *)addr,
                                                result, result)));
    return result;
  }
# define AO_HAVE_double_load_acquire
#endif

#if defined(AO_HAVE_double_compare_and_swap) && !defined(AO_HAVE_double_load)
  AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE AO_double_t
  AO_double_load(const volatile AO_double_t *addr)
  {
    AO_double_t result;

    do {
      result = *(const AO_double_t *)addr;
    } while (AO_EXPECT_FALSE(!AO_double_compare_and_swap(
                                                (volatile AO_double_t *)addr,
                                                result, result)));
    return result;
  }
# define AO_HAVE_double_load
#endif

#ifdef AO_NO_DD_ORDERING
# if defined(AO_HAVE_double_load_acquire_read)
#   define AO_double_load_dd_acquire_read(addr) \
                                AO_double_load_acquire_read(addr)
#   define AO_HAVE_double_load_dd_acquire_read
# endif
#else
# if defined(AO_HAVE_double_load)
#   define AO_double_load_dd_acquire_read(addr) AO_double_load(addr)
#   define AO_HAVE_double_load_dd_acquire_read
# endif
#endif /* !AO_NO_DD_ORDERING */

/* double_store */
#if defined(AO_HAVE_double_store_full) && !defined(AO_HAVE_double_store_release)
# define AO_double_store_release(addr, val) AO_double_store_full(addr, val)
# define AO_HAVE_double_store_release
#endif

#if defined(AO_HAVE_double_store_release) && !defined(AO_HAVE_double_store)
# define AO_double_store(addr, val) AO_double_store_release(addr, val)
# define AO_HAVE_double_store
#endif

#if defined(AO_HAVE_double_store_full) && !defined(AO_HAVE_double_store_write)
# define AO_double_store_write(addr, val) AO_double_store_full(addr, val)
# define AO_HAVE_double_store_write
#endif

#if defined(AO_HAVE_double_store_release) \
    && !defined(AO_HAVE_double_store_release_write)
# define AO_double_store_release_write(addr, val) \
                                AO_double_store_release(addr, val)
# define AO_HAVE_double_store_release_write
#endif

#if defined(AO_HAVE_double_store_write) && !defined(AO_HAVE_double_store)
# define AO_double_store(addr, val) AO_double_store_write(addr, val)
# define AO_HAVE_double_store
#endif

#if defined(AO_HAVE_double_store) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_double_store_release)
# define AO_double_store_release(addr, val) \
                                (AO_nop_full(), AO_double_store(addr, val))
# define AO_HAVE_double_store_release
#endif

#if defined(AO_HAVE_double_store) && defined(AO_HAVE_nop_write) \
    && !defined(AO_HAVE_double_store_write)
# define AO_double_store_write(addr, val) \
                                (AO_nop_write(), AO_double_store(addr, val))
# define AO_HAVE_double_store_write
#endif

#if defined(AO_HAVE_double_compare_and_swap_write) \
    && !defined(AO_HAVE_double_store_write)
  AO_ATTR_NO_SANITIZE_MEMORY AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE void
  AO_double_store_write(volatile AO_double_t *addr, AO_double_t new_val)
  {
    AO_double_t old_val;

    do {
      old_val = *(AO_double_t *)addr;
    } while (AO_EXPECT_FALSE(!AO_double_compare_and_swap_write(addr, old_val,
                                                              new_val)));
  }
# define AO_HAVE_double_store_write
#endif

#if defined(AO_HAVE_double_store_write) \
    && !defined(AO_HAVE_double_store_release_write)
# define AO_double_store_release_write(addr, val) \
                                AO_double_store_write(addr, val)
# define AO_HAVE_double_store_release_write
#endif

#if defined(AO_HAVE_double_store_release) && defined(AO_HAVE_nop_full) \
    && !defined(AO_HAVE_double_store_full)
# define AO_double_store_full(addr, val) \
                                (AO_double_store_release(addr, val), \
                                 AO_nop_full())
# define AO_HAVE_double_store_full
#endif

#if defined(AO_HAVE_double_compare_and_swap) && !defined(AO_HAVE_double_store)
  AO_ATTR_NO_SANITIZE_MEMORY AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE void
  AO_double_store(volatile AO_double_t *addr, AO_double_t new_val)
  {
    AO_double_t old_val;

    do {
      old_val = *(AO_double_t *)addr;
    } while (AO_EXPECT_FALSE(!AO_double_compare_and_swap(addr,
                                                        old_val, new_val)));
  }
# define AO_HAVE_double_store
#endif

#if defined(AO_HAVE_double_compare_and_swap_release) \
    && !defined(AO_HAVE_double_store_release)
  AO_ATTR_NO_SANITIZE_MEMORY AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE void
  AO_double_store_release(volatile AO_double_t *addr, AO_double_t new_val)
  {
    AO_double_t old_val;

    do {
      old_val = *(AO_double_t *)addr;
    } while (AO_EXPECT_FALSE(!AO_double_compare_and_swap_release(addr, old_val,
                                                                new_val)));
  }
# define AO_HAVE_double_store_release
#endif

#if defined(AO_HAVE_double_compare_and_swap_full) \
    && !defined(AO_HAVE_double_store_full)
  AO_ATTR_NO_SANITIZE_MEMORY AO_ATTR_NO_SANITIZE_THREAD
  AO_INLINE void
  AO_double_store_full(volatile AO_double_t *addr, AO_double_t new_val)
  {
    AO_double_t old_val;

    do {
      old_val = *(AO_double_t *)addr;
    } while (AO_EXPECT_FALSE(!AO_double_compare_and_swap_full(addr, old_val,
                                                             new_val)));
  }
# define AO_HAVE_double_store_full
#endif
