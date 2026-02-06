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

/* Inclusion of this file signifies that AO_t is in fact int.           */
/* Hence any AO_... operation can also serve as AO_int_... operation.   */

#if defined(AO_HAVE_load) && !defined(AO_HAVE_int_load)
# define AO_int_load(addr) \
                (unsigned)AO_load((const volatile AO_t *)(addr))
# define AO_HAVE_int_load
#endif

#if defined(AO_HAVE_store) && !defined(AO_HAVE_int_store)
# define AO_int_store(addr, val) \
                AO_store((volatile AO_t *)(addr), (AO_t)(val))
# define AO_HAVE_int_store
#endif

#if defined(AO_HAVE_fetch_and_add) \
    && !defined(AO_HAVE_int_fetch_and_add)
# define AO_int_fetch_and_add(addr, incr) \
                (unsigned)AO_fetch_and_add((volatile AO_t *)(addr), \
                                                (AO_t)(incr))
# define AO_HAVE_int_fetch_and_add
#endif

#if defined(AO_HAVE_fetch_and_add1) \
    && !defined(AO_HAVE_int_fetch_and_add1)
# define AO_int_fetch_and_add1(addr) \
                (unsigned)AO_fetch_and_add1((volatile AO_t *)(addr))
# define AO_HAVE_int_fetch_and_add1
#endif

#if defined(AO_HAVE_fetch_and_sub1) \
    && !defined(AO_HAVE_int_fetch_and_sub1)
# define AO_int_fetch_and_sub1(addr) \
                (unsigned)AO_fetch_and_sub1((volatile AO_t *)(addr))
# define AO_HAVE_int_fetch_and_sub1
#endif

#if defined(AO_HAVE_and) && !defined(AO_HAVE_int_and)
# define AO_int_and(addr, val) \
                AO_and((volatile AO_t *)(addr), (AO_t)(val))
# define AO_HAVE_int_and
#endif

#if defined(AO_HAVE_or) && !defined(AO_HAVE_int_or)
# define AO_int_or(addr, val) \
                AO_or((volatile AO_t *)(addr), (AO_t)(val))
# define AO_HAVE_int_or
#endif

#if defined(AO_HAVE_xor) && !defined(AO_HAVE_int_xor)
# define AO_int_xor(addr, val) \
                AO_xor((volatile AO_t *)(addr), (AO_t)(val))
# define AO_HAVE_int_xor
#endif

#if defined(AO_HAVE_fetch_compare_and_swap) \
    && !defined(AO_HAVE_int_fetch_compare_and_swap)
# define AO_int_fetch_compare_and_swap(addr, old, new_val) \
        (unsigned)AO_fetch_compare_and_swap((volatile AO_t *)(addr), \
                                                 (AO_t)(old), (AO_t)(new_val))
# define AO_HAVE_int_fetch_compare_and_swap
#endif

#if defined(AO_HAVE_compare_and_swap) \
    && !defined(AO_HAVE_int_compare_and_swap)
# define AO_int_compare_and_swap(addr, old, new_val) \
                AO_compare_and_swap((volatile AO_t *)(addr), \
                                         (AO_t)(old), (AO_t)(new_val))
# define AO_HAVE_int_compare_and_swap
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

/* Inclusion of this file signifies that AO_t is in fact int.           */
/* Hence any AO_... operation can also serve as AO_int_... operation.   */

#if defined(AO_HAVE_load_full) && !defined(AO_HAVE_int_load_full)
# define AO_int_load_full(addr) \
                (unsigned)AO_load_full((const volatile AO_t *)(addr))
# define AO_HAVE_int_load_full
#endif

#if defined(AO_HAVE_store_full) && !defined(AO_HAVE_int_store_full)
# define AO_int_store_full(addr, val) \
                AO_store_full((volatile AO_t *)(addr), (AO_t)(val))
# define AO_HAVE_int_store_full
#endif

#if defined(AO_HAVE_fetch_and_add_full) \
    && !defined(AO_HAVE_int_fetch_and_add_full)
# define AO_int_fetch_and_add_full(addr, incr) \
                (unsigned)AO_fetch_and_add_full((volatile AO_t *)(addr), \
                                                (AO_t)(incr))
# define AO_HAVE_int_fetch_and_add_full
#endif

#if defined(AO_HAVE_fetch_and_add1_full) \
    && !defined(AO_HAVE_int_fetch_and_add1_full)
# define AO_int_fetch_and_add1_full(addr) \
                (unsigned)AO_fetch_and_add1_full((volatile AO_t *)(addr))
# define AO_HAVE_int_fetch_and_add1_full
#endif

#if defined(AO_HAVE_fetch_and_sub1_full) \
    && !defined(AO_HAVE_int_fetch_and_sub1_full)
# define AO_int_fetch_and_sub1_full(addr) \
                (unsigned)AO_fetch_and_sub1_full((volatile AO_t *)(addr))
# define AO_HAVE_int_fetch_and_sub1_full
#endif

#if defined(AO_HAVE_and_full) && !defined(AO_HAVE_int_and_full)
# define AO_int_and_full(addr, val) \
                AO_and_full((volatile AO_t *)(addr), (AO_t)(val))
# define AO_HAVE_int_and_full
#endif

#if defined(AO_HAVE_or_full) && !defined(AO_HAVE_int_or_full)
# define AO_int_or_full(addr, val) \
                AO_or_full((volatile AO_t *)(addr), (AO_t)(val))
# define AO_HAVE_int_or_full
#endif

#if defined(AO_HAVE_xor_full) && !defined(AO_HAVE_int_xor_full)
# define AO_int_xor_full(addr, val) \
                AO_xor_full((volatile AO_t *)(addr), (AO_t)(val))
# define AO_HAVE_int_xor_full
#endif

#if defined(AO_HAVE_fetch_compare_and_swap_full) \
    && !defined(AO_HAVE_int_fetch_compare_and_swap_full)
# define AO_int_fetch_compare_and_swap_full(addr, old, new_val) \
        (unsigned)AO_fetch_compare_and_swap_full((volatile AO_t *)(addr), \
                                                 (AO_t)(old), (AO_t)(new_val))
# define AO_HAVE_int_fetch_compare_and_swap_full
#endif

#if defined(AO_HAVE_compare_and_swap_full) \
    && !defined(AO_HAVE_int_compare_and_swap_full)
# define AO_int_compare_and_swap_full(addr, old, new_val) \
                AO_compare_and_swap_full((volatile AO_t *)(addr), \
                                         (AO_t)(old), (AO_t)(new_val))
# define AO_HAVE_int_compare_and_swap_full
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

/* Inclusion of this file signifies that AO_t is in fact int.           */
/* Hence any AO_... operation can also serve as AO_int_... operation.   */

#if defined(AO_HAVE_load_acquire) && !defined(AO_HAVE_int_load_acquire)
# define AO_int_load_acquire(addr) \
                (unsigned)AO_load_acquire((const volatile AO_t *)(addr))
# define AO_HAVE_int_load_acquire
#endif

#if defined(AO_HAVE_store_acquire) && !defined(AO_HAVE_int_store_acquire)
# define AO_int_store_acquire(addr, val) \
                AO_store_acquire((volatile AO_t *)(addr), (AO_t)(val))
# define AO_HAVE_int_store_acquire
#endif

#if defined(AO_HAVE_fetch_and_add_acquire) \
    && !defined(AO_HAVE_int_fetch_and_add_acquire)
# define AO_int_fetch_and_add_acquire(addr, incr) \
                (unsigned)AO_fetch_and_add_acquire((volatile AO_t *)(addr), \
                                                (AO_t)(incr))
# define AO_HAVE_int_fetch_and_add_acquire
#endif

#if defined(AO_HAVE_fetch_and_add1_acquire) \
    && !defined(AO_HAVE_int_fetch_and_add1_acquire)
# define AO_int_fetch_and_add1_acquire(addr) \
                (unsigned)AO_fetch_and_add1_acquire((volatile AO_t *)(addr))
# define AO_HAVE_int_fetch_and_add1_acquire
#endif

#if defined(AO_HAVE_fetch_and_sub1_acquire) \
    && !defined(AO_HAVE_int_fetch_and_sub1_acquire)
# define AO_int_fetch_and_sub1_acquire(addr) \
                (unsigned)AO_fetch_and_sub1_acquire((volatile AO_t *)(addr))
# define AO_HAVE_int_fetch_and_sub1_acquire
#endif

#if defined(AO_HAVE_and_acquire) && !defined(AO_HAVE_int_and_acquire)
# define AO_int_and_acquire(addr, val) \
                AO_and_acquire((volatile AO_t *)(addr), (AO_t)(val))
# define AO_HAVE_int_and_acquire
#endif

#if defined(AO_HAVE_or_acquire) && !defined(AO_HAVE_int_or_acquire)
# define AO_int_or_acquire(addr, val) \
                AO_or_acquire((volatile AO_t *)(addr), (AO_t)(val))
# define AO_HAVE_int_or_acquire
#endif

#if defined(AO_HAVE_xor_acquire) && !defined(AO_HAVE_int_xor_acquire)
# define AO_int_xor_acquire(addr, val) \
                AO_xor_acquire((volatile AO_t *)(addr), (AO_t)(val))
# define AO_HAVE_int_xor_acquire
#endif

#if defined(AO_HAVE_fetch_compare_and_swap_acquire) \
    && !defined(AO_HAVE_int_fetch_compare_and_swap_acquire)
# define AO_int_fetch_compare_and_swap_acquire(addr, old, new_val) \
        (unsigned)AO_fetch_compare_and_swap_acquire((volatile AO_t *)(addr), \
                                                 (AO_t)(old), (AO_t)(new_val))
# define AO_HAVE_int_fetch_compare_and_swap_acquire
#endif

#if defined(AO_HAVE_compare_and_swap_acquire) \
    && !defined(AO_HAVE_int_compare_and_swap_acquire)
# define AO_int_compare_and_swap_acquire(addr, old, new_val) \
                AO_compare_and_swap_acquire((volatile AO_t *)(addr), \
                                         (AO_t)(old), (AO_t)(new_val))
# define AO_HAVE_int_compare_and_swap_acquire
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

/* Inclusion of this file signifies that AO_t is in fact int.           */
/* Hence any AO_... operation can also serve as AO_int_... operation.   */

#if defined(AO_HAVE_load_release) && !defined(AO_HAVE_int_load_release)
# define AO_int_load_release(addr) \
                (unsigned)AO_load_release((const volatile AO_t *)(addr))
# define AO_HAVE_int_load_release
#endif

#if defined(AO_HAVE_store_release) && !defined(AO_HAVE_int_store_release)
# define AO_int_store_release(addr, val) \
                AO_store_release((volatile AO_t *)(addr), (AO_t)(val))
# define AO_HAVE_int_store_release
#endif

#if defined(AO_HAVE_fetch_and_add_release) \
    && !defined(AO_HAVE_int_fetch_and_add_release)
# define AO_int_fetch_and_add_release(addr, incr) \
                (unsigned)AO_fetch_and_add_release((volatile AO_t *)(addr), \
                                                (AO_t)(incr))
# define AO_HAVE_int_fetch_and_add_release
#endif

#if defined(AO_HAVE_fetch_and_add1_release) \
    && !defined(AO_HAVE_int_fetch_and_add1_release)
# define AO_int_fetch_and_add1_release(addr) \
                (unsigned)AO_fetch_and_add1_release((volatile AO_t *)(addr))
# define AO_HAVE_int_fetch_and_add1_release
#endif

#if defined(AO_HAVE_fetch_and_sub1_release) \
    && !defined(AO_HAVE_int_fetch_and_sub1_release)
# define AO_int_fetch_and_sub1_release(addr) \
                (unsigned)AO_fetch_and_sub1_release((volatile AO_t *)(addr))
# define AO_HAVE_int_fetch_and_sub1_release
#endif

#if defined(AO_HAVE_and_release) && !defined(AO_HAVE_int_and_release)
# define AO_int_and_release(addr, val) \
                AO_and_release((volatile AO_t *)(addr), (AO_t)(val))
# define AO_HAVE_int_and_release
#endif

#if defined(AO_HAVE_or_release) && !defined(AO_HAVE_int_or_release)
# define AO_int_or_release(addr, val) \
                AO_or_release((volatile AO_t *)(addr), (AO_t)(val))
# define AO_HAVE_int_or_release
#endif

#if defined(AO_HAVE_xor_release) && !defined(AO_HAVE_int_xor_release)
# define AO_int_xor_release(addr, val) \
                AO_xor_release((volatile AO_t *)(addr), (AO_t)(val))
# define AO_HAVE_int_xor_release
#endif

#if defined(AO_HAVE_fetch_compare_and_swap_release) \
    && !defined(AO_HAVE_int_fetch_compare_and_swap_release)
# define AO_int_fetch_compare_and_swap_release(addr, old, new_val) \
        (unsigned)AO_fetch_compare_and_swap_release((volatile AO_t *)(addr), \
                                                 (AO_t)(old), (AO_t)(new_val))
# define AO_HAVE_int_fetch_compare_and_swap_release
#endif

#if defined(AO_HAVE_compare_and_swap_release) \
    && !defined(AO_HAVE_int_compare_and_swap_release)
# define AO_int_compare_and_swap_release(addr, old, new_val) \
                AO_compare_and_swap_release((volatile AO_t *)(addr), \
                                         (AO_t)(old), (AO_t)(new_val))
# define AO_HAVE_int_compare_and_swap_release
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

/* Inclusion of this file signifies that AO_t is in fact int.           */
/* Hence any AO_... operation can also serve as AO_int_... operation.   */

#if defined(AO_HAVE_load_write) && !defined(AO_HAVE_int_load_write)
# define AO_int_load_write(addr) \
                (unsigned)AO_load_write((const volatile AO_t *)(addr))
# define AO_HAVE_int_load_write
#endif

#if defined(AO_HAVE_store_write) && !defined(AO_HAVE_int_store_write)
# define AO_int_store_write(addr, val) \
                AO_store_write((volatile AO_t *)(addr), (AO_t)(val))
# define AO_HAVE_int_store_write
#endif

#if defined(AO_HAVE_fetch_and_add_write) \
    && !defined(AO_HAVE_int_fetch_and_add_write)
# define AO_int_fetch_and_add_write(addr, incr) \
                (unsigned)AO_fetch_and_add_write((volatile AO_t *)(addr), \
                                                (AO_t)(incr))
# define AO_HAVE_int_fetch_and_add_write
#endif

#if defined(AO_HAVE_fetch_and_add1_write) \
    && !defined(AO_HAVE_int_fetch_and_add1_write)
# define AO_int_fetch_and_add1_write(addr) \
                (unsigned)AO_fetch_and_add1_write((volatile AO_t *)(addr))
# define AO_HAVE_int_fetch_and_add1_write
#endif

#if defined(AO_HAVE_fetch_and_sub1_write) \
    && !defined(AO_HAVE_int_fetch_and_sub1_write)
# define AO_int_fetch_and_sub1_write(addr) \
                (unsigned)AO_fetch_and_sub1_write((volatile AO_t *)(addr))
# define AO_HAVE_int_fetch_and_sub1_write
#endif

#if defined(AO_HAVE_and_write) && !defined(AO_HAVE_int_and_write)
# define AO_int_and_write(addr, val) \
                AO_and_write((volatile AO_t *)(addr), (AO_t)(val))
# define AO_HAVE_int_and_write
#endif

#if defined(AO_HAVE_or_write) && !defined(AO_HAVE_int_or_write)
# define AO_int_or_write(addr, val) \
                AO_or_write((volatile AO_t *)(addr), (AO_t)(val))
# define AO_HAVE_int_or_write
#endif

#if defined(AO_HAVE_xor_write) && !defined(AO_HAVE_int_xor_write)
# define AO_int_xor_write(addr, val) \
                AO_xor_write((volatile AO_t *)(addr), (AO_t)(val))
# define AO_HAVE_int_xor_write
#endif

#if defined(AO_HAVE_fetch_compare_and_swap_write) \
    && !defined(AO_HAVE_int_fetch_compare_and_swap_write)
# define AO_int_fetch_compare_and_swap_write(addr, old, new_val) \
        (unsigned)AO_fetch_compare_and_swap_write((volatile AO_t *)(addr), \
                                                 (AO_t)(old), (AO_t)(new_val))
# define AO_HAVE_int_fetch_compare_and_swap_write
#endif

#if defined(AO_HAVE_compare_and_swap_write) \
    && !defined(AO_HAVE_int_compare_and_swap_write)
# define AO_int_compare_and_swap_write(addr, old, new_val) \
                AO_compare_and_swap_write((volatile AO_t *)(addr), \
                                         (AO_t)(old), (AO_t)(new_val))
# define AO_HAVE_int_compare_and_swap_write
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

/* Inclusion of this file signifies that AO_t is in fact int.           */
/* Hence any AO_... operation can also serve as AO_int_... operation.   */

#if defined(AO_HAVE_load_read) && !defined(AO_HAVE_int_load_read)
# define AO_int_load_read(addr) \
                (unsigned)AO_load_read((const volatile AO_t *)(addr))
# define AO_HAVE_int_load_read
#endif

#if defined(AO_HAVE_store_read) && !defined(AO_HAVE_int_store_read)
# define AO_int_store_read(addr, val) \
                AO_store_read((volatile AO_t *)(addr), (AO_t)(val))
# define AO_HAVE_int_store_read
#endif

#if defined(AO_HAVE_fetch_and_add_read) \
    && !defined(AO_HAVE_int_fetch_and_add_read)
# define AO_int_fetch_and_add_read(addr, incr) \
                (unsigned)AO_fetch_and_add_read((volatile AO_t *)(addr), \
                                                (AO_t)(incr))
# define AO_HAVE_int_fetch_and_add_read
#endif

#if defined(AO_HAVE_fetch_and_add1_read) \
    && !defined(AO_HAVE_int_fetch_and_add1_read)
# define AO_int_fetch_and_add1_read(addr) \
                (unsigned)AO_fetch_and_add1_read((volatile AO_t *)(addr))
# define AO_HAVE_int_fetch_and_add1_read
#endif

#if defined(AO_HAVE_fetch_and_sub1_read) \
    && !defined(AO_HAVE_int_fetch_and_sub1_read)
# define AO_int_fetch_and_sub1_read(addr) \
                (unsigned)AO_fetch_and_sub1_read((volatile AO_t *)(addr))
# define AO_HAVE_int_fetch_and_sub1_read
#endif

#if defined(AO_HAVE_and_read) && !defined(AO_HAVE_int_and_read)
# define AO_int_and_read(addr, val) \
                AO_and_read((volatile AO_t *)(addr), (AO_t)(val))
# define AO_HAVE_int_and_read
#endif

#if defined(AO_HAVE_or_read) && !defined(AO_HAVE_int_or_read)
# define AO_int_or_read(addr, val) \
                AO_or_read((volatile AO_t *)(addr), (AO_t)(val))
# define AO_HAVE_int_or_read
#endif

#if defined(AO_HAVE_xor_read) && !defined(AO_HAVE_int_xor_read)
# define AO_int_xor_read(addr, val) \
                AO_xor_read((volatile AO_t *)(addr), (AO_t)(val))
# define AO_HAVE_int_xor_read
#endif

#if defined(AO_HAVE_fetch_compare_and_swap_read) \
    && !defined(AO_HAVE_int_fetch_compare_and_swap_read)
# define AO_int_fetch_compare_and_swap_read(addr, old, new_val) \
        (unsigned)AO_fetch_compare_and_swap_read((volatile AO_t *)(addr), \
                                                 (AO_t)(old), (AO_t)(new_val))
# define AO_HAVE_int_fetch_compare_and_swap_read
#endif

#if defined(AO_HAVE_compare_and_swap_read) \
    && !defined(AO_HAVE_int_compare_and_swap_read)
# define AO_int_compare_and_swap_read(addr, old, new_val) \
                AO_compare_and_swap_read((volatile AO_t *)(addr), \
                                         (AO_t)(old), (AO_t)(new_val))
# define AO_HAVE_int_compare_and_swap_read
#endif
