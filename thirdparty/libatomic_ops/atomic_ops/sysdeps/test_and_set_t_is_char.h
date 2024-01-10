/*
 * Copyright (c) 2004 Hewlett-Packard Development Company, L.P.
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
 * These are common definitions for architectures on which test_and_set
 * operates on byte sized quantities, the "clear" value contains
 * all zeroes, and the "set" value contains all ones typically.
 */

#ifndef AO_GCC_ATOMIC_TEST_AND_SET
# define AO_TS_SET_TRUEVAL 0xff
#elif defined(__GCC_ATOMIC_TEST_AND_SET_TRUEVAL) \
      && !defined(AO_PREFER_GENERALIZED)
# define AO_TS_SET_TRUEVAL __GCC_ATOMIC_TEST_AND_SET_TRUEVAL
#else
# define AO_TS_SET_TRUEVAL 1 /* true */
#endif

typedef enum {
  AO_BYTE_TS_clear = 0,
  AO_BYTE_TS_set = AO_TS_SET_TRUEVAL
} AO_BYTE_TS_val;

#define AO_TS_VAL_t AO_BYTE_TS_val
#define AO_TS_CLEAR AO_BYTE_TS_clear
#define AO_TS_SET AO_BYTE_TS_set

#define AO_TS_t unsigned char

#define AO_CHAR_TS_T 1

#undef AO_TS_SET_TRUEVAL
