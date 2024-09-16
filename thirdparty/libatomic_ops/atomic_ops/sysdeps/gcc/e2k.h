/*
 * Copyright (c) 2022 Ivan Maidanski
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

/* As of clang-9, all __GCC_HAVE_SYNC_COMPARE_AND_SWAP_n are missing.   */
#define AO_GCC_FORCE_HAVE_CAS

#if (__SIZEOF_SIZE_T__ == 4) \
    || (defined(__GCC_HAVE_SYNC_COMPARE_AND_SWAP_16) && !defined(__clang__)) \
    || (defined(__clang__) && __iset__ >= 5 /* elbrus-v5 or later */ \
        && defined(AO_PREFER_BUILTIN_ATOMICS))
  /* Note for 128-bit operations: clang reports warning                 */
  /* "large atomic operation may incur significant performance penalty" */
  /* and requires LDFLAGS="-latomic", thus this is not on by default.   */
# define AO_GCC_HAVE_double_SYNC_CAS
# include "../standard_ao_double_t.h"
#endif

#include "generic.h"

#undef AO_GCC_FORCE_HAVE_CAS
#undef AO_GCC_HAVE_double_SYNC_CAS
