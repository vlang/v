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

#include "../all_atomic_load_store.h"

/* Real SPARC code uses TSO:                            */
#include "../ordered_except_wr.h"

/* Test_and_set location is just a byte.                */
#include "../test_and_set_t_is_char.h"

#ifdef __cplusplus
  extern "C" {
#endif

extern AO_TS_VAL_t
AO_test_and_set_full(volatile AO_TS_t *addr);
/* Implemented in separate .S file, for now.    */
#define AO_HAVE_test_and_set_full

/* TODO: Like the gcc version, extend this for V8 and V9.   */

#ifdef __cplusplus
  } /* extern "C" */
#endif
