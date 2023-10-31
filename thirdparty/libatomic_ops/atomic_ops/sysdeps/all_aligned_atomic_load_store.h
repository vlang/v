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

/* Describes architectures on which AO_t, unsigned char, unsigned       */
/* short, and unsigned int loads and stores are atomic but only if data */
/* is suitably aligned.                                                 */

#if defined(__m68k__) && !defined(AO_ALIGNOF_SUPPORTED)
  /* Even though AO_t is redefined in m68k.h, some clients use AO       */
  /* pointer size primitives to access variables not declared as AO_t.  */
  /* Such variables may have 2-byte alignment, while their sizeof is 4. */
#else
# define AO_ACCESS_CHECK_ALIGNED
#endif

/* Check for char type is a misnomer.   */
#define AO_ACCESS_short_CHECK_ALIGNED
#define AO_ACCESS_int_CHECK_ALIGNED
#include "all_atomic_load_store.h"
