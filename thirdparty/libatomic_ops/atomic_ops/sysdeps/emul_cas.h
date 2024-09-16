/*
 * Copyright (c) 2003 by Hewlett-Packard Company.  All rights reserved.
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
 * Ensure, if at all possible, that AO_compare_and_swap_full() is
 * available.  The emulation should be brute-force signal-safe, even
 * though it actually blocks.
 * Including this file will generate an error if AO_compare_and_swap_full()
 * cannot be made available.
 * This will be included from platform-specific atomic_ops files
 * if appropriate, and if AO_REQUIRE_CAS is defined.  It should not be
 * included directly, especially since it affects the implementation
 * of other atomic update primitives.
 * The implementation assumes that only AO_store_XXX and AO_test_and_set_XXX
 * variants are defined, and that AO_test_and_set_XXX is not used to
 * operate on compare_and_swap locations.
 */

#ifndef AO_ATOMIC_OPS_H
# error This file should not be included directly.
#endif

#ifndef AO_HAVE_double_t
# include "standard_ao_double_t.h"
#endif

#ifdef __cplusplus
  extern "C" {
#endif

AO_API AO_t AO_fetch_compare_and_swap_emulation(volatile AO_t *addr,
                                                AO_t old_val, AO_t new_val);

AO_API int
AO_compare_double_and_swap_double_emulation(volatile AO_double_t *addr,
                                            AO_t old_val1, AO_t old_val2,
                                            AO_t new_val1, AO_t new_val2);

AO_API void AO_store_full_emulation(volatile AO_t *addr, AO_t val);

#ifndef AO_HAVE_fetch_compare_and_swap_full
# define AO_fetch_compare_and_swap_full(addr, old, newval) \
                AO_fetch_compare_and_swap_emulation(addr, old, newval)
# define AO_HAVE_fetch_compare_and_swap_full
#endif

#ifndef AO_HAVE_compare_double_and_swap_double_full
# define AO_compare_double_and_swap_double_full(addr, old1, old2, \
                                                newval1, newval2) \
        AO_compare_double_and_swap_double_emulation(addr, old1, old2, \
                                                    newval1, newval2)
# define AO_HAVE_compare_double_and_swap_double_full
#endif

#undef AO_store
#undef AO_HAVE_store
#undef AO_store_write
#undef AO_HAVE_store_write
#undef AO_store_release
#undef AO_HAVE_store_release
#undef AO_store_full
#undef AO_HAVE_store_full
#define AO_store_full(addr, val) AO_store_full_emulation(addr, val)
#define AO_HAVE_store_full

#ifdef __cplusplus
  } /* extern "C" */
#endif
