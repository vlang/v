/*
 * Copyright (c) 2004-2011 Hewlett-Packard Development Company, L.P.
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

/* For 64-bit systems, we expect the double type to hold two int64's.   */

#if ((defined(__x86_64__) && defined(AO_GCC_ATOMIC_TEST_AND_SET)) \
     || defined(__aarch64__)) && !defined(__ILP32__)
  /* x86-64: __m128 is not applicable to atomic intrinsics.     */
# if AO_GNUC_PREREQ(4, 7) || AO_CLANG_PREREQ(3, 6)
#   pragma GCC diagnostic push
    /* Suppress warning about __int128 type.      */
#   if defined(__clang__) || AO_GNUC_PREREQ(6, 4)
#     pragma GCC diagnostic ignored "-Wpedantic"
#   else
      /* GCC before ~4.8 does not accept "-Wpedantic" quietly.  */
#     pragma GCC diagnostic ignored "-pedantic"
#   endif
    typedef unsigned __int128 double_ptr_storage;
#   pragma GCC diagnostic pop
# else /* pragma diagnostic is not supported */
    typedef unsigned __int128 double_ptr_storage;
# endif
#elif ((defined(__x86_64__) && AO_GNUC_PREREQ(4, 0)) || defined(_WIN64)) \
      && !defined(__ILP32__)
  /* x86-64 (except for x32): __m128 serves as a placeholder which also */
  /* requires the compiler to align it on 16-byte boundary (as required */
  /* by cmpxchg16b).                                                    */
  /* Similar things could be done for PPC 64-bit using a VMX data type. */
# include <xmmintrin.h>
  typedef __m128 double_ptr_storage;
#elif defined(_WIN32) && !defined(__GNUC__)
  typedef unsigned __int64 double_ptr_storage;
#elif defined(__i386__) && defined(__GNUC__)
  typedef unsigned long long double_ptr_storage
                                __attribute__((__aligned__(8)));
#else
  typedef unsigned long long double_ptr_storage;
#endif
# define AO_HAVE_DOUBLE_PTR_STORAGE

typedef union {
    struct { AO_t AO_v1; AO_t AO_v2; } AO_parts;
        /* Note that AO_v1 corresponds to the low or the high part of   */
        /* AO_whole depending on the machine endianness.                */
    double_ptr_storage AO_whole;
        /* AO_whole is now (starting from v7.3alpha3) the 2nd element   */
        /* of this union to make AO_DOUBLE_T_INITIALIZER portable       */
        /* (because __m128 definition could vary from a primitive type  */
        /* to a structure or array/vector).                             */
} AO_double_t;
#define AO_HAVE_double_t

/* Note: AO_double_t volatile variables are not intended to be local    */
/* ones (at least those which are passed to AO double-wide primitives   */
/* as the first argument), otherwise it is the client responsibility to */
/* ensure they have double-word alignment.                              */

/* Dummy declaration as a compile-time assertion for AO_double_t size.  */
struct AO_double_t_size_static_assert {
    char dummy[sizeof(AO_double_t) == 2 * sizeof(AO_t) ? 1 : -1];
};

#define AO_DOUBLE_T_INITIALIZER { { (AO_t)0, (AO_t)0 } }

#define AO_val1 AO_parts.AO_v1
#define AO_val2 AO_parts.AO_v2
