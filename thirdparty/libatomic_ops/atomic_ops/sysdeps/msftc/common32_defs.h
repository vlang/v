/*
 * Copyright (c) 2003-2011 Hewlett-Packard Development Company, L.P.
 * Copyright (c) 2009-2021 Ivan Maidanski
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

/* This file contains AO primitives based on VC++ built-in intrinsic    */
/* functions commonly available across 32- and 64-bit architectures.    */

/* This file should be included from arch-specific header files.        */
/* Define AO_USE_INTERLOCKED_INTRINSICS if _Interlocked primitives      */
/* (used below) are available as intrinsic ones for a target arch       */
/* (otherwise "Interlocked" functions family is used instead).          */
/* Define AO_ASSUME_WINDOWS98 if CAS is available.                      */

#if _MSC_VER <= 1400 || !defined(AO_USE_INTERLOCKED_INTRINSICS) \
    || defined(_WIN32_WCE)
# include <windows.h>
        /* Seems like over-kill, but that's what MSDN recommends.       */
        /* And apparently winbase.h is not always self-contained.       */
        /* Optionally, client could define WIN32_LEAN_AND_MEAN before   */
        /* include atomic_ops.h to reduce amount of Windows internal    */
        /* headers included by windows.h one.                           */
#endif

#if _MSC_VER < 1310 || !defined(AO_USE_INTERLOCKED_INTRINSICS)

# define _InterlockedIncrement       InterlockedIncrement
# define _InterlockedDecrement       InterlockedDecrement
# define _InterlockedExchangeAdd     InterlockedExchangeAdd
# define _InterlockedCompareExchange InterlockedCompareExchange

# define AO_INTERLOCKED_VOLATILE /**/

#else /* elif _MSC_VER >= 1310 */

# if _MSC_VER >= 1400
#   ifndef _WIN32_WCE
#     include <intrin.h>
#   endif

# else /* elif _MSC_VER < 1400 */
#  ifdef __cplusplus
     extern "C" {
#  endif
   LONG __cdecl _InterlockedIncrement(LONG volatile *);
   LONG __cdecl _InterlockedDecrement(LONG volatile *);
   LONG __cdecl _InterlockedExchangeAdd(LONG volatile *, LONG);
   LONG __cdecl _InterlockedCompareExchange(LONG volatile *,
                                        LONG /* Exchange */, LONG /* Comp */);
#  ifdef __cplusplus
     } /* extern "C" */
#  endif
# endif /* _MSC_VER < 1400 */

# if !defined(AO_PREFER_GENERALIZED) || !defined(AO_ASSUME_WINDOWS98)
#   pragma intrinsic (_InterlockedIncrement)
#   pragma intrinsic (_InterlockedDecrement)
#   pragma intrinsic (_InterlockedExchangeAdd)
#   ifndef AO_T_IS_INT
#     pragma intrinsic (_InterlockedIncrement64)
#     pragma intrinsic (_InterlockedDecrement64)
#     pragma intrinsic (_InterlockedExchangeAdd64)
#   endif
# endif /* !AO_PREFER_GENERALIZED */

# pragma intrinsic (_InterlockedCompareExchange)
# ifndef AO_T_IS_INT
#   pragma intrinsic (_InterlockedCompareExchange64)
# endif

# define AO_INTERLOCKED_VOLATILE volatile

#endif /* _MSC_VER >= 1310 */

#if !defined(AO_PREFER_GENERALIZED) || !defined(AO_ASSUME_WINDOWS98)
  AO_INLINE AO_t
  AO_fetch_and_add_full(volatile AO_t *p, AO_t incr)
  {
#   ifdef AO_T_IS_INT
      return _InterlockedExchangeAdd((long AO_INTERLOCKED_VOLATILE *)p, incr);
#   else
      return _InterlockedExchangeAdd64((__int64 volatile *)p, incr);
#   endif
  }
# define AO_HAVE_fetch_and_add_full

  AO_INLINE AO_t
  AO_fetch_and_add1_full(volatile AO_t *p)
  {
#   ifdef AO_T_IS_INT
      return _InterlockedIncrement((long AO_INTERLOCKED_VOLATILE *)p) - 1;
#   else
      return _InterlockedIncrement64((__int64 volatile *)p) - 1;
#   endif
  }
# define AO_HAVE_fetch_and_add1_full

  AO_INLINE AO_t
  AO_fetch_and_sub1_full(volatile AO_t *p)
  {
#   ifdef AO_T_IS_INT
      return _InterlockedDecrement((long AO_INTERLOCKED_VOLATILE *)p) + 1;
#   else
      return _InterlockedDecrement64((__int64 volatile *)p) + 1;
#   endif
  }
# define AO_HAVE_fetch_and_sub1_full

# ifndef AO_T_IS_INT
    AO_INLINE unsigned int
    AO_int_fetch_and_add_full(volatile unsigned int *p, unsigned int incr)
    {
      return _InterlockedExchangeAdd((long volatile *)p, incr);
    }
#   define AO_HAVE_int_fetch_and_add_full

    AO_INLINE unsigned int
    AO_int_fetch_and_add1_full(volatile unsigned int *p)
    {
      return _InterlockedIncrement((long volatile *)p) - 1;
    }
#   define AO_HAVE_int_fetch_and_add1_full

    AO_INLINE unsigned int
    AO_int_fetch_and_sub1_full(volatile unsigned int *p)
    {
      return _InterlockedDecrement((long volatile *)p) + 1;
    }
#   define AO_HAVE_int_fetch_and_sub1_full
# endif /* !AO_T_IS_INT */
#endif /* !AO_PREFER_GENERALIZED */

#ifdef AO_ASSUME_WINDOWS98
  AO_INLINE AO_t
  AO_fetch_compare_and_swap_full(volatile AO_t *addr, AO_t old_val,
                                 AO_t new_val)
  {
#   ifndef AO_T_IS_INT
      return (AO_t)_InterlockedCompareExchange64((__int64 volatile *)addr,
                                                 new_val, old_val);
#   elif defined(AO_OLD_STYLE_INTERLOCKED_COMPARE_EXCHANGE)
      return (AO_t)_InterlockedCompareExchange(
                                        (void *AO_INTERLOCKED_VOLATILE *)addr,
                                        (void *)new_val, (void *)old_val);
#   else
      return _InterlockedCompareExchange((long AO_INTERLOCKED_VOLATILE *)addr,
                                         new_val, old_val);
#   endif
  }
# define AO_HAVE_fetch_compare_and_swap_full

# ifndef AO_T_IS_INT
    AO_INLINE unsigned int
    AO_int_fetch_compare_and_swap_full(volatile unsigned int *addr,
                                       unsigned int old_val,
                                       unsigned int new_val)
    {
      return _InterlockedCompareExchange((long volatile *)addr,
                                         new_val, old_val);
    }
#   define AO_HAVE_int_fetch_compare_and_swap_full
# endif /* !AO_T_IS_INT */
#endif /* AO_ASSUME_WINDOWS98 */

#if (_MSC_VER > 1400) && (!defined(_M_ARM) || _MSC_VER >= 1800)

# if _MSC_VER < 1800 || !defined(AO_PREFER_GENERALIZED)
#   pragma intrinsic (_InterlockedAnd8)
#   pragma intrinsic (_InterlockedOr8)
#   pragma intrinsic (_InterlockedXor8)

    AO_INLINE void
    AO_char_and_full(volatile unsigned char *p, unsigned char value)
    {
      _InterlockedAnd8((char volatile *)p, value);
    }
#   define AO_HAVE_char_and_full

    AO_INLINE void
    AO_char_or_full(volatile unsigned char *p, unsigned char value)
    {
      _InterlockedOr8((char volatile *)p, value);
    }
#   define AO_HAVE_char_or_full

    AO_INLINE void
    AO_char_xor_full(volatile unsigned char *p, unsigned char value)
    {
      _InterlockedXor8((char volatile *)p, value);
    }
#   define AO_HAVE_char_xor_full
# endif /* _MSC_VER < 1800 || !AO_PREFER_GENERALIZED */

# pragma intrinsic (_InterlockedCompareExchange16)

  AO_INLINE unsigned short
  AO_short_fetch_compare_and_swap_full(volatile unsigned short *addr,
                                       unsigned short old_val,
                                       unsigned short new_val)
  {
    return _InterlockedCompareExchange16((short volatile *)addr,
                                         new_val, old_val);
  }
# define AO_HAVE_short_fetch_compare_and_swap_full

# ifndef AO_PREFER_GENERALIZED
#   pragma intrinsic (_InterlockedIncrement16)
#   pragma intrinsic (_InterlockedDecrement16)

    AO_INLINE unsigned short
    AO_short_fetch_and_add1_full(volatile unsigned short *p)
    {
      return _InterlockedIncrement16((short volatile *)p) - 1;
    }
#   define AO_HAVE_short_fetch_and_add1_full

    AO_INLINE unsigned short
    AO_short_fetch_and_sub1_full(volatile unsigned short *p)
    {
      return _InterlockedDecrement16((short volatile *)p) + 1;
    }
#   define AO_HAVE_short_fetch_and_sub1_full
# endif /* !AO_PREFER_GENERALIZED */

#endif /* _MSC_VER > 1400 */

#if _MSC_VER >= 1800 /* Visual Studio 2013+ */

# ifndef AO_PREFER_GENERALIZED
#   pragma intrinsic (_InterlockedAnd16)
#   pragma intrinsic (_InterlockedOr16)
#   pragma intrinsic (_InterlockedXor16)

    AO_INLINE void
    AO_short_and_full(volatile unsigned short *p, unsigned short value)
    {
      (void)_InterlockedAnd16((short volatile *)p, value);
    }
#   define AO_HAVE_short_and_full

    AO_INLINE void
    AO_short_or_full(volatile unsigned short *p, unsigned short value)
    {
      (void)_InterlockedOr16((short volatile *)p, value);
    }
#   define AO_HAVE_short_or_full

    AO_INLINE void
    AO_short_xor_full(volatile unsigned short *p, unsigned short value)
    {
      (void)_InterlockedXor16((short volatile *)p, value);
    }
#   define AO_HAVE_short_xor_full

#   pragma intrinsic (_InterlockedAnd)
#   pragma intrinsic (_InterlockedOr)
#   pragma intrinsic (_InterlockedXor)

#   ifndef AO_T_IS_INT
      AO_INLINE void
      AO_int_and_full(volatile unsigned int *p, unsigned int value)
      {
        (void)_InterlockedAnd((long volatile *)p, value);
      }
#     define AO_HAVE_int_and_full

      AO_INLINE void
      AO_int_or_full(volatile unsigned int *p, unsigned int value)
      {
        (void)_InterlockedOr((long volatile *)p, value);
      }
#     define AO_HAVE_int_or_full

      AO_INLINE void
      AO_int_xor_full(volatile unsigned int *p, unsigned int value)
      {
        (void)_InterlockedXor((long volatile *)p, value);
      }
#     define AO_HAVE_int_xor_full

#     pragma intrinsic (_InterlockedAnd64)
#     pragma intrinsic (_InterlockedOr64)
#     pragma intrinsic (_InterlockedXor64)
#   endif /* !AO_T_IS_INT */

    AO_INLINE void
    AO_and_full(volatile AO_t *p, AO_t value)
    {
#     ifdef AO_T_IS_INT
        (void)_InterlockedAnd((long volatile *)p, value);
#     else
        (void)_InterlockedAnd64((__int64 volatile *)p, value);
#     endif
    }
#   define AO_HAVE_and_full

    AO_INLINE void
    AO_or_full(volatile AO_t *p, AO_t value)
    {
#     ifdef AO_T_IS_INT
        (void)_InterlockedOr((long volatile *)p, value);
#     else
        (void)_InterlockedOr64((__int64 volatile *)p, value);
#     endif
    }
#   define AO_HAVE_or_full

    AO_INLINE void
    AO_xor_full(volatile AO_t *p, AO_t value)
    {
#     ifdef AO_T_IS_INT
        (void)_InterlockedXor((long volatile *)p, value);
#     else
        (void)_InterlockedXor64((__int64 volatile *)p, value);
#     endif
    }
#   define AO_HAVE_xor_full
# endif /* !AO_PREFER_GENERALIZED */

# if !defined(AO_PREFER_GENERALIZED) && (defined(_M_ARM) || defined(_M_ARM64))
#   pragma intrinsic (_InterlockedAnd8_acq)
#   pragma intrinsic (_InterlockedAnd8_nf)
#   pragma intrinsic (_InterlockedAnd8_rel)
#   pragma intrinsic (_InterlockedOr8_acq)
#   pragma intrinsic (_InterlockedOr8_nf)
#   pragma intrinsic (_InterlockedOr8_rel)
#   pragma intrinsic (_InterlockedXor8_acq)
#   pragma intrinsic (_InterlockedXor8_nf)
#   pragma intrinsic (_InterlockedXor8_rel)

    AO_INLINE void
    AO_char_and(volatile unsigned char *p, unsigned char value)
    {
      _InterlockedAnd8_nf((char volatile *)p, value);
    }
#   define AO_HAVE_char_and

    AO_INLINE void
    AO_char_or(volatile unsigned char *p, unsigned char value)
    {
      _InterlockedOr8_nf((char volatile *)p, value);
    }
#   define AO_HAVE_char_or

    AO_INLINE void
    AO_char_xor(volatile unsigned char *p, unsigned char value)
    {
      _InterlockedXor8_nf((char volatile *)p, value);
    }
#   define AO_HAVE_char_xor

    AO_INLINE void
    AO_char_and_acquire(volatile unsigned char *p, unsigned char value)
    {
      _InterlockedAnd8_acq((char volatile *)p, value);
    }
#   define AO_HAVE_char_and_acquire

    AO_INLINE void
    AO_char_or_acquire(volatile unsigned char *p, unsigned char value)
    {
      _InterlockedOr8_acq((char volatile *)p, value);
    }
#   define AO_HAVE_char_or_acquire

    AO_INLINE void
    AO_char_xor_acquire(volatile unsigned char *p, unsigned char value)
    {
      _InterlockedXor8_acq((char volatile *)p, value);
    }
#   define AO_HAVE_char_xor_acquire

    AO_INLINE void
    AO_char_and_release(volatile unsigned char *p, unsigned char value)
    {
      _InterlockedAnd8_rel((char volatile *)p, value);
    }
#   define AO_HAVE_char_and_release

    AO_INLINE void
    AO_char_or_release(volatile unsigned char *p, unsigned char value)
    {
      _InterlockedOr8_rel((char volatile *)p, value);
    }
#   define AO_HAVE_char_or_release

    AO_INLINE void
    AO_char_xor_release(volatile unsigned char *p, unsigned char value)
    {
      _InterlockedXor8_rel((char volatile *)p, value);
    }
#   define AO_HAVE_char_xor_release

#   pragma intrinsic (_InterlockedAnd16_acq)
#   pragma intrinsic (_InterlockedAnd16_nf)
#   pragma intrinsic (_InterlockedAnd16_rel)
#   pragma intrinsic (_InterlockedOr16_acq)
#   pragma intrinsic (_InterlockedOr16_nf)
#   pragma intrinsic (_InterlockedOr16_rel)
#   pragma intrinsic (_InterlockedXor16_acq)
#   pragma intrinsic (_InterlockedXor16_nf)
#   pragma intrinsic (_InterlockedXor16_rel)

    AO_INLINE void
    AO_short_and(volatile unsigned short *p, unsigned short value)
    {
      (void)_InterlockedAnd16_nf((short volatile *)p, value);
    }
#   define AO_HAVE_short_and

    AO_INLINE void
    AO_short_or(volatile unsigned short *p, unsigned short value)
    {
      (void)_InterlockedOr16_nf((short volatile *)p, value);
    }
#   define AO_HAVE_short_or

    AO_INLINE void
    AO_short_xor(volatile unsigned short *p, unsigned short value)
    {
      (void)_InterlockedXor16_nf((short volatile *)p, value);
    }
#   define AO_HAVE_short_xor

    AO_INLINE void
    AO_short_and_acquire(volatile unsigned short *p, unsigned short value)
    {
      (void)_InterlockedAnd16_acq((short volatile *)p, value);
    }
#   define AO_HAVE_short_and_acquire

    AO_INLINE void
    AO_short_or_acquire(volatile unsigned short *p, unsigned short value)
    {
      (void)_InterlockedOr16_acq((short volatile *)p, value);
    }
#   define AO_HAVE_short_or_acquire

    AO_INLINE void
    AO_short_xor_acquire(volatile unsigned short *p, unsigned short value)
    {
      (void)_InterlockedXor16_acq((short volatile *)p, value);
    }
#   define AO_HAVE_short_xor_acquire

    AO_INLINE void
    AO_short_and_release(volatile unsigned short *p, unsigned short value)
    {
      (void)_InterlockedAnd16_rel((short volatile *)p, value);
    }
#   define AO_HAVE_short_and_release

    AO_INLINE void
    AO_short_or_release(volatile unsigned short *p, unsigned short value)
    {
      (void)_InterlockedOr16_rel((short volatile *)p, value);
    }
#   define AO_HAVE_short_or_release

    AO_INLINE void
    AO_short_xor_release(volatile unsigned short *p, unsigned short value)
    {
      (void)_InterlockedXor16_rel((short volatile *)p, value);
    }
#   define AO_HAVE_short_xor_release

#   pragma intrinsic (_InterlockedAnd_acq)
#   pragma intrinsic (_InterlockedAnd_nf)
#   pragma intrinsic (_InterlockedAnd_rel)
#   pragma intrinsic (_InterlockedOr_acq)
#   pragma intrinsic (_InterlockedOr_nf)
#   pragma intrinsic (_InterlockedOr_rel)
#   pragma intrinsic (_InterlockedXor_acq)
#   pragma intrinsic (_InterlockedXor_nf)
#   pragma intrinsic (_InterlockedXor_rel)

#   ifndef AO_T_IS_INT
      AO_INLINE void
      AO_int_and(volatile unsigned int *p, unsigned int value)
      {
        (void)_InterlockedAnd_nf((long volatile *)p, value);
      }
#     define AO_HAVE_int_and

      AO_INLINE void
      AO_int_or(volatile unsigned int *p, unsigned int value)
      {
        (void)_InterlockedOr_nf((long volatile *)p, value);
      }
#     define AO_HAVE_int_or

      AO_INLINE void
      AO_int_xor(volatile unsigned int *p, unsigned int value)
      {
        (void)_InterlockedXor_nf((long volatile *)p, value);
      }
#     define AO_HAVE_int_xor

      AO_INLINE void
      AO_int_and_acquire(volatile unsigned int *p, unsigned int value)
      {
        (void)_InterlockedAnd_acq((long volatile *)p, value);
      }
#     define AO_HAVE_int_and_acquire

      AO_INLINE void
      AO_int_or_acquire(volatile unsigned int *p, unsigned int value)
      {
        (void)_InterlockedOr_acq((long volatile *)p, value);
      }
#     define AO_HAVE_int_or_acquire

      AO_INLINE void
      AO_int_xor_acquire(volatile unsigned int *p, unsigned int value)
      {
        (void)_InterlockedXor_acq((long volatile *)p, value);
      }
#     define AO_HAVE_int_xor_acquire

      AO_INLINE void
      AO_int_and_release(volatile unsigned int *p, unsigned int value)
      {
        (void)_InterlockedAnd_rel((long volatile *)p, value);
      }
#     define AO_HAVE_int_and_release

      AO_INLINE void
      AO_int_or_release(volatile unsigned int *p, unsigned int value)
      {
        (void)_InterlockedOr_rel((long volatile *)p, value);
      }
#     define AO_HAVE_int_or_release

      AO_INLINE void
      AO_int_xor_release(volatile unsigned int *p, unsigned int value)
      {
        (void)_InterlockedXor_rel((long volatile *)p, value);
      }
#     define AO_HAVE_int_xor_release

#     pragma intrinsic (_InterlockedAnd64_acq)
#     pragma intrinsic (_InterlockedAnd64_nf)
#     pragma intrinsic (_InterlockedAnd64_rel)
#     pragma intrinsic (_InterlockedOr64_acq)
#     pragma intrinsic (_InterlockedOr64_nf)
#     pragma intrinsic (_InterlockedOr64_rel)
#     pragma intrinsic (_InterlockedXor64_acq)
#     pragma intrinsic (_InterlockedXor64_nf)
#     pragma intrinsic (_InterlockedXor64_rel)
#   endif /* !AO_T_IS_INT */

    AO_INLINE void
    AO_and(volatile AO_t *p, AO_t value)
    {
#     ifdef AO_T_IS_INT
        (void)_InterlockedAnd_nf((long volatile *)p, value);
#     else
        (void)_InterlockedAnd64_nf((__int64 volatile *)p, value);
#     endif
    }
#   define AO_HAVE_and

    AO_INLINE void
    AO_or(volatile AO_t *p, AO_t value)
    {
#     ifdef AO_T_IS_INT
        (void)_InterlockedOr_nf((long volatile *)p, value);
#     else
        (void)_InterlockedOr64_nf((__int64 volatile *)p, value);
#     endif
    }
#   define AO_HAVE_or

    AO_INLINE void
    AO_xor(volatile AO_t *p, AO_t value)
    {
#     ifdef AO_T_IS_INT
        (void)_InterlockedXor_nf((long volatile *)p, value);
#     else
        (void)_InterlockedXor64_nf((__int64 volatile *)p, value);
#     endif
    }
#   define AO_HAVE_xor

    AO_INLINE void
    AO_and_acquire(volatile AO_t *p, AO_t value)
    {
#     ifdef AO_T_IS_INT
        (void)_InterlockedAnd_acq((long volatile *)p, value);
#     else
        (void)_InterlockedAnd64_acq((__int64 volatile *)p, value);
#     endif
    }
#   define AO_HAVE_and_acquire

    AO_INLINE void
    AO_or_acquire(volatile AO_t *p, AO_t value)
    {
#     ifdef AO_T_IS_INT
        (void)_InterlockedOr_acq((long volatile *)p, value);
#     else
        (void)_InterlockedOr64_acq((__int64 volatile *)p, value);
#     endif
    }
#   define AO_HAVE_or_acquire

    AO_INLINE void
    AO_xor_acquire(volatile AO_t *p, AO_t value)
    {
#     ifdef AO_T_IS_INT
        (void)_InterlockedXor_acq((long volatile *)p, value);
#     else
        (void)_InterlockedXor64_acq((__int64 volatile *)p, value);
#     endif
    }
#   define AO_HAVE_xor_acquire

    AO_INLINE void
    AO_and_release(volatile AO_t *p, AO_t value)
    {
#     ifdef AO_T_IS_INT
        (void)_InterlockedAnd_rel((long volatile *)p, value);
#     else
        (void)_InterlockedAnd64_rel((__int64 volatile *)p, value);
#     endif
    }
#   define AO_HAVE_and_release

    AO_INLINE void
    AO_or_release(volatile AO_t *p, AO_t value)
    {
#     ifdef AO_T_IS_INT
        (void)_InterlockedOr_rel((long volatile *)p, value);
#     else
        (void)_InterlockedOr64_rel((__int64 volatile *)p, value);
#     endif
    }
#   define AO_HAVE_or_release

    AO_INLINE void
    AO_xor_release(volatile AO_t *p, AO_t value)
    {
#     ifdef AO_T_IS_INT
        (void)_InterlockedXor_rel((long volatile *)p, value);
#     else
        (void)_InterlockedXor64_rel((__int64 volatile *)p, value);
#     endif
    }
#   define AO_HAVE_xor_release

#   pragma intrinsic (_InterlockedDecrement16_acq)
#   pragma intrinsic (_InterlockedDecrement16_nf)
#   pragma intrinsic (_InterlockedDecrement16_rel)
#   pragma intrinsic (_InterlockedIncrement16_acq)
#   pragma intrinsic (_InterlockedIncrement16_nf)
#   pragma intrinsic (_InterlockedIncrement16_rel)

    AO_INLINE unsigned short
    AO_short_fetch_and_add1(volatile unsigned short *p)
    {
      return _InterlockedIncrement16_nf((short volatile *)p) - 1;
    }
#   define AO_HAVE_short_fetch_and_add1

    AO_INLINE unsigned short
    AO_short_fetch_and_sub1(volatile unsigned short *p)
    {
      return _InterlockedDecrement16_nf((short volatile *)p) + 1;
    }
#   define AO_HAVE_short_fetch_and_sub1

    AO_INLINE unsigned short
    AO_short_fetch_and_add1_acquire(volatile unsigned short *p)
    {
      return _InterlockedIncrement16_acq((short volatile *)p) - 1;
    }
#   define AO_HAVE_short_fetch_and_add1_acquire

    AO_INLINE unsigned short
    AO_short_fetch_and_sub1_acquire(volatile unsigned short *p)
    {
      return _InterlockedDecrement16_acq((short volatile *)p) + 1;
    }
#   define AO_HAVE_short_fetch_and_sub1_acquire

    AO_INLINE unsigned short
    AO_short_fetch_and_add1_release(volatile unsigned short *p)
    {
      return _InterlockedIncrement16_rel((short volatile *)p) - 1;
    }
#   define AO_HAVE_short_fetch_and_add1_release

    AO_INLINE unsigned short
    AO_short_fetch_and_sub1_release(volatile unsigned short *p)
    {
      return _InterlockedDecrement16_rel((short volatile *)p) + 1;
    }
#   define AO_HAVE_short_fetch_and_sub1_release

#   pragma intrinsic (_InterlockedExchangeAdd_acq)
#   pragma intrinsic (_InterlockedExchangeAdd_nf)
#   pragma intrinsic (_InterlockedExchangeAdd_rel)

#   pragma intrinsic (_InterlockedDecrement_acq)
#   pragma intrinsic (_InterlockedDecrement_nf)
#   pragma intrinsic (_InterlockedDecrement_rel)
#   pragma intrinsic (_InterlockedIncrement_acq)
#   pragma intrinsic (_InterlockedIncrement_nf)
#   pragma intrinsic (_InterlockedIncrement_rel)

#   ifndef AO_T_IS_INT
#     pragma intrinsic (_InterlockedExchangeAdd64_acq)
#     pragma intrinsic (_InterlockedExchangeAdd64_nf)
#     pragma intrinsic (_InterlockedExchangeAdd64_rel)

#     pragma intrinsic (_InterlockedDecrement64_acq)
#     pragma intrinsic (_InterlockedDecrement64_nf)
#     pragma intrinsic (_InterlockedDecrement64_rel)
#     pragma intrinsic (_InterlockedIncrement64_acq)
#     pragma intrinsic (_InterlockedIncrement64_nf)
#     pragma intrinsic (_InterlockedIncrement64_rel)
#   endif

    AO_INLINE AO_t
    AO_fetch_and_add(volatile AO_t *p, AO_t incr)
    {
#     ifdef AO_T_IS_INT
        return _InterlockedExchangeAdd_nf((long volatile *)p, incr);
#     else
        return _InterlockedExchangeAdd64_nf((__int64 volatile *)p, incr);
#     endif
    }
#   define AO_HAVE_fetch_and_add

    AO_INLINE AO_t
    AO_fetch_and_add1(volatile AO_t *p)
    {
#     ifdef AO_T_IS_INT
        return _InterlockedIncrement_nf((long volatile *)p) - 1;
#     else
        return _InterlockedIncrement64_nf((__int64 volatile *)p) - 1;
#     endif
    }
#   define AO_HAVE_fetch_and_add1

    AO_INLINE AO_t
    AO_fetch_and_sub1(volatile AO_t *p)
    {
#     ifdef AO_T_IS_INT
        return _InterlockedDecrement_nf((long volatile *)p) + 1;
#     else
        return _InterlockedDecrement64_nf((__int64 volatile *)p) + 1;
#     endif
    }
#   define AO_HAVE_fetch_and_sub1

    AO_INLINE AO_t
    AO_fetch_and_add_acquire(volatile AO_t *p, AO_t incr)
    {
#     ifdef AO_T_IS_INT
        return _InterlockedExchangeAdd_acq((long volatile *)p, incr);
#     else
        return _InterlockedExchangeAdd64_acq((__int64 volatile *)p, incr);
#     endif
    }
#   define AO_HAVE_fetch_and_add_acquire

    AO_INLINE AO_t
    AO_fetch_and_add1_acquire(volatile AO_t *p)
    {
#     ifdef AO_T_IS_INT
        return _InterlockedIncrement_acq((long volatile *)p) - 1;
#     else
        return _InterlockedIncrement64_acq((__int64 volatile *)p) - 1;
#     endif
    }
#   define AO_HAVE_fetch_and_add1_acquire

    AO_INLINE AO_t
    AO_fetch_and_sub1_acquire(volatile AO_t *p)
    {
#     ifdef AO_T_IS_INT
        return _InterlockedDecrement_acq((long volatile *)p) + 1;
#     else
        return _InterlockedDecrement64_acq((__int64 volatile *)p) + 1;
#     endif
    }
#   define AO_HAVE_fetch_and_sub1_acquire

    AO_INLINE AO_t
    AO_fetch_and_add_release(volatile AO_t *p, AO_t incr)
    {
#     ifdef AO_T_IS_INT
        return _InterlockedExchangeAdd_rel((long volatile *)p, incr);
#     else
        return _InterlockedExchangeAdd64_rel((__int64 volatile *)p, incr);
#     endif
    }
#   define AO_HAVE_fetch_and_add_release

    AO_INLINE AO_t
    AO_fetch_and_add1_release(volatile AO_t *p)
    {
#     ifdef AO_T_IS_INT
        return _InterlockedIncrement_rel((long volatile *)p) - 1;
#     else
        return _InterlockedIncrement64_rel((__int64 volatile *)p) - 1;
#     endif
    }
#   define AO_HAVE_fetch_and_add1_release

    AO_INLINE AO_t
    AO_fetch_and_sub1_release(volatile AO_t *p)
    {
#     ifdef AO_T_IS_INT
        return _InterlockedDecrement_rel((long volatile *)p) + 1;
#     else
        return _InterlockedDecrement64_rel((__int64 volatile *)p) + 1;
#     endif
    }
#   define AO_HAVE_fetch_and_sub1_release

#   ifndef AO_T_IS_INT
      AO_INLINE unsigned int
      AO_int_fetch_and_add(volatile unsigned int *p, unsigned int incr)
      {
        return _InterlockedExchangeAdd_nf((long volatile *)p, incr);
      }
#     define AO_HAVE_int_fetch_and_add

      AO_INLINE unsigned int
      AO_int_fetch_and_add1(volatile unsigned int *p)
      {
        return _InterlockedIncrement_nf((long volatile *)p) - 1;
      }
#     define AO_HAVE_int_fetch_and_add1

      AO_INLINE unsigned int
      AO_int_fetch_and_sub1(volatile unsigned int *p)
      {
        return _InterlockedDecrement_nf((long volatile *)p) + 1;
      }
#     define AO_HAVE_int_fetch_and_sub1

      AO_INLINE unsigned int
      AO_int_fetch_and_add_acquire(volatile unsigned int *p,
                                   unsigned int incr)
      {
        return _InterlockedExchangeAdd_acq((long volatile *)p, incr);
      }
#     define AO_HAVE_int_fetch_and_add_acquire

      AO_INLINE unsigned int
      AO_int_fetch_and_add1_acquire(volatile unsigned int *p)
      {
        return _InterlockedIncrement_acq((long volatile *)p) - 1;
      }
#     define AO_HAVE_int_fetch_and_add1_acquire

      AO_INLINE unsigned int
      AO_int_fetch_and_sub1_acquire(volatile unsigned int *p)
      {
        return _InterlockedDecrement_acq((long volatile *)p) + 1;
      }
#     define AO_HAVE_int_fetch_and_sub1_acquire

      AO_INLINE unsigned int
      AO_int_fetch_and_add_release(volatile unsigned int *p,
                                   unsigned int incr)
      {
        return _InterlockedExchangeAdd_rel((long volatile *)p, incr);
      }
#     define AO_HAVE_int_fetch_and_add_release

      AO_INLINE unsigned int
      AO_int_fetch_and_add1_release(volatile unsigned int *p)
      {
        return _InterlockedIncrement_rel((long volatile *)p) - 1;
      }
#     define AO_HAVE_int_fetch_and_add1_release

      AO_INLINE unsigned int
      AO_int_fetch_and_sub1_release(volatile unsigned int *p)
      {
        return _InterlockedDecrement_rel((long volatile *)p) + 1;
      }
#     define AO_HAVE_int_fetch_and_sub1_release
#   endif /* !AO_T_IS_INT */

# endif /* !AO_PREFER_GENERALIZED && (_M_ARM || _M_ARM64) */

# pragma intrinsic (_InterlockedCompareExchange8)

  AO_INLINE unsigned char
  AO_char_fetch_compare_and_swap_full(volatile unsigned char *addr,
                                      unsigned char old_val,
                                      unsigned char new_val)
  {
    return _InterlockedCompareExchange8((char volatile *)addr,
                                        new_val, old_val);
  }
# define AO_HAVE_char_fetch_compare_and_swap_full

# if defined(_M_ARM) || defined(_M_ARM64)

#   pragma intrinsic (_InterlockedCompareExchange_acq)
#   pragma intrinsic (_InterlockedCompareExchange_nf)
#   pragma intrinsic (_InterlockedCompareExchange_rel)
#   ifndef AO_T_IS_INT
#     pragma intrinsic (_InterlockedCompareExchange64_acq)
#     pragma intrinsic (_InterlockedCompareExchange64_nf)
#     pragma intrinsic (_InterlockedCompareExchange64_rel)
#   endif

    AO_INLINE AO_t
    AO_fetch_compare_and_swap(volatile AO_t *addr, AO_t old_val, AO_t new_val)
    {
#     ifdef AO_T_IS_INT
        return _InterlockedCompareExchange_nf((long volatile *)addr,
                                              new_val, old_val);
#     else
        return (AO_t)_InterlockedCompareExchange64_nf(
                        (__int64 volatile *)addr, new_val, old_val);
#     endif
    }
#   define AO_HAVE_fetch_compare_and_swap

    AO_INLINE AO_t
    AO_fetch_compare_and_swap_acquire(volatile AO_t *addr, AO_t old_val,
                                      AO_t new_val)
    {
#     ifdef AO_T_IS_INT
        return _InterlockedCompareExchange_acq((long volatile *)addr,
                                               new_val, old_val);
#     else
        return (AO_t)_InterlockedCompareExchange64_acq(
                        (__int64 volatile *)addr, new_val, old_val);
#     endif
    }
#   define AO_HAVE_fetch_compare_and_swap_acquire

    AO_INLINE AO_t
    AO_fetch_compare_and_swap_release(volatile AO_t *addr, AO_t old_val,
                                      AO_t new_val)
    {
#     ifdef AO_T_IS_INT
        return _InterlockedCompareExchange_rel((long volatile *)addr,
                                               new_val, old_val);
#     else
        return (AO_t)_InterlockedCompareExchange64_rel(
                        (__int64 volatile *)addr, new_val, old_val);
#     endif
    }
#   define AO_HAVE_fetch_compare_and_swap_release

#   ifndef AO_T_IS_INT
      AO_INLINE unsigned int
      AO_int_fetch_compare_and_swap(volatile unsigned int *addr,
                                    unsigned int old_val,
                                    unsigned int new_val)
      {
        return _InterlockedCompareExchange_nf((long volatile *)addr,
                                              new_val, old_val);
      }
#     define AO_HAVE_int_fetch_compare_and_swap

      AO_INLINE unsigned int
      AO_int_fetch_compare_and_swap_acquire(volatile unsigned int *addr,
                                            unsigned int old_val,
                                            unsigned int new_val)
      {
        return _InterlockedCompareExchange_acq((long volatile *)addr,
                                               new_val, old_val);
      }
#     define AO_HAVE_int_fetch_compare_and_swap_acquire

      AO_INLINE unsigned int
      AO_int_fetch_compare_and_swap_release(volatile unsigned int *addr,
                                            unsigned int old_val,
                                            unsigned int new_val)
      {
        return _InterlockedCompareExchange_rel((long volatile *)addr,
                                               new_val, old_val);
      }
#     define AO_HAVE_int_fetch_compare_and_swap_release
#   endif /* !AO_T_IS_INT */

#   pragma intrinsic (_InterlockedCompareExchange16_acq)
#   pragma intrinsic (_InterlockedCompareExchange16_nf)
#   pragma intrinsic (_InterlockedCompareExchange16_rel)
#   pragma intrinsic (_InterlockedCompareExchange8_acq)
#   pragma intrinsic (_InterlockedCompareExchange8_nf)
#   pragma intrinsic (_InterlockedCompareExchange8_rel)

    AO_INLINE unsigned short
    AO_short_fetch_compare_and_swap(volatile unsigned short *addr,
                                    unsigned short old_val,
                                    unsigned short new_val)
    {
      return _InterlockedCompareExchange16_nf((short volatile *)addr,
                                              new_val, old_val);
    }
#   define AO_HAVE_short_fetch_compare_and_swap

    AO_INLINE unsigned short
    AO_short_fetch_compare_and_swap_acquire(volatile unsigned short *addr,
                                            unsigned short old_val,
                                            unsigned short new_val)
    {
      return _InterlockedCompareExchange16_acq((short volatile *)addr,
                                               new_val, old_val);
    }
#   define AO_HAVE_short_fetch_compare_and_swap_acquire

    AO_INLINE unsigned short
    AO_short_fetch_compare_and_swap_release(volatile unsigned short *addr,
                                            unsigned short old_val,
                                            unsigned short new_val)
    {
      return _InterlockedCompareExchange16_rel((short volatile *)addr,
                                               new_val, old_val);
    }
#   define AO_HAVE_short_fetch_compare_and_swap_release

    AO_INLINE unsigned char
    AO_char_fetch_compare_and_swap(volatile unsigned char *addr,
                                   unsigned char old_val,
                                   unsigned char new_val)
    {
      return _InterlockedCompareExchange8_nf((char volatile *)addr,
                                             new_val, old_val);
    }
#   define AO_HAVE_char_fetch_compare_and_swap

    AO_INLINE unsigned char
    AO_char_fetch_compare_and_swap_acquire(volatile unsigned char *addr,
                                           unsigned char old_val,
                                           unsigned char new_val)
    {
      return _InterlockedCompareExchange8_acq((char volatile *)addr,
                                              new_val, old_val);
    }
#   define AO_HAVE_char_fetch_compare_and_swap_acquire

    AO_INLINE unsigned char
    AO_char_fetch_compare_and_swap_release(volatile unsigned char *addr,
                                           unsigned char old_val,
                                           unsigned char new_val)
    {
      return _InterlockedCompareExchange8_rel((char volatile *)addr,
                                              new_val, old_val);
    }
#   define AO_HAVE_char_fetch_compare_and_swap_release
# endif /* _M_ARM || _M_ARM64 */

# if !defined(AO_PREFER_GENERALIZED) && !defined(_M_ARM)
#   pragma intrinsic (_InterlockedExchangeAdd16)
#   pragma intrinsic (_InterlockedExchangeAdd8)

    AO_INLINE unsigned char
    AO_char_fetch_and_add_full(volatile unsigned char *p, unsigned char incr)
    {
      return _InterlockedExchangeAdd8((char volatile *)p, incr);
    }
#   define AO_HAVE_char_fetch_and_add_full

    AO_INLINE unsigned short
    AO_short_fetch_and_add_full(volatile unsigned short *p,
                                unsigned short incr)
    {
      return _InterlockedExchangeAdd16((short volatile *)p, incr);
    }
#   define AO_HAVE_short_fetch_and_add_full

#   if defined(_M_ARM64)
#     pragma intrinsic (_InterlockedExchangeAdd16_acq)
#     pragma intrinsic (_InterlockedExchangeAdd16_nf)
#     pragma intrinsic (_InterlockedExchangeAdd16_rel)
#     pragma intrinsic (_InterlockedExchangeAdd8_acq)
#     pragma intrinsic (_InterlockedExchangeAdd8_nf)
#     pragma intrinsic (_InterlockedExchangeAdd8_rel)

      AO_INLINE unsigned char
      AO_char_fetch_and_add(volatile unsigned char *p, unsigned char incr)
      {
        return _InterlockedExchangeAdd8_nf((char volatile *)p, incr);
      }
#     define AO_HAVE_char_fetch_and_add

      AO_INLINE unsigned short
      AO_short_fetch_and_add(volatile unsigned short *p, unsigned short incr)
      {
        return _InterlockedExchangeAdd16_nf((short volatile *)p, incr);
      }
#     define AO_HAVE_short_fetch_and_add

      AO_INLINE unsigned char
      AO_char_fetch_and_add_acquire(volatile unsigned char *p,
                                    unsigned char incr)
      {
        return _InterlockedExchangeAdd8_acq((char volatile *)p, incr);
      }
#     define AO_HAVE_char_fetch_and_add_acquire

      AO_INLINE unsigned short
      AO_short_fetch_and_add_acquire(volatile unsigned short *p,
                                     unsigned short incr)
      {
        return _InterlockedExchangeAdd16_acq((short volatile *)p, incr);
      }
#     define AO_HAVE_short_fetch_and_add_acquire

      AO_INLINE unsigned char
      AO_char_fetch_and_add_release(volatile unsigned char *p,
                                    unsigned char incr)
      {
        return _InterlockedExchangeAdd8_rel((char volatile *)p, incr);
      }
#     define AO_HAVE_char_fetch_and_add_release

      AO_INLINE unsigned short
      AO_short_fetch_and_add_release(volatile unsigned short *p,
                                     unsigned short incr)
      {
        return _InterlockedExchangeAdd16_rel((short volatile *)p, incr);
      }
#     define AO_HAVE_short_fetch_and_add_release
#   endif /* _M_ARM64 */

# endif /* !AO_PREFER_GENERALIZED && !_M_ARM */

# if !defined(_M_ARM) || _M_ARM >= 6
#   include "../test_and_set_t_is_char.h"

#   pragma intrinsic (_InterlockedExchange8)

    AO_INLINE AO_TS_VAL_t
    AO_test_and_set_full(volatile AO_TS_t *addr)
    {
      return (AO_TS_VAL_t)(_InterlockedExchange8((char volatile *)addr,
                                                 (AO_TS_t)AO_TS_SET) & 0xff);
      /* Note: bitwise "and 0xff" is applied to the result because cast */
      /* to unsigned char does not work properly (for a reason) if /J   */
      /* option is passed to the MS VC compiler.                        */
    }
#   define AO_HAVE_test_and_set_full
# endif /* !_M_ARM || _M_ARM >= 6 */

# if _M_ARM >= 6 || defined(_M_ARM64)
#   pragma intrinsic (_InterlockedExchange8_acq)
#   pragma intrinsic (_InterlockedExchange8_nf)
#   pragma intrinsic (_InterlockedExchange8_rel)

    AO_INLINE AO_TS_VAL_t
    AO_test_and_set(volatile AO_TS_t *addr)
    {
      return (AO_TS_VAL_t)(_InterlockedExchange8_nf((char volatile *)addr,
                                                (AO_TS_t)AO_TS_SET) & 0xff);
    }
#   define AO_HAVE_test_and_set

    AO_INLINE AO_TS_VAL_t
    AO_test_and_set_acquire(volatile AO_TS_t *addr)
    {
      return (AO_TS_VAL_t)(_InterlockedExchange8_acq((char volatile *)addr,
                                                (AO_TS_t)AO_TS_SET) & 0xff);
    }
#   define AO_HAVE_test_and_set_acquire

    AO_INLINE AO_TS_VAL_t
    AO_test_and_set_release(volatile AO_TS_t *addr)
    {
      return (AO_TS_VAL_t)(_InterlockedExchange8_rel((char volatile *)addr,
                                                (AO_TS_t)AO_TS_SET) & 0xff);
    }
#   define AO_HAVE_test_and_set_release
# endif /* _M_ARM >= 6 || _M_ARM64 */

#endif /* _MSC_VER >= 1800 */
