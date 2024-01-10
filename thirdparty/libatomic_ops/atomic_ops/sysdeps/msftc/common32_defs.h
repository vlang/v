/*
 * Copyright (c) 2003-2011 Hewlett-Packard Development Company, L.P.
 * Copyright (c) 2009-2018 Ivan Maidanski
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
/* functions commonly available across 32-bit architectures.            */

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
# endif /* !AO_PREFER_GENERALIZED */
# pragma intrinsic (_InterlockedCompareExchange)

# define AO_INTERLOCKED_VOLATILE volatile

#endif /* _MSC_VER >= 1310 */

#if !defined(AO_PREFER_GENERALIZED) || !defined(AO_ASSUME_WINDOWS98)
AO_INLINE AO_t
AO_fetch_and_add_full(volatile AO_t *p, AO_t incr)
{
  return _InterlockedExchangeAdd((long AO_INTERLOCKED_VOLATILE *)p, incr);
}
#define AO_HAVE_fetch_and_add_full

AO_INLINE AO_t
AO_fetch_and_add1_full(volatile AO_t *p)
{
  return _InterlockedIncrement((long AO_INTERLOCKED_VOLATILE *)p) - 1;
}
#define AO_HAVE_fetch_and_add1_full

AO_INLINE AO_t
AO_fetch_and_sub1_full(volatile AO_t *p)
{
  return _InterlockedDecrement((long AO_INTERLOCKED_VOLATILE *)p) + 1;
}
#define AO_HAVE_fetch_and_sub1_full
#endif /* !AO_PREFER_GENERALIZED */

#ifdef AO_ASSUME_WINDOWS98
  AO_INLINE AO_t
  AO_fetch_compare_and_swap_full(volatile AO_t *addr, AO_t old_val,
                                 AO_t new_val)
  {
#   ifdef AO_OLD_STYLE_INTERLOCKED_COMPARE_EXCHANGE
      return (AO_t)_InterlockedCompareExchange(
                                        (void *AO_INTERLOCKED_VOLATILE *)addr,
                                        (void *)new_val, (void *)old_val);
#   else
      return _InterlockedCompareExchange((long AO_INTERLOCKED_VOLATILE *)addr,
                                         new_val, old_val);
#   endif
  }
# define AO_HAVE_fetch_compare_and_swap_full
#endif /* AO_ASSUME_WINDOWS98 */

#if (_MSC_VER > 1400) && (!defined(_M_ARM) || _MSC_VER >= 1800)

# pragma intrinsic (_InterlockedAnd8)
# pragma intrinsic (_InterlockedCompareExchange16)
# pragma intrinsic (_InterlockedOr8)
# pragma intrinsic (_InterlockedXor8)

  AO_INLINE void
  AO_char_and_full(volatile unsigned char *p, unsigned char value)
  {
    _InterlockedAnd8((char volatile *)p, value);
  }
# define AO_HAVE_char_and_full

  AO_INLINE void
  AO_char_or_full(volatile unsigned char *p, unsigned char value)
  {
    _InterlockedOr8((char volatile *)p, value);
  }
# define AO_HAVE_char_or_full

  AO_INLINE void
  AO_char_xor_full(volatile unsigned char *p, unsigned char value)
  {
    _InterlockedXor8((char volatile *)p, value);
  }
# define AO_HAVE_char_xor_full

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
# endif /* !AO_PREFER_GENERALIZED && !_M_ARM */
#endif /* _MSC_VER >= 1800 */
