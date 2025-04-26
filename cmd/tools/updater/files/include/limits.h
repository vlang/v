/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#include <_mingw.h>

#ifndef _INC_LIMITS
#define _INC_LIMITS

/*
* File system limits
*
* TODO: NAME_MAX and OPEN_MAX are file system limits or not? Are they the
*       same as FILENAME_MAX and FOPEN_MAX from stdio.h?
* NOTE: Apparently the actual size of PATH_MAX is 260, but a space is
*       required for the NUL. TODO: Test?
*/
#define PATH_MAX	(259)

#define CHAR_BIT 8
#define SCHAR_MIN (-128)
#define SCHAR_MAX 127
#define UCHAR_MAX 0xff

#ifndef __CHAR_UNSIGNED__
#define CHAR_MIN SCHAR_MIN
#define CHAR_MAX SCHAR_MAX
#else
#define CHAR_MIN 0
#define CHAR_MAX UCHAR_MAX
#endif

#define MB_LEN_MAX 5
#define SHRT_MIN (-32768)
#define SHRT_MAX 32767
#define USHRT_MAX 0xffff
#define INT_MIN (-2147483647 - 1)
#define INT_MAX 2147483647
#define UINT_MAX 0xffffffff
#define LONG_MIN (-2147483647L - 1)
#define LONG_MAX 2147483647L
#define ULONG_MAX 0xffffffffUL
#define LLONG_MAX 9223372036854775807ll
#define LLONG_MIN (-9223372036854775807ll - 1)
#define ULLONG_MAX 0xffffffffffffffffull

#if _INTEGRAL_MAX_BITS >= 8
#define _I8_MIN (-127 - 1)
#define _I8_MAX 127i8
#define _UI8_MAX 0xffu
#endif

#if _INTEGRAL_MAX_BITS >= 16
#define _I16_MIN (-32767 - 1)
#define _I16_MAX 32767i16
#define _UI16_MAX 0xffffu
#endif

#if _INTEGRAL_MAX_BITS >= 32
#define _I32_MIN (-2147483647 - 1)
#define _I32_MAX 2147483647
#define _UI32_MAX 0xffffffffu
#endif

#if defined(__GNUC__)
#undef LONG_LONG_MAX
#define LONG_LONG_MAX 9223372036854775807ll
#undef LONG_LONG_MIN
#define LONG_LONG_MIN (-LONG_LONG_MAX-1)
#undef ULONG_LONG_MAX
#define ULONG_LONG_MAX (2ull * LONG_LONG_MAX + 1ull)
#endif

#if _INTEGRAL_MAX_BITS >= 64
#define _I64_MIN (-9223372036854775807ll - 1)
#define _I64_MAX 9223372036854775807ll
#define _UI64_MAX 0xffffffffffffffffull
#endif

#ifndef SIZE_MAX
#ifdef _WIN64
#define SIZE_MAX _UI64_MAX
#else
#define SIZE_MAX UINT_MAX
#endif
#endif

#ifdef _POSIX_
#define _POSIX_ARG_MAX 4096
#define _POSIX_CHILD_MAX 6
#define _POSIX_LINK_MAX 8
#define _POSIX_MAX_CANON 255
#define _POSIX_MAX_INPUT 255
#define _POSIX_NAME_MAX 14
#define _POSIX_NGROUPS_MAX 0
#define _POSIX_OPEN_MAX 16
#define _POSIX_PATH_MAX 255
#define _POSIX_PIPE_BUF 512
#define _POSIX_SSIZE_MAX 32767
#define _POSIX_STREAM_MAX 8
#define _POSIX_TZNAME_MAX 3
#define ARG_MAX 14500
#define LINK_MAX 1024
#define MAX_CANON _POSIX_MAX_CANON
#define MAX_INPUT _POSIX_MAX_INPUT
#define NAME_MAX 255
#define NGROUPS_MAX 16
#define OPEN_MAX 32
#define PATH_MAX 512
#define PIPE_BUF _POSIX_PIPE_BUF
#define SSIZE_MAX _POSIX_SSIZE_MAX
#define STREAM_MAX 20
#define TZNAME_MAX 10
#endif
#endif
