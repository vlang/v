/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */

#ifndef _TIMEB_H_S
#define _TIMEB_H_S

#include <sys/timeb.h>

#ifdef __cplusplus
extern "C" {
#endif

#if defined(MINGW_HAS_SECURE_API)

#ifdef _USE_32BIT_TIME_T
#define _ftime_s _ftime32_s
#else
#define _ftime_s _ftime64_s
#endif

  _CRTIMP errno_t __cdecl _ftime32_s(struct __timeb32 *_Time);
#if _INTEGRAL_MAX_BITS >= 64
  _CRTIMP errno_t __cdecl _ftime64_s(struct __timeb64 *_Time);
#endif
#endif

#ifdef __cplusplus
}
#endif

#endif
