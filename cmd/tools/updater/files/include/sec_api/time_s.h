/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef _TIME_H__S
#define _TIME_H__S

#include <time.h>

#if defined(MINGW_HAS_SECURE_API)

#ifdef __cplusplus
extern "C" {
#endif

  _CRTIMP errno_t __cdecl _ctime32_s(char *_Buf,size_t _SizeInBytes,const __time32_t *_Time);
  _CRTIMP errno_t __cdecl _gmtime32_s(struct tm *_Tm,const __time32_t *_Time);
  _CRTIMP errno_t __cdecl _localtime32_s(struct tm *_Tm,const __time32_t *_Time);
  _CRTIMP errno_t __cdecl _strdate_s(char *_Buf,size_t _SizeInBytes);
  _CRTIMP errno_t __cdecl _strtime_s(char *_Buf ,size_t _SizeInBytes);
#if _INTEGRAL_MAX_BITS >= 64
  _CRTIMP errno_t __cdecl _ctime64_s(char *_Buf,size_t _SizeInBytes,const __time64_t *_Time);
  _CRTIMP errno_t __cdecl _gmtime64_s(struct tm *_Tm,const __time64_t *_Time);
  _CRTIMP errno_t __cdecl _localtime64_s(struct tm *_Tm,const __time64_t *_Time);
#endif

#ifndef _WTIME_S_DEFINED
#define _WTIME_S_DEFINED
  _CRTIMP errno_t __cdecl _wasctime_s(wchar_t *_Buf,size_t _SizeInWords,const struct tm *_Tm);
  _CRTIMP errno_t __cdecl _wctime32_s(wchar_t *_Buf,size_t _SizeInWords,const __time32_t *_Time);
  _CRTIMP errno_t __cdecl _wstrdate_s(wchar_t *_Buf,size_t _SizeInWords);
  _CRTIMP errno_t __cdecl _wstrtime_s(wchar_t *_Buf,size_t _SizeInWords);
#if _INTEGRAL_MAX_BITS >= 64
  _CRTIMP errno_t __cdecl _wctime64_s(wchar_t *_Buf,size_t _SizeInWords,const __time64_t *_Time);
#endif

#if !defined (RC_INVOKED) && !defined (_INC_WTIME_S_INL)
#define _INC_WTIME_S_INL
#ifdef _USE_32BIT_TIME_T
__CRT_INLINE errno_t __cdecl _wctime_s(wchar_t *_Buffer,size_t _SizeInWords,const time_t *_Time) { return _wctime32_s(_Buffer,_SizeInWords,_Time); }
#else
__CRT_INLINE errno_t __cdecl _wctime_s(wchar_t *_Buffer,size_t _SizeInWords,const time_t *_Time) { return _wctime64_s(_Buffer,_SizeInWords,_Time); }
#endif
#endif
#endif

#ifndef RC_INVOKED
#ifdef _USE_32BIT_TIME_T
__CRT_INLINE errno_t __cdecl localtime_s(struct tm *_Tm,const time_t *_Time) { return _localtime32_s(_Tm,_Time); }
#else
__CRT_INLINE errno_t __cdecl localtime_s(struct tm *_Tm,const time_t *_Time) { return _localtime64_s(_Tm,_Time); }
#endif
#endif

#ifdef __cplusplus
}
#endif

#endif
#endif
