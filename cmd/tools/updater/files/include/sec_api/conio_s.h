/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */

#ifndef _INC_CONIO_S
#define _INC_CONIO_S

#include <conio.h>

#if defined(MINGW_HAS_SECURE_API)

#ifdef __cplusplus
extern "C" {
#endif

  _CRTIMP errno_t __cdecl _cgets_s(char *_Buffer,size_t _Size,size_t *_SizeRead);
  _CRTIMP int __cdecl _cprintf_s(const char *_Format,...);
  _CRTIMP int __cdecl _cscanf_s(const char *_Format,...);
  _CRTIMP int __cdecl _cscanf_s_l(const char *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _vcprintf_s(const char *_Format,va_list _ArgList);
  _CRTIMP int __cdecl _cprintf_s_l(const char *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _vcprintf_s_l(const char *_Format,_locale_t _Locale,va_list _ArgList);

#ifndef _WCONIO_DEFINED_S
#define _WCONIO_DEFINED_S
  _CRTIMP errno_t __cdecl _cgetws_s(wchar_t *_Buffer,size_t _SizeInWords,size_t *_SizeRead);
  _CRTIMP int __cdecl _cwprintf_s(const wchar_t *_Format,...);
  _CRTIMP int __cdecl _cwscanf_s(const wchar_t *_Format,...);
  _CRTIMP int __cdecl _cwscanf_s_l(const wchar_t *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _vcwprintf_s(const wchar_t *_Format,va_list _ArgList);
  _CRTIMP int __cdecl _cwprintf_s_l(const wchar_t *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _vcwprintf_s_l(const wchar_t *_Format,_locale_t _Locale,va_list _ArgList);
#endif

#ifdef __cplusplus
}
#endif

#endif
#endif
