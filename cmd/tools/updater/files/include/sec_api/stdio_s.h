/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef _INC_STDIO_S
#define _INC_STDIO_S

#include <stdio.h>

#if defined(MINGW_HAS_SECURE_API)

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _STDIO_S_DEFINED
#define _STDIO_S_DEFINED
  _CRTIMP errno_t __cdecl clearerr_s(FILE *_File);
  int __cdecl fprintf_s(FILE *_File,const char *_Format,...);
  size_t __cdecl fread_s(void *_DstBuf,size_t _DstSize,size_t _ElementSize,size_t _Count,FILE *_File);
  _CRTIMP int __cdecl _fscanf_s_l(FILE *_File,const char *_Format,_locale_t _Locale,...);
  int __cdecl printf_s(const char *_Format,...);
  _CRTIMP int __cdecl _scanf_l(const char *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _scanf_s_l(const char *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _snprintf_s(char *_DstBuf,size_t _DstSize,size_t _MaxCount,const char *_Format,...);
  _CRTIMP int __cdecl _snprintf_c(char *_DstBuf,size_t _MaxCount,const char *_Format,...);
  _CRTIMP int __cdecl _vsnprintf_c(char *_DstBuf,size_t _MaxCount,const char *_Format,va_list _ArgList);
  int __cdecl sprintf_s(char *_DstBuf,size_t _DstSize,const char *_Format,...);
  _CRTIMP int __cdecl _fscanf_l(FILE *_File,const char *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _sscanf_l(const char *_Src,const char *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _sscanf_s_l(const char *_Src,const char *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _snscanf_s(const char *_Src,size_t _MaxCount,const char *_Format,...);
  _CRTIMP int __cdecl _snscanf_l(const char *_Src,size_t _MaxCount,const char *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _snscanf_s_l(const char *_Src,size_t _MaxCount,const char *_Format,_locale_t _Locale,...);
  int __cdecl vfprintf_s(FILE *_File,const char *_Format,va_list _ArgList);
  int __cdecl vprintf_s(const char *_Format,va_list _ArgList);
  int __cdecl vsnprintf_s(char *_DstBuf,size_t _DstSize,size_t _MaxCount,const char *_Format,va_list _ArgList);
  _CRTIMP int __cdecl _vsnprintf_s(char *_DstBuf,size_t _DstSize,size_t _MaxCount,const char *_Format,va_list _ArgList);
  int __cdecl vsprintf_s(char *_DstBuf,size_t _Size,const char *_Format,va_list _ArgList);
  _CRTIMP int __cdecl _fprintf_p(FILE *_File,const char *_Format,...);
  _CRTIMP int __cdecl _printf_p(const char *_Format,...);
  _CRTIMP int __cdecl _sprintf_p(char *_Dst,size_t _MaxCount,const char *_Format,...);
  _CRTIMP int __cdecl _vfprintf_p(FILE *_File,const char *_Format,va_list _ArgList);
  _CRTIMP int __cdecl _vprintf_p(const char *_Format,va_list _ArgList);
  _CRTIMP int __cdecl _vsprintf_p(char *_Dst,size_t _MaxCount,const char *_Format,va_list _ArgList);
  _CRTIMP int __cdecl _scprintf_p(const char *_Format,...);
  _CRTIMP int __cdecl _vscprintf_p(const char *_Format,va_list _ArgList);
  _CRTIMP int __cdecl _printf_l(const char *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _printf_p_l(const char *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _vprintf_l(const char *_Format,_locale_t _Locale,va_list _ArgList);
  _CRTIMP int __cdecl _vprintf_p_l(const char *_Format,_locale_t _Locale,va_list _ArgList);
  _CRTIMP int __cdecl _fprintf_l(FILE *_File,const char *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _fprintf_p_l(FILE *_File,const char *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _vfprintf_l(FILE *_File,const char *_Format,_locale_t _Locale,va_list _ArgList);
  _CRTIMP int __cdecl _vfprintf_p_l(FILE *_File,const char *_Format,_locale_t _Locale,va_list _ArgList);
  _CRTIMP int __cdecl _sprintf_l(char *_DstBuf,const char *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _sprintf_p_l(char *_DstBuf,size_t _MaxCount,const char *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _vsprintf_l(char *_DstBuf,const char *_Format,_locale_t,va_list _ArgList);
  _CRTIMP int __cdecl _vsprintf_p_l(char *_DstBuf,size_t _MaxCount,const char *_Format,_locale_t _Locale,va_list _ArgList);
  _CRTIMP int __cdecl _scprintf_l(const char *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _scprintf_p_l(const char *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _vscprintf_l(const char *_Format,_locale_t _Locale,va_list _ArgList);
  _CRTIMP int __cdecl _vscprintf_p_l(const char *_Format,_locale_t _Locale,va_list _ArgList);
  _CRTIMP int __cdecl _printf_s_l(const char *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _vprintf_s_l(const char *_Format,_locale_t _Locale,va_list _ArgList);
  _CRTIMP int __cdecl _fprintf_s_l(FILE *_File,const char *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _vfprintf_s_l(FILE *_File,const char *_Format,_locale_t _Locale,va_list _ArgList);
  _CRTIMP int __cdecl _sprintf_s_l(char *_DstBuf,size_t _DstSize,const char *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _vsprintf_s_l(char *_DstBuf,size_t _DstSize,const char *_Format,_locale_t _Locale,va_list _ArgList);
  _CRTIMP int __cdecl _snprintf_s_l(char *_DstBuf,size_t _DstSize,size_t _MaxCount,const char *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _vsnprintf_s_l(char *_DstBuf,size_t _DstSize,size_t _MaxCount,const char *_Format,_locale_t _Locale,va_list _ArgList);
  _CRTIMP int __cdecl _snprintf_l(char *_DstBuf,size_t _MaxCount,const char *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _snprintf_c_l(char *_DstBuf,size_t _MaxCount,const char *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _vsnprintf_l(char *_DstBuf,size_t _MaxCount,const char *_Format,_locale_t _Locale,va_list _ArgList);
  _CRTIMP int __cdecl _vsnprintf_c_l(char *_DstBuf,size_t _MaxCount,const char *,_locale_t _Locale,va_list _ArgList);

#ifndef _WSTDIO_S_DEFINED
#define _WSTDIO_S_DEFINED
  _CRTIMP wchar_t *__cdecl _getws_s(wchar_t *_Str,size_t _SizeInWords);
  int __cdecl fwprintf_s(FILE *_File,const wchar_t *_Format,...);
  int __cdecl wprintf_s(const wchar_t *_Format,...);
  int __cdecl vwprintf_s(const wchar_t *_Format,va_list _ArgList);
  int __cdecl swprintf_s(wchar_t *_Dst,size_t _SizeInWords,const wchar_t *_Format,...);
  int __cdecl vswprintf_s(wchar_t *_Dst,size_t _SizeInWords,const wchar_t *_Format,va_list _ArgList);
  _CRTIMP int __cdecl _snwprintf_s(wchar_t *_DstBuf,size_t _DstSizeInWords,size_t _MaxCount,const wchar_t *_Format,...);
  _CRTIMP int __cdecl _vsnwprintf_s(wchar_t *_DstBuf,size_t _DstSizeInWords,size_t _MaxCount,const wchar_t *_Format,va_list _ArgList);
  _CRTIMP int __cdecl _wprintf_s_l(const wchar_t *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _vwprintf_s_l(const wchar_t *_Format,_locale_t _Locale,va_list _ArgList);
  _CRTIMP int __cdecl _fwprintf_s_l(FILE *_File,const wchar_t *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _vfwprintf_s_l(FILE *_File,const wchar_t *_Format,_locale_t _Locale,va_list _ArgList);
  _CRTIMP int __cdecl _swprintf_s_l(wchar_t *_DstBuf,size_t _DstSize,const wchar_t *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _vswprintf_s_l(wchar_t *_DstBuf,size_t _DstSize,const wchar_t *_Format,_locale_t _Locale,va_list _ArgList);
  _CRTIMP int __cdecl _snwprintf_s_l(wchar_t *_DstBuf,size_t _DstSize,size_t _MaxCount,const wchar_t *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _vsnwprintf_s_l(wchar_t *_DstBuf,size_t _DstSize,size_t _MaxCount,const wchar_t *_Format,_locale_t _Locale,va_list _ArgList);
  _CRTIMP int __cdecl _fwscanf_s_l(FILE *_File,const wchar_t *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _swscanf_s_l(const wchar_t *_Src,const wchar_t *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _snwscanf_s(const wchar_t *_Src,size_t _MaxCount,const wchar_t *_Format,...);
  _CRTIMP int __cdecl _snwscanf_s_l(const wchar_t *_Src,size_t _MaxCount,const wchar_t *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _wscanf_s_l(const wchar_t *_Format,_locale_t _Locale,...);
  _CRTIMP errno_t __cdecl _wfopen_s(FILE **_File,const wchar_t *_Filename,const wchar_t *_Mode);
  _CRTIMP errno_t __cdecl _wfreopen_s(FILE **_File,const wchar_t *_Filename,const wchar_t *_Mode,FILE *_OldFile);
  _CRTIMP errno_t __cdecl _wtmpnam_s(wchar_t *_DstBuf,size_t _SizeInWords);
  _CRTIMP int __cdecl _fwprintf_p(FILE *_File,const wchar_t *_Format,...);
  _CRTIMP int __cdecl _wprintf_p(const wchar_t *_Format,...);
  _CRTIMP int __cdecl _vfwprintf_p(FILE *_File,const wchar_t *_Format,va_list _ArgList);
  _CRTIMP int __cdecl _vwprintf_p(const wchar_t *_Format,va_list _ArgList);
  _CRTIMP int __cdecl _swprintf_p(wchar_t *_DstBuf,size_t _MaxCount,const wchar_t *_Format,...);
  _CRTIMP int __cdecl _vswprintf_p(wchar_t *_DstBuf,size_t _MaxCount,const wchar_t *_Format,va_list _ArgList);
  _CRTIMP int __cdecl _scwprintf_p(const wchar_t *_Format,...);
  _CRTIMP int __cdecl _vscwprintf_p(const wchar_t *_Format,va_list _ArgList);
  _CRTIMP int __cdecl _wprintf_l(const wchar_t *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _wprintf_p_l(const wchar_t *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _vwprintf_l(const wchar_t *_Format,_locale_t _Locale,va_list _ArgList);
  _CRTIMP int __cdecl _vwprintf_p_l(const wchar_t *_Format,_locale_t _Locale,va_list _ArgList);
  _CRTIMP int __cdecl _fwprintf_l(FILE *_File,const wchar_t *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _fwprintf_p_l(FILE *_File,const wchar_t *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _vfwprintf_l(FILE *_File,const wchar_t *_Format,_locale_t _Locale,va_list _ArgList);
  _CRTIMP int __cdecl _vfwprintf_p_l(FILE *_File,const wchar_t *_Format,_locale_t _Locale,va_list _ArgList);
  _CRTIMP int __cdecl _swprintf_c_l(wchar_t *_DstBuf,size_t _MaxCount,const wchar_t *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _swprintf_p_l(wchar_t *_DstBuf,size_t _MaxCount,const wchar_t *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _vswprintf_c_l(wchar_t *_DstBuf,size_t _MaxCount,const wchar_t *_Format,_locale_t _Locale,va_list _ArgList);
  _CRTIMP int __cdecl _vswprintf_p_l(wchar_t *_DstBuf,size_t _MaxCount,const wchar_t *_Format,_locale_t _Locale,va_list _ArgList);
  _CRTIMP int __cdecl _scwprintf_l(const wchar_t *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _scwprintf_p_l(const wchar_t *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _vscwprintf_p_l(const wchar_t *_Format,_locale_t _Locale,va_list _ArgList);
  _CRTIMP int __cdecl _snwprintf_l(wchar_t *_DstBuf,size_t _MaxCount,const wchar_t *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _vsnwprintf_l(wchar_t *_DstBuf,size_t _MaxCount,const wchar_t *_Format,_locale_t _Locale,va_list _ArgList);
  _CRTIMP int __cdecl __swprintf_l(wchar_t *_Dest,const wchar_t *_Format,_locale_t _Plocinfo,...);
  _CRTIMP int __cdecl __vswprintf_l(wchar_t *_Dest,const wchar_t *_Format,_locale_t _Plocinfo,va_list _Args);
  _CRTIMP int __cdecl _vscwprintf_l(const wchar_t *_Format,_locale_t _Locale,va_list _ArgList);
  _CRTIMP int __cdecl _fwscanf_l(FILE *_File,const wchar_t *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _swscanf_l(const wchar_t *_Src,const wchar_t *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _snwscanf_l(const wchar_t *_Src,size_t _MaxCount,const wchar_t *_Format,_locale_t _Locale,...);
  _CRTIMP int __cdecl _wscanf_l(const wchar_t *_Format,_locale_t _Locale,...);
#endif
#endif

  _CRTIMP size_t __cdecl _fread_nolock_s(void *_DstBuf,size_t _DstSize,size_t _ElementSize,size_t _Count,FILE *_File);

#ifdef __cplusplus
}
#endif
#endif
#endif
