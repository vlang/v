/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef __ASSERT_H_
#define __ASSERT_H_

#include <_mingw.h>
#ifdef __cplusplus
#include <stdlib.h>
#endif

#ifdef NDEBUG
#ifndef assert
#define assert(_Expression) ((void)0)
#endif
#else

#ifndef _CRT_TERMINATE_DEFINED
#define _CRT_TERMINATE_DEFINED
  void __cdecl __MINGW_NOTHROW exit(int _Code) __MINGW_ATTRIB_NORETURN;
 _CRTIMP void __cdecl __MINGW_NOTHROW _exit(int _Code) __MINGW_ATTRIB_NORETURN;
#if !defined __NO_ISOCEXT /* extern stub in static libmingwex.a */
/* C99 function name */
void __cdecl _Exit(int) __MINGW_ATTRIB_NORETURN;
__CRT_INLINE __MINGW_ATTRIB_NORETURN void __cdecl _Exit(int status)
{  _exit(status); }
#endif

#pragma push_macro("abort")
#undef abort
  void __cdecl __declspec(noreturn) abort(void);
#pragma pop_macro("abort")

#endif

#ifdef __cplusplus
extern "C" {
#endif


extern void __cdecl _wassert(const wchar_t *_Message,const wchar_t *_File,unsigned _Line);
extern void __cdecl _assert(const char *, const char *, unsigned);

#ifdef __cplusplus
}
#endif

#ifndef assert
//#define assert(_Expression) (void)((!!(_Expression)) || (_wassert(_CRT_WIDE(#_Expression),_CRT_WIDE(__FILE__),__LINE__),0))
#define assert(e) ((e) ? (void)0 : _assert(#e, __FILE__, __LINE__))
#endif

#endif

#if (__STDC_VERSION__ >= 201112L) && !defined(static_assert)
/* C11, section 7.2: The macro static_assert expands to _Static_assert. */
#define static_assert(exp, str) _Static_assert(exp, str) 
#endif

#endif
