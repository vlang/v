/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef _INC_CTYPE
#define _INC_CTYPE

#include <_mingw.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifndef WEOF
#define WEOF (wint_t)(0xFFFF)
#endif

#ifndef _CRT_CTYPEDATA_DEFINED
#define _CRT_CTYPEDATA_DEFINED
#ifndef _CTYPE_DISABLE_MACROS

#ifndef __PCTYPE_FUNC
#define __PCTYPE_FUNC __pctype_func()
#ifdef _MSVCRT_
#define __pctype_func()	(_pctype)
#else
#define __pctype_func()	(*_imp___pctype)
#endif
#endif

#ifndef _pctype
#ifdef _MSVCRT_
  extern unsigned short *_pctype;
#else
  extern unsigned short **_imp___pctype;
#define _pctype (*_imp___pctype)
#endif
#endif

#endif
#endif

#ifndef _CRT_WCTYPEDATA_DEFINED
#define _CRT_WCTYPEDATA_DEFINED
#ifndef _CTYPE_DISABLE_MACROS
#ifndef _wctype
#ifdef _MSVCRT_
  extern unsigned short *_wctype;
#else
  extern unsigned short **_imp___wctype;
#define _wctype (*_imp___wctype)
#endif
#endif
#ifdef _MSVCRT_
#define __pwctype_func() (_pwctype)
#ifndef _pwctype
  extern unsigned short *_pwctype;
#endif
#else
#define __pwctype_func() (*_imp___pwctype)
#ifndef _pwctype
  extern unsigned short **_imp___pwctype;
#define _pwctype (*_imp___pwctype)
#endif
#endif
#endif
#endif

  /* CRT stuff */
#if 1
  extern const unsigned char __newclmap[];
  extern const unsigned char __newcumap[];
  extern pthreadlocinfo __ptlocinfo;
  extern pthreadmbcinfo __ptmbcinfo;
  extern int __globallocalestatus;
  extern int __locale_changed;
  extern struct threadlocaleinfostruct __initiallocinfo;
  extern _locale_tstruct __initiallocalestructinfo;
  pthreadlocinfo __cdecl __updatetlocinfo(void);
  pthreadmbcinfo __cdecl __updatetmbcinfo(void);
#endif

#define _UPPER 0x1
#define _LOWER 0x2
#define _DIGIT 0x4
#define _SPACE 0x8

#define _PUNCT 0x10
#define _CONTROL 0x20
#define _BLANK 0x40
#define _HEX 0x80

#define _LEADBYTE 0x8000
#define _ALPHA (0x0100|_UPPER|_LOWER)

#ifndef _CTYPE_DEFINED
#define _CTYPE_DEFINED

  _CRTIMP int __cdecl _isctype(int _C,int _Type);
  _CRTIMP int __cdecl _isctype_l(int _C,int _Type,_locale_t _Locale);
  _CRTIMP int __cdecl isalpha(int _C);
  _CRTIMP int __cdecl _isalpha_l(int _C,_locale_t _Locale);
  _CRTIMP int __cdecl isupper(int _C);
  _CRTIMP int __cdecl _isupper_l(int _C,_locale_t _Locale);
  _CRTIMP int __cdecl islower(int _C);
  _CRTIMP int __cdecl _islower_l(int _C,_locale_t _Locale);
  _CRTIMP int __cdecl isdigit(int _C);
  _CRTIMP int __cdecl _isdigit_l(int _C,_locale_t _Locale);
  _CRTIMP int __cdecl isxdigit(int _C);
  _CRTIMP int __cdecl _isxdigit_l(int _C,_locale_t _Locale);
  _CRTIMP int __cdecl isspace(int _C);
  _CRTIMP int __cdecl _isspace_l(int _C,_locale_t _Locale);
  _CRTIMP int __cdecl ispunct(int _C);
  _CRTIMP int __cdecl _ispunct_l(int _C,_locale_t _Locale);
  _CRTIMP int __cdecl isalnum(int _C);
  _CRTIMP int __cdecl _isalnum_l(int _C,_locale_t _Locale);
  _CRTIMP int __cdecl isprint(int _C);
  _CRTIMP int __cdecl _isprint_l(int _C,_locale_t _Locale);
  _CRTIMP int __cdecl isgraph(int _C);
  _CRTIMP int __cdecl _isgraph_l(int _C,_locale_t _Locale);
  _CRTIMP int __cdecl iscntrl(int _C);
  _CRTIMP int __cdecl _iscntrl_l(int _C,_locale_t _Locale);
  _CRTIMP int __cdecl toupper(int _C);
  _CRTIMP int __cdecl tolower(int _C);
  _CRTIMP int __cdecl _tolower(int _C);
  _CRTIMP int __cdecl _tolower_l(int _C,_locale_t _Locale);
  _CRTIMP int __cdecl _toupper(int _C);
  _CRTIMP int __cdecl _toupper_l(int _C,_locale_t _Locale);
  _CRTIMP int __cdecl __isascii(int _C);
  _CRTIMP int __cdecl __toascii(int _C);
  _CRTIMP int __cdecl __iscsymf(int _C);
  _CRTIMP int __cdecl __iscsym(int _C);

#if (defined (__STDC_VERSION__) && __STDC_VERSION__ >= 199901L) || !defined (NO_OLDNAMES)
int __cdecl isblank(int _C);
#endif
#endif

#ifndef _WCTYPE_DEFINED
#define _WCTYPE_DEFINED

  int __cdecl iswalpha(wint_t _C);
  _CRTIMP int __cdecl _iswalpha_l(wint_t _C,_locale_t _Locale);
  int __cdecl iswupper(wint_t _C);
  _CRTIMP int __cdecl _iswupper_l(wint_t _C,_locale_t _Locale);
  int __cdecl iswlower(wint_t _C);
  _CRTIMP int __cdecl _iswlower_l(wint_t _C,_locale_t _Locale);
  int __cdecl iswdigit(wint_t _C);
  _CRTIMP int __cdecl _iswdigit_l(wint_t _C,_locale_t _Locale);
  int __cdecl iswxdigit(wint_t _C);
  _CRTIMP int __cdecl _iswxdigit_l(wint_t _C,_locale_t _Locale);
  int __cdecl iswspace(wint_t _C);
  _CRTIMP int __cdecl _iswspace_l(wint_t _C,_locale_t _Locale);
  int __cdecl iswpunct(wint_t _C);
  _CRTIMP int __cdecl _iswpunct_l(wint_t _C,_locale_t _Locale);
  int __cdecl iswalnum(wint_t _C);
  _CRTIMP int __cdecl _iswalnum_l(wint_t _C,_locale_t _Locale);
  int __cdecl iswprint(wint_t _C);
  _CRTIMP int __cdecl _iswprint_l(wint_t _C,_locale_t _Locale);
  int __cdecl iswgraph(wint_t _C);
  _CRTIMP int __cdecl _iswgraph_l(wint_t _C,_locale_t _Locale);
  int __cdecl iswcntrl(wint_t _C);
  _CRTIMP int __cdecl _iswcntrl_l(wint_t _C,_locale_t _Locale);
  int __cdecl iswascii(wint_t _C);
  int __cdecl isleadbyte(int _C);
  _CRTIMP int __cdecl _isleadbyte_l(int _C,_locale_t _Locale);
  wint_t __cdecl towupper(wint_t _C);
  _CRTIMP wint_t __cdecl _towupper_l(wint_t _C,_locale_t _Locale);
  wint_t __cdecl towlower(wint_t _C);
  _CRTIMP wint_t __cdecl _towlower_l(wint_t _C,_locale_t _Locale);
  int __cdecl iswctype(wint_t _C,wctype_t _Type);
  _CRTIMP int __cdecl _iswctype_l(wint_t _C,wctype_t _Type,_locale_t _Locale);
  _CRTIMP int __cdecl __iswcsymf(wint_t _C);
  _CRTIMP int __cdecl _iswcsymf_l(wint_t _C,_locale_t _Locale);
  _CRTIMP int __cdecl __iswcsym(wint_t _C);
  _CRTIMP int __cdecl _iswcsym_l(wint_t _C,_locale_t _Locale);
  int __cdecl is_wctype(wint_t _C,wctype_t _Type);

#if (defined (__STDC_VERSION__) && __STDC_VERSION__ >= 199901L) || !defined (NO_OLDNAMES)
int __cdecl iswblank(wint_t _C);
#endif
#endif

#ifndef _CTYPE_DISABLE_MACROS

#ifndef MB_CUR_MAX
#define MB_CUR_MAX ___mb_cur_max_func()
#ifndef __mb_cur_max
#ifdef _MSVCRT_
  extern int __mb_cur_max;
#else
#define __mb_cur_max	(*_imp____mb_cur_max)
  extern int *_imp____mb_cur_max;
#endif
#endif
#ifdef _MSVCRT_
#define ___mb_cur_max_func() (__mb_cur_max)
#else
#define ___mb_cur_max_func() (*_imp____mb_cur_max)
#endif
#endif

#define __chvalidchk(a,b) (__PCTYPE_FUNC[(a)] & (b))
#define _chvalidchk_l(_Char,_Flag,_Locale) (!_Locale ? __chvalidchk(_Char,_Flag) : ((_locale_t)_Locale)->locinfo->pctype[_Char] & (_Flag))
#define _ischartype_l(_Char,_Flag,_Locale) (((_Locale)!=NULL && (((_locale_t)(_Locale))->locinfo->mb_cur_max) > 1) ? _isctype_l(_Char,(_Flag),_Locale) : _chvalidchk_l(_Char,_Flag,_Locale))
#define _isalpha_l(_Char,_Locale) _ischartype_l(_Char,_ALPHA,_Locale)
#define _isupper_l(_Char,_Locale) _ischartype_l(_Char,_UPPER,_Locale)
#define _islower_l(_Char,_Locale) _ischartype_l(_Char,_LOWER,_Locale)
#define _isdigit_l(_Char,_Locale) _ischartype_l(_Char,_DIGIT,_Locale)
#define _isxdigit_l(_Char,_Locale) _ischartype_l(_Char,_HEX,_Locale)
#define _isspace_l(_Char,_Locale) _ischartype_l(_Char,_SPACE,_Locale)
#define _ispunct_l(_Char,_Locale) _ischartype_l(_Char,_PUNCT,_Locale)
#define _isalnum_l(_Char,_Locale) _ischartype_l(_Char,_ALPHA|_DIGIT,_Locale)
#define _isprint_l(_Char,_Locale) _ischartype_l(_Char,_BLANK|_PUNCT|_ALPHA|_DIGIT,_Locale)
#define _isgraph_l(_Char,_Locale) _ischartype_l(_Char,_PUNCT|_ALPHA|_DIGIT,_Locale)
#define _iscntrl_l(_Char,_Locale) _ischartype_l(_Char,_CONTROL,_Locale)
#define _tolower(_Char) ((_Char)-'A'+'a')
#define _toupper(_Char) ((_Char)-'a'+'A')
#define __isascii(_Char) ((unsigned)(_Char) < 0x80)
#define __toascii(_Char) ((_Char) & 0x7f)

#ifndef _WCTYPE_INLINE_DEFINED
#define _WCTYPE_INLINE_DEFINED

#undef _CRT_WCTYPE_NOINLINE
#ifndef __cplusplus
#define iswalpha(_c) (iswctype(_c,_ALPHA))
#define iswupper(_c) (iswctype(_c,_UPPER))
#define iswlower(_c) (iswctype(_c,_LOWER))
#define iswdigit(_c) (iswctype(_c,_DIGIT))
#define iswxdigit(_c) (iswctype(_c,_HEX))
#define iswspace(_c) (iswctype(_c,_SPACE))
#define iswpunct(_c) (iswctype(_c,_PUNCT))
#define iswalnum(_c) (iswctype(_c,_ALPHA|_DIGIT))
#define iswprint(_c) (iswctype(_c,_BLANK|_PUNCT|_ALPHA|_DIGIT))
#define iswgraph(_c) (iswctype(_c,_PUNCT|_ALPHA|_DIGIT))
#define iswcntrl(_c) (iswctype(_c,_CONTROL))
#define iswascii(_c) ((unsigned)(_c) < 0x80)
#define _iswalpha_l(_c,_p) (_iswctype_l(_c,_ALPHA,_p))
#define _iswupper_l(_c,_p) (_iswctype_l(_c,_UPPER,_p))
#define _iswlower_l(_c,_p) (_iswctype_l(_c,_LOWER,_p))
#define _iswdigit_l(_c,_p) (_iswctype_l(_c,_DIGIT,_p))
#define _iswxdigit_l(_c,_p) (_iswctype_l(_c,_HEX,_p))
#define _iswspace_l(_c,_p) (_iswctype_l(_c,_SPACE,_p))
#define _iswpunct_l(_c,_p) (_iswctype_l(_c,_PUNCT,_p))
#define _iswalnum_l(_c,_p) (_iswctype_l(_c,_ALPHA|_DIGIT,_p))
#define _iswprint_l(_c,_p) (_iswctype_l(_c,_BLANK|_PUNCT|_ALPHA|_DIGIT,_p))
#define _iswgraph_l(_c,_p) (_iswctype_l(_c,_PUNCT|_ALPHA|_DIGIT,_p))
#define _iswcntrl_l(_c,_p) (_iswctype_l(_c,_CONTROL,_p))
#endif
#endif

#define __iscsymf(_c) (isalpha(_c) || ((_c)=='_'))
#define __iscsym(_c) (isalnum(_c) || ((_c)=='_'))
#define __iswcsymf(_c) (iswalpha(_c) || ((_c)=='_'))
#define __iswcsym(_c) (iswalnum(_c) || ((_c)=='_'))
#define _iscsymf_l(_c,_p) (_isalpha_l(_c,_p) || ((_c)=='_'))
#define _iscsym_l(_c,_p) (_isalnum_l(_c,_p) || ((_c)=='_'))
#define _iswcsymf_l(_c,_p) (_iswalpha_l(_c,_p) || ((_c)=='_'))
#define _iswcsym_l(_c,_p) (_iswalnum_l(_c,_p) || ((_c)=='_'))
#endif

#ifndef	NO_OLDNAMES
#ifndef _CTYPE_DEFINED
  int __cdecl isascii(int _C);
  int __cdecl toascii(int _C);
  int __cdecl iscsymf(int _C);
  int __cdecl iscsym(int _C);
#else
#define isascii __isascii
#define toascii __toascii
#define iscsymf __iscsymf
#define iscsym __iscsym
#endif
#endif

#ifdef __cplusplus
}
#endif
#endif
