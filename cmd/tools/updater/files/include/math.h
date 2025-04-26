/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef _MATH_H_
#define _MATH_H_

#if __GNUC__ >= 3
#pragma GCC system_header
#endif

#include <_mingw.h>

struct exception;

#pragma pack(push,_CRT_PACKING)

#define _DOMAIN 1
#define _SING 2
#define _OVERFLOW 3
#define _UNDERFLOW 4
#define _TLOSS 5
#define _PLOSS 6

#ifndef __STRICT_ANSI__
#ifndef	NO_OLDNAMES
#define DOMAIN _DOMAIN
#define SING _SING
#define OVERFLOW _OVERFLOW
#define UNDERFLOW _UNDERFLOW
#define TLOSS _TLOSS
#define PLOSS _PLOSS
#endif
#endif

#ifndef __STRICT_ANSI__
#define M_E 2.71828182845904523536
#define M_LOG2E 1.44269504088896340736
#define M_LOG10E 0.434294481903251827651
#define M_LN2 0.693147180559945309417
#define M_LN10 2.30258509299404568402
#define M_PI 3.14159265358979323846
#define M_PI_2 1.57079632679489661923
#define M_PI_4 0.785398163397448309616
#define M_1_PI 0.318309886183790671538
#define M_2_PI 0.636619772367581343076
#define M_2_SQRTPI 1.12837916709551257390
#define M_SQRT2 1.41421356237309504880
#define M_SQRT1_2 0.707106781186547524401
#endif

#ifndef __STRICT_ANSI__
/* See also float.h  */
#ifndef __MINGW_FPCLASS_DEFINED
#define __MINGW_FPCLASS_DEFINED 1
#define	_FPCLASS_SNAN	0x0001	/* Signaling "Not a Number" */
#define	_FPCLASS_QNAN	0x0002	/* Quiet "Not a Number" */
#define	_FPCLASS_NINF	0x0004	/* Negative Infinity */
#define	_FPCLASS_NN	0x0008	/* Negative Normal */
#define	_FPCLASS_ND	0x0010	/* Negative Denormal */
#define	_FPCLASS_NZ	0x0020	/* Negative Zero */
#define	_FPCLASS_PZ	0x0040	/* Positive Zero */
#define	_FPCLASS_PD	0x0080	/* Positive Denormal */
#define	_FPCLASS_PN	0x0100	/* Positive Normal */
#define	_FPCLASS_PINF	0x0200	/* Positive Infinity */
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _EXCEPTION_DEFINED
#define _EXCEPTION_DEFINED
  struct _exception {
    int type;
    char *name;
    double arg1;
    double arg2;
    double retval;
  };
#endif

#ifndef _COMPLEX_DEFINED
#define _COMPLEX_DEFINED
  struct _complex {
    double x,y;
  };
#endif

#define EDOM 33
#define ERANGE 34

#ifndef _HUGE
#ifdef _MSVCRT_
  extern double *_HUGE;
#else
  extern double *_imp___HUGE;
#define _HUGE	(*_imp___HUGE)
#endif
#endif

#define HUGE_VAL _HUGE

#ifndef _CRT_ABS_DEFINED
#define _CRT_ABS_DEFINED
  int __cdecl abs(int _X);
  long __cdecl labs(long _X);
#endif
  double __cdecl acos(double _X);
  double __cdecl asin(double _X);
  double __cdecl atan(double _X);
  double __cdecl atan2(double _Y,double _X);
#ifndef _SIGN_DEFINED
#define _SIGN_DEFINED
  _CRTIMP double __cdecl _copysign (double _Number,double _Sign);
  _CRTIMP double __cdecl _chgsign (double _X);
#endif
  double __cdecl cos(double _X);
  double __cdecl cosh(double _X);
  double __cdecl exp(double _X);
  double __cdecl expm1(double _X);
  double __cdecl fabs(double _X);
  double __cdecl fmod(double _X,double _Y);
  double __cdecl log(double _X);
  double __cdecl log10(double _X);
  double __cdecl pow(double _X,double _Y);
  double __cdecl sin(double _X);
  double __cdecl sinh(double _X);
  double __cdecl tan(double _X);
  double __cdecl tanh(double _X);
  double __cdecl sqrt(double _X);
#ifndef _CRT_ATOF_DEFINED
#define _CRT_ATOF_DEFINED
  double __cdecl atof(const char *_String);
  double __cdecl _atof_l(const char *_String,_locale_t _Locale);
#endif

  _CRTIMP double __cdecl _cabs(struct _complex _ComplexA);
  double __cdecl ceil(double _X);
  double __cdecl floor(double _X);
  double __cdecl frexp(double _X,int *_Y);
  double __cdecl _hypot(double _X,double _Y);
  _CRTIMP double __cdecl _j0(double _X);
  _CRTIMP double __cdecl _j1(double _X);
  _CRTIMP double __cdecl _jn(int _X,double _Y);
  double __cdecl ldexp(double _X,int _Y);
#ifndef _CRT_MATHERR_DEFINED
#define _CRT_MATHERR_DEFINED
  int __cdecl _matherr(struct _exception *_Except);
#endif
  double __cdecl modf(double _X,double *_Y);
  _CRTIMP double __cdecl _y0(double _X);
  _CRTIMP double __cdecl _y1(double _X);
  _CRTIMP double __cdecl _yn(int _X,double _Y);

#if(defined(_X86_) && !defined(__x86_64))
  _CRTIMP int __cdecl _set_SSE2_enable(int _Flag);
  /* from libmingwex */
  float __cdecl _hypotf(float _X,float _Y);
#endif

  float frexpf(float _X,int *_Y);
  float __cdecl ldexpf(float _X,int _Y);
  long double __cdecl ldexpl(long double _X,int _Y);
  float __cdecl acosf(float _X);
  float __cdecl asinf(float _X);
   float __cdecl atanf(float _X);
   float __cdecl atan2f(float _X,float _Y);
   float __cdecl cosf(float _X);
   float __cdecl sinf(float _X);
   float __cdecl tanf(float _X);
   float __cdecl coshf(float _X);
   float __cdecl sinhf(float _X);
   float __cdecl tanhf(float _X);
   float __cdecl expf(float _X);
   float __cdecl expm1f(float _X);
   float __cdecl logf(float _X);
   float __cdecl log10f(float _X);
   float __cdecl modff(float _X,float *_Y);
   float __cdecl powf(float _X,float _Y);
   float __cdecl sqrtf(float _X);
   float __cdecl ceilf(float _X);
   float __cdecl floorf(float _X);
  float __cdecl fmodf(float _X,float _Y);
   float __cdecl _hypotf(float _X,float _Y);
  float __cdecl fabsf(float _X);
#if !defined(__ia64__)
   /* from libmingwex */
   float __cdecl _copysignf (float _Number,float _Sign);
   float __cdecl _chgsignf (float _X);
   float __cdecl _logbf(float _X);
   float __cdecl _nextafterf(float _X,float _Y);
   int __cdecl _finitef(float _X);
   int __cdecl _isnanf(float _X);
   int __cdecl _fpclassf(float _X);
#endif

#ifndef	NO_OLDNAMES
#define matherr _matherr

#define HUGE	_HUGE
  /*	double __cdecl cabs(struct _complex _X); */
  double __cdecl hypot(double _X,double _Y);
  _CRTIMP double __cdecl j0(double _X);
  _CRTIMP double __cdecl j1(double _X);
  _CRTIMP double __cdecl jn(int _X,double _Y);
  _CRTIMP double __cdecl y0(double _X);
  _CRTIMP double __cdecl y1(double _X);
  _CRTIMP double __cdecl yn(int _X,double _Y);
#endif

#ifndef __NO_ISOCEXT
#if (defined (__STDC_VERSION__) && __STDC_VERSION__ >= 199901L) \
  || !defined __STRICT_ANSI__ || defined __GLIBCPP__

#define NAN (0.0F/0.0F)
#define HUGE_VALF (1.0F/0.0F)
#define HUGE_VALL (1.0L/0.0L)
#define INFINITY (1.0F/0.0F)


#define FP_NAN		0x0100
#define FP_NORMAL	0x0400
#define FP_INFINITE	(FP_NAN | FP_NORMAL)
#define FP_ZERO		0x4000
#define FP_SUBNORMAL	(FP_NORMAL | FP_ZERO)
  /* 0x0200 is signbit mask */


  /*
  We can't __CRT_INLINE float or double, because we want to ensure truncation
  to semantic type before classification. 
  (A normal long double value might become subnormal when 
  converted to double, and zero when converted to float.)
  */

  extern int __cdecl __fpclassifyf (float);
  extern int __cdecl __fpclassifyd (double);
  extern int __cdecl __fpclassifyl (long double);

/* Implemented at tcc/tcc_libm.h
#define fpclassify(x) (sizeof (x) == sizeof (float) ? __fpclassifyf (x)	  \
  : sizeof (x) == sizeof (double) ? __fpclassify (x) \
  : __fpclassifyl (x))
*/
#define fpclassify(x) \
  _Generic(x, float: __fpclassifyf, double: __fpclassifyd, long double: __fpclassifyl)(x)

  /* 7.12.3.2 */
#define isfinite(x) ((fpclassify(x) & FP_NAN) == 0)

  /* 7.12.3.3 */
#define isinf(x) (fpclassify(x) == FP_INFINITE)

  /* 7.12.3.4 */
  /* We don't need to worry about truncation here:
  A NaN stays a NaN. */
#define isnan(x) (fpclassify(x) == FP_NAN)

  /* 7.12.3.5 */
#define isnormal(x) (fpclassify(x) == FP_NORMAL)

  /* 7.12.3.6 The signbit macro */

  extern int __cdecl __signbitf (float);
  extern int __cdecl __signbit (double);
  extern int __cdecl __signbitl (long double);

/* Implemented at tcc/tcc_libm.h
#define signbit(x) (sizeof (x) == sizeof (float) ? __signbitf (x)	\
  : sizeof (x) == sizeof (double) ? __signbit (x)	\
  : __signbitl (x))
*/
#define signbit(x) \
  _Generic(x, float: __signbitf, double: __signbit, long double: __signbitl)(x)

  extern double __cdecl exp2(double);
  extern float __cdecl exp2f(float);
  extern long double __cdecl exp2l(long double);

#define FP_ILOGB0 ((int)0x80000000)
#define FP_ILOGBNAN ((int)0x80000000)
  extern int __cdecl ilogb (double);
  extern int __cdecl ilogbf (float);
  extern int __cdecl ilogbl (long double);

  extern double __cdecl log1p(double);
  extern float __cdecl log1pf(float);
  extern long double __cdecl log1pl(long double);

  extern double __cdecl log2 (double);
  extern float __cdecl log2f (float);
  extern long double __cdecl log2l (long double);

  extern double __cdecl logb (double);
  extern float __cdecl logbf (float);
  extern long double __cdecl logbl (long double);

  extern long double __cdecl modfl (long double, long double*);

  /* 7.12.6.13 */
  extern double __cdecl scalbn (double, int);
  extern float __cdecl scalbnf (float, int);
  extern long double __cdecl scalbnl (long double, int);

  extern double __cdecl scalbln (double, long);
  extern float __cdecl scalblnf (float, long);
  extern long double __cdecl scalblnl (long double, long);

  /* 7.12.7.1 */
  /* Implementations adapted from Cephes versions */ 
  extern double __cdecl cbrt (double);
  extern float __cdecl cbrtf (float);
  extern long double __cdecl cbrtl (long double);

  extern double __cdecl hypot (double, double);
  extern float __cdecl hypotf (float, float);
  extern long double __cdecl hypotl (long double, long double);

  extern long double __cdecl powl (long double, long double);
  extern long double __cdecl expl(long double);
  extern long double __cdecl expm1l(long double);
  extern long double __cdecl coshl(long double);
  extern long double __cdecl fabsl (long double);
  extern long double __cdecl acosl(long double);
  extern long double __cdecl asinl(long double);
  extern long double __cdecl atanl(long double);
  extern long double __cdecl atan2l(long double,long double);
  extern long double __cdecl sinhl(long double);
  extern long double __cdecl tanhl(long double);

  /* 7.12.8.1 The erf functions  */
  extern double __cdecl erf (double);
  extern float __cdecl erff (float);
  /* TODO
  extern long double __cdecl erfl (long double);
  */ 

  /* 7.12.8.2 The erfc functions  */
  extern double __cdecl erfc (double);
  extern float __cdecl erfcf (float);
  /* TODO
  extern long double __cdecl erfcl (long double);
  */ 

  /* 7.12.8.3 The lgamma functions */
  extern double __cdecl lgamma (double);
  extern float __cdecl lgammaf (float);
  extern long double __cdecl lgammal (long double);

  /* 7.12.8.4 The tgamma functions */
  extern double __cdecl tgamma (double);
  extern float __cdecl tgammaf (float);
  extern long double __cdecl tgammal (long double);

  extern long double __cdecl ceill (long double);
  extern long double __cdecl floorl (long double);
  extern long double __cdecl frexpl(long double,int *);
  extern long double __cdecl log10l(long double);
  extern long double __cdecl logl(long double);
  extern long double __cdecl cosl(long double);
  extern long double __cdecl sinl(long double);
  extern long double __cdecl tanl(long double);
  extern long double sqrtl(long double);

  /* 7.12.9.3 */
  extern double __cdecl nearbyint ( double);
  extern float __cdecl nearbyintf (float);
  extern long double __cdecl nearbyintl (long double);

  /* 7.12.9.4 */
  /* round, using fpu control word settings */
  extern double __cdecl rint (double);
  extern float __cdecl rintf (float);
  extern long double __cdecl rintl (long double);

  extern long __cdecl lrint (double);
  extern long __cdecl lrintf (float);
  extern long __cdecl lrintl (long double);

  extern long long __cdecl llrint (double);
  extern long long __cdecl llrintf (float);
  extern long long __cdecl llrintl (long double);

  #define FE_TONEAREST	0x0000
  #define FE_DOWNWARD	0x0400
  #define FE_UPWARD	0x0800
  #define FE_TOWARDZERO	0x0c00

  /* 7.12.9.6 */
  /* round away from zero, regardless of fpu control word settings */
  extern double __cdecl round (double);
  extern float __cdecl roundf (float);
  extern long double __cdecl roundl (long double);

  /* 7.12.9.7  */
  extern long __cdecl lround (double);
  extern long __cdecl lroundf (float);
  extern long __cdecl lroundl (long double);

  extern long long __cdecl llround (double);
  extern long long __cdecl llroundf (float);
  extern long long __cdecl llroundl (long double);

  /* 7.12.9.8 */
  /* round towards zero, regardless of fpu control word settings */
  extern double __cdecl trunc (double);
  extern float __cdecl truncf (float);
  extern long double __cdecl truncl (long double);

  extern long double __cdecl fmodl (long double, long double);

  /* 7.12.10.2 */ 
  extern double __cdecl remainder (double, double);
  extern float __cdecl remainderf (float, float);
  extern long double __cdecl remainderl (long double, long double);

  /* 7.12.10.3 */
  extern double __cdecl remquo(double, double, int *);
  extern float __cdecl remquof(float, float, int *);
  extern long double __cdecl remquol(long double, long double, int *);

  /* 7.12.11.1 */
  extern double __cdecl copysign (double, double); /* in libmoldname.a */
  extern float __cdecl copysignf (float, float);
  extern long double __cdecl copysignl (long double, long double);

  /* 7.12.11.2 Return a NaN */
  extern double __cdecl nan(const char *tagp);
  extern float __cdecl nanf(const char *tagp);
  extern long double __cdecl nanl(const char *tagp);

#ifndef __STRICT_ANSI__
#define _nan() nan("")
#define _nanf() nanf("")
#define _nanl() nanl("")
#endif

  /* 7.12.11.3 */
  extern double __cdecl nextafter (double, double); /* in libmoldname.a */
  extern float __cdecl nextafterf (float, float);
  extern long double __cdecl nextafterl (long double, long double);

  /* 7.12.11.4 The nexttoward functions: TODO */

  /* 7.12.12.1 */
  /*  x > y ? (x - y) : 0.0  */
  extern double __cdecl fdim (double x, double y);
  extern float __cdecl fdimf (float x, float y);
  extern long double __cdecl fdiml (long double x, long double y);

  /* fmax and fmin.
  NaN arguments are treated as missing data: if one argument is a NaN
  and the other numeric, then these functions choose the numeric
  value. */

  /* 7.12.12.2 */
  extern double __cdecl fmax  (double, double);
  extern float __cdecl fmaxf (float, float);
  extern long double __cdecl fmaxl (long double, long double);

  /* 7.12.12.3 */
  extern double __cdecl fmin (double, double);
  extern float __cdecl fminf (float, float);
  extern long double __cdecl fminl (long double, long double);

  /* 7.12.13.1 */
  /* return x * y + z as a ternary op */ 
  extern double __cdecl fma (double, double, double);
  extern float __cdecl fmaf (float, float, float);
  extern long double __cdecl fmal (long double, long double, long double);


#endif /* __STDC_VERSION__ >= 199901L */
#endif /* __NO_ISOCEXT */

#ifdef __cplusplus
}
#endif
#pragma pack(pop)

/* 7.12.14 */
/* 
 *  With these functions, comparisons involving quiet NaNs set the FP
 *  condition code to "unordered".  The IEEE floating-point spec
 *  dictates that the result of floating-point comparisons should be
 *  false whenever a NaN is involved, with the exception of the != op, 
 *  which always returns true: yes, (NaN != NaN) is true).
 */

#endif /* End _MATH_H_ */

