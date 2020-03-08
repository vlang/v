module compiler

const (
	c_common_macros = '

#define EMPTY_STRUCT_DECLARATION
#define EMPTY_STRUCT_INITIALIZATION 0
// Due to a tcc bug, the length of an array needs to be specified, but GCC crashes if it is...
#define EMPTY_ARRAY_OF_ELEMS(x,n) (x[])
#define TCCSKIP(x) x

#ifdef __TINYC__
#undef EMPTY_STRUCT_DECLARATION
#undef EMPTY_STRUCT_INITIALIZATION
#define EMPTY_STRUCT_DECLARATION char _dummy
#define EMPTY_STRUCT_INITIALIZATION 0
#undef EMPTY_ARRAY_OF_ELEMS
#define EMPTY_ARRAY_OF_ELEMS(x,n) (x[n])
#undef TCCSKIP
#define TCCSKIP(x)
#endif

// for __offset_of
#ifndef __offsetof
#define __offsetof(s,memb) \\
    ((size_t)((char *)&((s *)0)->memb - (char *)0))
#endif

#define OPTION_CAST(x) (x)

#ifndef V64_PRINTFORMAT
#ifdef PRIx64
#define V64_PRINTFORMAT "0x%"PRIx64
#elif defined(__WIN32__)
#define V64_PRINTFORMAT "0x%I64x"
#elif defined(__LINUX__) && defined(__LP64__)
#define V64_PRINTFORMAT "0x%lx"
#else
#define V64_PRINTFORMAT "0x%llx"
#endif
#endif

'
	c_headers = '

//#include <inttypes.h>  // int64_t etc
#include <stdio.h>  // TODO remove all these includes, define all function signatures and types manually
#include <stdlib.h>

//#include "fns.h"
#include <signal.h>
#include <stdarg.h> // for va_list
#include <string.h> // memcpy

#if INTPTR_MAX == INT32_MAX
    #define TARGET_IS_32BIT 1
#elif INTPTR_MAX == INT64_MAX
    #define TARGET_IS_64BIT 1
#else
    #error "The environment is not 32 or 64-bit."
#endif

#if defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__ || defined(__BYTE_ORDER) && __BYTE_ORDER == __BIG_ENDIAN || defined(__BIG_ENDIAN__) || defined(__ARMEB__) || defined(__THUMBEB__) || defined(__AARCH64EB__) || defined(_MIBSEB) || defined(__MIBSEB) || defined(__MIBSEB__)
    #define TARGET_ORDER_IS_BIG
#elif defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__ || defined(__BYTE_ORDER) && __BYTE_ORDER == __LITTLE_ENDIAN || defined(__LITTLE_ENDIAN__) || defined(__ARMEL__) || defined(__THUMBEL__) || defined(__AARCH64EL__) || defined(_MIPSEL) || defined(__MIPSEL) || defined(__MIPSEL__) || defined(_M_AMD64) || defined(_M_X64) || defined(_M_IX86)
    #define TARGET_ORDER_IS_LITTLE
#else
    #error "Unknown architecture endianness"
#endif

#ifndef _WIN32
#include <ctype.h>
#include <locale.h> // tolower
#include <sys/time.h>
#include <unistd.h> // sleep
#endif

#if defined(__CYGWIN__) && !defined(_WIN32)
#error Cygwin is not supported, please use MinGW or Visual Studio.
#endif


#ifdef __linux__
#include <sys/types.h>
#include <sys/wait.h> // os__wait uses wait on nix
#endif

#ifdef __FreeBSD__
#include <sys/types.h>
#include <sys/wait.h> // os__wait uses wait on nix
#endif

#ifdef __DragonFly__
#include <sys/types.h>
#include <sys/wait.h> // os__wait uses wait on nix
#endif

#ifdef __OpenBSD__
#include <sys/types.h>
#include <sys/resource.h>
#include <sys/wait.h> // os__wait uses wait on nix
#endif

#ifdef __NetBSD__
#include <sys/wait.h> // os__wait uses wait on nix
#endif

$c_common_macros

#ifdef _WIN32
#define WINVER 0x0600
#ifdef _WIN32_WINNT
#undef _WIN32_WINNT
#endif
#define _WIN32_WINNT 0x0600
#define WIN32_LEAN_AND_MEAN
#define _UNICODE
#define UNICODE
#include <windows.h>

#include <io.h> // _waccess
#include <direct.h> // _wgetcwd
//#include <WinSock2.h>
#ifdef _MSC_VER

// On MSVC these are the same (as long as /volatile:ms is passed)
#define _Atomic volatile

// MSVC cannot parse some things properly
#undef EMPTY_STRUCT_DECLARATION
#undef OPTION_CAST

#define EMPTY_STRUCT_DECLARATION int ____dummy_variable
#define OPTION_CAST(x)

#include <dbghelp.h>
#pragma comment(lib, "Dbghelp.lib")

#endif

#else
#include <pthread.h>
#endif


//============================== HELPER C MACROS =============================*/
#define _PUSH(arr, val, tmp, tmp_typ) {tmp_typ tmp = (val); array_push(arr, &tmp);}
#define _PUSH_MANY(arr, val, tmp, tmp_typ) {tmp_typ tmp = (val); array_push_many(arr, tmp.data, tmp.len);}
#define _IN(typ, val, arr) array_##typ##_contains(arr, val)
#define _IN_MAP(val, m) map_exists(m, val)
#define DEFAULT_EQUAL(a, b) (a == b)
#define DEFAULT_NOT_EQUAL(a, b) (a != b)
#define DEFAULT_LT(a, b) (a < b)
#define DEFAULT_LE(a, b) (a <= b)
#define DEFAULT_GT(a, b) (a > b)
#define DEFAULT_GE(a, b) (a >= b)

// NB: macro_fXX_eq and macro_fXX_ne are NOT used
// in the generated C code. They are here just for
// completeness/testing.

#define macro_f64_eq(a, b) (a == b)
#define macro_f64_ne(a, b) (a != b)
#define macro_f64_lt(a, b) (a <  b)
#define macro_f64_le(a, b) (a <= b)
#define macro_f64_gt(a, b) (a >  b)
#define macro_f64_ge(a, b) (a >= b)

#define macro_f32_eq(a, b) (a == b)
#define macro_f32_ne(a, b) (a != b)
#define macro_f32_lt(a, b) (a <  b)
#define macro_f32_le(a, b) (a <= b)
#define macro_f32_gt(a, b) (a >  b)
#define macro_f32_ge(a, b) (a >= b)

//================================== GLOBALS =================================*/
byte g_str_buf[1024];
int load_so(byteptr);
void reload_so();

// ============== wyhash ==============
//	Author: Wang Yi <godspeed_china@yeah.net>
#ifndef wyhash_version_4
#define wyhash_version_4
#include	<stdint.h>
#include	<string.h>
#if defined(_MSC_VER) && defined(_M_X64)
#include <intrin.h>
#pragma	intrinsic(_umul128)
#endif
const	uint64_t	_wyp0=0xa0761d6478bd642full,	_wyp1=0xe7037ed1a0b428dbull,	_wyp2=0x8ebc6af09c88c6e3ull,	_wyp3=0x589965cc75374cc3ull,	_wyp4=0x1d8e4e27c47d124full;
static	inline	uint64_t	_wyrotr(uint64_t v, unsigned k) {	return	(v>>k)|(v<<(64-k));	}
static	inline	uint64_t	_wymum(uint64_t	A,	uint64_t	B) {
#ifdef	WYHASH32
	uint64_t    hh=(A>>32)*(B>>32), hl=(A>>32)*(unsigned)B, lh=(unsigned)A*(B>>32), ll=(uint64_t)(unsigned)A*(unsigned)B;
	return  _wyrotr(hl,32)^_wyrotr(lh,32)^hh^ll;
#else
	#ifdef __SIZEOF_INT128__
		__uint128_t	r=A;	r*=B;	return	(r>>64)^r;
	#elif	defined(_MSC_VER) && defined(_M_X64)
		A=_umul128(A, B, &B);	return	A^B;
	#else
		uint64_t	ha=A>>32,	hb=B>>32,	la=(uint32_t)A,	lb=(uint32_t)B,	hi, lo;
		uint64_t	rh=ha*hb,	rm0=ha*lb,	rm1=hb*la,	rl=la*lb,	t=rl+(rm0<<32),	c=t<rl;
		lo=t+(rm1<<32);	c+=lo<t;hi=rh+(rm0>>32)+(rm1>>32)+c;	return hi^lo;
	#endif
#endif
}
#ifndef WYHASH_LITTLE_ENDIAN
	#if	defined(_WIN32) || defined(__LITTLE_ENDIAN__) || (defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__)
		#define WYHASH_LITTLE_ENDIAN 1
	#elif	defined(__BIG_ENDIAN__) || (defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
		#define WYHASH_LITTLE_ENDIAN 0
	#endif
#endif
#if(WYHASH_LITTLE_ENDIAN) || defined(__TINYC__)
static	inline	uint64_t	_wyr8(const	uint8_t	*p)	{	uint64_t	v;	memcpy(&v,  p,  8);	return  v;	}
static	inline	uint64_t	_wyr4(const	uint8_t	*p)	{	unsigned	v;	memcpy(&v,  p,  4);	return  v;	}
#else
	#if defined(__GNUC__) || defined(__INTEL_COMPILER)
static	inline	uint64_t	_wyr8(const	uint8_t	*p)	{	uint64_t	v;	memcpy(&v,  p,  8);	return   __builtin_bswap64(v);	}
static	inline	uint64_t	_wyr4(const	uint8_t	*p)	{	unsigned	v;	memcpy(&v,  p,  4);	return   __builtin_bswap32(v);	}
	#elif	defined(_MSC_VER)
static	inline	uint64_t	_wyr8(const	uint8_t	*p)	{	uint64_t	v;	memcpy(&v,  p,  8);	return  _byteswap_uint64(v);}
static	inline	uint64_t	_wyr4(const	uint8_t	*p)	{	unsigned	v;	memcpy(&v,  p,  4);	return  _byteswap_ulong(v);	}
	#endif
#endif
static	inline	uint64_t	_wyr3(const	uint8_t	*p,	unsigned	k) {	return	(((uint64_t)p[0])<<16)|(((uint64_t)p[k>>1])<<8)|p[k-1];	}
static	inline	uint64_t	wyhash(const void* key,	uint64_t	len,	uint64_t	seed) {
	const	uint8_t	*p=(const	uint8_t*)key;	uint64_t	i=len&63;
	#if defined(__GNUC__) || defined(__INTEL_COMPILER)
		#define	_like_(x)	__builtin_expect(x,1)
		#define	_unlike_(x)	__builtin_expect(x,0)
	#else
		#define _like_(x)  (x)
		#define _unlike_(x)  (x)
	#endif
	if(_unlike_(!i)) {	}
	else	if(_unlike_(i<4))	seed=_wymum(_wyr3(p,i)^seed^_wyp0,seed^_wyp1);
	else	if(_like_(i<=8))	seed=_wymum(_wyr4(p)^seed^_wyp0,_wyr4(p+i-4)^seed^_wyp1);
	else	if(_like_(i<=16))	seed=_wymum(_wyr8(p)^seed^_wyp0,_wyr8(p+i-8)^seed^_wyp1);
	else	if(_like_(i<=24))	seed=_wymum(_wyr8(p)^seed^_wyp0,_wyr8(p+8)^seed^_wyp1)^_wymum(_wyr8(p+i-8)^seed^_wyp2,seed^_wyp3);
	else	if(_like_(i<=32))	seed=_wymum(_wyr8(p)^seed^_wyp0,_wyr8(p+8)^seed^_wyp1)^_wymum(_wyr8(p+16)^seed^_wyp2,_wyr8(p+i-8)^seed^_wyp3);
	else{	seed=_wymum(_wyr8(p)^seed^_wyp0,_wyr8(p+8)^seed^_wyp1)^_wymum(_wyr8(p+16)^seed^_wyp2,_wyr8(p+24)^seed^_wyp3)^_wymum(_wyr8(p+i-32)^seed^_wyp1,_wyr8(p+i-24)^seed^_wyp2)^_wymum(_wyr8(p+i-16)^seed^_wyp3,_wyr8(p+i-8)^seed^_wyp0);	}
	if(_like_(i==len))	return	_wymum(seed,len^_wyp4);
	uint64_t	see1=seed,	see2=seed,	see3=seed;
	for(p+=i,i=len-i;	_like_(i>=64); i-=64,p+=64) {
		seed=_wymum(_wyr8(p)^seed^_wyp0,_wyr8(p+8)^seed^_wyp1);		see1=_wymum(_wyr8(p+16)^see1^_wyp2,_wyr8(p+24)^see1^_wyp3);
		see2=_wymum(_wyr8(p+32)^see2^_wyp1,_wyr8(p+40)^see2^_wyp2);	see3=_wymum(_wyr8(p+48)^see3^_wyp3,_wyr8(p+56)^see3^_wyp0);
	}
	return	_wymum(seed^see1^see2,see3^len^_wyp4);
}
static	inline	uint64_t	wyhash64(uint64_t	A, uint64_t	B) {	return	_wymum(_wymum(A^_wyp0,	B^_wyp1),	_wyp2);	}
static	inline	uint64_t	wyrand(uint64_t	*seed) {	*seed+=_wyp0;	return	_wymum(*seed^_wyp1,*seed);	}
static	inline	double	wy2u01(uint64_t	r) {	const	double	_wynorm=1.0/(1ull<<52);	return	(r>>11)*_wynorm;	}
static	inline	double	wy2gau(uint64_t	r) {	const	double	_wynorm=1.0/(1ull<<20);	return	((r&0x1fffff)+((r>>21)&0x1fffff)+((r>>42)&0x1fffff))*_wynorm-3.0;	}
static inline uint64_t fastest_hash(const void *key, size_t len, uint64_t seed) {
  const uint8_t *p = (const uint8_t *)key;
  return _like_(len >= 4) ? (_wyr4(p) + _wyr4(p + len - 4)) * (_wyr4(p + (len >> 1) - 2) ^ seed) : (_like_(len) ? _wyr3(p, len) * (_wyp0 ^ seed) : seed);
}
#endif

'
	js_headers = '

var array_string = function() {}
var array_byte = function() {}
var array_int = function() {}
var byte = function() {}
var double = function() {}
var int = function() {}
var f64 = function() {}
var f32 = function() {}
var i64 = function() {}
var i32 = function() {}
var i16 = function() {}
var u64 = function() {}
var u32 = function() {}
var u16 = function() {}
var i8 = function() {}
var bool = function() {}
var rune = function() {}
var map_string = function() {}
var map_int = function() {}

'
	c_builtin_types = '

//#include <inttypes.h>  // int64_t etc
//#include <stdint.h>  // int64_t etc

//================================== 1TYPEDEFS ================================*/

typedef int64_t i64;
typedef int16_t i16;
typedef int8_t i8;
typedef uint64_t u64;
typedef uint32_t u32;
typedef uint16_t u16;
typedef uint8_t byte;
typedef uint32_t rune;
typedef float f32;
typedef double f64;
typedef unsigned char* byteptr;
typedef int* intptr;
typedef void* voidptr;
typedef char* charptr;
typedef struct array array;
typedef struct map map;
typedef array array_string;
typedef array array_int;
typedef array array_byte;
typedef array array_f32;
typedef array array_f64;
typedef array array_u16;
typedef array array_u32;
typedef array array_u64;
typedef map map_int;
typedef map map_string;
#ifndef bool
	typedef int bool;
	#define true 1
	#define false 0
#endif
'
	bare_c_headers = '

$c_common_macros

#ifndef exit
#define exit(rc) sys_exit(rc)
void sys_exit (int);
#endif

'
)

