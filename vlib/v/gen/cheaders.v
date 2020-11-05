module gen

// NB: @@@ here serve as placeholders.
// They will be replaced with correct strings
// for each constant, during C code generation.

const (
	// V_COMMIT_HASH is generated by cmd/tools/gen_vc.v .
	c_commit_hash_default = '
#ifndef V_COMMIT_HASH
	#define V_COMMIT_HASH "@@@"
#endif
'
	// V_CURRENT_COMMIT_HASH is updated, when V is rebuilt inside a git repo.
	c_current_commit_hash_default = '
#ifndef V_CURRENT_COMMIT_HASH
	#define V_CURRENT_COMMIT_HASH "@@@"
#endif
'

	c_common_macros = '
#define EMPTY_STRUCT_DECLARATION
#define EMPTY_STRUCT_INITIALIZATION 0
// Due to a tcc bug, the length of an array needs to be specified, but GCC crashes if it is...
#define EMPTY_ARRAY_OF_ELEMS(x,n) (x[])
#define TCCSKIP(x) x

#define __NOINLINE __attribute__((noinline))
#define __IRQHANDLER __attribute__((interrupt))

#if defined(__x86_64__)
#define __V_amd64  1
#endif
#if defined(__aarch64__) || defined(__arm64__)
#define __V_aarch64  1
#endif

// Using just __GNUC__ for detecting gcc, is not reliable because other compilers define it too:
#ifdef __GNUC__
	#define __V_GCC__
#endif
#ifdef __TINYC__
	#undef __V_GCC__
#endif
#ifdef __cplusplus
	#undef __V_GCC__
#endif
#ifdef __clang__
	#undef __V_GCC__
#endif
#ifdef _MSC_VER
	#undef __V_GCC__
#endif

#ifdef __TINYC__
	#undef EMPTY_STRUCT_DECLARATION
	#undef EMPTY_STRUCT_INITIALIZATION
	#define EMPTY_STRUCT_DECLARATION char _dummy
	#define EMPTY_STRUCT_INITIALIZATION 0
	#undef EMPTY_ARRAY_OF_ELEMS
	#define EMPTY_ARRAY_OF_ELEMS(x,n) (x[n])
	#undef __NOINLINE
	#undef __IRQHANDLER
	// tcc does not support inlining at all
	#define __NOINLINE
	#define __IRQHANDLER
	#undef TCCSKIP
	#define TCCSKIP(x)
	// #include <byteswap.h>
	#ifndef _WIN32
		#include <execinfo.h>
		int tcc_backtrace(const char *fmt, ...);
	#endif
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
	#elif defined(__linux__) && defined(__LP64__)
		#define V64_PRINTFORMAT "0x%lx"
	#else
		#define V64_PRINTFORMAT "0x%llx"
	#endif
#endif
'
	c_headers = '
// c_headers
typedef int (*qsort_callback_func)(const void*, const void*);
#include <stdio.h>  // TODO remove all these includes, define all function signatures and types manually
#include <stdlib.h>

#ifdef __cplusplus
	#include <utility>
	#define _MOV std::move
#else
	#define _MOV
#endif

#ifndef _WIN32
	#if defined __has_include
		#if __has_include (<execinfo.h>)
			#include <execinfo.h>
		#else
			// Most probably musl OR __ANDROID__ ...
			int backtrace (void **__array, int __size) { return 0; }
			char **backtrace_symbols (void *const *__array, int __size){ return 0; }
			void backtrace_symbols_fd (void *const *__array, int __size, int __fd){}
		#endif
	#endif
#endif

//#include "fns.h"
#include <signal.h>
#include <stdarg.h> // for va_list
#include <string.h> // memcpy
#include <assert.h>

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
	extern char **environ;
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

#ifdef __sun
	#include <sys/types.h>
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
	#ifndef _UNICODE
	#define _UNICODE
	#endif
	#ifndef UNICODE
	#define UNICODE
	#endif
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
		#undef __NOINLINE
		#undef __IRQHANDLER
		#define __NOINLINE __declspec(noinline)
		#define __IRQHANDLER __declspec(naked)

		#include <dbghelp.h>
		#pragma comment(lib, "Dbghelp.lib")

		extern wchar_t **_wenviron;
	#elif !defined(SRWLOCK_INIT)
		// these seem to be missing on Windows tcc
		typedef struct SRWLOCK { void* SRWLOCK; } SRWLOCK;
		void InitializeSRWLock(void*);
		void AcquireSRWLockShared(void*);
		void AcquireSRWLockExclusive(void*);
		void ReleaseSRWLockShared(void*);
		void ReleaseSRWLockExclusive(void*);
	#endif
#else
	#include <pthread.h>
	#ifndef PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP
		// musl does not have that
		#define pthread_rwlockattr_setkind_np(a, b)
	#endif
#endif

// g_live_info is used by live.info()
void* g_live_info = NULL;

//============================== HELPER C MACROS =============================*/
//#define tos4(s, slen) ((string){.str=(s), .len=(slen)})
#define _SLIT(s) ((string){.str=(byteptr)(s), .len=(strlen(s))})
#define _PUSH_MANY(arr, val, tmp, tmp_typ) {tmp_typ tmp = (val); array_push_many(arr, tmp.data, tmp.len);}
#define _IN(typ, val, arr) array_##typ##_contains(arr, val)
#define _IN_MAP(val, m) map_exists(m, val)

// these macros have corresponding implementations in builtin/int.v with different signedness
#define array_i8_contains(a, b) array_byte_contains(a, (byte)(b))
#define array_i16_contains(a, b) array_u16_contains(a, (u16)(b))
#define array_u32_contains(a, b) array_int_contains(a, (int)(b))
#define array_i64_contains(a, b) array_u64_contains(a, (u64)(b))
#define array_rune_contains(a, b) array_int_contains(a, (int)(b))
#define array_f32_contains(a, b) array_int_contains(a, *(int*)&((f32[]){(b)}))
#define array_f64_contains(a, b) array_u64_contains(a, *(u64*)&((f64[]){(b)}))
#ifdef TARGET_IS_64BIT
#define array_voidptr_contains(a, b) array_u64_contains(a, (u64)(b))
#else
#define array_voidptr_contains(a, b) array_int_contains(a, (int)(b))
#endif

// unsigned/signed comparisons
static inline bool _us32_gt(uint32_t a, int32_t b) { return a > INT32_MAX || (int32_t)a > b; }
static inline bool _us32_ge(uint32_t a, int32_t b) { return a >= INT32_MAX || (int32_t)a >= b; }
static inline bool _us32_eq(uint32_t a, int32_t b) { return a <= INT32_MAX && (int32_t)a == b; }
static inline bool _us32_ne(uint32_t a, int32_t b) { return a > INT32_MAX || (int32_t)a != b; }
static inline bool _us32_le(uint32_t a, int32_t b) { return a <= INT32_MAX && (int32_t)a <= b; }
static inline bool _us32_lt(uint32_t a, int32_t b) { return a < INT32_MAX && (int32_t)a < b; }
static inline bool _us64_gt(uint64_t a, int64_t b) { return a > INT64_MAX || (int64_t)a > b; }
static inline bool _us64_ge(uint64_t a, int64_t b) { return a >= INT64_MAX || (int64_t)a >= b; }
static inline bool _us64_eq(uint64_t a, int64_t b) { return a <= INT64_MAX && (int64_t)a == b; }
static inline bool _us64_ne(uint64_t a, int64_t b) { return a > INT64_MAX || (int64_t)a != b; }
static inline bool _us64_le(uint64_t a, int64_t b) { return a <= INT64_MAX && (int64_t)a <= b; }
static inline bool _us64_lt(uint64_t a, int64_t b) { return a < INT64_MAX && (int64_t)a < b; }

#if defined(__MINGW32__) || defined(__MINGW64__) || (defined(_WIN32) && defined(__TINYC__))
	#undef PRId64
	#undef PRIi64
	#undef PRIo64
	#undef PRIu64
	#undef PRIx64
	#undef PRIX64
	#define PRId64 "lld"
	#define PRIi64 "lli"
	#define PRIo64 "llo"
	#define PRIu64 "llu"
	#define PRIx64 "llx"
	#define PRIX64 "llX"
#endif

//================================== GLOBALS =================================*/
//byte g_str_buf[1024];
byte* g_str_buf;
int load_so(byteptr);
void reload_so();
void _vinit();
void _vcleanup();
#define sigaction_size sizeof(sigaction);
#define _ARR_LEN(a) ( (sizeof(a)) / (sizeof(a[0])) )

// ============== wyhash ==============
//Author: Wang Yi
#ifndef wyhash_version_gamma
	#define wyhash_version_gamma
	#define WYHASH_CONDOM 0
	#include <stdint.h>
	#include <string.h>
	#if defined(_MSC_VER) && defined(_M_X64)
		#include <intrin.h>
		#pragma intrinsic(_umul128)
	#endif

	//const uint64_t _wyp0=0xa0761d6478bd642full, _wyp1=0xe7037ed1a0b428dbull;
	#define _wyp0 ((uint64_t)0xa0761d6478bd642full)
	#define _wyp1 ((uint64_t)0xe7037ed1a0b428dbull)

	#if defined(__GNUC__) || defined(__INTEL_COMPILER) || defined(__clang__) || defined(__TINYC__)
		#define _likely_(x) __builtin_expect(x, 1)
		#define _unlikely_(x) __builtin_expect((x), 0)
	#else
		#define _likely_(x) (x)
		#define _unlikely_(x) (x)
	#endif

	#if defined(TARGET_ORDER_IS_LITTLE)
		#define WYHASH_LITTLE_ENDIAN 1
	#elif defined(TARGET_ORDER_IS_BIG)
		#define WYHASH_LITTLE_ENDIAN 0
	#endif

	#if (WYHASH_LITTLE_ENDIAN)
		static inline uint64_t _wyr8(const uint8_t *p) { uint64_t v; memcpy(&v, p, 8); return v;}
		static inline uint64_t _wyr4(const uint8_t *p) { unsigned v; memcpy(&v, p, 4); return v;}
	#else
		#if defined(__GNUC__) || defined(__INTEL_COMPILER) || defined(__clang__)
			static inline uint64_t _wyr8(const uint8_t *p) { uint64_t v; memcpy(&v, p, 8); return __builtin_bswap64(v);}
			static inline uint64_t _wyr4(const uint8_t *p) { unsigned v; memcpy(&v, p, 4); return __builtin_bswap32(v);}
		#elif defined(_MSC_VER)
			static inline uint64_t _wyr8(const uint8_t *p) { uint64_t v; memcpy(&v, p, 8); return _byteswap_uint64(v);}
			static inline uint64_t _wyr4(const uint8_t *p) { unsigned v; memcpy(&v, p, 4); return _byteswap_ulong(v);}
		#elif defined(__TINYC__)
			static inline uint64_t _wyr8(const uint8_t *p) { uint64_t v; memcpy(&v, p, 8); return bswap_64(v);}
			static inline uint64_t _wyr4(const uint8_t *p) { unsigned v; memcpy(&v, p, 4); return bswap_32(v);}
		#endif
	#endif

	static inline uint64_t _wyr3(const uint8_t *p, unsigned k) { return (((uint64_t)p[0]) << 16) | (((uint64_t)p[k >> 1]) << 8) | p[k - 1];}
	static inline uint64_t _wyrotr(uint64_t v, unsigned k) { return (v >> k) | (v << (64 - k));}
	static inline void _wymix128(uint64_t A, uint64_t B, uint64_t *C, uint64_t *D){
		A^=*C;	B^=*D;
	#ifdef UNOFFICIAL_WYHASH_32BIT
		uint64_t hh=(A>>32)*(B>>32), hl=(A>>32)*(unsigned)B, lh=(unsigned)A*(B>>32), ll=(uint64_t)(unsigned)A*(unsigned)B;
		*C=_wyrotr(hl,32)^hh; *D=_wyrotr(lh,32)^ll;
	#else
		#ifdef __SIZEOF_INT128__
			__uint128_t r=A; r*=B; *C=(uint64_t)r; *D=(uint64_t)(r>>64);
		#elif defined(_MSC_VER) && defined(_M_X64)
			A=_umul128(A,B,&B); *C=A; *D=B;
		#else
			uint64_t ha=A>>32, hb=B>>32, la=(uint32_t)A, lb=(uint32_t)B, hi, lo;
			uint64_t rh=ha*hb, rm0=ha*lb, rm1=hb*la, rl=la*lb, t=rl+(rm0<<32), c=t<rl;
			lo=t+(rm1<<32); c+=lo<t; hi=rh+(rm0>>32)+(rm1>>32)+c;
			*C=lo;	*D=hi;
		#endif
	#endif
	}
	static inline uint64_t wyhash(const void *key, uint64_t len, uint64_t seed){
		const uint8_t *p=(const uint8_t *)key;
		uint64_t i=len, see1=seed;
		start:
		if (_likely_(i<=16)) {
	#ifndef	WYHASH_CONDOM
			uint64_t shift = (i<8)*((8-i)<<3);
			//WARNING: intended reading outside buffer, trading for speed.
			_wymix128((_wyr8(p)<<shift)^_wyp0, (_wyr8(p+i-8)>>shift)^_wyp1, &seed, &see1);
	#else
			if (_likely_(i<=8)) {
				if (_likely_(i>=4)) _wymix128(_wyr4(p)^_wyp0,_wyr4(p+i-4)^_wyp1, &seed, &see1);
				else if (_likely_(i)) _wymix128(_wyr3(p,i)^_wyp0,_wyp1, &seed, &see1);
				else _wymix128(_wyp0,_wyp1, &seed, &see1);
			}
			else _wymix128(_wyr8(p)^_wyp0,_wyr8(p+i-8)^_wyp1, &seed, &see1);
	#endif
			_wymix128(len,_wyp0, &seed, &see1);
			return	seed^see1;
		}
		_wymix128(_wyr8(p)^_wyp0,_wyr8(p+8)^_wyp1, &seed, &see1);
		i-=16;	p+=16;	goto start;
	}
	static inline uint64_t wyhash64(uint64_t A, uint64_t B){
		_wymix128(_wyp0,_wyp1,&A,&B);
		_wymix128(0,0,&A,&B);
		return	A^B;
	}
	static inline uint64_t wyrand(uint64_t *seed){
		*seed+=_wyp0;
		uint64_t	a=0, b=0;
		_wymix128(*seed,*seed^_wyp1,&a,&b);
		return	a^b;
	}
	static inline double wy2u01(uint64_t r) {
		const double _wynorm=1.0/(1ull<<52);
		return (r>>12)*_wynorm;
	}
	static inline double wy2gau(uint64_t r) {
		const double _wynorm=1.0/(1ull<<20);
		return ((r&0x1fffff)+((r>>21)&0x1fffff)+((r>>42)&0x1fffff))*_wynorm-3.0;
	}
#endif

voidptr memdup(voidptr src, int sz);
voidptr memfreedup(voidptr ptr, voidptr src, int sz) {
	free(ptr);
	return memdup(src, sz);
}
'
	c_builtin_types = '
//================================== builtin types ================================*/
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
typedef int64_t any_int;
typedef double any_float;
typedef unsigned char* byteptr;
typedef void* voidptr;
typedef char* charptr;
typedef byte array_fixed_byte_300 [300];

typedef struct sync__Channel* chan;

#ifndef __cplusplus
	#ifndef bool
		typedef int bool;
		#define true 1
		#define false 0
	#endif
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
