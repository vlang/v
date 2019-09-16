module main

const (

CommonCHeaders = '

#include <stdio.h>  // TODO remove all these includes, define all function signatures and types manually
#include <stdlib.h>
#include <signal.h>
#include <stdarg.h> // for va_list
#include <inttypes.h>  // int64_t etc
#include <string.h> // memcpy

#ifndef _WIN32
#include <ctype.h>
#include <locale.h> // tolower
#include <sys/time.h>
#include <unistd.h> // sleep	
#endif


#ifdef __APPLE__
#include <libproc.h> // proc_pidpath
#include <execinfo.h> // backtrace and backtrace_symbols_fd
#endif

#ifdef __linux__
#ifndef __BIONIC__
#include <execinfo.h> // backtrace and backtrace_symbols_fd
#endif
#pragma weak backtrace
#pragma weak backtrace_symbols_fd
#endif


#ifdef __linux__
#include <sys/types.h>
#include <sys/wait.h> // os__wait uses wait on nix
#endif

#define EMPTY_STRUCT_DECLARATION
#define EMPTY_STRUCT_INITIALIZATION 0
#define OPTION_CAST(x) (x)

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

// must be included after <windows.h>
#ifndef __TINYC__
#undef EMPTY_STRUCT_INITIALIZATION
#define EMPTY_STRUCT_INITIALIZATION
#include <shellapi.h>
#endif

#include <io.h> // _waccess
#include <fcntl.h> // _O_U8TEXT
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
#endif

void pthread_mutex_lock(HANDLE *m) {
	WaitForSingleObject(*m, INFINITE);
}

void pthread_mutex_unlock(HANDLE *m) {
	ReleaseMutex(*m);
}
#else
#include <pthread.h>
#endif

//================================== TYPEDEFS ================================*/

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
typedef struct array array;
typedef struct map map;
typedef array array_string;
typedef array array_int;
typedef array array_byte;
typedef array array_f32;
typedef array array_f64;
typedef map map_int;
typedef map map_string;
#ifndef bool
	typedef int bool;
	#define true 1
	#define false 0
#endif

//============================== HELPER C MACROS =============================*/

#define _PUSH(arr, val, tmp, tmp_typ) {tmp_typ tmp = (val); array__push(arr, &tmp);}
#define _PUSH_MANY(arr, val, tmp, tmp_typ) {tmp_typ tmp = (val); array__push_many(arr, tmp.data, tmp.len);}
#define _IN(typ, val, arr) array_##typ##_contains(arr, val)
#define _IN_MAP(val, m) map__exists(m, val)
//#define ALLOC_INIT(type, ...) (type *)memdup((type[]){ __VA_ARGS__ }, sizeof(type))

//================================== GLOBALS =================================*/
byteptr g_str_buf;
int load_so(byteptr);
void reload_so();
void init_consts();

'

js_headers = '

class array_string {}
class array_byte {}
class array_int {}
class byte {}
class double {}
class int {}
class f64 {}
class f32 {}
class i64 {}
class i32 {}
class i16 {}
class u64 {}
class u32 {}
class u16 {}
class i8 {}
class u8 {}
class bool {}
class rune {}
class map_string {}
class map_int {}

function init_consts() {
	
}	

'

)
