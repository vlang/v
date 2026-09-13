// c_headers
typedef int (*qsort_callback_func)(const void*, const void*);
#if defined(_MSC_VER) && !defined(__clang__)
	#define V_CRT_LINKAGE __declspec(dllimport)
	#define V_CRT_CALL VCALLCONV(cdecl)
#else
	#define V_CRT_LINKAGE
	#define V_CRT_CALL
#endif
#if (defined(__MINGW32__) || defined(__MINGW64__)) && defined(__V_GCC__)
	#define V_CRT_STDIO_LINKAGE __attribute__((dllimport))
#else
	#define V_CRT_STDIO_LINKAGE V_CRT_LINKAGE
#endif
#if (defined(_MSC_VER) && !defined(__clang__)) || defined(__cplusplus)
// Under C++ (g++/clang++), let libc declare FILE/stdio/string/stdlib to keep
// noexcept specifiers consistent — the manual extern "C" prototypes below
// would otherwise conflict with system headers under -std=c++NN.
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef va_copy
	#define va_copy(dest, src) ((dest) = (src))
#endif
#ifndef _TRUNCATE
	#define _TRUNCATE ((size_t)-1)
#endif
#elif defined(__NetBSD__)
// NetBSD exposes stdin/stdout/stderr as macros into a single `__sF[3]`
// array whose element size (sizeof(FILE)) depends on the platform and libc
// version, so we cannot forward-declare them. The FreeBSD-style
// `__stdinp/__stdoutp/__stderrp` symbols also do not exist on NetBSD (see
// vlang/v#27190). Defer to the system headers for FILE, the stdio streams,
// and the libc prototypes that would otherwise clash with the
// `__restrict`-qualified declarations in NetBSD libc.
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#elif defined(__TINYC__) && (defined(__FreeBSD__) || defined(__OpenBSD__))
// TinyCC reports a hard redefinition error if system OpenSSL pulls in
// <stdarg.h> after V has provided its own va_start macro. Include it first,
// but keep V manual FILE declarations on these BSD libc variants.
#include <stdarg.h>
#if defined(__FreeBSD__)
typedef struct __sFILE FILE;
extern FILE* __stdinp;
extern FILE* __stdoutp;
extern FILE* __stderrp;
#define stdin __stdinp
#define stdout __stdoutp
#define stderr __stderrp
#else
typedef struct __sFILE FILE;
#ifndef _STDFILES_DECLARED
	#define _STDFILES_DECLARED
struct __sFstub { long _stub; };
extern struct __sFstub __stdin[];
extern struct __sFstub __stdout[];
extern struct __sFstub __stderr[];
#endif
#define stdin ((struct __sFILE *)__stdin)
#define stdout ((struct __sFILE *)__stdout)
#define stderr ((struct __sFILE *)__stderr)
#endif
#elif (defined(__MINGW32__) || defined(__MINGW64__)) && defined(__V_GCC__)
// mingw-w64 stdio.h provides fprintf/vfprintf as static inline overrides
// when __USE_MINGW_ANSI_STDIO is enabled, so use the system declarations
// instead of the manual formatted-stdio prototypes below.
#include <stdarg.h>
#include <stdio.h>
#elif defined(__MINGW32__) || defined(__MINGW64__) || (defined(__clang__) && (defined(_WIN32) || defined(_WIN64)))
typedef struct _iobuf FILE;
FILE* __cdecl __acrt_iob_func(unsigned index);
#define stdin  (__acrt_iob_func(0))
#define stdout (__acrt_iob_func(1))
#define stderr (__acrt_iob_func(2))
#elif defined(__TINYC__) && (defined(_WIN32) || defined(_WIN64))
#ifndef _FILE_DEFINED
struct _iobuf {
	char *_ptr;
	int _cnt;
	char *_base;
	int _flag;
	int _file;
	int _charbuf;
	int _bufsiz;
	char *_tmpfname;
};
typedef struct _iobuf FILE;
#define _FILE_DEFINED
#endif
	#if defined(_WIN64)
FILE* __cdecl __iob_func(void);
	#else
		#ifdef _MSVCRT_
extern FILE _iob[];
			#define __iob_func() (_iob)
		#else
extern FILE (*_imp___iob)[];
			#define __iob_func() (*_imp___iob)
			#define _iob __iob_func()
		#endif
	#endif
#define stdin (&__iob_func()[0])
#define stdout (&__iob_func()[1])
#define stderr (&__iob_func()[2])
#elif defined(__vinix__)
typedef struct __file FILE;
extern FILE* stdin;
extern FILE* stdout;
extern FILE* stderr;
struct __thread_data;
struct __threadattr;
// pthread_t handling for vinix builds:
//  - Vinix kernel (freestanding, __STDC_HOSTED__=0): no libc, define
//    pthread_t ourselves so V code that references it compiles.
//  - util-vinix cross-compiled on a libc-providing host (hosted, e.g.
//    glibc on Linux or macOS with -D__vinix__): pull pthread_t from
//    libc to avoid colliding with the libc typedef.
#if defined(__STDC_HOSTED__) && __STDC_HOSTED__ && defined(__has_include) && __has_include(<pthread.h>)
#include <pthread.h>
#else
typedef struct __thread_data *pthread_t;
#endif
typedef __builtin_va_list va_list;
#ifndef va_start
	#define va_start(ap, v) __builtin_va_start(ap, v)
#endif
#ifndef va_arg
	#define va_arg(ap, t) __builtin_va_arg(ap, t)
#endif
#ifndef va_end
	#define va_end(ap) __builtin_va_end(ap)
#endif
#ifndef va_copy
	#define va_copy(dest, src) __builtin_va_copy(dest, src)
#endif
#else
	#if defined(__APPLE__) || defined(__FreeBSD__)
typedef struct __sFILE FILE;
extern FILE* __stdinp;
extern FILE* __stdoutp;
extern FILE* __stderrp;
#define stdin __stdinp
#define stdout __stdoutp
#define stderr __stderrp
	#elif defined(__DragonFly__)
typedef struct __sFILE FILE;
extern FILE* __stdinp;
extern FILE* __stdoutp;
extern FILE* __stderrp;
#define stdin __stdinp
#define stdout __stdoutp
#define stderr __stderrp
	#elif defined(__OpenBSD__)
typedef struct __sFILE FILE;
#ifndef _STDFILES_DECLARED
	#define _STDFILES_DECLARED
struct __sFstub { long _stub; };
extern struct __sFstub __stdin[];
extern struct __sFstub __stdout[];
extern struct __sFstub __stderr[];
#endif
#define stdin ((struct __sFILE *)__stdin)
#define stdout ((struct __sFILE *)__stdout)
#define stderr ((struct __sFILE *)__stderr)
	#elif defined(__BIONIC__)
struct __sFILE;
typedef struct __sFILE FILE;
extern FILE* stdin;
extern FILE* stdout;
extern FILE* stderr;
	#elif defined(__linux__) && !defined(__GLIBC__) && !defined(__GNU_LIBRARY__) && !defined(__BIONIC__) && !defined(__UCLIBC__)
typedef struct _IO_FILE FILE;
// musl exposes the stdio streams as `FILE *const`, so match that to stay
// compatible with later <stdio.h> includes from headers like miniz.h.
extern FILE* const stdin;
extern FILE* const stdout;
extern FILE* const stderr;
	#else
typedef struct _IO_FILE FILE;
extern FILE* stdin;
extern FILE* stdout;
extern FILE* stderr;
#if defined(__GLIBC__) || defined(__GNU_LIBRARY__)
// V declares the stdio functions manually here, instead of including <stdio.h>.
// glibc defines L_tmpnam only while <stdio.h> is being processed (it sits behind
// `#ifdef _STDIO_H` in <bits/stdio_lim.h>), and it is the one stdio limit macro that
// <stdio.h> itself uses in a prototype: char *tmpnam(char[L_tmpnam]). So a <stdio.h>
// pulled in later by a module header (sqlite3.h, gc.h, ...) can fail with L_tmpnam
// being undeclared; see vlang/v#28108. Define it here, to the stable glibc value,
// without adding an include. A later identical redefinition by glibc is a no-op.
#ifndef L_tmpnam
#define L_tmpnam 20
#endif
#endif
	#endif
typedef __builtin_va_list va_list;
#ifndef va_start
	#define va_start(ap, v) __builtin_va_start(ap, v)
#endif
#ifndef va_arg
	#define va_arg(ap, t) __builtin_va_arg(ap, t)
#endif
#ifndef va_end
	#define va_end(ap) __builtin_va_end(ap)
#endif
#ifndef va_copy
	#define va_copy(dest, src) __builtin_va_copy(dest, src)
#endif
#endif
#if (!defined(_MSC_VER) || defined(__clang__)) && !defined(__cplusplus) && !defined(__NetBSD__)
// mingw-w64 stdio.h declares these as static __mingw_ovr inline overrides
// when __USE_MINGW_ANSI_STDIO is on. Skip them under gcc+mingw to avoid
// static-after-extern conflicts; clang+mingw needs them because it builds
// with -Werror=implicit-function-declaration and does not hit the conflict.
// NetBSD pulls these prototypes from <stdio.h>/<stdlib.h>/<string.h> via
// the include block above to avoid `__restrict` qualifier conflicts.
#if !((defined(__MINGW32__) || defined(__MINGW64__)) && !defined(__clang__))
V_CRT_LINKAGE int V_CRT_CALL vfprintf(FILE *stream, const char *format, va_list ap);
V_CRT_LINKAGE int V_CRT_CALL vsnprintf(char *str, size_t size, const char *format, va_list ap);
V_CRT_LINKAGE int V_CRT_CALL fprintf(FILE *stream, const char *format, ...);
V_CRT_LINKAGE int V_CRT_CALL printf(const char *format, ...);
V_CRT_LINKAGE int V_CRT_CALL snprintf(char *str, size_t size, const char *format, ...);
V_CRT_LINKAGE int V_CRT_CALL sprintf(char *str, const char *format, ...);
V_CRT_LINKAGE int V_CRT_CALL sscanf(const char *str, const char *format, ...);
V_CRT_LINKAGE int V_CRT_CALL scanf(const char *format, ...);
#endif
V_CRT_LINKAGE int V_CRT_CALL puts(const char *str);
V_CRT_LINKAGE void V_CRT_CALL perror(const char *str);
V_CRT_LINKAGE int V_CRT_CALL fputs(const char *str, FILE *stream);
V_CRT_LINKAGE int V_CRT_CALL getchar(void);
V_CRT_LINKAGE int V_CRT_CALL putchar(int ch);
V_CRT_LINKAGE int V_CRT_CALL getc(FILE *stream);
V_CRT_LINKAGE int V_CRT_CALL fgetc(FILE *stream);
V_CRT_LINKAGE int V_CRT_CALL ungetc(int ch, FILE *stream);
V_CRT_LINKAGE int V_CRT_CALL fflush(FILE *stream);
V_CRT_LINKAGE int V_CRT_CALL feof(FILE *stream);
V_CRT_LINKAGE int V_CRT_CALL ferror(FILE *stream);
V_CRT_LINKAGE void V_CRT_CALL clearerr(FILE *stream);
V_CRT_LINKAGE int V_CRT_CALL setvbuf(FILE *stream, char *buf, int mode, size_t size);
V_CRT_LINKAGE long V_CRT_CALL ftell(FILE *stream);
V_CRT_LINKAGE void V_CRT_CALL rewind(FILE *stream);
V_CRT_LINKAGE FILE * V_CRT_CALL fopen(const char *filename, const char *mode);
V_CRT_LINKAGE FILE * V_CRT_CALL fdopen(int fd, const char *mode);
V_CRT_LINKAGE FILE * V_CRT_CALL freopen(const char *filename, const char *mode, FILE *stream);
V_CRT_LINKAGE int V_CRT_CALL fileno(FILE *stream);
V_CRT_LINKAGE size_t V_CRT_CALL fread(void *ptr, size_t size, size_t items, FILE *stream);
V_CRT_LINKAGE size_t V_CRT_CALL fwrite(const void *ptr, size_t size, size_t items, FILE *stream);
#if defined(__vinix__)
V_CRT_LINKAGE char * V_CRT_CALL fgets(char *str, size_t size, FILE *stream);
#else
V_CRT_LINKAGE char * V_CRT_CALL fgets(char *str, int size, FILE *stream);
#endif
V_CRT_LINKAGE int V_CRT_CALL fclose(FILE *stream);
#if defined(__vinix__)
V_CRT_LINKAGE FILE * V_CRT_CALL popen(char *command, char *mode);
#else
V_CRT_STDIO_LINKAGE FILE * V_CRT_CALL popen(const char *command, const char *mode);
#endif
V_CRT_STDIO_LINKAGE int V_CRT_CALL pclose(FILE *stream);
V_CRT_LINKAGE void * V_CRT_CALL malloc(size_t size);
V_CRT_LINKAGE void * V_CRT_CALL calloc(size_t nitems, size_t size);
V_CRT_LINKAGE void * V_CRT_CALL realloc(void *ptr, size_t size);
V_CRT_LINKAGE void * V_CRT_CALL aligned_alloc(size_t alignment, size_t size);
V_CRT_LINKAGE int V_CRT_CALL posix_memalign(void **memptr, size_t alignment, size_t size);
V_CRT_LINKAGE void V_CRT_CALL free(void *ptr);
V_CRT_LINKAGE int V_CRT_CALL rand(void);
V_CRT_LINKAGE void V_CRT_CALL srand(unsigned int seed);
V_CRT_LINKAGE int V_CRT_CALL atexit(void (*cb)(void));
V_CRT_LINKAGE void V_CRT_CALL exit(int status);
V_CRT_LINKAGE int V_CRT_CALL abs(int n);
V_CRT_LINKAGE int V_CRT_CALL atoi(const char *str);
V_CRT_LINKAGE double V_CRT_CALL atof(const char *str);
V_CRT_LINKAGE char * V_CRT_CALL getenv(const char *name);
V_CRT_LINKAGE int V_CRT_CALL setenv(const char *name, const char *value, int overwrite);
V_CRT_LINKAGE int V_CRT_CALL unsetenv(const char *name);
V_CRT_LINKAGE int V_CRT_CALL system(const char *command);
V_CRT_LINKAGE int V_CRT_CALL remove(const char *path);
V_CRT_LINKAGE int V_CRT_CALL rename(const char *old_path, const char *new_path);
V_CRT_LINKAGE char * V_CRT_CALL realpath(const char *path, char *resolved_path);
V_CRT_LINKAGE int V_CRT_CALL mkstemp(char *stemplate);
V_CRT_LINKAGE void V_CRT_CALL qsort(void *base, size_t items, size_t item_size, qsort_callback_func cb);
#if defined(__vinix__)
V_CRT_LINKAGE int V_CRT_CALL strcmp(char *left, char *right);
V_CRT_LINKAGE int V_CRT_CALL strncmp(char *left, char *right, size_t n);
#else
V_CRT_LINKAGE int V_CRT_CALL strcmp(const char *left, const char *right);
V_CRT_LINKAGE int V_CRT_CALL strncmp(const char *left, const char *right, size_t n);
#endif
#if !defined(_WIN32) && !defined(_WIN64) && !defined(__BIONIC__)
V_CRT_LINKAGE char * V_CRT_CALL strdup(const char *str);
#endif
#if !defined(_WIN32) && !defined(_WIN64)
V_CRT_LINKAGE int V_CRT_CALL strcasecmp(const char *left, const char *right);
V_CRT_LINKAGE int V_CRT_CALL strncasecmp(const char *left, const char *right, size_t n);
#endif
#if defined(__vinix__)
V_CRT_LINKAGE size_t V_CRT_CALL strlen(char *str);
#else
V_CRT_LINKAGE size_t V_CRT_CALL strlen(const char *str);
#endif
V_CRT_LINKAGE char * V_CRT_CALL strerror(int errnum);
V_CRT_LINKAGE void * V_CRT_CALL memcpy(void *dest, const void *src, size_t n);
V_CRT_LINKAGE void * V_CRT_CALL memmove(void *dest, const void *src, size_t n);
V_CRT_LINKAGE void * V_CRT_CALL memset(void *dest, int ch, size_t n);
V_CRT_LINKAGE int V_CRT_CALL memcmp(const void *left, const void *right, size_t n);
// memchr/strchr/strrchr/strstr are the C23 type-generic string functions, and
// glibc 2.42+ implements them as function-like macros over _Generic, so that a
// const-qualified argument yields a const-qualified return type. If any include
// above already pulled in <string.h> (mbedtls/net_sockets.h, netdb.h, dirent.h,
// ... all do, and gcc 15 defaults to -std=gnu23), the name is already a macro
// here, and a declaration like
// `void *memchr(const void *str, int c, size_t n);` expands into the middle of
// a _Generic expression, which fails to parse:
// `error: expected identifier or ( before _Generic`.
// A defined macro also means <string.h> has already declared the real function,
// so skipping the declaration below loses nothing in that case. The reverse
// order stays fine as-is: a <string.h> pulled in later by a module header
// defines the macro after these declarations, which C permits.
#ifndef memchr
V_CRT_LINKAGE void * V_CRT_CALL memchr(const void *str, int c, size_t n);
#endif
#ifndef strchr
V_CRT_LINKAGE char * V_CRT_CALL strchr(const char *str, int c);
#endif
#ifndef strrchr
V_CRT_LINKAGE char * V_CRT_CALL strrchr(const char *str, int c);
#endif
#ifndef strstr
V_CRT_LINKAGE char * V_CRT_CALL strstr(const char *haystack, const char *needle);
#endif
V_CRT_LINKAGE int V_CRT_CALL fseek(FILE *stream, long offset, int whence);
V_CRT_LINKAGE isize V_CRT_CALL getline(char **lineptr, size_t *n, FILE *stream);
#if defined(_WIN32) || defined(_WIN64)
V_CRT_STDIO_LINKAGE int V_CRT_CALL _fseeki64(FILE *stream, i64 offset, int whence);
V_CRT_LINKAGE int V_CRT_CALL fgetpos(FILE *stream, i64 *pos);
V_CRT_STDIO_LINKAGE int V_CRT_CALL _fileno(FILE *stream);
V_CRT_STDIO_LINKAGE FILE * V_CRT_CALL _wfopen(const unsigned short *filename, const unsigned short *mode);
V_CRT_STDIO_LINKAGE int V_CRT_CALL freopen_s(FILE **new_stream, const char *filename, const char *mode, FILE *stream);
V_CRT_STDIO_LINKAGE FILE * V_CRT_CALL _wfreopen(const unsigned short *filename, const unsigned short *mode, FILE *stream);
V_CRT_STDIO_LINKAGE FILE * V_CRT_CALL _wpopen(const unsigned short *command, const unsigned short *mode);
V_CRT_STDIO_LINKAGE int V_CRT_CALL _pclose(FILE *stream);
V_CRT_STDIO_LINKAGE int V_CRT_CALL _wremove(const unsigned short *path);
V_CRT_LINKAGE void * V_CRT_CALL _aligned_malloc(size_t size, size_t alignment);
V_CRT_LINKAGE void * V_CRT_CALL _aligned_realloc(void *memory, size_t size, size_t alignment);
V_CRT_LINKAGE void V_CRT_CALL _aligned_free(void *memory);
V_CRT_LINKAGE unsigned short * V_CRT_CALL _wgetenv(const unsigned short *varname);
V_CRT_LINKAGE int V_CRT_CALL _wputenv(const unsigned short *envstring);
#endif
#if defined(_MSC_VER) && !defined(__clang__)
#ifndef _TRUNCATE
	#define _TRUNCATE ((size_t)-1)
#endif
V_CRT_LINKAGE int V_CRT_CALL _vscprintf(const char *format, va_list ap);
V_CRT_LINKAGE int V_CRT_CALL _vsnprintf_s(char *buffer, size_t size, size_t count, const char *format, va_list ap);
#endif
#endif
#ifndef _IOFBF
	#define _IOFBF 0
#endif
#ifndef _IOLBF
	#define _IOLBF 1
#endif
#ifndef _IONBF
	#define _IONBF 2
#endif
#ifndef EOF
	#define EOF (-1)
#endif
#ifndef SEEK_SET
	#define SEEK_SET 0
#endif
#ifndef SEEK_CUR
	#define SEEK_CUR 1
#endif
#ifndef SEEK_END
	#define SEEK_END 2
#endif
#ifndef RAND_MAX
enum {
	#if defined(_MSC_VER)
		RAND_MAX = 0x7fff
	#else
		RAND_MAX = 2147483647
	#endif
};
#endif
#undef V_CRT_STDIO_LINKAGE
#undef V_CRT_LINKAGE
#undef V_CRT_CALL
