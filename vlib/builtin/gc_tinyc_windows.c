// Windows TinyCC supports GNU x86 assembly, but does not advertise __GNUC__.
// Select libatomic_ops' assembly implementation, rather than GCC's unavailable
// __atomic builtins or MSVC's literal and structured-exception syntax. Keep this
// compatibility setting local to the collector's translation unit.
#if defined(__TINYC__) && defined(_WIN32) && !defined(__GNUC__)
#define __GNUC__ 2
#define __GNUC_MINOR__ 7
#endif

#if defined(__TINYC__) && defined(_WIN32) && !defined(__SIZEOF_SIZE_T__)
// Win32 and Win64 size_t has the same width as a pointer. Without this macro,
// gc_inline.h mistakes TinyCC's missing size macro for a fat-pointer target.
#define __SIZEOF_SIZE_T__ __SIZEOF_POINTER__
#endif

#include "../../thirdparty/libgc/gc.c"
