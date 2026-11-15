#ifndef V_NATIVE_OBJECT_CACHE_H
#define V_NATIVE_OBJECT_CACHE_H

#include <errno.h>

#ifndef _WIN32
#if (defined(__V_HEADERLESS_LIBC) && __V_HEADERLESS_LIBC) || defined(CUSTOM_DEFINE_v3_headerless_bootstrap)
/* Match the libc declarations in headerless compiler snapshots. Including
 * unistd.h/fcntl.h here would redefine their types and structures. */
#define V_NATIVE_OBJECT_CACHE_HEADERLESS 1
int link(char *, char *);
#if defined(__linux__)
long syscall(long, ...);
#endif
#else
#include <fcntl.h>
#include <unistd.h>
#endif
#endif

#if defined(__linux__)
#include <sys/syscall.h>
#endif

#include "native_object_cache_impl.c"

#endif
