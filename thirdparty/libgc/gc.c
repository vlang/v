/*
 * Copyright (c) 1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1996 by Silicon Graphics.  All rights reserved.
 * Copyright (c) 1998 by Fergus Henderson.  All rights reserved.
 * Copyright (c) 2000-2009 by Hewlett-Packard Development Company.
 * All rights reserved.
 * Copyright (c) 2009-2018 Ivan Maidanski
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose,  provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 */

#ifndef __cplusplus
#define GC_INNER STATIC
#define GC_EXTERN GC_INNER
#endif
#ifndef GC_DBG_MLC_H
#define GC_DBG_MLC_H
#ifndef GC_PRIVATE_H
#define GC_PRIVATE_H
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#if !defined(GC_BUILD) && !defined(NOT_GCBUILD)
#define GC_BUILD
#endif
#if (defined(__linux__) || defined(__GLIBC__) || defined(__GNU__) \
     || (defined(__CYGWIN__) && (defined(GC_THREADS) || !defined(USE_MMAP)))) \
    && !defined(_GNU_SOURCE)
#define _GNU_SOURCE 1
#endif
#if defined(__INTERIX) && !defined(_ALL_SOURCE)
#define _ALL_SOURCE 1
#endif
#if (defined(DGUX) && defined(GC_THREADS) || defined(DGUX386_THREADS) \
     || defined(GC_DGUX386_THREADS)) && !defined(_USING_POSIX4A_DRAFT10)
#define _USING_POSIX4A_DRAFT10 1
#endif
#if defined(__MINGW32__) && !defined(__MINGW_EXCPT_DEFINE_PSDK) \
    && defined(__i386__) && defined(GC_EXTERN)
#define __MINGW_EXCPT_DEFINE_PSDK 1
#endif
#if defined(NO_DEBUGGING) && !defined(GC_ASSERTIONS) && !defined(NDEBUG)
#define NDEBUG 1
#endif
#ifndef GC_H
#ifndef GC_H
#define GC_H
#if (defined(WIN64) && !defined(_WIN64)) && defined(_MSC_VER)
#pragma message("Warning: Expecting _WIN64 for x64 targets! Notice the leading underscore!")
#endif
#if defined(GC_H)
#define GC_TMP_VERSION_MAJOR 8
#define GC_TMP_VERSION_MINOR 2
#define GC_TMP_VERSION_MICRO 0
#ifdef GC_VERSION_MAJOR
#if GC_TMP_VERSION_MAJOR != GC_VERSION_MAJOR \
     || GC_TMP_VERSION_MINOR != GC_VERSION_MINOR \
     || GC_TMP_VERSION_MICRO != GC_VERSION_MICRO
#error Inconsistent version info.  Check README.md, include/gc_version.h and configure.ac.
#endif
#else
#define GC_VERSION_MAJOR GC_TMP_VERSION_MAJOR
#define GC_VERSION_MINOR GC_TMP_VERSION_MINOR
#define GC_VERSION_MICRO GC_TMP_VERSION_MICRO
#endif
#endif
#if defined(GC_H)
#if defined(__GNUC__) && defined(__GNUC_MINOR__)
#define GC_GNUC_PREREQ(major, minor) \
            ((__GNUC__ << 16) + __GNUC_MINOR__ >= ((major) << 16) + (minor))
#else
#define GC_GNUC_PREREQ(major, minor) 0
#endif
#if defined(SOLARIS_THREADS) || defined(_SOLARIS_THREADS) \
    || defined(_SOLARIS_PTHREADS) || defined(GC_SOLARIS_PTHREADS)
#ifndef GC_SOLARIS_THREADS
#define GC_SOLARIS_THREADS
#endif
#endif
#if defined(IRIX_THREADS)
#define GC_IRIX_THREADS
#endif
#if defined(DGUX_THREADS) && !defined(GC_DGUX386_THREADS)
#define GC_DGUX386_THREADS
#endif
#if defined(AIX_THREADS)
#define GC_AIX_THREADS
#endif
#if defined(HPUX_THREADS)
#define GC_HPUX_THREADS
#endif
#if defined(OSF1_THREADS)
#define GC_OSF1_THREADS
#endif
#if defined(LINUX_THREADS)
#define GC_LINUX_THREADS
#endif
#if defined(WIN32_THREADS)
#define GC_WIN32_THREADS
#endif
#if defined(RTEMS_THREADS)
#define GC_RTEMS_PTHREADS
#endif
#if defined(USE_LD_WRAP)
#define GC_USE_LD_WRAP
#endif
#if defined(GC_WIN32_PTHREADS) && !defined(GC_WIN32_THREADS)
#define GC_WIN32_THREADS
#endif
#if defined(GC_AIX_THREADS) || defined(GC_DARWIN_THREADS) \
    || defined(GC_DGUX386_THREADS) || defined(GC_FREEBSD_THREADS) \
    || defined(GC_HPUX_THREADS) \
    || defined(GC_IRIX_THREADS) || defined(GC_LINUX_THREADS) \
    || defined(GC_NETBSD_THREADS) || defined(GC_OPENBSD_THREADS) \
    || defined(GC_OSF1_THREADS) || defined(GC_SOLARIS_THREADS) \
    || defined(GC_WIN32_THREADS) || defined(GC_RTEMS_PTHREADS)
#ifndef GC_THREADS
#define GC_THREADS
#endif
#elif defined(GC_THREADS)
#if defined(__linux__)
#define GC_LINUX_THREADS
#elif defined(__OpenBSD__)
#define GC_OPENBSD_THREADS
#elif defined(_PA_RISC1_1) || defined(_PA_RISC2_0) || defined(hppa) \
       || defined(__HPPA) || (defined(__ia64) && defined(_HPUX_SOURCE))
#define GC_HPUX_THREADS
#elif defined(__HAIKU__)
#define GC_HAIKU_THREADS
#elif (defined(__DragonFly__) || defined(__FreeBSD_kernel__) \
        || defined(__FreeBSD__)) && !defined(GC_NO_FREEBSD)
#define GC_FREEBSD_THREADS
#elif defined(__NetBSD__)
#define GC_NETBSD_THREADS
#elif defined(__alpha) || defined(__alpha__)
#define GC_OSF1_THREADS
#elif (defined(mips) || defined(__mips) || defined(_mips)) \
        && !(defined(nec_ews) || defined(_nec_ews) \
             || defined(ultrix) || defined(__ultrix))
#define GC_IRIX_THREADS
#elif defined(__sparc)  \
       || ((defined(sun) || defined(__sun)) \
           && (defined(i386) || defined(__i386__) \
               || defined(__amd64) || defined(__amd64__)))
#define GC_SOLARIS_THREADS
#elif defined(__APPLE__) && defined(__MACH__)
#define GC_DARWIN_THREADS
#endif
#if defined(DGUX) && (defined(i386) || defined(__i386__))
#define GC_DGUX386_THREADS
#endif
#if defined(_AIX)
#define GC_AIX_THREADS
#endif
#if (defined(_WIN32) || defined(_MSC_VER) || defined(__BORLANDC__) \
      || defined(__CYGWIN32__) || defined(__CYGWIN__) || defined(__CEGCC__) \
      || defined(_WIN32_WCE) || defined(__MINGW32__)) \
     && !defined(GC_WIN32_THREADS)
#define GC_WIN32_THREADS
#endif
#if defined(__rtems__) && (defined(i386) || defined(__i386__))
#define GC_RTEMS_PTHREADS
#endif
#endif
#undef GC_PTHREADS
#if (!defined(GC_WIN32_THREADS) || defined(GC_WIN32_PTHREADS) \
     || defined(__CYGWIN32__) || defined(__CYGWIN__)) && defined(GC_THREADS) \
    && !defined(NN_PLATFORM_CTR) && !defined(NN_BUILD_TARGET_PLATFORM_NX)
#define GC_PTHREADS
#endif
#if !defined(_PTHREADS) && defined(GC_NETBSD_THREADS)
#define _PTHREADS
#endif
#if defined(GC_DGUX386_THREADS) && !defined(_POSIX4A_DRAFT10_SOURCE)
#define _POSIX4A_DRAFT10_SOURCE 1
#endif
#if !defined(_REENTRANT) && defined(GC_PTHREADS) && !defined(GC_WIN32_THREADS)
#define _REENTRANT 1
#endif
#define __GC
#if !defined(_WIN32_WCE) || defined(__GNUC__)
#include <stddef.h>
#if defined(__MINGW32__) && !defined(_WIN32_WCE)
#include <stdint.h>
#endif
#else
#include <stdlib.h>
#ifndef _PTRDIFF_T_DEFINED
#define _PTRDIFF_T_DEFINED
    typedef long ptrdiff_t;
#endif
#endif
#if !defined(GC_NOT_DLL) && !defined(GC_DLL) \
    && ((defined(_DLL) && !defined(__GNUC__)) \
        || (defined(DLL_EXPORT) && defined(GC_BUILD)))
#define GC_DLL
#endif
#if defined(GC_DLL) && !defined(GC_API)
#if defined(__CEGCC__)
#if defined(GC_BUILD)
#define GC_API __declspec(dllexport)
#else
#define GC_API __declspec(dllimport)
#endif
#elif defined(__MINGW32__)
#if defined(__cplusplus) && defined(GC_BUILD)
#define GC_API extern __declspec(dllexport)
#elif defined(GC_BUILD) || defined(__MINGW32_DELAY_LOAD__)
#define GC_API __declspec(dllexport)
#else
#define GC_API extern __declspec(dllimport)
#endif
#elif defined(_MSC_VER) || defined(__DMC__) || defined(__BORLANDC__) \
        || defined(__CYGWIN__)
#ifdef GC_BUILD
#define GC_API extern __declspec(dllexport)
#else
#define GC_API __declspec(dllimport)
#endif
#elif defined(__WATCOMC__)
#ifdef GC_BUILD
#define GC_API extern __declspec(dllexport)
#else
#define GC_API extern __declspec(dllimport)
#endif
#elif defined(__SYMBIAN32__)
#ifdef GC_BUILD
#define GC_API extern EXPORT_C
#else
#define GC_API extern IMPORT_C
#endif
#elif defined(__GNUC__)
#if defined(GC_BUILD) && !defined(GC_NO_VISIBILITY) \
            && (GC_GNUC_PREREQ(4, 0) || defined(GC_VISIBILITY_HIDDEN_SET))
#define GC_API extern __attribute__((__visibility__("default")))
#endif
#endif
#endif
#ifndef GC_API
#define GC_API extern
#endif
#ifndef GC_CALL
#define GC_CALL
#endif
#ifndef GC_CALLBACK
#define GC_CALLBACK GC_CALL
#endif
#ifndef GC_ATTR_MALLOC
#ifdef GC_OOM_FUNC_RETURNS_ALIAS
#define GC_ATTR_MALLOC
#elif GC_GNUC_PREREQ(3, 1)
#define GC_ATTR_MALLOC __attribute__((__malloc__))
#elif defined(_MSC_VER) && (_MSC_VER >= 1900) && !defined(__EDG__)
#define GC_ATTR_MALLOC \
                __declspec(allocator) __declspec(noalias) __declspec(restrict)
#elif defined(_MSC_VER) && _MSC_VER >= 1400
#define GC_ATTR_MALLOC __declspec(noalias) __declspec(restrict)
#else
#define GC_ATTR_MALLOC
#endif
#endif
#ifndef GC_ATTR_ALLOC_SIZE
#undef GC_ATTR_CALLOC_SIZE
#ifdef __clang__
#if __has_attribute(__alloc_size__)
#define GC_ATTR_ALLOC_SIZE(argnum) __attribute__((__alloc_size__(argnum)))
#define GC_ATTR_CALLOC_SIZE(n, s) __attribute__((__alloc_size__(n, s)))
#else
#define GC_ATTR_ALLOC_SIZE(argnum)
#endif
#elif GC_GNUC_PREREQ(4, 3) && !defined(__ICC)
#define GC_ATTR_ALLOC_SIZE(argnum) __attribute__((__alloc_size__(argnum)))
#define GC_ATTR_CALLOC_SIZE(n, s) __attribute__((__alloc_size__(n, s)))
#else
#define GC_ATTR_ALLOC_SIZE(argnum)
#endif
#endif
#ifndef GC_ATTR_CALLOC_SIZE
#define GC_ATTR_CALLOC_SIZE(n, s)
#endif
#ifndef GC_ATTR_NONNULL
#if GC_GNUC_PREREQ(4, 0)
#define GC_ATTR_NONNULL(argnum) __attribute__((__nonnull__(argnum)))
#else
#define GC_ATTR_NONNULL(argnum)
#endif
#endif
#ifndef GC_ATTR_CONST
#if GC_GNUC_PREREQ(4, 0)
#define GC_ATTR_CONST __attribute__((__const__))
#else
#define GC_ATTR_CONST
#endif
#endif
#ifndef GC_ATTR_DEPRECATED
#ifdef GC_BUILD
#undef GC_ATTR_DEPRECATED
#define GC_ATTR_DEPRECATED
#elif GC_GNUC_PREREQ(4, 0)
#define GC_ATTR_DEPRECATED __attribute__((__deprecated__))
#elif defined(_MSC_VER) && _MSC_VER >= 1200
#define GC_ATTR_DEPRECATED __declspec(deprecated)
#else
#define GC_ATTR_DEPRECATED
#endif
#endif
#if defined(__sgi) && !defined(__GNUC__) && _COMPILER_VERSION >= 720
#define GC_ADD_CALLER
#define GC_RETURN_ADDR (GC_word)__return_address
#endif
#if defined(__linux__) || defined(__GLIBC__)
#if !defined(__native_client__)
#include <features.h>
#endif
#if (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 1 || __GLIBC__ > 2) \
        && !defined(__ia64__) \
        && !defined(GC_MISSING_EXECINFO_H) \
        && !defined(GC_HAVE_BUILTIN_BACKTRACE)
#define GC_HAVE_BUILTIN_BACKTRACE
#endif
#if defined(__i386__) || defined(__amd64__) || defined(__x86_64__)
#define GC_CAN_SAVE_CALL_STACKS
#endif
#endif
#if defined(_MSC_VER) && _MSC_VER >= 1200  \
        && !defined(_AMD64_) && !defined(_M_X64) && !defined(_WIN32_WCE) \
        && !defined(GC_HAVE_NO_BUILTIN_BACKTRACE) \
        && !defined(GC_HAVE_BUILTIN_BACKTRACE)
#define GC_HAVE_BUILTIN_BACKTRACE
#endif
#if defined(GC_HAVE_BUILTIN_BACKTRACE) && !defined(GC_CAN_SAVE_CALL_STACKS)
#define GC_CAN_SAVE_CALL_STACKS
#endif
#if defined(__sparc__)
#define GC_CAN_SAVE_CALL_STACKS
#endif
#if (defined(__linux__) || defined(__DragonFly__) || defined(__FreeBSD__) \
     || defined(__FreeBSD_kernel__) || defined(__HAIKU__) \
     || defined(__NetBSD__) || defined(__OpenBSD__) \
     || defined(HOST_ANDROID) || defined(__ANDROID__)) \
    && !defined(GC_CAN_SAVE_CALL_STACKS)
#define GC_ADD_CALLER
#if GC_GNUC_PREREQ(2, 95)
#define GC_RETURN_ADDR (GC_word)__builtin_return_address(0)
#if GC_GNUC_PREREQ(4, 0) && (defined(__i386__) || defined(__amd64__) \
                        || defined(__x86_64__) ) \
       && !defined(GC_NO_RETURN_ADDR_PARENT)
#define GC_HAVE_RETURN_ADDR_PARENT
#define GC_RETURN_ADDR_PARENT \
        (GC_word)__builtin_extract_return_addr(__builtin_return_address(1))
#endif
#else
#define GC_RETURN_ADDR 0
#endif
#endif
#ifdef GC_PTHREADS
#if (defined(GC_DARWIN_THREADS) || defined(GC_WIN32_PTHREADS) \
      || defined(__native_client__) || defined(GC_RTEMS_PTHREADS)) \
      && !defined(GC_NO_DLOPEN)
#define GC_NO_DLOPEN
#endif
#if (defined(GC_DARWIN_THREADS) || defined(GC_WIN32_PTHREADS) \
      || defined(GC_OPENBSD_THREADS) || defined(__native_client__)) \
     && !defined(GC_NO_PTHREAD_SIGMASK)
#define GC_NO_PTHREAD_SIGMASK
#endif
#if defined(__native_client__)
#ifndef GC_PTHREAD_CREATE_CONST
#define GC_PTHREAD_CREATE_CONST
#endif
#ifndef GC_HAVE_PTHREAD_EXIT
#define GC_HAVE_PTHREAD_EXIT
#define GC_PTHREAD_EXIT_ATTRIBUTE
#endif
#endif
#if !defined(GC_HAVE_PTHREAD_EXIT) \
     && !defined(HOST_ANDROID) && !defined(__ANDROID__) \
     && (defined(GC_LINUX_THREADS) || defined(GC_SOLARIS_THREADS))
#define GC_HAVE_PTHREAD_EXIT
#if GC_GNUC_PREREQ(2, 7)
#define GC_PTHREAD_EXIT_ATTRIBUTE __attribute__((__noreturn__))
#elif defined(__NORETURN)
#define GC_PTHREAD_EXIT_ATTRIBUTE __NORETURN
#else
#define GC_PTHREAD_EXIT_ATTRIBUTE
#endif
#endif
#if (!defined(GC_HAVE_PTHREAD_EXIT) || defined(__native_client__)) \
     && !defined(GC_NO_PTHREAD_CANCEL)
#define GC_NO_PTHREAD_CANCEL
#endif
#endif
#ifdef __cplusplus
#ifndef GC_ATTR_EXPLICIT
#if __cplusplus >= 201103L && !defined(__clang__) || _MSVC_LANG >= 201103L \
     || defined(CPPCHECK)
#define GC_ATTR_EXPLICIT explicit
#else
#define GC_ATTR_EXPLICIT
#endif
#endif
#ifndef GC_NOEXCEPT
#if defined(__DMC__) || (defined(__BORLANDC__) \
        && (defined(_RWSTD_NO_EXCEPTIONS) || defined(_RWSTD_NO_EX_SPEC))) \
     || (defined(_MSC_VER) && defined(_HAS_EXCEPTIONS) && !_HAS_EXCEPTIONS) \
     || (defined(__WATCOMC__) && !defined(_CPPUNWIND))
#define GC_NOEXCEPT
#ifndef GC_NEW_ABORTS_ON_OOM
#define GC_NEW_ABORTS_ON_OOM
#endif
#elif __cplusplus >= 201103L || _MSVC_LANG >= 201103L
#define GC_NOEXCEPT noexcept
#else
#define GC_NOEXCEPT throw()
#endif
#endif
#endif
#endif
#ifdef __cplusplus
  extern "C" {
#endif
typedef void * GC_PTR;
#ifdef _WIN64
#if defined(__int64) && !defined(CPPCHECK)
    typedef unsigned __int64 GC_word;
    typedef __int64 GC_signed_word;
#else
    typedef unsigned long long GC_word;
    typedef long long GC_signed_word;
#endif
#else
  typedef unsigned long GC_word;
  typedef long GC_signed_word;
#endif
GC_API unsigned GC_CALL GC_get_version(void);
GC_API GC_ATTR_DEPRECATED GC_word GC_gc_no;
GC_API GC_word GC_CALL GC_get_gc_no(void);
#ifdef GC_THREADS
  GC_API GC_ATTR_DEPRECATED int GC_parallel;
  GC_API int GC_CALL GC_get_parallel(void);
  GC_API void GC_CALL GC_set_markers_count(unsigned);
#endif
typedef void * (GC_CALLBACK * GC_oom_func)(size_t );
GC_API GC_ATTR_DEPRECATED GC_oom_func GC_oom_fn;
GC_API void GC_CALL GC_set_oom_fn(GC_oom_func) GC_ATTR_NONNULL(1);
GC_API GC_oom_func GC_CALL GC_get_oom_fn(void);
typedef void (GC_CALLBACK * GC_on_heap_resize_proc)(GC_word );
GC_API GC_ATTR_DEPRECATED GC_on_heap_resize_proc GC_on_heap_resize;
GC_API void GC_CALL GC_set_on_heap_resize(GC_on_heap_resize_proc);
GC_API GC_on_heap_resize_proc GC_CALL GC_get_on_heap_resize(void);
typedef enum {
    GC_EVENT_START ,
    GC_EVENT_MARK_START,
    GC_EVENT_MARK_END,
    GC_EVENT_RECLAIM_START,
    GC_EVENT_RECLAIM_END,
    GC_EVENT_END ,
    GC_EVENT_PRE_STOP_WORLD ,
    GC_EVENT_POST_STOP_WORLD ,
    GC_EVENT_PRE_START_WORLD ,
    GC_EVENT_POST_START_WORLD ,
    GC_EVENT_THREAD_SUSPENDED,
    GC_EVENT_THREAD_UNSUSPENDED
} GC_EventType;
typedef void (GC_CALLBACK * GC_on_collection_event_proc)(GC_EventType);
GC_API void GC_CALL GC_set_on_collection_event(GC_on_collection_event_proc);
GC_API GC_on_collection_event_proc GC_CALL GC_get_on_collection_event(void);
#if defined(GC_THREADS) || (defined(GC_BUILD) && defined(NN_PLATFORM_CTR))
  typedef void (GC_CALLBACK * GC_on_thread_event_proc)(GC_EventType,
                                                void * );
  GC_API void GC_CALL GC_set_on_thread_event(GC_on_thread_event_proc);
  GC_API GC_on_thread_event_proc GC_CALL GC_get_on_thread_event(void);
#endif
GC_API GC_ATTR_DEPRECATED int GC_find_leak;
GC_API void GC_CALL GC_set_find_leak(int);
GC_API int GC_CALL GC_get_find_leak(void);
GC_API GC_ATTR_DEPRECATED int GC_all_interior_pointers;
GC_API void GC_CALL GC_set_all_interior_pointers(int);
GC_API int GC_CALL GC_get_all_interior_pointers(void);
GC_API GC_ATTR_DEPRECATED int GC_finalize_on_demand;
GC_API void GC_CALL GC_set_finalize_on_demand(int);
GC_API int GC_CALL GC_get_finalize_on_demand(void);
GC_API GC_ATTR_DEPRECATED int GC_java_finalization;
GC_API void GC_CALL GC_set_java_finalization(int);
GC_API int GC_CALL GC_get_java_finalization(void);
typedef void (GC_CALLBACK * GC_finalizer_notifier_proc)(void);
GC_API GC_ATTR_DEPRECATED GC_finalizer_notifier_proc GC_finalizer_notifier;
GC_API void GC_CALL GC_set_finalizer_notifier(GC_finalizer_notifier_proc);
GC_API GC_finalizer_notifier_proc GC_CALL GC_get_finalizer_notifier(void);
GC_API
#ifndef GC_DONT_GC
    GC_ATTR_DEPRECATED
#endif
  int GC_dont_gc;
GC_API GC_ATTR_DEPRECATED int GC_dont_expand;
GC_API void GC_CALL GC_set_dont_expand(int);
GC_API int GC_CALL GC_get_dont_expand(void);
GC_API GC_ATTR_DEPRECATED int GC_use_entire_heap;
GC_API GC_ATTR_DEPRECATED int GC_full_freq;
GC_API void GC_CALL GC_set_full_freq(int);
GC_API int GC_CALL GC_get_full_freq(void);
GC_API GC_ATTR_DEPRECATED GC_word GC_non_gc_bytes;
GC_API void GC_CALL GC_set_non_gc_bytes(GC_word);
GC_API GC_word GC_CALL GC_get_non_gc_bytes(void);
GC_API GC_ATTR_DEPRECATED int GC_no_dls;
GC_API void GC_CALL GC_set_no_dls(int);
GC_API int GC_CALL GC_get_no_dls(void);
GC_API GC_ATTR_DEPRECATED GC_word GC_free_space_divisor;
GC_API void GC_CALL GC_set_free_space_divisor(GC_word);
GC_API GC_word GC_CALL GC_get_free_space_divisor(void);
GC_API GC_ATTR_DEPRECATED GC_word GC_max_retries;
GC_API void GC_CALL GC_set_max_retries(GC_word);
GC_API GC_word GC_CALL GC_get_max_retries(void);
GC_API GC_ATTR_DEPRECATED char *GC_stackbottom;
GC_API GC_ATTR_DEPRECATED int GC_dont_precollect;
GC_API void GC_CALL GC_set_dont_precollect(int);
GC_API int GC_CALL GC_get_dont_precollect(void);
GC_API GC_ATTR_DEPRECATED unsigned long GC_time_limit;
#define GC_TIME_UNLIMITED 999999
GC_API void GC_CALL GC_set_time_limit(unsigned long);
GC_API unsigned long GC_CALL GC_get_time_limit(void);
struct GC_timeval_s {
  unsigned long tv_ms;
  unsigned long tv_nsec;
};
GC_API void GC_CALL GC_set_time_limit_tv(struct GC_timeval_s);
GC_API struct GC_timeval_s GC_CALL GC_get_time_limit_tv(void);
GC_API void GC_CALL GC_set_allocd_bytes_per_finalizer(GC_word);
GC_API GC_word GC_CALL GC_get_allocd_bytes_per_finalizer(void);
GC_API void GC_CALL GC_start_performance_measurement(void);
GC_API unsigned long GC_CALL GC_get_full_gc_total_time(void);
GC_API void GC_CALL GC_set_pages_executable(int);
GC_API int GC_CALL GC_get_pages_executable(void);
GC_API void GC_CALL GC_set_min_bytes_allocd(size_t);
GC_API size_t GC_CALL GC_get_min_bytes_allocd(void);
GC_API void GC_CALL GC_set_rate(int);
GC_API int GC_CALL GC_get_rate(void);
GC_API void GC_CALL GC_set_max_prior_attempts(int);
GC_API int GC_CALL GC_get_max_prior_attempts(void);
GC_API void GC_CALL GC_set_disable_automatic_collection(int);
GC_API int GC_CALL GC_get_disable_automatic_collection(void);
GC_API void GC_CALL GC_set_handle_fork(int);
GC_API void GC_CALL GC_atfork_prepare(void);
GC_API void GC_CALL GC_atfork_parent(void);
GC_API void GC_CALL GC_atfork_child(void);
GC_API void GC_CALL GC_init(void);
GC_API int GC_CALL GC_is_init_called(void);
GC_API void GC_CALL GC_deinit(void);
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
        GC_malloc(size_t );
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
        GC_malloc_atomic(size_t );
GC_API GC_ATTR_MALLOC char * GC_CALL GC_strdup(const char *);
GC_API GC_ATTR_MALLOC char * GC_CALL
        GC_strndup(const char *, size_t) GC_ATTR_NONNULL(1);
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
        GC_malloc_uncollectable(size_t );
GC_API GC_ATTR_DEPRECATED void * GC_CALL GC_malloc_stubborn(size_t);
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(2) void * GC_CALL
        GC_memalign(size_t , size_t );
GC_API int GC_CALL GC_posix_memalign(void ** , size_t ,
                        size_t ) GC_ATTR_NONNULL(1);
GC_API void GC_CALL GC_free(void *);
#define GC_MALLOC_STUBBORN(sz)  GC_MALLOC(sz)
#define GC_NEW_STUBBORN(t)      GC_NEW(t)
#define GC_CHANGE_STUBBORN(p)   GC_change_stubborn(p)
GC_API GC_ATTR_DEPRECATED void GC_CALL GC_change_stubborn(const void *);
GC_API void GC_CALL GC_end_stubborn_change(const void *) GC_ATTR_NONNULL(1);
GC_API void * GC_CALL GC_base(void * );
GC_API int GC_CALL GC_is_heap_ptr(const void *);
GC_API size_t GC_CALL GC_size(const void * ) GC_ATTR_NONNULL(1);
GC_API void * GC_CALL GC_realloc(void * ,
                                 size_t )
                         GC_ATTR_ALLOC_SIZE(2);
GC_API int GC_CALL GC_expand_hp(size_t );
GC_API void GC_CALL GC_set_max_heap_size(GC_word );
GC_API void GC_CALL GC_exclude_static_roots(void * ,
                                            void * );
GC_API void GC_CALL GC_clear_exclusion_table(void);
GC_API void GC_CALL GC_clear_roots(void);
GC_API void GC_CALL GC_add_roots(void * ,
                                 void * );
GC_API void GC_CALL GC_remove_roots(void * ,
                                    void * );
GC_API void GC_CALL GC_register_displacement(size_t );
GC_API void GC_CALL GC_debug_register_displacement(size_t );
GC_API void GC_CALL GC_gcollect(void);
GC_API void GC_CALL GC_gcollect_and_unmap(void);
typedef int (GC_CALLBACK * GC_stop_func)(void);
GC_API int GC_CALL GC_try_to_collect(GC_stop_func )
                                                        GC_ATTR_NONNULL(1);
GC_API void GC_CALL GC_set_stop_func(GC_stop_func )
                                                        GC_ATTR_NONNULL(1);
GC_API GC_stop_func GC_CALL GC_get_stop_func(void);
GC_API size_t GC_CALL GC_get_heap_size(void);
GC_API size_t GC_CALL GC_get_free_bytes(void);
GC_API size_t GC_CALL GC_get_unmapped_bytes(void);
GC_API size_t GC_CALL GC_get_bytes_since_gc(void);
GC_API size_t GC_CALL GC_get_expl_freed_bytes_since_gc(void);
GC_API size_t GC_CALL GC_get_total_bytes(void);
GC_API size_t GC_CALL GC_get_obtained_from_os_bytes(void);
GC_API void GC_CALL GC_get_heap_usage_safe(GC_word * ,
                                           GC_word * ,
                                           GC_word * ,
                                           GC_word * ,
                                           GC_word * );
struct GC_prof_stats_s {
  GC_word heapsize_full;
  GC_word free_bytes_full;
  GC_word unmapped_bytes;
  GC_word bytes_allocd_since_gc;
  GC_word allocd_bytes_before_gc;
  GC_word non_gc_bytes;
  GC_word gc_no;
  GC_word markers_m1;
  GC_word bytes_reclaimed_since_gc;
  GC_word reclaimed_bytes_before_gc;
  GC_word expl_freed_bytes_since_gc;
  GC_word obtained_from_os_bytes;
};
GC_API size_t GC_CALL GC_get_prof_stats(struct GC_prof_stats_s *,
                                        size_t );
#ifdef GC_THREADS
  GC_API size_t GC_CALL GC_get_prof_stats_unsafe(struct GC_prof_stats_s *,
                                                 size_t );
#endif
GC_API size_t GC_CALL GC_get_size_map_at(int i);
GC_API size_t GC_CALL GC_get_memory_use(void);
GC_API void GC_CALL GC_disable(void);
GC_API int GC_CALL GC_is_disabled(void);
GC_API void GC_CALL GC_enable(void);
GC_API void GC_CALL GC_set_manual_vdb_allowed(int);
GC_API int GC_CALL GC_get_manual_vdb_allowed(void);
GC_API void GC_CALL GC_enable_incremental(void);
GC_API int GC_CALL GC_is_incremental_mode(void);
#define GC_PROTECTS_POINTER_HEAP  1
#define GC_PROTECTS_PTRFREE_HEAP  2
#define GC_PROTECTS_STATIC_DATA   4
#define GC_PROTECTS_STACK         8
#define GC_PROTECTS_NONE 0
GC_API int GC_CALL GC_incremental_protection_needs(void);
GC_API void GC_CALL GC_start_incremental_collection(void);
GC_API int GC_CALL GC_collect_a_little(void);
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
        GC_malloc_ignore_off_page(size_t );
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
        GC_malloc_atomic_ignore_off_page(size_t );
#ifdef GC_ADD_CALLER
#define GC_EXTRAS GC_RETURN_ADDR, __FILE__, __LINE__
#define GC_EXTRA_PARAMS GC_word ra, const char * s, int i
#else
#define GC_EXTRAS __FILE__, __LINE__
#define GC_EXTRA_PARAMS const char * s, int i
#endif
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
        GC_malloc_atomic_uncollectable(size_t );
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
        GC_debug_malloc_atomic_uncollectable(size_t, GC_EXTRA_PARAMS);
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
        GC_debug_malloc(size_t , GC_EXTRA_PARAMS);
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
        GC_debug_malloc_atomic(size_t , GC_EXTRA_PARAMS);
GC_API GC_ATTR_MALLOC char * GC_CALL
        GC_debug_strdup(const char *, GC_EXTRA_PARAMS);
GC_API GC_ATTR_MALLOC char * GC_CALL
        GC_debug_strndup(const char *, size_t, GC_EXTRA_PARAMS)
                                                        GC_ATTR_NONNULL(1);
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
        GC_debug_malloc_uncollectable(size_t ,
                                      GC_EXTRA_PARAMS);
GC_API GC_ATTR_DEPRECATED void * GC_CALL
        GC_debug_malloc_stubborn(size_t , GC_EXTRA_PARAMS);
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
        GC_debug_malloc_ignore_off_page(size_t ,
                                        GC_EXTRA_PARAMS);
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
        GC_debug_malloc_atomic_ignore_off_page(size_t ,
                                        GC_EXTRA_PARAMS);
GC_API void GC_CALL GC_debug_free(void *);
GC_API void * GC_CALL GC_debug_realloc(void * ,
                        size_t , GC_EXTRA_PARAMS)
                         GC_ATTR_ALLOC_SIZE(2);
GC_API GC_ATTR_DEPRECATED void GC_CALL GC_debug_change_stubborn(const void *);
GC_API void GC_CALL GC_debug_end_stubborn_change(const void *)
                                                        GC_ATTR_NONNULL(1);
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
        GC_debug_malloc_replacement(size_t );
GC_API  GC_ATTR_ALLOC_SIZE(2) void * GC_CALL
        GC_debug_realloc_replacement(void * ,
                                     size_t );
#ifdef GC_DEBUG_REPLACEMENT
#define GC_MALLOC(sz) GC_debug_malloc_replacement(sz)
#define GC_REALLOC(old, sz) GC_debug_realloc_replacement(old, sz)
#elif defined(GC_DEBUG)
#define GC_MALLOC(sz) GC_debug_malloc(sz, GC_EXTRAS)
#define GC_REALLOC(old, sz) GC_debug_realloc(old, sz, GC_EXTRAS)
#else
#define GC_MALLOC(sz) GC_malloc(sz)
#define GC_REALLOC(old, sz) GC_realloc(old, sz)
#endif
#ifdef GC_DEBUG
#define GC_MALLOC_ATOMIC(sz) GC_debug_malloc_atomic(sz, GC_EXTRAS)
#define GC_STRDUP(s) GC_debug_strdup(s, GC_EXTRAS)
#define GC_STRNDUP(s, sz) GC_debug_strndup(s, sz, GC_EXTRAS)
#define GC_MALLOC_ATOMIC_UNCOLLECTABLE(sz) \
                        GC_debug_malloc_atomic_uncollectable(sz, GC_EXTRAS)
#define GC_MALLOC_UNCOLLECTABLE(sz) \
                        GC_debug_malloc_uncollectable(sz, GC_EXTRAS)
#define GC_MALLOC_IGNORE_OFF_PAGE(sz) \
                        GC_debug_malloc_ignore_off_page(sz, GC_EXTRAS)
#define GC_MALLOC_ATOMIC_IGNORE_OFF_PAGE(sz) \
                        GC_debug_malloc_atomic_ignore_off_page(sz, GC_EXTRAS)
#define GC_FREE(p) GC_debug_free(p)
#define GC_REGISTER_FINALIZER(p, f, d, of, od) \
      GC_debug_register_finalizer(p, f, d, of, od)
#define GC_REGISTER_FINALIZER_IGNORE_SELF(p, f, d, of, od) \
      GC_debug_register_finalizer_ignore_self(p, f, d, of, od)
#define GC_REGISTER_FINALIZER_NO_ORDER(p, f, d, of, od) \
      GC_debug_register_finalizer_no_order(p, f, d, of, od)
#define GC_REGISTER_FINALIZER_UNREACHABLE(p, f, d, of, od) \
      GC_debug_register_finalizer_unreachable(p, f, d, of, od)
#define GC_END_STUBBORN_CHANGE(p) GC_debug_end_stubborn_change(p)
#define GC_PTR_STORE_AND_DIRTY(p, q) GC_debug_ptr_store_and_dirty(p, q)
#define GC_GENERAL_REGISTER_DISAPPEARING_LINK(link, obj) \
      GC_general_register_disappearing_link(link, \
                                        GC_base(( void *)(obj)))
#define GC_REGISTER_LONG_LINK(link, obj) \
      GC_register_long_link(link, GC_base(( void *)(obj)))
#define GC_REGISTER_DISPLACEMENT(n) GC_debug_register_displacement(n)
#else
#define GC_MALLOC_ATOMIC(sz) GC_malloc_atomic(sz)
#define GC_STRDUP(s) GC_strdup(s)
#define GC_STRNDUP(s, sz) GC_strndup(s, sz)
#define GC_MALLOC_ATOMIC_UNCOLLECTABLE(sz) GC_malloc_atomic_uncollectable(sz)
#define GC_MALLOC_UNCOLLECTABLE(sz) GC_malloc_uncollectable(sz)
#define GC_MALLOC_IGNORE_OFF_PAGE(sz) \
                        GC_malloc_ignore_off_page(sz)
#define GC_MALLOC_ATOMIC_IGNORE_OFF_PAGE(sz) \
                        GC_malloc_atomic_ignore_off_page(sz)
#define GC_FREE(p) GC_free(p)
#define GC_REGISTER_FINALIZER(p, f, d, of, od) \
      GC_register_finalizer(p, f, d, of, od)
#define GC_REGISTER_FINALIZER_IGNORE_SELF(p, f, d, of, od) \
      GC_register_finalizer_ignore_self(p, f, d, of, od)
#define GC_REGISTER_FINALIZER_NO_ORDER(p, f, d, of, od) \
      GC_register_finalizer_no_order(p, f, d, of, od)
#define GC_REGISTER_FINALIZER_UNREACHABLE(p, f, d, of, od) \
      GC_register_finalizer_unreachable(p, f, d, of, od)
#define GC_END_STUBBORN_CHANGE(p) GC_end_stubborn_change(p)
#define GC_PTR_STORE_AND_DIRTY(p, q) GC_ptr_store_and_dirty(p, q)
#define GC_GENERAL_REGISTER_DISAPPEARING_LINK(link, obj) \
      GC_general_register_disappearing_link(link, obj)
#define GC_REGISTER_LONG_LINK(link, obj) \
      GC_register_long_link(link, obj)
#define GC_REGISTER_DISPLACEMENT(n) GC_register_displacement(n)
#endif
#define GC_NEW(t)               ((t*)GC_MALLOC(sizeof(t)))
#define GC_NEW_ATOMIC(t)        ((t*)GC_MALLOC_ATOMIC(sizeof(t)))
#define GC_NEW_UNCOLLECTABLE(t) ((t*)GC_MALLOC_UNCOLLECTABLE(sizeof(t)))
#ifdef GC_REQUIRE_WCSDUP
  GC_API GC_ATTR_MALLOC wchar_t * GC_CALL
        GC_wcsdup(const wchar_t *) GC_ATTR_NONNULL(1);
  GC_API GC_ATTR_MALLOC wchar_t * GC_CALL
        GC_debug_wcsdup(const wchar_t *, GC_EXTRA_PARAMS) GC_ATTR_NONNULL(1);
#ifdef GC_DEBUG
#define GC_WCSDUP(s) GC_debug_wcsdup(s, GC_EXTRAS)
#else
#define GC_WCSDUP(s) GC_wcsdup(s)
#endif
#endif
typedef void (GC_CALLBACK * GC_finalization_proc)(void * ,
                                                  void * );
GC_API void GC_CALL GC_register_finalizer(void * ,
                        GC_finalization_proc , void * ,
                        GC_finalization_proc * , void ** )
                                                GC_ATTR_NONNULL(1);
GC_API void GC_CALL GC_debug_register_finalizer(void * ,
                        GC_finalization_proc , void * ,
                        GC_finalization_proc * , void ** )
                                                GC_ATTR_NONNULL(1);
GC_API void GC_CALL GC_register_finalizer_ignore_self(void * ,
                        GC_finalization_proc , void * ,
                        GC_finalization_proc * , void ** )
                                                GC_ATTR_NONNULL(1);
GC_API void GC_CALL GC_debug_register_finalizer_ignore_self(void * ,
                        GC_finalization_proc , void * ,
                        GC_finalization_proc * , void ** )
                                                GC_ATTR_NONNULL(1);
GC_API void GC_CALL GC_register_finalizer_no_order(void * ,
                        GC_finalization_proc , void * ,
                        GC_finalization_proc * , void ** )
                                                GC_ATTR_NONNULL(1);
GC_API void GC_CALL GC_debug_register_finalizer_no_order(void * ,
                        GC_finalization_proc , void * ,
                        GC_finalization_proc * , void ** )
                                                GC_ATTR_NONNULL(1);
GC_API void GC_CALL GC_register_finalizer_unreachable(void * ,
                        GC_finalization_proc , void * ,
                        GC_finalization_proc * , void ** )
                                                GC_ATTR_NONNULL(1);
GC_API void GC_CALL GC_debug_register_finalizer_unreachable(void * ,
                        GC_finalization_proc , void * ,
                        GC_finalization_proc * , void ** )
                                                GC_ATTR_NONNULL(1);
#define GC_NO_MEMORY 2
GC_API int GC_CALL GC_register_disappearing_link(void ** )
                                                GC_ATTR_NONNULL(1);
GC_API int GC_CALL GC_general_register_disappearing_link(void ** ,
                                                    const void * )
                        GC_ATTR_NONNULL(1) GC_ATTR_NONNULL(2);
GC_API int GC_CALL GC_move_disappearing_link(void ** ,
                                             void ** )
                        GC_ATTR_NONNULL(2);
GC_API int GC_CALL GC_unregister_disappearing_link(void ** );
GC_API int GC_CALL GC_register_long_link(void ** ,
                                    const void * )
                        GC_ATTR_NONNULL(1) GC_ATTR_NONNULL(2);
GC_API int GC_CALL GC_move_long_link(void ** ,
                                     void ** )
                        GC_ATTR_NONNULL(2);
GC_API int GC_CALL GC_unregister_long_link(void ** );
typedef enum {
   GC_TOGGLE_REF_DROP,
   GC_TOGGLE_REF_STRONG,
   GC_TOGGLE_REF_WEAK
} GC_ToggleRefStatus;
typedef GC_ToggleRefStatus (GC_CALLBACK *GC_toggleref_func)(void * );
GC_API void GC_CALL GC_set_toggleref_func(GC_toggleref_func);
GC_API GC_toggleref_func GC_CALL GC_get_toggleref_func(void);
GC_API int GC_CALL GC_toggleref_add(void * , int )
                                                GC_ATTR_NONNULL(1);
typedef void (GC_CALLBACK * GC_await_finalize_proc)(void * );
GC_API void GC_CALL GC_set_await_finalize_proc(GC_await_finalize_proc);
GC_API GC_await_finalize_proc GC_CALL GC_get_await_finalize_proc(void);
GC_API int GC_CALL GC_should_invoke_finalizers(void);
GC_API int GC_CALL GC_invoke_finalizers(void);
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)
#define GC_reachable_here(ptr) \
                __asm__ __volatile__(" " : : "X"(ptr) : "memory")
#else
  GC_API void GC_CALL GC_noop1(GC_word);
#ifdef LINT2
#define GC_reachable_here(ptr) GC_noop1(~(GC_word)(ptr)^(~(GC_word)0))
#else
#define GC_reachable_here(ptr) GC_noop1((GC_word)(ptr))
#endif
#endif
typedef void (GC_CALLBACK * GC_warn_proc)(char * ,
                                          GC_word );
GC_API void GC_CALL GC_set_warn_proc(GC_warn_proc ) GC_ATTR_NONNULL(1);
GC_API GC_warn_proc GC_CALL GC_get_warn_proc(void);
GC_API void GC_CALLBACK GC_ignore_warn_proc(char *, GC_word);
GC_API void GC_CALL GC_set_log_fd(int);
typedef void (GC_CALLBACK * GC_abort_func)(const char * );
GC_API void GC_CALL GC_set_abort_func(GC_abort_func) GC_ATTR_NONNULL(1);
GC_API GC_abort_func GC_CALL GC_get_abort_func(void);
GC_API void GC_CALL GC_abort_on_oom(void);
typedef GC_word GC_hidden_pointer;
#define GC_HIDE_POINTER(p) (~(GC_hidden_pointer)(p))
#define GC_REVEAL_POINTER(p) ((void *)GC_HIDE_POINTER(p))
#if defined(I_HIDE_POINTERS) || defined(GC_I_HIDE_POINTERS)
#define HIDE_POINTER(p) GC_HIDE_POINTER(p)
#define REVEAL_POINTER(p) GC_REVEAL_POINTER(p)
#endif
#ifdef GC_THREADS
  GC_API void GC_CALL GC_alloc_lock(void);
  GC_API void GC_CALL GC_alloc_unlock(void);
#else
#define GC_alloc_lock() (void)0
#define GC_alloc_unlock() (void)0
#endif
typedef void * (GC_CALLBACK * GC_fn_type)(void * );
GC_API void * GC_CALL GC_call_with_alloc_lock(GC_fn_type ,
                                void * ) GC_ATTR_NONNULL(1);
struct GC_stack_base {
  void * mem_base;
#if defined(__ia64) || defined(__ia64__) || defined(_M_IA64)
    void * reg_base;
#endif
};
typedef void * (GC_CALLBACK * GC_stack_base_func)(
                struct GC_stack_base * , void * );
GC_API void * GC_CALL GC_call_with_stack_base(GC_stack_base_func ,
                                        void * ) GC_ATTR_NONNULL(1);
#define GC_SUCCESS 0
#define GC_DUPLICATE 1
#define GC_NO_THREADS 2
#define GC_UNIMPLEMENTED 3
#define GC_NOT_FOUND 4
#if defined(GC_DARWIN_THREADS) || defined(GC_WIN32_THREADS)
  GC_API void GC_CALL GC_use_threads_discovery(void);
#endif
#ifdef GC_THREADS
  GC_API void GC_CALL GC_set_suspend_signal(int);
  GC_API void GC_CALL GC_set_thr_restart_signal(int);
  GC_API int GC_CALL GC_get_suspend_signal(void);
  GC_API int GC_CALL GC_get_thr_restart_signal(void);
  GC_API void GC_CALL GC_start_mark_threads(void);
  GC_API void GC_CALL GC_allow_register_threads(void);
  GC_API int GC_CALL GC_register_my_thread(const struct GC_stack_base *)
                                                        GC_ATTR_NONNULL(1);
  GC_API int GC_CALL GC_thread_is_registered(void);
  GC_API void GC_CALL GC_register_altstack(void * ,
                                           GC_word ,
                                           void * ,
                                           GC_word );
  GC_API int GC_CALL GC_unregister_my_thread(void);
  GC_API void GC_CALL GC_stop_world_external(void);
  GC_API void GC_CALL GC_start_world_external(void);
#endif
GC_API void * GC_CALL GC_do_blocking(GC_fn_type ,
                                void * ) GC_ATTR_NONNULL(1);
GC_API void * GC_CALL GC_call_with_gc_active(GC_fn_type ,
                                void * ) GC_ATTR_NONNULL(1);
GC_API int GC_CALL GC_get_stack_base(struct GC_stack_base *)
                                                        GC_ATTR_NONNULL(1);
GC_API void * GC_CALL GC_get_my_stackbottom(struct GC_stack_base *)
                                                        GC_ATTR_NONNULL(1);
GC_API void GC_CALL GC_set_stackbottom(void * ,
                                       const struct GC_stack_base *)
                                                        GC_ATTR_NONNULL(2);
GC_API void * GC_CALL GC_same_obj(void * , void * );
GC_API void * GC_CALL GC_pre_incr(void **, ptrdiff_t )
                                                        GC_ATTR_NONNULL(1);
GC_API void * GC_CALL GC_post_incr(void **, ptrdiff_t )
                                                        GC_ATTR_NONNULL(1);
GC_API void * GC_CALL GC_is_visible(void * );
GC_API void * GC_CALL GC_is_valid_displacement(void * );
GC_API void GC_CALL GC_dump(void);
GC_API void GC_CALL GC_dump_named(const char * );
GC_API void GC_CALL GC_dump_regions(void);
GC_API void GC_CALL GC_dump_finalization(void);
#if defined(GC_DEBUG) && defined(__GNUC__)
#define GC_PTR_ADD3(x, n, type_of_result) \
        ((type_of_result)GC_same_obj((x)+(n), (x)))
#define GC_PRE_INCR3(x, n, type_of_result) \
        ((type_of_result)GC_pre_incr((void **)(&(x)), (n)*sizeof(*x)))
#define GC_POST_INCR3(x, n, type_of_result) \
        ((type_of_result)GC_post_incr((void **)(&(x)), (n)*sizeof(*x)))
#define GC_PTR_ADD(x, n) GC_PTR_ADD3(x, n, __typeof__(x))
#define GC_PRE_INCR(x, n) GC_PRE_INCR3(x, n, __typeof__(x))
#define GC_POST_INCR(x) GC_POST_INCR3(x, 1, __typeof__(x))
#define GC_POST_DECR(x) GC_POST_INCR3(x, -1, __typeof__(x))
#else
#define GC_PTR_ADD(x, n) ((x)+(n))
#define GC_PRE_INCR(x, n) ((x) += (n))
#define GC_POST_INCR(x) ((x)++)
#define GC_POST_DECR(x) ((x)--)
#endif
#ifdef GC_DEBUG
#define GC_PTR_STORE(p, q) \
        (*(void **)GC_is_visible((void *)(p)) = \
                    GC_is_valid_displacement((void *)(q)))
#else
#define GC_PTR_STORE(p, q) (*(void **)(p) = (void *)(q))
#endif
GC_API void GC_CALL GC_ptr_store_and_dirty(void * ,
                                           const void * );
GC_API void GC_CALL GC_debug_ptr_store_and_dirty(void * ,
                                                 const void * );
GC_API void (GC_CALLBACK * GC_same_obj_print_proc)(void * ,
                                                   void * );
GC_API void (GC_CALLBACK * GC_is_valid_displacement_print_proc)(void *);
GC_API void (GC_CALLBACK * GC_is_visible_print_proc)(void *);
#ifdef GC_PTHREADS
#ifdef __cplusplus
    }
#endif
#ifndef GC_PTHREAD_REDIRECTS_H
#define GC_PTHREAD_REDIRECTS_H
#if defined(GC_H) && defined(GC_PTHREADS)
#ifndef GC_PTHREAD_REDIRECTS_ONLY
#include <pthread.h>
#ifndef GC_NO_DLOPEN
#include <dlfcn.h>
#endif
#ifndef GC_NO_PTHREAD_SIGMASK
#include <signal.h>
#endif
#ifdef __cplusplus
    extern "C" {
#endif
#ifndef GC_SUSPEND_THREAD_ID
#define GC_SUSPEND_THREAD_ID pthread_t
#endif
#ifndef GC_NO_DLOPEN
    GC_API void *GC_dlopen(const char * , int );
#endif
#ifndef GC_NO_PTHREAD_SIGMASK
#if defined(GC_PTHREAD_SIGMASK_NEEDED) \
        || defined(_BSD_SOURCE) || defined(_GNU_SOURCE) \
        || (_POSIX_C_SOURCE >= 199506L) || (_XOPEN_SOURCE >= 500)
      GC_API int GC_pthread_sigmask(int , const sigset_t *,
                                    sigset_t * );
#endif
#endif
#ifndef GC_PTHREAD_CREATE_CONST
#define GC_PTHREAD_CREATE_CONST const
#endif
  GC_API int GC_pthread_create(pthread_t *,
                               GC_PTHREAD_CREATE_CONST pthread_attr_t *,
                               void *(*)(void *), void * );
  GC_API int GC_pthread_join(pthread_t, void ** );
  GC_API int GC_pthread_detach(pthread_t);
#ifndef GC_NO_PTHREAD_CANCEL
    GC_API int GC_pthread_cancel(pthread_t);
#endif
#if defined(GC_HAVE_PTHREAD_EXIT) && !defined(GC_PTHREAD_EXIT_DECLARED)
#define GC_PTHREAD_EXIT_DECLARED
    GC_API void GC_pthread_exit(void *) GC_PTHREAD_EXIT_ATTRIBUTE;
#endif
#ifdef __cplusplus
    }
#endif
#endif
#if !defined(GC_NO_THREAD_REDIRECTS) && !defined(GC_USE_LD_WRAP)
#undef pthread_create
#undef pthread_join
#undef pthread_detach
#define pthread_create GC_pthread_create
#define pthread_join GC_pthread_join
#define pthread_detach GC_pthread_detach
#ifndef GC_NO_PTHREAD_SIGMASK
#undef pthread_sigmask
#define pthread_sigmask GC_pthread_sigmask
#endif
#ifndef GC_NO_DLOPEN
#undef dlopen
#define dlopen GC_dlopen
#endif
#ifndef GC_NO_PTHREAD_CANCEL
#undef pthread_cancel
#define pthread_cancel GC_pthread_cancel
#endif
#ifdef GC_HAVE_PTHREAD_EXIT
#undef pthread_exit
#define pthread_exit GC_pthread_exit
#endif
#endif
#endif
#endif
#ifdef __cplusplus
    extern "C" {
#endif
#endif
GC_API GC_ATTR_MALLOC void * GC_CALL GC_malloc_many(size_t );
#define GC_NEXT(p) (*(void * *)(p))
typedef int (GC_CALLBACK * GC_has_static_roots_func)(
                                        const char * ,
                                        void * ,
                                        size_t );
GC_API void GC_CALL GC_register_has_static_roots_callback(
                                        GC_has_static_roots_func);
#if !defined(CPPCHECK) && !defined(GC_WINDOWS_H_INCLUDED) && defined(WINAPI)
#define GC_WINDOWS_H_INCLUDED
#endif
#if defined(GC_WIN32_THREADS) \
    && (!defined(GC_PTHREADS) || defined(GC_BUILD) \
        || defined(GC_WINDOWS_H_INCLUDED))
#if (!defined(GC_NO_THREAD_DECLS) || defined(GC_BUILD)) \
     && !defined(GC_DONT_INCL_WINDOWS_H)
#ifdef __cplusplus
      }
#endif
#if !defined(_WIN32_WCE) && !defined(__CEGCC__)
#include <process.h>
#endif
#if defined(GC_BUILD) || !defined(GC_DONT_INCLUDE_WINDOWS_H)
#include <windows.h>
#define GC_WINDOWS_H_INCLUDED
#endif
#ifdef __cplusplus
      extern "C" {
#endif
#ifdef GC_UNDERSCORE_STDCALL
#define GC_CreateThread _GC_CreateThread
#define GC_ExitThread _GC_ExitThread
#endif
#ifndef DECLSPEC_NORETURN
#ifdef GC_WINDOWS_H_INCLUDED
#define DECLSPEC_NORETURN
#else
#define DECLSPEC_NORETURN __declspec(noreturn)
#endif
#endif
#if !defined(_UINTPTR_T) && !defined(_UINTPTR_T_DEFINED) \
       && !defined(UINTPTR_MAX)
      typedef GC_word GC_uintptr_t;
#else
      typedef uintptr_t GC_uintptr_t;
#endif
#ifdef _WIN64
#define GC_WIN32_SIZE_T GC_uintptr_t
#elif defined(GC_WINDOWS_H_INCLUDED)
#define GC_WIN32_SIZE_T DWORD
#else
#define GC_WIN32_SIZE_T unsigned long
#endif
#ifdef GC_INSIDE_DLL
#ifdef GC_UNDERSCORE_STDCALL
#define GC_DllMain _GC_DllMain
#endif
#ifdef GC_WINDOWS_H_INCLUDED
        GC_API BOOL WINAPI GC_DllMain(HINSTANCE ,
                                      ULONG ,
                                      LPVOID );
#else
        GC_API int __stdcall GC_DllMain(void *, unsigned long, void *);
#endif
#endif
#ifdef GC_WINDOWS_H_INCLUDED
      GC_API HANDLE WINAPI GC_CreateThread(
                LPSECURITY_ATTRIBUTES ,
                GC_WIN32_SIZE_T ,
                LPTHREAD_START_ROUTINE ,
                LPVOID , DWORD ,
                LPDWORD );
      GC_API DECLSPEC_NORETURN void WINAPI GC_ExitThread(
                                                DWORD );
#else
      struct _SECURITY_ATTRIBUTES;
      GC_API void *__stdcall GC_CreateThread(struct _SECURITY_ATTRIBUTES *,
                                GC_WIN32_SIZE_T,
                                unsigned long (__stdcall *)(void *),
                                void *, unsigned long, unsigned long *);
      GC_API DECLSPEC_NORETURN void __stdcall GC_ExitThread(unsigned long);
#endif
#if !defined(_WIN32_WCE) && !defined(__CEGCC__)
      GC_API GC_uintptr_t GC_CALL GC_beginthreadex(
                        void * , unsigned ,
                        unsigned (__stdcall *)(void *),
                        void * , unsigned ,
                        unsigned * );
      GC_API void GC_CALL GC_endthreadex(unsigned );
#endif
#endif
#ifdef GC_WINMAIN_REDIRECT
#define WinMain GC_WinMain
#endif
#define GC_use_DllMain GC_use_threads_discovery
#ifndef GC_NO_THREAD_REDIRECTS
#define CreateThread GC_CreateThread
#define ExitThread GC_ExitThread
#undef _beginthreadex
#define _beginthreadex GC_beginthreadex
#undef _endthreadex
#define _endthreadex GC_endthreadex
#endif
#endif
GC_API void GC_CALL GC_set_force_unmap_on_gcollect(int);
GC_API int GC_CALL GC_get_force_unmap_on_gcollect(void);
#if defined(__CYGWIN32__) || defined(__CYGWIN__)
#ifdef __x86_64__
    extern int __data_start__[], __data_end__[];
    extern int __bss_start__[], __bss_end__[];
#define GC_DATASTART ((GC_word)__data_start__ < (GC_word)__bss_start__ \
                         ? (void *)__data_start__ : (void *)__bss_start__)
#define GC_DATAEND ((GC_word)__data_end__ > (GC_word)__bss_end__ \
                       ? (void *)__data_end__ : (void *)__bss_end__)
#else
    extern int _data_start__[], _data_end__[], _bss_start__[], _bss_end__[];
#define GC_DATASTART ((GC_word)_data_start__ < (GC_word)_bss_start__ \
                         ? (void *)_data_start__ : (void *)_bss_start__)
#define GC_DATAEND ((GC_word)_data_end__ > (GC_word)_bss_end__ \
                      ? (void *)_data_end__ : (void *)_bss_end__)
#endif
#define GC_INIT_CONF_ROOTS GC_add_roots(GC_DATASTART, GC_DATAEND); \
                                 GC_gcollect()
#elif defined(_AIX)
  extern int _data[], _end[];
#define GC_DATASTART ((void *)_data)
#define GC_DATAEND ((void *)_end)
#define GC_INIT_CONF_ROOTS GC_add_roots(GC_DATASTART, GC_DATAEND)
#elif (defined(HOST_ANDROID) || defined(__ANDROID__)) \
      && defined(IGNORE_DYNAMIC_LOADING)
#pragma weak __dso_handle
  extern int __dso_handle[];
  GC_API void * GC_CALL GC_find_limit(void * , int );
#define GC_INIT_CONF_ROOTS (void)(__dso_handle != 0 \
                                   ? (GC_add_roots(__dso_handle, \
                                            GC_find_limit(__dso_handle, \
                                                          1 )), 0) : 0)
#else
#define GC_INIT_CONF_ROOTS
#endif
#ifdef GC_DONT_EXPAND
#define GC_INIT_CONF_DONT_EXPAND GC_set_dont_expand(1)
#else
#define GC_INIT_CONF_DONT_EXPAND
#endif
#ifdef GC_FORCE_UNMAP_ON_GCOLLECT
#define GC_INIT_CONF_FORCE_UNMAP_ON_GCOLLECT \
                GC_set_force_unmap_on_gcollect(1)
#else
#define GC_INIT_CONF_FORCE_UNMAP_ON_GCOLLECT
#endif
#ifdef GC_DONT_GC
#define GC_INIT_CONF_MAX_RETRIES (void)(GC_dont_gc = 1)
#elif defined(GC_MAX_RETRIES) && !defined(CPPCHECK)
#define GC_INIT_CONF_MAX_RETRIES GC_set_max_retries(GC_MAX_RETRIES)
#else
#define GC_INIT_CONF_MAX_RETRIES
#endif
#if defined(GC_ALLOCD_BYTES_PER_FINALIZER) && !defined(CPPCHECK)
#define GC_INIT_CONF_ALLOCD_BYTES_PER_FINALIZER \
        GC_set_allocd_bytes_per_finalizer(GC_ALLOCD_BYTES_PER_FINALIZER)
#else
#define GC_INIT_CONF_ALLOCD_BYTES_PER_FINALIZER
#endif
#if defined(GC_FREE_SPACE_DIVISOR) && !defined(CPPCHECK)
#define GC_INIT_CONF_FREE_SPACE_DIVISOR \
                GC_set_free_space_divisor(GC_FREE_SPACE_DIVISOR)
#else
#define GC_INIT_CONF_FREE_SPACE_DIVISOR
#endif
#if defined(GC_FULL_FREQ) && !defined(CPPCHECK)
#define GC_INIT_CONF_FULL_FREQ GC_set_full_freq(GC_FULL_FREQ)
#else
#define GC_INIT_CONF_FULL_FREQ
#endif
#if defined(GC_TIME_LIMIT) && !defined(CPPCHECK)
#define GC_INIT_CONF_TIME_LIMIT GC_set_time_limit(GC_TIME_LIMIT)
#else
#define GC_INIT_CONF_TIME_LIMIT
#endif
#if defined(GC_MARKERS) && defined(GC_THREADS) && !defined(CPPCHECK)
#define GC_INIT_CONF_MARKERS GC_set_markers_count(GC_MARKERS)
#else
#define GC_INIT_CONF_MARKERS
#endif
#if defined(GC_SIG_SUSPEND) && defined(GC_THREADS) && !defined(CPPCHECK)
#define GC_INIT_CONF_SUSPEND_SIGNAL GC_set_suspend_signal(GC_SIG_SUSPEND)
#else
#define GC_INIT_CONF_SUSPEND_SIGNAL
#endif
#if defined(GC_SIG_THR_RESTART) && defined(GC_THREADS) && !defined(CPPCHECK)
#define GC_INIT_CONF_THR_RESTART_SIGNAL \
                GC_set_thr_restart_signal(GC_SIG_THR_RESTART)
#else
#define GC_INIT_CONF_THR_RESTART_SIGNAL
#endif
#if defined(GC_MAXIMUM_HEAP_SIZE) && !defined(CPPCHECK)
#define GC_INIT_CONF_MAXIMUM_HEAP_SIZE \
                GC_set_max_heap_size(GC_MAXIMUM_HEAP_SIZE)
#else
#define GC_INIT_CONF_MAXIMUM_HEAP_SIZE
#endif
#ifdef GC_IGNORE_WARN
#define GC_INIT_CONF_IGNORE_WARN GC_set_warn_proc(GC_ignore_warn_proc)
#else
#define GC_INIT_CONF_IGNORE_WARN
#endif
#if defined(GC_INITIAL_HEAP_SIZE) && !defined(CPPCHECK)
#define GC_INIT_CONF_INITIAL_HEAP_SIZE \
                { size_t heap_size = GC_get_heap_size(); \
                  if (heap_size < (GC_INITIAL_HEAP_SIZE)) \
                    (void)GC_expand_hp((GC_INITIAL_HEAP_SIZE) - heap_size); }
#else
#define GC_INIT_CONF_INITIAL_HEAP_SIZE
#endif
#define GC_INIT() { GC_INIT_CONF_DONT_EXPAND;  \
                    GC_INIT_CONF_FORCE_UNMAP_ON_GCOLLECT; \
                    GC_INIT_CONF_MAX_RETRIES; \
                    GC_INIT_CONF_ALLOCD_BYTES_PER_FINALIZER; \
                    GC_INIT_CONF_FREE_SPACE_DIVISOR; \
                    GC_INIT_CONF_FULL_FREQ; \
                    GC_INIT_CONF_TIME_LIMIT; \
                    GC_INIT_CONF_MARKERS; \
                    GC_INIT_CONF_SUSPEND_SIGNAL; \
                    GC_INIT_CONF_THR_RESTART_SIGNAL; \
                    GC_INIT_CONF_MAXIMUM_HEAP_SIZE; \
                    GC_init();  \
                    GC_INIT_CONF_ROOTS;  \
                    GC_INIT_CONF_IGNORE_WARN; \
                    GC_INIT_CONF_INITIAL_HEAP_SIZE; }
GC_API void GC_CALL GC_win32_free_heap(void);
#if defined(__SYMBIAN32__)
  void GC_init_global_static_roots(void);
#endif
#if defined(_AMIGA) && !defined(GC_AMIGA_MAKINGLIB)
  void *GC_amiga_realloc(void *, size_t);
#define GC_realloc(a,b) GC_amiga_realloc(a,b)
  void GC_amiga_set_toany(void (*)(void));
  extern int GC_amiga_free_space_divisor_inc;
  extern void *(*GC_amiga_allocwrapper_do)(size_t, void *(GC_CALL *)(size_t));
#define GC_malloc(a) \
        (*GC_amiga_allocwrapper_do)(a,GC_malloc)
#define GC_malloc_atomic(a) \
        (*GC_amiga_allocwrapper_do)(a,GC_malloc_atomic)
#define GC_malloc_uncollectable(a) \
        (*GC_amiga_allocwrapper_do)(a,GC_malloc_uncollectable)
#define GC_malloc_atomic_uncollectable(a) \
        (*GC_amiga_allocwrapper_do)(a,GC_malloc_atomic_uncollectable)
#define GC_malloc_ignore_off_page(a) \
        (*GC_amiga_allocwrapper_do)(a,GC_malloc_ignore_off_page)
#define GC_malloc_atomic_ignore_off_page(a) \
        (*GC_amiga_allocwrapper_do)(a,GC_malloc_atomic_ignore_off_page)
#endif
#ifdef __cplusplus
  }
#endif
#endif
#endif
#include <stdlib.h>
#if !defined(sony_news)
#include <stddef.h>
#endif
#ifdef DGUX
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#endif
#ifdef BSD_TIME
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#endif
#ifdef PARALLEL_MARK
#define AO_REQUIRE_CAS
#if !defined(__GNUC__) && !defined(AO_ASSUME_WINDOWS98)
#define AO_ASSUME_WINDOWS98
#endif
#endif
#ifndef GC_TINY_FL_H
#define GC_TINY_FL_H
#ifndef GC_GRANULE_BYTES
#if defined(__LP64__) || defined (_LP64) || defined(_WIN64) \
        || defined(__s390x__) \
        || (defined(__x86_64__) && !defined(__ILP32__)) \
        || defined(__alpha__) || defined(__powerpc64__) \
        || defined(__arch64__)
#define GC_GRANULE_BYTES 16
#define GC_GRANULE_WORDS 2
#else
#define GC_GRANULE_BYTES 8
#define GC_GRANULE_WORDS 2
#endif
#endif
#if GC_GRANULE_WORDS == 2
#define GC_WORDS_TO_GRANULES(n) ((n)>>1)
#else
#define GC_WORDS_TO_GRANULES(n) ((n)*sizeof(void *)/GC_GRANULE_BYTES)
#endif
#ifndef GC_TINY_FREELISTS
#if GC_GRANULE_BYTES == 16
#define GC_TINY_FREELISTS 25
#else
#define GC_TINY_FREELISTS 33
#endif
#endif
#define GC_RAW_BYTES_FROM_INDEX(i) ((i) * GC_GRANULE_BYTES)
#endif
#ifndef GC_MARK_H
#define GC_MARK_H
#ifndef GC_H
#endif
#ifdef __cplusplus
  extern "C" {
#endif
#define GC_PROC_BYTES 100
#if defined(GC_BUILD) || defined(NOT_GCBUILD)
  struct GC_ms_entry;
#else
  struct GC_ms_entry { void *opaque; };
#endif
typedef struct GC_ms_entry * (*GC_mark_proc)(GC_word * ,
                                struct GC_ms_entry * ,
                                struct GC_ms_entry * ,
                                GC_word );
#define GC_LOG_MAX_MARK_PROCS 6
#define GC_MAX_MARK_PROCS (1 << GC_LOG_MAX_MARK_PROCS)
#define GC_RESERVED_MARK_PROCS 8
#define GC_GCJ_RESERVED_MARK_PROC_INDEX 0
#define GC_DS_TAG_BITS 2
#define GC_DS_TAGS   ((1 << GC_DS_TAG_BITS) - 1)
#define GC_DS_LENGTH 0
#define GC_DS_BITMAP 1
#define GC_DS_PROC   2
#define GC_MAKE_PROC(proc_index, env) \
            (((((env) << GC_LOG_MAX_MARK_PROCS) \
               | (proc_index)) << GC_DS_TAG_BITS) | GC_DS_PROC)
#define GC_DS_PER_OBJECT 3
#define GC_INDIR_PER_OBJ_BIAS 0x10
GC_API void * GC_least_plausible_heap_addr;
GC_API void * GC_greatest_plausible_heap_addr;
GC_API struct GC_ms_entry * GC_CALL GC_mark_and_push(void * ,
                                struct GC_ms_entry * ,
                                struct GC_ms_entry * ,
                                void ** );
#define GC_MARK_AND_PUSH(obj, msp, lim, src) \
          ((GC_word)(obj) >= (GC_word)GC_least_plausible_heap_addr && \
           (GC_word)(obj) <= (GC_word)GC_greatest_plausible_heap_addr ? \
           GC_mark_and_push(obj, msp, lim, src) : (msp))
GC_API GC_ATTR_CONST size_t GC_CALL GC_get_debug_header_size(void);
#define GC_USR_PTR_FROM_BASE(p) \
                ((void *)((char *)(p) + GC_get_debug_header_size()))
GC_API GC_ATTR_DEPRECATED
#ifdef GC_BUILD
    const
#endif
  size_t GC_debug_header_size;
GC_API void ** GC_CALL GC_new_free_list(void);
GC_API void ** GC_CALL GC_new_free_list_inner(void);
GC_API unsigned GC_CALL GC_new_kind(void ** ,
                            GC_word ,
                            int ,
                            int ) GC_ATTR_NONNULL(1);
GC_API unsigned GC_CALL GC_new_kind_inner(void ** ,
                            GC_word ,
                            int ,
                            int ) GC_ATTR_NONNULL(1);
GC_API unsigned GC_CALL GC_new_proc(GC_mark_proc);
GC_API unsigned GC_CALL GC_new_proc_inner(GC_mark_proc);
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL GC_generic_malloc(
                                                            size_t ,
                                                            int );
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
                                        GC_generic_malloc_ignore_off_page(
                                            size_t , int );
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
                                        GC_generic_malloc_uncollectable(
                                            size_t , int );
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
                                        GC_generic_or_special_malloc(
                                            size_t , int );
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
                                        GC_debug_generic_or_special_malloc(
                                            size_t , int ,
                                            GC_EXTRA_PARAMS);
#ifdef GC_DEBUG
#define GC_GENERIC_OR_SPECIAL_MALLOC(sz, knd) \
                GC_debug_generic_or_special_malloc(sz, knd, GC_EXTRAS)
#else
#define GC_GENERIC_OR_SPECIAL_MALLOC(sz, knd) \
                GC_generic_or_special_malloc(sz, knd)
#endif
GC_API int GC_CALL GC_get_kind_and_size(const void *, size_t * )
                                                        GC_ATTR_NONNULL(1);
typedef void (GC_CALLBACK * GC_describe_type_fn)(void * ,
                                                 char * );
#define GC_TYPE_DESCR_LEN 40
GC_API void GC_CALL GC_register_describe_type_fn(int ,
                                                 GC_describe_type_fn);
GC_API void * GC_CALL GC_clear_stack(void *);
typedef void (GC_CALLBACK * GC_start_callback_proc)(void);
GC_API void GC_CALL GC_set_start_callback(GC_start_callback_proc);
GC_API GC_start_callback_proc GC_CALL GC_get_start_callback(void);
GC_API int GC_CALL GC_is_marked(const void *) GC_ATTR_NONNULL(1);
GC_API void GC_CALL GC_clear_mark_bit(const void *) GC_ATTR_NONNULL(1);
GC_API void GC_CALL GC_set_mark_bit(const void *) GC_ATTR_NONNULL(1);
GC_API void GC_CALL GC_push_all(void * , void * );
GC_API void GC_CALL GC_push_all_eager(void * , void * );
GC_API void GC_CALL GC_push_conditional(void * , void * ,
                                        int );
GC_API void GC_CALL GC_push_finalizer_structures(void);
typedef void (GC_CALLBACK * GC_push_other_roots_proc)(void);
GC_API void GC_CALL GC_set_push_other_roots(GC_push_other_roots_proc);
GC_API GC_push_other_roots_proc GC_CALL GC_get_push_other_roots(void);
typedef void (GC_CALLBACK *GC_reachable_object_proc)(void * ,
                                                size_t ,
                                                void * );
GC_API void GC_CALL GC_enumerate_reachable_objects_inner(
                                GC_reachable_object_proc,
                                void * ) GC_ATTR_NONNULL(1);
GC_API int GC_CALL GC_is_tmp_root(void *);
GC_API void GC_CALL GC_print_trace(GC_word );
GC_API void GC_CALL GC_print_trace_inner(GC_word );
#ifdef __cplusplus
  }
#endif
#endif
typedef GC_word word;
typedef GC_signed_word signed_word;
typedef unsigned int unsigned32;
typedef int GC_bool;
#define TRUE 1
#define FALSE 0
#ifndef PTR_T_DEFINED
  typedef char * ptr_t;
#define PTR_T_DEFINED
#endif
#ifndef SIZE_MAX
#include <limits.h>
#endif
#if defined(SIZE_MAX) && !defined(CPPCHECK)
#define GC_SIZE_MAX ((size_t)SIZE_MAX)
#else
#define GC_SIZE_MAX (~(size_t)0)
#endif
#if GC_GNUC_PREREQ(3, 0) && !defined(LINT2)
#define EXPECT(expr, outcome) __builtin_expect(expr,outcome)
#else
#define EXPECT(expr, outcome) (expr)
#endif
#define SIZET_SAT_ADD(a, b) \
            (EXPECT((a) < GC_SIZE_MAX - (b), TRUE) ? (a) + (b) : GC_SIZE_MAX)
#ifndef GCCONFIG_H
#define GCCONFIG_H
#ifdef CPPCHECK
#undef CLOCKS_PER_SEC
#undef FIXUP_POINTER
#undef POINTER_MASK
#undef POINTER_SHIFT
#undef REDIRECT_REALLOC
#undef _MAX_PATH
#endif
#ifndef PTR_T_DEFINED
  typedef char * ptr_t;
#define PTR_T_DEFINED
#endif
#if !defined(sony_news)
#include <stddef.h>
#endif
#ifdef __cplusplus
#define EXTERN_C_BEGIN extern "C" {
#define EXTERN_C_END }
#else
#define EXTERN_C_BEGIN
#define EXTERN_C_END
#endif
EXTERN_C_BEGIN
#if defined(__clang__) && defined(__clang_major__)
#define GC_CLANG_PREREQ(major, minor) \
    ((__clang_major__ << 16) + __clang_minor__ >= ((major) << 16) + (minor))
#define GC_CLANG_PREREQ_FULL(major, minor, patchlevel) \
            (GC_CLANG_PREREQ(major, (minor) + 1) \
                || (__clang_major__ == (major) && __clang_minor__ == (minor) \
                    && __clang_patchlevel__ >= (patchlevel)))
#else
#define GC_CLANG_PREREQ(major, minor) 0
#define GC_CLANG_PREREQ_FULL(major, minor, patchlevel) 0
#endif
#ifdef LINT2
#define COVERT_DATAFLOW(w) (~(GC_word)(w)^(~(GC_word)0))
#else
#define COVERT_DATAFLOW(w) ((GC_word)(w))
#endif
#if defined(__ANDROID__) && !defined(HOST_ANDROID)
#define HOST_ANDROID 1
#endif
#if defined(TIZEN) && !defined(HOST_TIZEN)
#define HOST_TIZEN 1
#endif
#if defined(__SYMBIAN32__) && !defined(SYMBIAN)
#define SYMBIAN
#ifdef __WINS__
#pragma data_seg(".data2")
#endif
#endif
#if (defined(linux) || defined(__linux__) || defined(HOST_ANDROID)) \
     && !defined(LINUX) && !defined(__native_client__)
#define LINUX
#endif
#if defined(__NetBSD__)
#define NETBSD
#endif
#if defined(__OpenBSD__)
#define OPENBSD
#endif
#if (defined(__FreeBSD__) || defined(__DragonFly__) \
      || defined(__FreeBSD_kernel__)) && !defined(FREEBSD) \
     && !defined(GC_NO_FREEBSD)
#define FREEBSD
#endif
#if defined(macosx) || (defined(__APPLE__) && defined(__MACH__))
#define DARWIN
    EXTERN_C_END
#include <TargetConditionals.h>
    EXTERN_C_BEGIN
#endif
#if defined(__native_client__)
#define NACL
#if !defined(__portable_native_client__) && !defined(__arm__)
#define I386
#define mach_type_known
#else
#endif
#endif
#if defined(__aarch64__)
#define AARCH64
#if !defined(LINUX) && !defined(DARWIN) && !defined(FREEBSD) \
        && !defined(NETBSD) && !defined(NN_BUILD_TARGET_PLATFORM_NX) \
        && !defined(OPENBSD)
#define NOSYS
#define mach_type_known
#endif
#endif
#if defined(__arm) || defined(__arm__) || defined(__thumb__)
#define ARM32
#if defined(NACL)
#define mach_type_known
#elif !defined(LINUX) && !defined(NETBSD) && !defined(FREEBSD) \
          && !defined(OPENBSD) && !defined(DARWIN) && !defined(_WIN32) \
          && !defined(__CEGCC__) && !defined(NN_PLATFORM_CTR) \
          && !defined(GC_NO_NOSYS) && !defined(SN_TARGET_PSP2) \
          && !defined(SYMBIAN)
#define NOSYS
#define mach_type_known
#endif
#endif
#if defined(sun) && defined(mc68000) && !defined(CPPCHECK)
#error SUNOS4 no longer supported
#endif
#if defined(hp9000s300) && !defined(CPPCHECK)
#error M68K based HP machines no longer supported
#endif
#if defined(OPENBSD) && defined(m68k)
#define M68K
#define mach_type_known
#endif
#if defined(OPENBSD) && defined(__sparc__)
#define SPARC
#define mach_type_known
#endif
#if defined(OPENBSD) && defined(__arm__)
#define ARM32
#define mach_type_known
#endif
#if defined(OPENBSD) && defined(__aarch64__)
#define AARCH64
#define mach_type_known
#endif
#if defined(OPENBSD) && defined(__sh__)
#define SH
#define mach_type_known
#endif
#if defined(NETBSD) && (defined(m68k) || defined(__m68k__))
#define M68K
#define mach_type_known
#endif
#if defined(NETBSD) && defined(__powerpc__)
#define POWERPC
#define mach_type_known
#endif
#if defined(NETBSD) && (defined(__arm32__) || defined(__arm__))
#define ARM32
#define mach_type_known
#endif
#if defined(NETBSD) && defined(__aarch64__)
#define AARCH64
#define mach_type_known
#endif
#if defined(NETBSD) && defined(__sh__)
#define SH
#define mach_type_known
#endif
#if defined(vax) || defined(__vax__)
#define VAX
#ifdef ultrix
#define ULTRIX
#else
#define BSD
#endif
#define mach_type_known
#endif
#if defined(NETBSD) && defined(__vax__)
#define VAX
#define mach_type_known
#endif
#if defined(mips) || defined(__mips) || defined(_mips)
#define MIPS
#if defined(nec_ews) || defined(_nec_ews)
#define EWS4800
#endif
#if !defined(LINUX) && !defined(EWS4800) && !defined(NETBSD) \
        && !defined(OPENBSD)
#if defined(ultrix) || defined(__ultrix)
#define ULTRIX
#else
#define IRIX5
#endif
#endif
#if defined(NETBSD) && defined(__MIPSEL__)
#undef ULTRIX
#endif
#define mach_type_known
#endif
#if defined(__QNX__)
#define I386
#define mach_type_known
#endif
#if defined(__NIOS2__) || defined(__NIOS2) || defined(__nios2__)
#define NIOS2
#define mach_type_known
#endif
#if defined(__or1k__)
#define OR1K
#define mach_type_known
#endif
#if defined(DGUX) && (defined(i386) || defined(__i386__))
#define I386
#ifndef _USING_DGUX
#define _USING_DGUX
#endif
#define mach_type_known
#endif
#if defined(sequent) && (defined(i386) || defined(__i386__))
#define I386
#define SEQUENT
#define mach_type_known
#endif
#if (defined(sun) || defined(__sun)) && (defined(i386) || defined(__i386__))
#define I386
#define SOLARIS
#define mach_type_known
#endif
#if (defined(sun) || defined(__sun)) && defined(__amd64)
#define X86_64
#define SOLARIS
#define mach_type_known
#endif
#if (defined(__OS2__) || defined(__EMX__)) && defined(__32BIT__)
#define I386
#define OS2
#define mach_type_known
#endif
#if defined(ibm032) && !defined(CPPCHECK)
#error IBM PC/RT no longer supported
#endif
#if (defined(sun) || defined(__sun)) && (defined(sparc) || defined(__sparc))
    EXTERN_C_END
#include <errno.h>
    EXTERN_C_BEGIN
#define SPARC
#define SOLARIS
#define mach_type_known
#elif defined(sparc) && defined(unix) && !defined(sun) && !defined(linux) \
       && !defined(FREEBSD) && !defined(NETBSD) && !defined(OPENBSD)
#define SPARC
#define DRSNX
#define mach_type_known
#endif
#if defined(_IBMR2)
#define POWERPC
#define AIX
#define mach_type_known
#endif
#if defined(NETBSD) && defined(__sparc__)
#define SPARC
#define mach_type_known
#endif
#if defined(_M_XENIX) && defined(_M_SYSV) && defined(_M_I386)
#define I386
#if defined(_SCO_ELF)
#define SCO_ELF
#else
#define SCO
#endif
#define mach_type_known
#endif
#if defined(_AUX_SOURCE) && !defined(CPPCHECK)
#error A/UX no longer supported
#endif
#if defined(_PA_RISC1_0) || defined(_PA_RISC1_1) || defined(_PA_RISC2_0) \
     || defined(hppa) || defined(__hppa__)
#define HP_PA
#if !defined(LINUX) && !defined(HPUX) && !defined(OPENBSD)
#define HPUX
#endif
#define mach_type_known
#endif
#if defined(__ia64) && (defined(_HPUX_SOURCE) || defined(__HP_aCC))
#define IA64
#ifndef HPUX
#define HPUX
#endif
#define mach_type_known
#endif
#if (defined(__BEOS__) || defined(__HAIKU__)) && defined(_X86_)
#define I386
#define HAIKU
#define mach_type_known
#endif
#if defined(__HAIKU__) && (defined(__amd64__) || defined(__x86_64__))
#define X86_64
#define HAIKU
#define mach_type_known
#endif
#if defined(OPENBSD) && defined(__amd64__)
#define X86_64
#define mach_type_known
#endif
#if defined(LINUX) && (defined(i386) || defined(__i386__))
#define I386
#define mach_type_known
#endif
#if defined(LINUX) && defined(__x86_64__)
#define X86_64
#define mach_type_known
#endif
#if defined(LINUX) && (defined(__ia64__) || defined(__ia64))
#define IA64
#define mach_type_known
#endif
#if defined(LINUX) && defined(__aarch64__)
#define AARCH64
#define mach_type_known
#endif
#if defined(LINUX) && (defined(__arm) || defined(__arm__))
#define ARM32
#define mach_type_known
#endif
#if defined(LINUX) && defined(__cris__)
#ifndef CRIS
#define CRIS
#endif
#define mach_type_known
#endif
#if defined(LINUX) && defined(__loongarch__)
#define LOONGARCH
#define mach_type_known
#endif
#if defined(LINUX) && (defined(powerpc) || defined(__powerpc__) \
                        || defined(powerpc64) || defined(__powerpc64__))
#define POWERPC
#define mach_type_known
#endif
#if defined(LINUX) && defined(__mc68000__)
#define M68K
#define mach_type_known
#endif
#if defined(LINUX) && (defined(sparc) || defined(__sparc__))
#define SPARC
#define mach_type_known
#endif
#if defined(LINUX) && defined(__sh__)
#define SH
#define mach_type_known
#endif
#if defined(LINUX) && defined(__avr32__)
#define AVR32
#define mach_type_known
#endif
#if defined(LINUX) && defined(__m32r__)
#define M32R
#define mach_type_known
#endif
#if defined(__alpha) || defined(__alpha__)
#define ALPHA
#if !defined(LINUX) && !defined(NETBSD) && !defined(OPENBSD) \
       && !defined(FREEBSD)
#define OSF1
#endif
#define mach_type_known
#endif
#if defined(_AMIGA) && !defined(AMIGA)
#define AMIGA
#endif
#ifdef AMIGA
#define M68K
#define mach_type_known
#endif
#if defined(THINK_C) \
     || (defined(__MWERKS__) && !defined(__powerc) && !defined(SYMBIAN))
#define M68K
#define MACOS
#define mach_type_known
#endif
#if defined(__MWERKS__) && defined(__powerc) && !defined(__MACH__) \
     && !defined(SYMBIAN)
#define POWERPC
#define MACOS
#define mach_type_known
#endif
#if defined(OPENBSD) && defined(__powerpc__)
#define POWERPC
#define mach_type_known
#endif
#if defined(DARWIN)
#if defined(__ppc__)  || defined(__ppc64__)
#define POWERPC
#define mach_type_known
#elif defined(__x86_64__) || defined(__x86_64)
#define X86_64
#define mach_type_known
#elif defined(__i386__)
#define I386
#define mach_type_known
#elif defined(__arm__)
#define ARM32
#define mach_type_known
#elif defined(__aarch64__)
#define AARCH64
#define mach_type_known
#endif
#endif
#if defined(__rtems__) && (defined(i386) || defined(__i386__))
#define I386
#define RTEMS
#define mach_type_known
#endif
#if defined(NeXT) && defined(mc68000)
#define M68K
#define NEXT
#define mach_type_known
#endif
#if defined(NeXT) && (defined(i386) || defined(__i386__))
#define I386
#define NEXT
#define mach_type_known
#endif
#if defined(OPENBSD) && (defined(i386) || defined(__i386__))
#define I386
#define mach_type_known
#endif
#if defined(NETBSD) && (defined(i386) || defined(__i386__))
#define I386
#define mach_type_known
#endif
#if defined(NETBSD) && defined(__x86_64__)
#define X86_64
#define mach_type_known
#endif
#if defined(FREEBSD) && (defined(i386) || defined(__i386__))
#define I386
#define mach_type_known
#endif
#if defined(FREEBSD) && (defined(__amd64__) || defined(__x86_64__))
#define X86_64
#define mach_type_known
#endif
#if defined(FREEBSD) && defined(__sparc__)
#define SPARC
#define mach_type_known
#endif
#if defined(FREEBSD) && (defined(powerpc) || defined(__powerpc__))
#define POWERPC
#define mach_type_known
#endif
#if defined(FREEBSD) && defined(__arm__)
#define ARM32
#define mach_type_known
#endif
#if defined(FREEBSD) && defined(__aarch64__)
#define AARCH64
#define mach_type_known
#endif
#if defined(FREEBSD) && (defined(mips) || defined(__mips) || defined(_mips))
#define MIPS
#define mach_type_known
#endif
#if defined(bsdi) && (defined(i386) || defined(__i386__))
#define I386
#define BSDI
#define mach_type_known
#endif
#if !defined(mach_type_known) && defined(__386BSD__)
#define I386
#define THREE86BSD
#define mach_type_known
#endif
#if defined(_CX_UX) && defined(_M88K)
#define M88K
#define CX_UX
#define mach_type_known
#endif
#if defined(DGUX) && defined(m88k)
#define M88K
#define mach_type_known
#endif
#if defined(_WIN32_WCE) || defined(__CEGCC__) || defined(__MINGW32CE__)
#if defined(SH3) || defined(SH4)
#define SH
#endif
#if defined(x86) || defined(__i386__)
#define I386
#endif
#if defined(_M_ARM) || defined(ARM) || defined(_ARM_)
#define ARM32
#endif
#define MSWINCE
#define mach_type_known
#else
#if ((defined(_MSDOS) || defined(_MSC_VER)) && (_M_IX86 >= 300)) \
       || (defined(_WIN32) && !defined(__CYGWIN32__) && !defined(__CYGWIN__) \
           && !defined(__INTERIX) && !defined(SYMBIAN))
#if defined(__LP64__) || defined(_M_X64)
#define X86_64
#elif defined(_M_ARM)
#define ARM32
#elif defined(_M_ARM64)
#define AARCH64
#else
#define I386
#endif
#ifdef _XBOX_ONE
#define MSWIN_XBOX1
#else
#ifndef MSWIN32
#define MSWIN32
#endif
#if defined(WINAPI_FAMILY) && (WINAPI_FAMILY == WINAPI_FAMILY_APP)
#define MSWINRT_FLAVOR
#endif
#endif
#define mach_type_known
#endif
#if defined(_MSC_VER) && defined(_M_IA64)
#define IA64
#define MSWIN32
#endif
#endif
#if defined(__DJGPP__)
#define I386
#ifndef DJGPP
#define DJGPP
#endif
#define mach_type_known
#endif
#if defined(__CYGWIN32__) || defined(__CYGWIN__)
#if defined(__LP64__)
#define X86_64
#else
#define I386
#endif
#define CYGWIN32
#define mach_type_known
#endif
#if defined(__INTERIX)
#define I386
#define INTERIX
#define mach_type_known
#endif
#if defined(__MINGW32__) && !defined(mach_type_known)
#define I386
#define MSWIN32
#define mach_type_known
#endif
#if defined(__BORLANDC__)
#define I386
#define MSWIN32
#define mach_type_known
#endif
#if defined(_UTS) && !defined(mach_type_known)
#define S370
#define UTS4
#define mach_type_known
#endif
#if defined(__pj__) && !defined(CPPCHECK)
#error PicoJava no longer supported
#endif
#if defined(__embedded__) && defined(PPC)
#define POWERPC
#define NOSYS
#define mach_type_known
#endif
#if defined(__WATCOMC__) && defined(__386__)
#define I386
#if !defined(OS2) && !defined(MSWIN32) && !defined(DOS4GW)
#if defined(__OS2__)
#define OS2
#else
#if defined(__WINDOWS_386__) || defined(__NT__)
#define MSWIN32
#else
#define DOS4GW
#endif
#endif
#endif
#define mach_type_known
#endif
#if defined(__s390__) && defined(LINUX)
#define S390
#define mach_type_known
#endif
#if defined(__GNU__)
#if defined(__i386__)
#define  HURD
#define  I386
#define  mach_type_known
#endif
#endif
#if defined(__TANDEM)
#define MIPS
#define NONSTOP
#define mach_type_known
#endif
#if defined(__arc__) && defined(LINUX)
#define ARC
#define mach_type_known
#endif
#if defined(__hexagon__) && defined(LINUX)
#define HEXAGON
#define mach_type_known
#endif
#if defined(__tile__) && defined(LINUX)
#ifdef __tilegx__
#define TILEGX
#else
#define TILEPRO
#endif
#define mach_type_known
#endif
#if defined(__riscv) && (defined(FREEBSD) || defined(LINUX))
#define RISCV
#define mach_type_known
#endif
#if defined(SN_TARGET_PSP2)
#define mach_type_known
#endif
#if defined(NN_PLATFORM_CTR)
#define mach_type_known
#endif
#if defined(NN_BUILD_TARGET_PLATFORM_NX)
#define NINTENDO_SWITCH
#define mach_type_known
#endif
#if defined(SYMBIAN)
#define mach_type_known
#endif
#if defined(__EMSCRIPTEN__)
#define EMSCRIPTEN
#define I386
#define mach_type_known
#endif
#if !defined(mach_type_known) && !defined(CPPCHECK)
#error The collector has not been ported to this machine/OS combination
#endif
#if GC_GNUC_PREREQ(2, 8) \
     && !GC_GNUC_PREREQ(11, 0)  \
     && !defined(__INTEL_COMPILER) && !defined(__PATHCC__) \
     && !defined(__FUJITSU)  \
     && !(defined(POWERPC) && defined(DARWIN))  \
     && !defined(RTEMS) \
     && !defined(__ARMCC_VERSION)  \
     && (!defined(__clang__) \
         || GC_CLANG_PREREQ(8, 0) )
#define HAVE_BUILTIN_UNWIND_INIT
#endif
#ifdef CYGWIN32
#define OS_TYPE "CYGWIN32"
#define RETRY_GET_THREAD_CONTEXT
#ifdef USE_WINALLOC
#define GWW_VDB
#elif defined(USE_MMAP)
#define USE_MMAP_ANON
#endif
#endif
#ifdef DARWIN
#define OS_TYPE "DARWIN"
#define DYNAMIC_LOADING
#define DATASTART ((ptr_t)get_etext())
#define DATAEND   ((ptr_t)get_end())
#define USE_MMAP_ANON
    EXTERN_C_END
#include <unistd.h>
    EXTERN_C_BEGIN
#define GETPAGESIZE() (unsigned)getpagesize()
#define NO_PTHREAD_TRYLOCK
#endif
#ifdef FREEBSD
#define OS_TYPE "FREEBSD"
#define FREEBSD_STACKBOTTOM
#ifdef __ELF__
#define DYNAMIC_LOADING
#endif
#if !defined(ALPHA) && !defined(SPARC)
      extern char etext[];
#define DATASTART GC_FreeBSDGetDataStart(0x1000, (ptr_t)etext)
#define DATASTART_USES_BSDGETDATASTART
#ifndef GC_FREEBSD_THREADS
#define MPROTECT_VDB
#endif
#endif
#endif
#ifdef HAIKU
#define OS_TYPE "HAIKU"
#define DYNAMIC_LOADING
#define MPROTECT_VDB
    EXTERN_C_END
#include <OS.h>
    EXTERN_C_BEGIN
#define GETPAGESIZE() (unsigned)B_PAGE_SIZE
#endif
#ifdef HPUX
#define OS_TYPE "HPUX"
    extern int __data_start[];
#define DATASTART ((ptr_t)(__data_start))
#ifdef USE_MMAP
#define USE_MMAP_ANON
#endif
#define DYNAMIC_LOADING
    EXTERN_C_END
#include <unistd.h>
    EXTERN_C_BEGIN
#define GETPAGESIZE() (unsigned)sysconf(_SC_PAGE_SIZE)
#endif
#ifdef LINUX
#define OS_TYPE "LINUX"
    EXTERN_C_END
#include <features.h>
    EXTERN_C_BEGIN
#define COUNT_UNMAPPED_REGIONS
#if !defined(MIPS) && !defined(POWERPC)
#define LINUX_STACKBOTTOM
#endif
#if defined(__ELF__) && !defined(IA64)
#define DYNAMIC_LOADING
#endif
#if defined(__ELF__) && !defined(ARC) && !defined(RISCV) \
       && !defined(S390) && !defined(TILEGX) && !defined(TILEPRO)
      extern int _end[];
#define DATAEND ((ptr_t)(_end))
#endif
#endif
#ifdef MACOS
#define OS_TYPE "MACOS"
#ifndef __LOWMEM__
      EXTERN_C_END
#include <LowMem.h>
      EXTERN_C_BEGIN
#endif
#define STACKBOTTOM ((ptr_t)LMGetCurStackBase())
#define DATAEND
#endif
#ifdef MSWIN32
#define OS_TYPE "MSWIN32"
#define DATAEND
#define GWW_VDB
#endif
#ifdef MSWINCE
#define OS_TYPE "MSWINCE"
#define DATAEND
#endif
#ifdef NETBSD
#define OS_TYPE "NETBSD"
#define HEURISTIC2
#ifdef __ELF__
      extern ptr_t GC_data_start;
#define DATASTART GC_data_start
#define DYNAMIC_LOADING
#elif !defined(MIPS)
      extern char etext[];
#define DATASTART ((ptr_t)(etext))
#endif
#endif
#ifdef NEXT
#define OS_TYPE "NEXT"
#define DATASTART ((ptr_t)get_etext())
#define DATASTART_IS_FUNC
#define DATAEND
#endif
#ifdef OPENBSD
#define OS_TYPE "OPENBSD"
#if !defined(M68K)
#ifndef GC_OPENBSD_THREADS
#define HEURISTIC2
#endif
      extern int __data_start[];
#define DATASTART ((ptr_t)__data_start)
      extern int _end[];
#define DATAEND ((ptr_t)(&_end))
#define DYNAMIC_LOADING
#endif
#endif
#ifdef SOLARIS
#define OS_TYPE "SOLARIS"
    extern int _etext[], _end[];
    ptr_t GC_SysVGetDataStart(size_t, ptr_t);
#define DATASTART_IS_FUNC
#define DATAEND ((ptr_t)(_end))
#if !defined(USE_MMAP) && defined(REDIRECT_MALLOC)
#define USE_MMAP 1
#endif
#ifdef USE_MMAP
#define HEAP_START (ptr_t)0x40000000
#else
#define HEAP_START DATAEND
#endif
#ifndef GC_THREADS
#define MPROTECT_VDB
#endif
#define DYNAMIC_LOADING
    EXTERN_C_END
#include <sys/vmparam.h>
#include <unistd.h>
    EXTERN_C_BEGIN
#ifdef USERLIMIT
#define STACKBOTTOM ((ptr_t)USRSTACK)
#else
#define HEURISTIC2
#endif
#endif
#define STACK_GRAN 0x1000000
#ifdef SYMBIAN
#define MACH_TYPE "SYMBIAN"
#define OS_TYPE "SYMBIAN"
#define CPP_WORDSZ 32
#define ALIGNMENT 4
#define DATASTART (ptr_t)ALIGNMENT
#define DATAEND (ptr_t)ALIGNMENT
#endif
#ifdef M68K
#define MACH_TYPE "M68K"
#define ALIGNMENT 2
#ifdef OPENBSD
#define HEURISTIC2
#ifdef __ELF__
          extern ptr_t GC_data_start;
#define DATASTART GC_data_start
#define DYNAMIC_LOADING
#else
          extern char etext[];
#define DATASTART ((ptr_t)(etext))
#endif
#endif
#ifdef NETBSD
#endif
#ifdef LINUX
#if !defined(REDIRECT_MALLOC)
#define MPROTECT_VDB
#endif
#ifdef __ELF__
#if defined(__GLIBC__) && __GLIBC__ >= 2
#define SEARCH_FOR_DATA_START
#else
            extern char **__environ;
#define DATASTART ((ptr_t)(&__environ))
#endif
#else
          extern int etext[];
#define DATASTART ((ptr_t)((((word)(etext)) + 0xfff) & ~0xfff))
#endif
#endif
#ifdef AMIGA
#define OS_TYPE "AMIGA"
#define DATAEND
#define GETPAGESIZE() 4096
#endif
#ifdef MACOS
#define GETPAGESIZE() 4096
#endif
#ifdef NEXT
#define STACKBOTTOM ((ptr_t)0x4000000)
#endif
#endif
#ifdef POWERPC
#define MACH_TYPE "POWERPC"
#ifdef MACOS
#define ALIGNMENT 2
#endif
#ifdef LINUX
#if defined(__powerpc64__)
#define ALIGNMENT 8
#define CPP_WORDSZ 64
#ifndef HBLKSIZE
#define HBLKSIZE 4096
#endif
#else
#define ALIGNMENT 4
#endif
#if defined(__bg__)
#define HEURISTIC2
#define NO_PTHREAD_GETATTR_NP
#else
#define LINUX_STACKBOTTOM
#endif
#define SEARCH_FOR_DATA_START
#if !defined(REDIRECT_MALLOC)
#define MPROTECT_VDB
#endif
#ifndef SOFT_VDB
#define SOFT_VDB
#endif
#endif
#ifdef DARWIN
#if defined(__ppc64__)
#define ALIGNMENT 8
#define CPP_WORDSZ 64
#define STACKBOTTOM ((ptr_t)0x7fff5fc00000)
#define CACHE_LINE_SIZE 64
#ifndef HBLKSIZE
#define HBLKSIZE 4096
#endif
#else
#define ALIGNMENT 4
#define STACKBOTTOM ((ptr_t)0xc0000000)
#endif
#define MPROTECT_VDB
#if defined(USE_PPC_PREFETCH) && defined(__GNUC__)
#define PREFETCH(x) \
          __asm__ __volatile__ ("dcbt 0,%0" : : "r" ((const void *) (x)))
#define GC_PREFETCH_FOR_WRITE(x) \
          __asm__ __volatile__ ("dcbtst 0,%0" : : "r" ((const void *) (x)))
#endif
#endif
#ifdef OPENBSD
#if defined(__powerpc64__)
#define ALIGNMENT 8
#define CPP_WORDSZ 64
#else
#define ALIGNMENT 4
#endif
#endif
#ifdef FREEBSD
#if defined(__powerpc64__)
#define ALIGNMENT 8
#define CPP_WORDSZ 64
#ifndef HBLKSIZE
#define HBLKSIZE 4096
#endif
#else
#define ALIGNMENT 4
#endif
#define SIG_SUSPEND SIGUSR1
#define SIG_THR_RESTART SIGUSR2
#endif
#ifdef NETBSD
#define ALIGNMENT 4
#endif
#ifdef SN_TARGET_PS3
#define OS_TYPE "SN_TARGET_PS3"
#define NO_GETENV
#define CPP_WORDSZ 32
#define ALIGNMENT 4
      extern int _end[];
      extern int __bss_start;
#define DATASTART ((ptr_t)(__bss_start))
#define DATAEND ((ptr_t)(_end))
#define STACKBOTTOM ((ptr_t)ps3_get_stack_bottom())
#define NO_PTHREAD_TRYLOCK
#endif
#ifdef AIX
#define OS_TYPE "AIX"
#undef ALIGNMENT
#undef IA64
#ifdef __64BIT__
#define ALIGNMENT 8
#define CPP_WORDSZ 64
#define STACKBOTTOM ((ptr_t)0x1000000000000000)
#else
#define ALIGNMENT 4
#define CPP_WORDSZ 32
#define STACKBOTTOM ((ptr_t)((ulong)&errno))
#endif
#define USE_MMAP_ANON
      extern int _data[], _end[];
#define DATASTART ((ptr_t)((ulong)_data))
#define DATAEND ((ptr_t)((ulong)_end))
      extern int errno;
#define DYNAMIC_LOADING
#endif
#ifdef NOSYS
#define OS_TYPE "NOSYS"
#define ALIGNMENT 4
      extern void __end[], __dso_handle[];
#define DATASTART ((ptr_t)__dso_handle)
#define DATAEND ((ptr_t)(__end))
#undef STACK_GRAN
#define STACK_GRAN 0x10000000
#define HEURISTIC1
#endif
#endif
#ifdef NACL
#define OS_TYPE "NACL"
#if defined(__GLIBC__)
#define DYNAMIC_LOADING
#endif
#define DATASTART ((ptr_t)0x10020000)
    extern int _end[];
#define DATAEND ((ptr_t)_end)
#undef STACK_GRAN
#define STACK_GRAN 0x10000
#define HEURISTIC1
#define NO_PTHREAD_GETATTR_NP
#define USE_MMAP_ANON
#define GETPAGESIZE() 65536
#define MAX_NACL_GC_THREADS 1024
#endif
#ifdef VAX
#define MACH_TYPE "VAX"
#define ALIGNMENT 4
    extern char etext[];
#define DATASTART ((ptr_t)(etext))
#ifdef BSD
#define OS_TYPE "BSD"
#define HEURISTIC1
#endif
#ifdef ULTRIX
#define OS_TYPE "ULTRIX"
#define STACKBOTTOM ((ptr_t)0x7fffc800)
#endif
#endif
#ifdef SPARC
#define MACH_TYPE "SPARC"
#if defined(__arch64__) || defined(__sparcv9)
#define ALIGNMENT 8
#define CPP_WORDSZ 64
#define ELF_CLASS ELFCLASS64
#else
#define ALIGNMENT 4
#define CPP_WORDSZ 32
#endif
#ifdef SOLARIS
#define DATASTART GC_SysVGetDataStart(0x10000, (ptr_t)_etext)
#define PROC_VDB
#define GETPAGESIZE() (unsigned)sysconf(_SC_PAGESIZE)
#endif
#ifdef DRSNX
#define OS_TYPE "DRSNX"
        extern int etext[];
        ptr_t GC_SysVGetDataStart(size_t, ptr_t);
#define DATASTART GC_SysVGetDataStart(0x10000, (ptr_t)etext)
#define DATASTART_IS_FUNC
#define MPROTECT_VDB
#define STACKBOTTOM ((ptr_t)0xdfff0000)
#define DYNAMIC_LOADING
#endif
#ifdef LINUX
#if !defined(__ELF__) && !defined(CPPCHECK)
#error Linux SPARC a.out not supported
#endif
#define SVR4
      extern int _etext[];
      ptr_t GC_SysVGetDataStart(size_t, ptr_t);
#ifdef __arch64__
#define DATASTART GC_SysVGetDataStart(0x100000, (ptr_t)_etext)
#else
#define DATASTART GC_SysVGetDataStart(0x10000, (ptr_t)_etext)
#endif
#define DATASTART_IS_FUNC
#endif
#ifdef OPENBSD
#endif
#ifdef NETBSD
#endif
#ifdef FREEBSD
#define SIG_SUSPEND SIGUSR1
#define SIG_THR_RESTART SIGUSR2
        extern char etext[];
        extern char edata[];
#if !defined(CPPCHECK)
          extern char end[];
#endif
#define NEED_FIND_LIMIT
#define DATASTART ((ptr_t)(&etext))
        void * GC_find_limit(void *, int);
#define DATAEND (ptr_t)GC_find_limit(DATASTART, TRUE)
#define DATAEND_IS_FUNC
#define GC_HAVE_DATAREGION2
#define DATASTART2 ((ptr_t)(&edata))
#define DATAEND2 ((ptr_t)(&end))
#endif
#endif
#ifdef I386
#define MACH_TYPE "I386"
#if (defined(__LP64__) || defined(_WIN64)) && !defined(CPPCHECK)
#error This should be handled as X86_64
#else
#define CPP_WORDSZ 32
#define ALIGNMENT 4
#endif
#ifdef SEQUENT
#define OS_TYPE "SEQUENT"
        extern int etext[];
#define DATASTART ((ptr_t)((((word)(etext)) + 0xfff) & ~0xfff))
#define STACKBOTTOM ((ptr_t)0x3ffff000)
#endif
#ifdef EMSCRIPTEN
#define OS_TYPE "EMSCRIPTEN"
#define DATASTART (ptr_t)ALIGNMENT
#define DATAEND (ptr_t)ALIGNMENT
#define USE_MMAP_ANON
#define STACK_GROWS_DOWN
#endif
#if defined(__QNX__)
#define OS_TYPE "QNX"
#define SA_RESTART 0
#define HEURISTIC1
      extern char etext[];
      extern int _end[];
#define DATASTART ((ptr_t)etext)
#define DATAEND ((ptr_t)_end)
#endif
#ifdef HAIKU
      extern int etext[];
#define DATASTART ((ptr_t)((((word)(etext)) + 0xfff) & ~0xfff))
#endif
#ifdef SOLARIS
#define DATASTART GC_SysVGetDataStart(0x1000, (ptr_t)_etext)
#ifdef SOLARIS25_PROC_VDB_BUG_FIXED
#define PROC_VDB
#endif
#endif
#ifdef SCO
#define OS_TYPE "SCO"
        extern int etext[];
#define DATASTART ((ptr_t)((((word)(etext)) + 0x3fffff) & ~0x3fffff) \
                                 + ((word)(etext) & 0xfff))
#define STACKBOTTOM ((ptr_t)0x7ffffffc)
#endif
#ifdef SCO_ELF
#define OS_TYPE "SCO_ELF"
        extern int etext[];
#define DATASTART ((ptr_t)(etext))
#define STACKBOTTOM ((ptr_t)0x08048000)
#define DYNAMIC_LOADING
#define ELF_CLASS ELFCLASS32
#endif
#ifdef DGUX
#define OS_TYPE "DGUX"
        extern int _etext, _end;
        ptr_t GC_SysVGetDataStart(size_t, ptr_t);
#define DATASTART GC_SysVGetDataStart(0x1000, (ptr_t)(&_etext))
#define DATASTART_IS_FUNC
#define DATAEND ((ptr_t)(&_end))
#define STACK_GROWS_DOWN
#define HEURISTIC2
        EXTERN_C_END
#include <unistd.h>
        EXTERN_C_BEGIN
#define GETPAGESIZE() (unsigned)sysconf(_SC_PAGESIZE)
#define DYNAMIC_LOADING
#ifndef USE_MMAP
#define USE_MMAP 1
#endif
#define MAP_FAILED (void *) ((word)-1)
#define HEAP_START (ptr_t)0x40000000
#endif
#ifdef LINUX
#if !defined(REDIRECT_MALLOC)
#define MPROTECT_VDB
#else
#endif
#define HEAP_START (ptr_t)0x1000
#ifdef __ELF__
#if defined(__GLIBC__) && __GLIBC__ >= 2 \
                || defined(HOST_ANDROID) || defined(HOST_TIZEN)
#define SEARCH_FOR_DATA_START
#else
                 extern char **__environ;
#define DATASTART ((ptr_t)(&__environ))
#endif
#if !defined(GC_NO_SIGSETJMP) && (defined(HOST_TIZEN) \
                    || (defined(HOST_ANDROID) \
                        && !(GC_GNUC_PREREQ(4, 8) || GC_CLANG_PREREQ(3, 2) \
                             || __ANDROID_API__ >= 18)))
#define GC_NO_SIGSETJMP 1
#endif
#else
             extern int etext[];
#define DATASTART ((ptr_t)((((word)(etext)) + 0xfff) & ~0xfff))
#endif
#ifdef USE_I686_PREFETCH
#define PREFETCH(x) \
            __asm__ __volatile__ ("prefetchnta %0" : : "m"(*(char *)(x)))
#ifdef FORCE_WRITE_PREFETCH
#define GC_PREFETCH_FOR_WRITE(x) \
              __asm__ __volatile__ ("prefetcht0 %0" : : "m"(*(char *)(x)))
#else
#define GC_NO_PREFETCH_FOR_WRITE
#endif
#elif defined(USE_3DNOW_PREFETCH)
#define PREFETCH(x) \
            __asm__ __volatile__ ("prefetch %0" : : "m"(*(char *)(x)))
#define GC_PREFETCH_FOR_WRITE(x) \
            __asm__ __volatile__ ("prefetchw %0" : : "m"(*(char *)(x)))
#endif
#if defined(__GLIBC__) && !defined(__UCLIBC__) \
           && !defined(GLIBC_TSX_BUG_FIXED)
#define GLIBC_2_19_TSX_BUG
          EXTERN_C_END
#include <gnu/libc-version.h>
          EXTERN_C_BEGIN
#endif
#ifndef SOFT_VDB
#define SOFT_VDB
#endif
#endif
#ifdef CYGWIN32
#define WOW64_THREAD_CONTEXT_WORKAROUND
#define DATASTART ((ptr_t)GC_DATASTART)
#define DATAEND   ((ptr_t)GC_DATAEND)
#ifndef USE_WINALLOC
#
#ifdef USE_MMAP
#define NEED_FIND_LIMIT
#endif
#endif
#endif
#ifdef INTERIX
#define OS_TYPE "INTERIX"
      extern int _data_start__[];
      extern int _bss_end__[];
#define DATASTART ((ptr_t)_data_start__)
#define DATAEND   ((ptr_t)_bss_end__)
#define STACKBOTTOM ({ ptr_t rv; \
                            __asm__ __volatile__ ("movl %%fs:4, %%eax" \
                                                  : "=a" (rv)); \
                            rv; })
#define USE_MMAP_ANON
#endif
#ifdef OS2
#define OS_TYPE "OS2"
#define DATAEND
#endif
#ifdef MSWIN32
#define WOW64_THREAD_CONTEXT_WORKAROUND
#define RETRY_GET_THREAD_CONTEXT
#define MPROTECT_VDB
#endif
#ifdef MSWINCE
#endif
#ifdef DJGPP
#define OS_TYPE "DJGPP"
        EXTERN_C_END
#include "stubinfo.h"
        EXTERN_C_BEGIN
        extern int etext[];
        extern int _stklen;
        extern int __djgpp_stack_limit;
#define DATASTART ((ptr_t)((((word)(etext)) + 0x1ff) & ~0x1ff))
#define STACKBOTTOM ((ptr_t)((word)__djgpp_stack_limit + _stklen))
#endif
#ifdef OPENBSD
#endif
#ifdef FREEBSD
#ifdef __GLIBC__
#define SIG_SUSPEND          (32+6)
#define SIG_THR_RESTART      (32+5)
            extern int _end[];
#define DATAEND ((ptr_t)(_end))
#else
#define SIG_SUSPEND SIGUSR1
#define SIG_THR_RESTART SIGUSR2
#endif
#endif
#ifdef NETBSD
#endif
#ifdef THREE86BSD
#define OS_TYPE "THREE86BSD"
#define HEURISTIC2
        extern char etext[];
#define DATASTART ((ptr_t)(etext))
#endif
#ifdef BSDI
#define OS_TYPE "BSDI"
#define HEURISTIC2
        extern char etext[];
#define DATASTART ((ptr_t)(etext))
#endif
#ifdef NEXT
#define STACKBOTTOM ((ptr_t)0xc0000000)
#endif
#ifdef RTEMS
#define OS_TYPE "RTEMS"
        EXTERN_C_END
#include <sys/unistd.h>
        EXTERN_C_BEGIN
        extern int etext[];
        void *rtems_get_stack_bottom(void);
#define InitStackBottom rtems_get_stack_bottom()
#define DATASTART ((ptr_t)etext)
#define STACKBOTTOM ((ptr_t)InitStackBottom)
#define SIG_SUSPEND SIGUSR1
#define SIG_THR_RESTART SIGUSR2
#endif
#ifdef DOS4GW
#define OS_TYPE "DOS4GW"
      extern long __nullarea;
      extern char _end;
      extern char *_STACKTOP;
      #pragma aux __nullarea "*";
      #pragma aux _end "*";
#define STACKBOTTOM ((ptr_t)_STACKTOP)
#define DATASTART ((ptr_t)(&__nullarea))
#define DATAEND ((ptr_t)(&_end))
#endif
#ifdef HURD
#define OS_TYPE "HURD"
#define STACK_GROWS_DOWN
#define HEURISTIC2
#define SIG_SUSPEND SIGUSR1
#define SIG_THR_RESTART SIGUSR2
#define SEARCH_FOR_DATA_START
      extern int _end[];
#define DATAEND ((ptr_t)(_end))
#define DYNAMIC_LOADING
#define USE_MMAP_ANON
#endif
#ifdef DARWIN
#define DARWIN_DONT_PARSE_STACK 1
#define STACKBOTTOM ((ptr_t)0xc0000000)
#define MPROTECT_VDB
#if TARGET_OS_IPHONE && !defined(NO_DYLD_BIND_FULLY_IMAGE)
#define NO_DYLD_BIND_FULLY_IMAGE
#endif
#endif
#endif
#ifdef NS32K
#define MACH_TYPE "NS32K"
#define ALIGNMENT 4
    extern char **environ;
#define DATASTART ((ptr_t)(&environ))
#define STACKBOTTOM ((ptr_t)0xfffff000)
#endif
#ifdef LOONGARCH
#define MACH_TYPE "LoongArch"
#ifdef LINUX
#pragma weak __data_start
      extern int __data_start[];
#define DATASTART ((ptr_t)(__data_start))
#define CPP_WORDSZ _LOONGARCH_SZPTR
#define ALIGNMENT (_LOONGARCH_SZPTR/8)
#endif
#endif
#ifdef MIPS
#define MACH_TYPE "MIPS"
#ifdef LINUX
#pragma weak __data_start
      extern int __data_start[];
#define DATASTART ((ptr_t)(__data_start))
#ifdef _MIPS_SZPTR
#define CPP_WORDSZ _MIPS_SZPTR
#define ALIGNMENT (_MIPS_SZPTR/8)
#else
#define ALIGNMENT 4
#endif
#ifndef HBLKSIZE
#define HBLKSIZE 4096
#endif
#if __GLIBC__ == 2 && __GLIBC_MINOR__ >= 2 || __GLIBC__ > 2
#define LINUX_STACKBOTTOM
#else
#define STACKBOTTOM ((ptr_t)0x7fff8000)
#endif
#endif
#ifdef EWS4800
#define OS_TYPE "EWS4800"
#define HEURISTIC2
#if defined(_MIPS_SZPTR) && (_MIPS_SZPTR == 64)
        extern int _fdata[], _end[];
#define DATASTART ((ptr_t)_fdata)
#define DATAEND ((ptr_t)_end)
#define CPP_WORDSZ _MIPS_SZPTR
#define ALIGNMENT (_MIPS_SZPTR/8)
#else
        extern int etext[], edata[];
#if !defined(CPPCHECK)
          extern int end[];
#endif
        extern int _DYNAMIC_LINKING[], _gp[];
#define DATASTART ((ptr_t)((((word)(etext) + 0x3ffff) & ~0x3ffff) \
                                  + ((word)(etext) & 0xffff)))
#define DATAEND ((ptr_t)(edata))
#define GC_HAVE_DATAREGION2
#define DATASTART2 (_DYNAMIC_LINKING \
                ? (ptr_t)(((word)_gp + 0x8000 + 0x3ffff) & ~0x3ffff) \
                : (ptr_t)edata)
#define DATAEND2 ((ptr_t)(end))
#define ALIGNMENT 4
#endif
#endif
#ifdef ULTRIX
#define OS_TYPE "ULTRIX"
#define HEURISTIC2
#define DATASTART ((ptr_t)0x10000000)
#define ALIGNMENT 4
#endif
#ifdef IRIX5
#define OS_TYPE "IRIX5"
#define HEURISTIC2
        extern int _fdata[];
#define DATASTART ((ptr_t)(_fdata))
#ifdef USE_MMAP
#define HEAP_START (ptr_t)0x30000000
#else
#define HEAP_START DATASTART
#endif
#ifdef _MIPS_SZPTR
#define CPP_WORDSZ _MIPS_SZPTR
#define ALIGNMENT (_MIPS_SZPTR/8)
#else
#define ALIGNMENT 4
#endif
#define DYNAMIC_LOADING
#endif
#ifdef MSWINCE
#define ALIGNMENT 4
#endif
#ifdef NETBSD
#define ALIGNMENT 4
#ifndef __ELF__
#define DATASTART ((ptr_t)0x10000000)
#define STACKBOTTOM ((ptr_t)0x7ffff000)
#endif
#endif
#ifdef OPENBSD
#define CPP_WORDSZ 64
#define ALIGNMENT 8
#endif
#ifdef FREEBSD
#define ALIGNMENT 4
#define SIG_SUSPEND SIGUSR1
#define SIG_THR_RESTART SIGUSR2
#endif
#ifdef NONSTOP
#define OS_TYPE "NONSTOP"
#define CPP_WORDSZ 32
#define ALIGNMENT 4
#define DATASTART ((ptr_t)0x08000000)
     extern char **environ;
#define DATAEND ((ptr_t)(environ - 0x10))
#define STACKBOTTOM ((ptr_t)0x4fffffff)
#endif
#endif
#ifdef NIOS2
#define CPP_WORDSZ 32
#define MACH_TYPE "NIOS2"
#ifdef LINUX
      extern int __data_start[];
#define DATASTART ((ptr_t)(__data_start))
#define ALIGNMENT 4
#ifndef HBLKSIZE
#define HBLKSIZE 4096
#endif
#endif
#endif
#ifdef OR1K
#define CPP_WORDSZ 32
#define MACH_TYPE "OR1K"
#ifdef LINUX
      extern int __data_start[];
#define DATASTART ((ptr_t)(__data_start))
#define ALIGNMENT 4
#ifndef HBLKSIZE
#define HBLKSIZE 4096
#endif
#endif
#endif
#ifdef HP_PA
#define MACH_TYPE "HP_PA"
#ifdef __LP64__
#define CPP_WORDSZ 64
#define ALIGNMENT 8
#else
#define CPP_WORDSZ 32
#define ALIGNMENT 4
#endif
#define STACK_GROWS_UP
#ifdef HPUX
#ifndef GC_THREADS
#define MPROTECT_VDB
#endif
#ifdef USE_HPUX_FIXED_STACKBOTTOM
#define STACKBOTTOM ((ptr_t)0x7b033000)
#elif defined(USE_ENVIRON_POINTER)
        extern char ** environ;
#define STACKBOTTOM ((ptr_t)environ)
#elif !defined(HEURISTIC2)
#define HPUX_MAIN_STACKBOTTOM
#define NEED_FIND_LIMIT
#endif
#ifndef __GNUC__
#define PREFETCH(x)  do { \
                              register long addr = (long)(x); \
                              (void) _asm ("LDW", 0, 0, addr, 0); \
                            } while (0)
#endif
#endif
#ifdef LINUX
#define SEARCH_FOR_DATA_START
#endif
#ifdef OPENBSD
#endif
#endif
#ifdef ALPHA
#define MACH_TYPE "ALPHA"
#define ALIGNMENT 8
#define CPP_WORDSZ 64
#ifdef NETBSD
#define ELFCLASS32 32
#define ELFCLASS64 64
#define ELF_CLASS ELFCLASS64
#endif
#ifdef OPENBSD
#endif
#ifdef FREEBSD
#define SIG_SUSPEND SIGUSR1
#define SIG_THR_RESTART SIGUSR2
        extern char etext[];
        extern char edata[];
#if !defined(CPPCHECK)
          extern char end[];
#endif
#define NEED_FIND_LIMIT
#define DATASTART ((ptr_t)(&etext))
        void * GC_find_limit(void *, int);
#define DATAEND (ptr_t)GC_find_limit(DATASTART, TRUE)
#define DATAEND_IS_FUNC
#define GC_HAVE_DATAREGION2
#define DATASTART2 ((ptr_t)(&edata))
#define DATAEND2 ((ptr_t)(&end))
#endif
#ifdef OSF1
#define OS_TYPE "OSF1"
#define DATASTART ((ptr_t)0x140000000)
        extern int _end[];
#define DATAEND ((ptr_t)(&_end))
        extern char ** environ;
#define STACKBOTTOM ((ptr_t)(((word)(environ) | (getpagesize()-1))+1))
        extern int __start[];
#define HEURISTIC2_LIMIT ((ptr_t)((word)(__start) & ~(getpagesize()-1)))
#ifndef GC_OSF1_THREADS
#define MPROTECT_VDB
#endif
#define DYNAMIC_LOADING
#endif
#ifdef LINUX
#ifdef __ELF__
#define SEARCH_FOR_DATA_START
#else
#define DATASTART ((ptr_t)0x140000000)
          extern int _end[];
#define DATAEND ((ptr_t)(_end))
#endif
#if !defined(REDIRECT_MALLOC)
#define MPROTECT_VDB
#endif
#endif
#endif
#ifdef IA64
#define MACH_TYPE "IA64"
#ifdef HPUX
#ifdef _ILP32
#define CPP_WORDSZ 32
#define ALIGNMENT 4
#else
#if !defined(_LP64) && !defined(CPPCHECK)
#error Unknown ABI
#endif
#define CPP_WORDSZ 64
#define ALIGNMENT 8
#endif
        extern char ** environ;
#define STACKBOTTOM ((ptr_t)environ)
#define HPUX_STACKBOTTOM
#define BACKING_STORE_DISPLACEMENT 0x1000000
#define BACKING_STORE_ALIGNMENT 0x1000
        extern ptr_t GC_register_stackbottom;
#define BACKING_STORE_BASE GC_register_stackbottom
#endif
#ifdef LINUX
#define CPP_WORDSZ 64
#define ALIGNMENT 8
        extern ptr_t GC_register_stackbottom;
#define BACKING_STORE_BASE GC_register_stackbottom
#define SEARCH_FOR_DATA_START
#ifdef __GNUC__
#define DYNAMIC_LOADING
#else
#endif
#if !defined(REDIRECT_MALLOC)
#define MPROTECT_VDB
#endif
#ifdef __GNUC__
#ifndef __INTEL_COMPILER
#define PREFETCH(x) \
              __asm__ ("        lfetch  [%0]": : "r"(x))
#define GC_PREFETCH_FOR_WRITE(x) \
              __asm__ ("        lfetch.excl     [%0]": : "r"(x))
#define CLEAR_DOUBLE(x) \
              __asm__ ("        stf.spill       [%0]=f0": : "r"((void *)(x)))
#else
            EXTERN_C_END
#include <ia64intrin.h>
            EXTERN_C_BEGIN
#define PREFETCH(x) __lfetch(__lfhint_none, (x))
#define GC_PREFETCH_FOR_WRITE(x) __lfetch(__lfhint_nta, (x))
#define CLEAR_DOUBLE(x) __stf_spill((void *)(x), 0)
#endif
#endif
#endif
#ifdef MSWIN32
#if defined(_WIN64)
#define CPP_WORDSZ 64
#else
#define CPP_WORDSZ 32
#endif
#define ALIGNMENT 8
#endif
#endif
#ifdef M88K
#define MACH_TYPE "M88K"
#define ALIGNMENT 4
#define STACKBOTTOM ((char*)0xf0000000)
    extern int etext[];
#ifdef CX_UX
#define OS_TYPE "CX_UX"
#define DATASTART ((ptr_t)((((word)(etext) + 0x3fffff) & ~0x3fffff) \
                                  + 0x10000))
#endif
#ifdef DGUX
#define OS_TYPE "DGUX"
        ptr_t GC_SysVGetDataStart(size_t, ptr_t);
#define DATASTART GC_SysVGetDataStart(0x10000, (ptr_t)etext)
#define DATASTART_IS_FUNC
#endif
#endif
#ifdef S370
#define MACH_TYPE "S370"
#define ALIGNMENT 4
#ifdef UTS4
#define OS_TYPE "UTS4"
        extern int etext[];
        extern int _etext[];
        extern int _end[];
        ptr_t GC_SysVGetDataStart(size_t, ptr_t);
#define DATASTART GC_SysVGetDataStart(0x10000, (ptr_t)_etext)
#define DATASTART_IS_FUNC
#define DATAEND ((ptr_t)(_end))
#define HEURISTIC2
#endif
#endif
#ifdef S390
#define MACH_TYPE "S390"
#ifndef __s390x__
#define ALIGNMENT 4
#define CPP_WORDSZ 32
#else
#define ALIGNMENT 8
#define CPP_WORDSZ 64
#ifndef HBLKSIZE
#define HBLKSIZE 4096
#endif
#endif
#ifdef LINUX
        extern int __data_start[] __attribute__((__weak__));
#define DATASTART ((ptr_t)(__data_start))
        extern int _end[] __attribute__((__weak__));
#define DATAEND ((ptr_t)(_end))
#define CACHE_LINE_SIZE 256
#define GETPAGESIZE() 4096
#if !defined(REDIRECT_MALLOC)
#define MPROTECT_VDB
#endif
#ifndef SOFT_VDB
#define SOFT_VDB
#endif
#endif
#endif
#ifdef AARCH64
#define MACH_TYPE "AARCH64"
#ifdef __ILP32__
#define CPP_WORDSZ 32
#define ALIGNMENT 4
#else
#define CPP_WORDSZ 64
#define ALIGNMENT 8
#endif
#ifndef HBLKSIZE
#define HBLKSIZE 4096
#endif
#ifdef LINUX
#if !defined(REDIRECT_MALLOC)
#define MPROTECT_VDB
#endif
#if defined(HOST_ANDROID)
#define SEARCH_FOR_DATA_START
#else
        extern int __data_start[];
#define DATASTART ((ptr_t)__data_start)
#endif
#endif
#ifdef DARWIN
#define DARWIN_DONT_PARSE_STACK 1
#define STACKBOTTOM ((ptr_t)0x16fdfffff)
#if TARGET_OS_IPHONE && !defined(NO_DYLD_BIND_FULLY_IMAGE)
#define NO_DYLD_BIND_FULLY_IMAGE
#endif
#endif
#ifdef FREEBSD
#endif
#ifdef NETBSD
#define ELF_CLASS ELFCLASS64
#endif
#ifdef OPENBSD
#endif
#ifdef NINTENDO_SWITCH
#define OS_TYPE "NINTENDO_SWITCH"
      extern int __bss_end[];
#define NO_HANDLE_FORK 1
#define DATASTART (ptr_t)ALIGNMENT
#define DATAEND (ptr_t)(&__bss_end)
      void *switch_get_stack_bottom(void);
#define STACKBOTTOM ((ptr_t)switch_get_stack_bottom())
#endif
#ifdef MSWIN32
#endif
#ifdef NOSYS
#define OS_TYPE "NOSYS"
      extern int __data_start[];
#define DATASTART ((ptr_t)__data_start)
      extern void *__stack_base__;
#define STACKBOTTOM ((ptr_t)__stack_base__)
#endif
#endif
#ifdef ARM32
#if defined(NACL)
#define MACH_TYPE "NACL"
#else
#define MACH_TYPE "ARM32"
#endif
#define CPP_WORDSZ 32
#define ALIGNMENT 4
#ifdef NETBSD
#endif
#ifdef LINUX
#if !defined(REDIRECT_MALLOC)
#define MPROTECT_VDB
#endif
#if defined(__GLIBC__) && __GLIBC__ >= 2 \
                || defined(HOST_ANDROID) || defined(HOST_TIZEN)
#define SEARCH_FOR_DATA_START
#else
            extern char **__environ;
#define DATASTART ((ptr_t)(&__environ))
#endif
#endif
#ifdef MSWINCE
#endif
#ifdef FREEBSD
#define SIG_SUSPEND SIGUSR1
#define SIG_THR_RESTART SIGUSR2
#endif
#ifdef DARWIN
#define DARWIN_DONT_PARSE_STACK 1
#define STACKBOTTOM ((ptr_t)0x30000000)
#if TARGET_OS_IPHONE && !defined(NO_DYLD_BIND_FULLY_IMAGE)
#define NO_DYLD_BIND_FULLY_IMAGE
#endif
#endif
#ifdef OPENBSD
#endif
#ifdef SN_TARGET_PSP2
#define OS_TYPE "SN_TARGET_PSP2"
#define NO_HANDLE_FORK 1
#define DATASTART (ptr_t)ALIGNMENT
#define DATAEND (ptr_t)ALIGNMENT
      void *psp2_get_stack_bottom(void);
#define STACKBOTTOM ((ptr_t)psp2_get_stack_bottom())
#endif
#ifdef NN_PLATFORM_CTR
#define OS_TYPE "NN_PLATFORM_CTR"
      extern unsigned char Image$$ZI$$ZI$$Base[];
#define DATASTART (ptr_t)(Image$$ZI$$ZI$$Base)
      extern unsigned char Image$$ZI$$ZI$$Limit[];
#define DATAEND (ptr_t)(Image$$ZI$$ZI$$Limit)
      void *n3ds_get_stack_bottom(void);
#define STACKBOTTOM ((ptr_t)n3ds_get_stack_bottom())
#endif
#ifdef MSWIN32
#endif
#ifdef NOSYS
#define OS_TYPE "NOSYS"
      extern int __data_start[];
#define DATASTART ((ptr_t)(__data_start))
      extern void *__stack_base__;
#define STACKBOTTOM ((ptr_t)(__stack_base__))
#endif
#endif
#ifdef CRIS
#define MACH_TYPE "CRIS"
#define CPP_WORDSZ 32
#define ALIGNMENT 1
#ifdef LINUX
#define SEARCH_FOR_DATA_START
#endif
#endif
#if defined(SH) && !defined(SH4)
#define MACH_TYPE "SH"
#define ALIGNMENT 4
#ifdef LINUX
#define SEARCH_FOR_DATA_START
#endif
#ifdef NETBSD
#endif
#ifdef OPENBSD
#endif
#ifdef MSWINCE
#endif
#endif
#ifdef SH4
#define MACH_TYPE "SH4"
#define ALIGNMENT 4
#ifdef MSWINCE
#endif
#endif
#ifdef AVR32
#define MACH_TYPE "AVR32"
#define CPP_WORDSZ 32
#define ALIGNMENT 4
#ifdef LINUX
#define SEARCH_FOR_DATA_START
#endif
#endif
#ifdef M32R
#define CPP_WORDSZ 32
#define MACH_TYPE "M32R"
#define ALIGNMENT 4
#ifdef LINUX
#define SEARCH_FOR_DATA_START
#endif
#endif
#ifdef X86_64
#define MACH_TYPE "X86_64"
#ifdef __ILP32__
#define ALIGNMENT 4
#define CPP_WORDSZ 32
#else
#define ALIGNMENT 8
#define CPP_WORDSZ 64
#endif
#ifndef HBLKSIZE
#define HBLKSIZE 4096
#endif
#ifndef CACHE_LINE_SIZE
#define CACHE_LINE_SIZE 64
#endif
#ifdef PLATFORM_GETMEM
#define OS_TYPE "PLATFORM_GETMEM"
#define DATASTART (ptr_t)ALIGNMENT
#define DATAEND (ptr_t)ALIGNMENT
      EXTERN_C_END
#include <pthread.h>
      EXTERN_C_BEGIN
      void *platform_get_stack_bottom(void);
#define STACKBOTTOM ((ptr_t)platform_get_stack_bottom())
#endif
#ifdef LINUX
#if !defined(REDIRECT_MALLOC)
#define MPROTECT_VDB
#else
#endif
#define SEARCH_FOR_DATA_START
#if defined(__GLIBC__) && !defined(__UCLIBC__)
#define USE_MMAP_ANON
#endif
#if defined(__GLIBC__) && !defined(__UCLIBC__) \
           && !defined(GETCONTEXT_FPU_BUG_FIXED)
#define GETCONTEXT_FPU_EXCMASK_BUG
#endif
#if defined(__GLIBC__) && !defined(__UCLIBC__) \
           && !defined(GLIBC_TSX_BUG_FIXED)
#define GLIBC_2_19_TSX_BUG
          EXTERN_C_END
#include <gnu/libc-version.h>
          EXTERN_C_BEGIN
#endif
#ifndef SOFT_VDB
#define SOFT_VDB
#endif
#endif
#ifdef DARWIN
#define DARWIN_DONT_PARSE_STACK 1
#define STACKBOTTOM ((ptr_t)0x7fff5fc00000)
#define MPROTECT_VDB
#if TARGET_OS_IPHONE && !defined(NO_DYLD_BIND_FULLY_IMAGE)
#define NO_DYLD_BIND_FULLY_IMAGE
#endif
#endif
#ifdef FREEBSD
#ifdef __GLIBC__
#define SIG_SUSPEND          (32+6)
#define SIG_THR_RESTART      (32+5)
            extern int _end[];
#define DATAEND ((ptr_t)(_end))
#else
#define SIG_SUSPEND SIGUSR1
#define SIG_THR_RESTART SIGUSR2
#endif
#if defined(__DragonFly__)
#define COUNT_UNMAPPED_REGIONS
#endif
#endif
#ifdef NETBSD
#endif
#ifdef OPENBSD
#endif
#ifdef HAIKU
#define HEURISTIC2
#define SEARCH_FOR_DATA_START
#endif
#ifdef SOLARIS
#define ELF_CLASS ELFCLASS64
#define DATASTART GC_SysVGetDataStart(0x1000, (ptr_t)_etext)
#ifdef SOLARIS25_PROC_VDB_BUG_FIXED
#define PROC_VDB
#endif
#endif
#ifdef CYGWIN32
#ifndef USE_WINALLOC
#if defined(THREAD_LOCAL_ALLOC)
#else
#define MPROTECT_VDB
#endif
#endif
#endif
#ifdef MSWIN_XBOX1
#define OS_TYPE "MSWIN_XBOX1"
#define NO_GETENV
#define DATASTART (ptr_t)ALIGNMENT
#define DATAEND (ptr_t)ALIGNMENT
      LONG64 durango_get_stack_bottom(void);
#define STACKBOTTOM ((ptr_t)durango_get_stack_bottom())
#define GETPAGESIZE() 4096
#ifndef USE_MMAP
#define USE_MMAP 1
#endif
#define PROT_NONE  0
#define PROT_READ  1
#define PROT_WRITE 2
#define PROT_EXEC  4
#define MAP_PRIVATE 2
#define MAP_FIXED  0x10
#define MAP_FAILED ((void *)-1)
#endif
#ifdef MSWIN32
#define RETRY_GET_THREAD_CONTEXT
#if !defined(__GNUC__) || defined(__INTEL_COMPILER) \
           || GC_GNUC_PREREQ(4, 7)
#define MPROTECT_VDB
#endif
#endif
#endif
#ifdef ARC
#define CPP_WORDSZ 32
#define MACH_TYPE "ARC"
#define ALIGNMENT 4
#define CACHE_LINE_SIZE 64
#ifdef LINUX
      extern int __data_start[] __attribute__((__weak__));
#define DATASTART ((ptr_t)__data_start)
#endif
#endif
#ifdef HEXAGON
#define CPP_WORDSZ 32
#define MACH_TYPE "HEXAGON"
#define ALIGNMENT 4
#ifdef LINUX
#if !defined(REDIRECT_MALLOC)
#define MPROTECT_VDB
#endif
#if defined(__GLIBC__)
#define SEARCH_FOR_DATA_START
#elif !defined(CPPCHECK)
#error Unknown Hexagon libc configuration
#endif
#endif
#endif
#ifdef TILEPRO
#define CPP_WORDSZ 32
#define MACH_TYPE "TILEPro"
#define ALIGNMENT 4
#define PREFETCH(x) __insn_prefetch(x)
#define CACHE_LINE_SIZE 64
#ifdef LINUX
      extern int __data_start[];
#define DATASTART ((ptr_t)__data_start)
#endif
#endif
#ifdef TILEGX
#define CPP_WORDSZ (__SIZEOF_POINTER__ * 8)
#define MACH_TYPE "TILE-Gx"
#define ALIGNMENT __SIZEOF_POINTER__
#if CPP_WORDSZ < 64
#define CLEAR_DOUBLE(x) (*(long long *)(x) = 0)
#endif
#define PREFETCH(x) __insn_prefetch_l1(x)
#define CACHE_LINE_SIZE 64
#ifdef LINUX
      extern int __data_start[];
#define DATASTART ((ptr_t)__data_start)
#endif
#endif
#ifdef RISCV
#define MACH_TYPE "RISC-V"
#define CPP_WORDSZ __riscv_xlen
#define ALIGNMENT (CPP_WORDSZ/8)
#ifdef FREEBSD
#define SIG_SUSPEND SIGUSR1
#define SIG_THR_RESTART SIGUSR2
#endif
#ifdef LINUX
      extern int __data_start[] __attribute__((__weak__));
#define DATASTART ((ptr_t)__data_start)
#endif
#endif
#if defined(__GLIBC__) && !defined(DONT_USE_LIBC_PRIVATES)
#define USE_LIBC_PRIVATES
#endif
#ifdef NO_RETRY_GET_THREAD_CONTEXT
#undef RETRY_GET_THREAD_CONTEXT
#endif
#if defined(LINUX_STACKBOTTOM) && defined(NO_PROC_STAT) \
    && !defined(USE_LIBC_PRIVATES)
#undef LINUX_STACKBOTTOM
#define HEURISTIC2
#endif
#if defined(USE_MMAP_ANON) && !defined(USE_MMAP)
#define USE_MMAP 1
#elif (defined(LINUX) || defined(OPENBSD)) && defined(USE_MMAP)
#define USE_MMAP_ANON
#endif
#if defined(GC_LINUX_THREADS) && defined(REDIRECT_MALLOC) \
    && !defined(USE_PROC_FOR_LIBRARIES)
#define USE_PROC_FOR_LIBRARIES
#endif
#ifndef STACK_GROWS_UP
#define STACK_GROWS_DOWN
#endif
#ifndef CPP_WORDSZ
#define CPP_WORDSZ 32
#endif
#ifndef OS_TYPE
#define OS_TYPE ""
#endif
#ifndef DATAEND
#if !defined(CPPCHECK)
    extern int end[];
#endif
#define DATAEND ((ptr_t)(end))
#endif
#if defined(HOST_ANDROID) && defined(__clang__) \
    && !defined(BROKEN_UUENDUU_SYM)
#undef DATAEND
#pragma weak __end__
  extern int __end__[];
#define DATAEND (__end__ != 0 ? (ptr_t)__end__ : (ptr_t)_end)
#endif
#if (defined(SVR4) || defined(HOST_ANDROID) || defined(HOST_TIZEN)) \
    && !defined(GETPAGESIZE)
  EXTERN_C_END
#include <unistd.h>
  EXTERN_C_BEGIN
#define GETPAGESIZE() (unsigned)sysconf(_SC_PAGESIZE)
#endif
#ifndef GETPAGESIZE
#if defined(SOLARIS) || defined(IRIX5) || defined(LINUX) \
     || defined(NETBSD) || defined(FREEBSD) || defined(HPUX)
    EXTERN_C_END
#include <unistd.h>
    EXTERN_C_BEGIN
#endif
#define GETPAGESIZE() (unsigned)getpagesize()
#endif
#if defined(HOST_ANDROID) && !(__ANDROID_API__ >= 23) \
    && ((defined(MIPS) && (CPP_WORDSZ == 32)) \
        || defined(ARM32) || defined(I386) )
#define USE_TKILL_ON_ANDROID
#endif
#if defined(SOLARIS) || defined(DRSNX) || defined(UTS4)
#define SVR4
#endif
#if defined(SOLARIS) || defined(DRSNX)
#define SOLARISDL
#define SUNOS5SIGS
#endif
#if defined(HPUX)
#define SUNOS5SIGS
#endif
#if defined(FREEBSD) && (defined(__DragonFly__) || __FreeBSD__ >= 4 \
                         || (__FreeBSD_kernel__ >= 4))
#define SUNOS5SIGS
#endif
#if !defined(GC_EXPLICIT_SIGNALS_UNBLOCK) && defined(SUNOS5SIGS) \
    && !defined(GC_NO_PTHREAD_SIGMASK)
#define GC_EXPLICIT_SIGNALS_UNBLOCK
#endif
#if !defined(NO_SIGNALS_UNBLOCK_IN_MAIN) && defined(GC_NO_PTHREAD_SIGMASK)
#define NO_SIGNALS_UNBLOCK_IN_MAIN
#endif
#if !defined(NO_MARKER_SPECIAL_SIGMASK) \
    && (defined(NACL) || defined(GC_WIN32_PTHREADS))
#define NO_MARKER_SPECIAL_SIGMASK
#endif
#ifdef GC_NETBSD_THREADS
#define SIGRTMIN 33
#define SIGRTMAX 63
#define GC_NETBSD_THREADS_WORKAROUND
#endif
#ifdef GC_OPENBSD_THREADS
  EXTERN_C_END
#include <sys/param.h>
  EXTERN_C_BEGIN
#if OpenBSD < 201211
#define GC_OPENBSD_UTHREADS 1
#endif
#endif
#if defined(SVR4) || defined(LINUX) || defined(IRIX5) || defined(HPUX) \
    || defined(OPENBSD) || defined(NETBSD) || defined(FREEBSD) \
    || defined(DGUX) || defined(BSD) || defined(HAIKU) || defined(HURD) \
    || defined(AIX) || defined(DARWIN) || defined(OSF1)
#define UNIX_LIKE
#endif
#if defined(CPPCHECK)
#undef CPP_WORDSZ
#define CPP_WORDSZ (__SIZEOF_POINTER__ * 8)
#elif CPP_WORDSZ != 32 && CPP_WORDSZ != 64
#error Bad word size
#endif
#if !defined(ALIGNMENT) && !defined(CPPCHECK)
#error Undefined ALIGNMENT
#endif
#ifdef PCR
#undef DYNAMIC_LOADING
#undef STACKBOTTOM
#undef HEURISTIC1
#undef HEURISTIC2
#undef PROC_VDB
#undef MPROTECT_VDB
#define PCR_VDB
#endif
#if !defined(STACKBOTTOM) && (defined(ECOS) || defined(NOSYS)) \
    && !defined(CPPCHECK)
#error Undefined STACKBOTTOM
#endif
#ifdef IGNORE_DYNAMIC_LOADING
#undef DYNAMIC_LOADING
#endif
#if defined(SMALL_CONFIG) && !defined(GC_DISABLE_INCREMENTAL)
#define GC_DISABLE_INCREMENTAL
#endif
#if (defined(MSWIN32) || defined(MSWINCE)) && !defined(USE_WINALLOC)
#define USE_WINALLOC 1
#endif
#ifdef USE_WINALLOC
#undef USE_MMAP
#endif
#if defined(DARWIN) || defined(FREEBSD) || defined(HAIKU) \
    || defined(IRIX5) || defined(LINUX) || defined(NETBSD) \
    || defined(OPENBSD) || defined(SOLARIS) \
    || ((defined(CYGWIN32) || defined(USE_MMAP) || defined(USE_MUNMAP)) \
        && !defined(USE_WINALLOC))
#define MMAP_SUPPORTED
#endif
#if defined(USE_MUNMAP) && !defined(MUNMAP_THRESHOLD) \
    && (defined(SN_TARGET_PS3) \
        || defined(SN_TARGET_PSP2) || defined(MSWIN_XBOX1))
#define MUNMAP_THRESHOLD 2
#endif
#if defined(USE_MUNMAP) && defined(COUNT_UNMAPPED_REGIONS) \
    && !defined(GC_UNMAPPED_REGIONS_SOFT_LIMIT)
#if defined(__DragonFly__)
#define GC_UNMAPPED_REGIONS_SOFT_LIMIT (1000000 / 4)
#else
#define GC_UNMAPPED_REGIONS_SOFT_LIMIT 16384
#endif
#endif
#if defined(GC_DISABLE_INCREMENTAL) || defined(DEFAULT_VDB)
#undef GWW_VDB
#undef MPROTECT_VDB
#undef PCR_VDB
#undef PROC_VDB
#undef SOFT_VDB
#endif
#ifdef NO_GWW_VDB
#undef GWW_VDB
#endif
#ifdef NO_MPROTECT_VDB
#undef MPROTECT_VDB
#endif
#ifdef NO_SOFT_VDB
#undef SOFT_VDB
#endif
#if defined(SOFT_VDB) && defined(SOFT_VDB_LINUX_VER_STATIC_CHECK)
  EXTERN_C_END
#include <linux/version.h>
  EXTERN_C_BEGIN
#if LINUX_VERSION_CODE < KERNEL_VERSION(3, 18, 0)
#undef SOFT_VDB
#endif
#endif
#ifdef GC_DISABLE_INCREMENTAL
#undef CHECKSUMS
#endif
#ifdef USE_GLOBAL_ALLOC
#undef GWW_VDB
#endif
#if defined(BASE_ATOMIC_OPS_EMULATED)
#undef MPROTECT_VDB
#endif
#if defined(USE_PROC_FOR_LIBRARIES) && defined(GC_LINUX_THREADS)
#undef MPROTECT_VDB
#endif
#if defined(MPROTECT_VDB) && defined(GC_PREFER_MPROTECT_VDB)
#undef PCR_VDB
#undef PROC_VDB
#endif
#ifdef PROC_VDB
#undef MPROTECT_VDB
#undef SOFT_VDB
#endif
#if defined(MPROTECT_VDB) && !defined(MSWIN32) && !defined(MSWINCE)
#include <signal.h>
#endif
#if defined(SIGBUS) && !defined(HAVE_SIGBUS) && !defined(CPPCHECK)
#define HAVE_SIGBUS
#endif
#ifndef SA_SIGINFO
#define NO_SA_SIGACTION
#endif
#if (defined(NO_SA_SIGACTION) || defined(GC_NO_SIGSETJMP)) \
    && defined(MPROTECT_VDB) && !defined(DARWIN) \
    && !defined(MSWIN32) && !defined(MSWINCE)
#undef MPROTECT_VDB
#endif
#if !defined(PCR_VDB) && !defined(PROC_VDB) && !defined(MPROTECT_VDB) \
    && !defined(GWW_VDB) && !defined(SOFT_VDB) && !defined(DEFAULT_VDB) \
    && !defined(GC_DISABLE_INCREMENTAL)
#define DEFAULT_VDB
#endif
#if !defined(PROC_VDB) && !defined(SOFT_VDB) \
    && !defined(NO_VDB_FOR_STATIC_ROOTS)
#define NO_VDB_FOR_STATIC_ROOTS
#endif
#if ((defined(UNIX_LIKE) && (defined(DARWIN) || defined(HAIKU) \
                             || defined(HURD) || defined(OPENBSD) \
                             || defined(ARM32) \
                             || defined(AVR32) || defined(MIPS) \
                             || defined(NIOS2) || defined(OR1K))) \
     || (defined(LINUX) && !defined(__gnu_linux__)) \
     || (defined(RTEMS) && defined(I386)) || defined(HOST_ANDROID)) \
    && !defined(NO_GETCONTEXT)
#define NO_GETCONTEXT 1
#endif
#ifndef PREFETCH
#if GC_GNUC_PREREQ(3, 0) && !defined(NO_PREFETCH)
#define PREFETCH(x) __builtin_prefetch((x), 0, 0)
#else
#define PREFETCH(x) (void)0
#endif
#endif
#ifndef GC_PREFETCH_FOR_WRITE
#if GC_GNUC_PREREQ(3, 0) && !defined(GC_NO_PREFETCH_FOR_WRITE)
#define GC_PREFETCH_FOR_WRITE(x) __builtin_prefetch((x), 1)
#else
#define GC_PREFETCH_FOR_WRITE(x) (void)0
#endif
#endif
#ifndef CACHE_LINE_SIZE
#define CACHE_LINE_SIZE 32
#endif
#ifndef STATIC
#ifdef GC_ASSERTIONS
#define STATIC
#else
#define STATIC static
#endif
#endif
#if defined(LINUX) && (defined(USE_PROC_FOR_LIBRARIES) || defined(IA64) \
                       || !defined(SMALL_CONFIG))
#define NEED_PROC_MAPS
#endif
#if defined(LINUX) || defined(HURD) || defined(__GLIBC__)
#define REGISTER_LIBRARIES_EARLY
#endif
#if defined(SEARCH_FOR_DATA_START)
  extern ptr_t GC_data_start;
#define DATASTART GC_data_start
#endif
#ifndef HEAP_START
#define HEAP_START ((ptr_t)0)
#endif
#ifndef CLEAR_DOUBLE
#define CLEAR_DOUBLE(x) (((word*)(x))[0] = 0, ((word*)(x))[1] = 0)
#endif
#if defined(GC_LINUX_THREADS) && defined(REDIRECT_MALLOC) \
    && !defined(INCLUDE_LINUX_THREAD_DESCR)
#define INCLUDE_LINUX_THREAD_DESCR
#endif
#if !defined(CPPCHECK)
#if defined(GC_IRIX_THREADS) && !defined(IRIX5)
#error Inconsistent configuration
#endif
#if defined(GC_LINUX_THREADS) && !defined(LINUX) && !defined(NACL)
#error Inconsistent configuration
#endif
#if defined(GC_NETBSD_THREADS) && !defined(NETBSD)
#error Inconsistent configuration
#endif
#if defined(GC_FREEBSD_THREADS) && !defined(FREEBSD)
#error Inconsistent configuration
#endif
#if defined(GC_SOLARIS_THREADS) && !defined(SOLARIS)
#error Inconsistent configuration
#endif
#if defined(GC_HPUX_THREADS) && !defined(HPUX)
#error Inconsistent configuration
#endif
#if defined(GC_AIX_THREADS) && !defined(_AIX)
#error Inconsistent configuration
#endif
#if defined(GC_WIN32_THREADS) && !defined(CYGWIN32) && !defined(MSWIN32) \
     && !defined(MSWINCE) && !defined(MSWIN_XBOX1)
#error Inconsistent configuration
#endif
#if defined(GC_WIN32_PTHREADS) && defined(CYGWIN32)
#error Inconsistent configuration
#endif
#endif
#if defined(PCR) || defined(GC_WIN32_THREADS) || defined(GC_PTHREADS) \
    || ((defined(NN_PLATFORM_CTR) || defined(NINTENDO_SWITCH) \
         || defined(SN_TARGET_PS3) \
         || defined(SN_TARGET_PSP2)) && defined(GC_THREADS))
#define THREADS
#endif
#if defined(PARALLEL_MARK) && !defined(THREADS) && !defined(CPPCHECK)
#error Invalid config: PARALLEL_MARK requires GC_THREADS
#endif
#if defined(GWW_VDB) && !defined(USE_WINALLOC) && !defined(CPPCHECK)
#error Invalid config: GWW_VDB requires USE_WINALLOC
#endif
#if (((defined(MSWIN32) || defined(MSWINCE)) && !defined(__GNUC__)) \
        || (defined(MSWIN32) && defined(I386))  \
        || (defined(USE_PROC_FOR_LIBRARIES) && defined(THREADS))) \
    && !defined(NO_CRT) && !defined(NO_WRAP_MARK_SOME)
#define WRAP_MARK_SOME
#endif
#if defined(PARALLEL_MARK) && !defined(DEFAULT_STACK_MAYBE_SMALL) \
    && (defined(HPUX) || defined(GC_DGUX386_THREADS) \
        || defined(NO_GETCONTEXT) )
#define DEFAULT_STACK_MAYBE_SMALL
#endif
#ifdef PARALLEL_MARK
#define MIN_STACK_SIZE (8 * HBLKSIZE * sizeof(word))
#endif
#if defined(HOST_ANDROID) && !defined(THREADS) \
    && !defined(USE_GET_STACKBASE_FOR_MAIN)
#define USE_GET_STACKBASE_FOR_MAIN
#endif
#if ((defined(FREEBSD) && defined(__GLIBC__))  \
     || defined(LINUX) || defined(NETBSD) || defined(HOST_ANDROID)) \
    && !defined(NO_PTHREAD_GETATTR_NP)
#define HAVE_PTHREAD_GETATTR_NP 1
#elif defined(FREEBSD) && !defined(__GLIBC__) \
      && !defined(NO_PTHREAD_ATTR_GET_NP)
#define HAVE_PTHREAD_NP_H 1
#define HAVE_PTHREAD_ATTR_GET_NP 1
#endif
#if defined(UNIX_LIKE) && defined(THREADS) && !defined(NO_CANCEL_SAFE) \
    && !defined(HOST_ANDROID)
#define CANCEL_SAFE
#endif
#ifdef CANCEL_SAFE
#define IF_CANCEL(x) x
#else
#define IF_CANCEL(x)
#endif
#if !defined(CAN_HANDLE_FORK) && !defined(NO_HANDLE_FORK) \
    && !defined(HAVE_NO_FORK) \
    && ((defined(GC_PTHREADS) && !defined(NACL) \
         && !defined(GC_WIN32_PTHREADS) && !defined(USE_WINALLOC)) \
        || (defined(DARWIN) && defined(MPROTECT_VDB)) || defined(HANDLE_FORK))
#define CAN_HANDLE_FORK
#endif
#if defined(CAN_HANDLE_FORK) && !defined(CAN_CALL_ATFORK) \
    && !defined(GC_NO_CAN_CALL_ATFORK) && !defined(HOST_TIZEN) \
    && !defined(HURD) && (!defined(HOST_ANDROID) || __ANDROID_API__ >= 21)
#define CAN_CALL_ATFORK
#endif
#if !defined(CAN_HANDLE_FORK) && !defined(HAVE_NO_FORK) \
    && !(defined(CYGWIN32) || defined(SOLARIS) || defined(UNIX_LIKE))
#define HAVE_NO_FORK
#endif
#if !defined(USE_MARK_BITS) && !defined(USE_MARK_BYTES) \
    && defined(PARALLEL_MARK)
#define USE_MARK_BYTES
#endif
#if (defined(MSWINCE) && !defined(__CEGCC__) || defined(MSWINRT_FLAVOR)) \
    && !defined(NO_GETENV)
#define NO_GETENV
#endif
#if (defined(NO_GETENV) || defined(MSWINCE)) && !defined(NO_GETENV_WIN32)
#define NO_GETENV_WIN32
#endif
#if !defined(MSGBOX_ON_ERROR) && !defined(NO_MSGBOX_ON_ERROR) \
    && !defined(SMALL_CONFIG) && defined(MSWIN32) \
    && !defined(MSWINRT_FLAVOR) && !defined(MSWIN_XBOX1)
#define MSGBOX_ON_ERROR
#endif
#ifndef STRTOULL
#if defined(_WIN64) && !defined(__GNUC__)
#define STRTOULL _strtoui64
#elif defined(_LLP64) || defined(__LLP64__) || defined(_WIN64)
#define STRTOULL strtoull
#else
#define STRTOULL strtoul
#endif
#endif
#ifndef GC_WORD_C
#if defined(_WIN64) && !defined(__GNUC__)
#define GC_WORD_C(val) val##ui64
#elif defined(_LLP64) || defined(__LLP64__) || defined(_WIN64)
#define GC_WORD_C(val) val##ULL
#else
#define GC_WORD_C(val) ((word)val##UL)
#endif
#endif
#if defined(__has_feature)
#if __has_feature(address_sanitizer) && !defined(ADDRESS_SANITIZER)
#define ADDRESS_SANITIZER
#endif
#if __has_feature(memory_sanitizer) && !defined(MEMORY_SANITIZER)
#define MEMORY_SANITIZER
#endif
#if __has_feature(thread_sanitizer) && !defined(THREAD_SANITIZER)
#define THREAD_SANITIZER
#endif
#else
#ifdef __SANITIZE_ADDRESS__
#define ADDRESS_SANITIZER
#endif
#endif
#if defined(SPARC)
#define ASM_CLEAR_CODE
#endif
#if defined(SPARC)
#define CAN_SAVE_CALL_ARGS
#endif
#if (defined(I386) || defined(X86_64)) \
    && (defined(LINUX) || defined(__GLIBC__))
#define CAN_SAVE_CALL_ARGS
#endif
#if defined(SAVE_CALL_COUNT) && !defined(GC_ADD_CALLER) \
    && defined(GC_CAN_SAVE_CALL_STACKS)
#define SAVE_CALL_CHAIN
#endif
#ifdef SAVE_CALL_CHAIN
#if defined(SAVE_CALL_NARGS) && defined(CAN_SAVE_CALL_ARGS)
#define NARGS SAVE_CALL_NARGS
#else
#define NARGS 0
#endif
#endif
#ifdef SAVE_CALL_CHAIN
#if !defined(SAVE_CALL_COUNT) || defined(CPPCHECK)
#define NFRAMES 6
#else
#define NFRAMES ((SAVE_CALL_COUNT + 1) & ~1)
#endif
#define NEED_CALLINFO
#endif
#ifdef GC_ADD_CALLER
#define NFRAMES 1
#define NARGS 0
#define NEED_CALLINFO
#endif
#if (defined(FREEBSD) || (defined(DARWIN) && !defined(_POSIX_C_SOURCE)) \
        || (defined(SOLARIS) && (!defined(_XOPEN_SOURCE) \
                                 || defined(__EXTENSIONS__))) \
        || defined(LINUX)) && !defined(HAVE_DLADDR)
#define HAVE_DLADDR 1
#endif
#if defined(MAKE_BACK_GRAPH) && !defined(DBG_HDRS_ALL)
#define DBG_HDRS_ALL 1
#endif
#if defined(POINTER_MASK) && !defined(POINTER_SHIFT)
#define POINTER_SHIFT 0
#endif
#if defined(POINTER_SHIFT) && !defined(POINTER_MASK)
#define POINTER_MASK ((word)(-1))
#endif
#if !defined(FIXUP_POINTER) && defined(POINTER_MASK)
#define FIXUP_POINTER(p) (p = ((p) & POINTER_MASK) << POINTER_SHIFT)
#endif
#if defined(FIXUP_POINTER)
#define NEED_FIXUP_POINTER
#else
#define FIXUP_POINTER(p)
#endif
#if !defined(MARK_BIT_PER_GRANULE) && !defined(MARK_BIT_PER_OBJ)
#define MARK_BIT_PER_GRANULE
#endif
#if !defined(CPPCHECK)
#if defined(MARK_BIT_PER_GRANULE) && defined(MARK_BIT_PER_OBJ)
#error Define only one of MARK_BIT_PER_GRANULE and MARK_BIT_PER_OBJ
#endif
#if defined(STACK_GROWS_UP) && defined(STACK_GROWS_DOWN)
#error Only one of STACK_GROWS_UP and STACK_GROWS_DOWN should be defined
#endif
#if !defined(STACK_GROWS_UP) && !defined(STACK_GROWS_DOWN)
#error One of STACK_GROWS_UP and STACK_GROWS_DOWN should be defined
#endif
#if defined(REDIRECT_MALLOC) && defined(THREADS) && !defined(LINUX) \
     && !defined(REDIRECT_MALLOC_IN_HEADER)
#error REDIRECT_MALLOC with THREADS works at most on Linux
#endif
#endif
#ifdef GC_PRIVATE_H
        struct hblk;
#if defined(PCR)
    char * real_malloc(size_t bytes);
#define GET_MEM(bytes) HBLKPTR(real_malloc(SIZET_SAT_ADD(bytes, \
                                                            GC_page_size)) \
                                          + GC_page_size-1)
#elif defined(OS2)
    void * os2_alloc(size_t bytes);
#define GET_MEM(bytes) HBLKPTR((ptr_t)os2_alloc( \
                                            SIZET_SAT_ADD(bytes, \
                                                          GC_page_size)) \
                                  + GC_page_size-1)
#elif defined(NEXT) || defined(DOS4GW) || defined(NONSTOP) \
        || (defined(AMIGA) && !defined(GC_AMIGA_FASTALLOC)) \
        || (defined(SOLARIS) && !defined(USE_MMAP)) || defined(RTEMS) \
        || defined(__CC_ARM)
#define GET_MEM(bytes) HBLKPTR((size_t)calloc(1, \
                                            SIZET_SAT_ADD(bytes, \
                                                          GC_page_size)) \
                                  + GC_page_size - 1)
#elif defined(MSWIN_XBOX1)
    ptr_t GC_durango_get_mem(size_t bytes);
#define GET_MEM(bytes) (struct hblk *)GC_durango_get_mem(bytes)
#elif defined(MSWIN32) || defined(CYGWIN32)
    ptr_t GC_win32_get_mem(size_t bytes);
#define GET_MEM(bytes) (struct hblk *)GC_win32_get_mem(bytes)
#elif defined(MACOS)
#if defined(USE_TEMPORARY_MEMORY)
      Ptr GC_MacTemporaryNewPtr(size_t size, Boolean clearMemory);
#define GET_MEM(bytes) HBLKPTR(GC_MacTemporaryNewPtr( \
                                        SIZET_SAT_ADD(bytes, \
                                                      GC_page_size), true) \
                        + GC_page_size-1)
#else
#define GET_MEM(bytes) HBLKPTR(NewPtrClear(SIZET_SAT_ADD(bytes, \
                                                              GC_page_size)) \
                                    + GC_page_size-1)
#endif
#elif defined(MSWINCE)
    ptr_t GC_wince_get_mem(size_t bytes);
#define GET_MEM(bytes) (struct hblk *)GC_wince_get_mem(bytes)
#elif defined(AMIGA) && defined(GC_AMIGA_FASTALLOC)
    void *GC_amiga_get_mem(size_t bytes);
#define GET_MEM(bytes) HBLKPTR((size_t)GC_amiga_get_mem( \
                                            SIZET_SAT_ADD(bytes, \
                                                          GC_page_size)) \
                          + GC_page_size-1)
#elif defined(PLATFORM_GETMEM)
    void *platform_get_mem(size_t bytes);
#define GET_MEM(bytes) (struct hblk*)platform_get_mem(bytes)
#elif defined(SN_TARGET_PS3)
    void *ps3_get_mem(size_t bytes);
#define GET_MEM(bytes) (struct hblk*)ps3_get_mem(bytes)
#elif defined(SN_TARGET_PSP2)
    void *psp2_get_mem(size_t bytes);
#define GET_MEM(bytes) (struct hblk*)psp2_get_mem(bytes)
#elif defined(NINTENDO_SWITCH)
    void *switch_get_mem(size_t bytes);
#define GET_MEM(bytes) (struct hblk*)switch_get_mem(bytes)
#elif defined(HAIKU)
    ptr_t GC_haiku_get_mem(size_t bytes);
#define GET_MEM(bytes) (struct hblk*)GC_haiku_get_mem(bytes)
#elif defined(EMSCRIPTEN_TINY)
    void *emmalloc_memalign(size_t alignment, size_t size);
#define GET_MEM(bytes) (struct hblk*)emmalloc_memalign(GC_page_size, bytes)
#else
    ptr_t GC_unix_get_mem(size_t bytes);
#define GET_MEM(bytes) (struct hblk *)GC_unix_get_mem(bytes)
#endif
#endif
EXTERN_C_END
#endif
#if !defined(GC_ATOMIC_UNCOLLECTABLE) && defined(ATOMIC_UNCOLLECTABLE)
#define GC_ATOMIC_UNCOLLECTABLE
#endif
#ifndef GC_INNER
#if defined(GC_DLL) && defined(__GNUC__) && !defined(MSWIN32) \
        && !defined(MSWINCE) && !defined(CYGWIN32)
#if GC_GNUC_PREREQ(4, 0) && !defined(GC_NO_VISIBILITY)
#define GC_INNER __attribute__((__visibility__("hidden")))
#else
#define GC_INNER
#endif
#else
#define GC_INNER
#endif
#define GC_EXTERN extern GC_INNER
#endif
#ifdef __cplusplus
#define REGISTER
#else
#define REGISTER register
#endif
#if defined(CPPCHECK)
#define MACRO_BLKSTMT_BEGIN {
#define MACRO_BLKSTMT_END   }
#else
#define MACRO_BLKSTMT_BEGIN do {
#define MACRO_BLKSTMT_END   } while (0)
#endif
#if defined(M68K) && defined(__GNUC__)
#define GC_ATTR_WORD_ALIGNED __attribute__((__aligned__(sizeof(word))))
#else
#define GC_ATTR_WORD_ALIGNED
#endif
#ifndef HEADERS_H
#ifndef GC_HEADERS_H
#define GC_HEADERS_H
#if CPP_WORDSZ != 32 && CPP_WORDSZ < 36 && !defined(CPPCHECK)
#error Get a real machine
#endif
EXTERN_C_BEGIN
typedef struct hblkhdr hdr;
#if CPP_WORDSZ > 32
#define HASH_TL
#endif
#if defined(LARGE_CONFIG) || !defined(SMALL_CONFIG)
#define LOG_BOTTOM_SZ 10
#else
#define LOG_BOTTOM_SZ 11
#endif
#define BOTTOM_SZ (1 << LOG_BOTTOM_SZ)
#ifndef HASH_TL
#define LOG_TOP_SZ (WORDSZ - LOG_BOTTOM_SZ - LOG_HBLKSIZE)
#else
#define LOG_TOP_SZ 11
#endif
#define TOP_SZ (1 << LOG_TOP_SZ)
#ifdef COUNT_HDR_CACHE_HITS
  extern word GC_hdr_cache_hits;
  extern word GC_hdr_cache_misses;
#define HC_HIT() (void)(++GC_hdr_cache_hits)
#define HC_MISS() (void)(++GC_hdr_cache_misses)
#else
#define HC_HIT()
#define HC_MISS()
#endif
typedef struct hce {
  word block_addr;
  hdr * hce_hdr;
} hdr_cache_entry;
#define HDR_CACHE_SIZE 8
#define DECLARE_HDR_CACHE \
        hdr_cache_entry hdr_cache[HDR_CACHE_SIZE]
#define INIT_HDR_CACHE BZERO(hdr_cache, sizeof(hdr_cache))
#define HCE(h) \
        (hdr_cache + (((word)(h) >> LOG_HBLKSIZE) & (HDR_CACHE_SIZE-1)))
#define HCE_VALID_FOR(hce, h) ((hce) -> block_addr == \
                                ((word)(h) >> LOG_HBLKSIZE))
#define HCE_HDR(h) ((hce) -> hce_hdr)
#ifdef PRINT_BLACK_LIST
  GC_INNER hdr * GC_header_cache_miss(ptr_t p, hdr_cache_entry *hce,
                                      ptr_t source);
#define HEADER_CACHE_MISS(p, hce, source) \
          GC_header_cache_miss(p, hce, source)
#else
  GC_INNER hdr * GC_header_cache_miss(ptr_t p, hdr_cache_entry *hce);
#define HEADER_CACHE_MISS(p, hce, source) GC_header_cache_miss(p, hce)
#endif
#define HC_GET_HDR(p, hhdr, source) \
        {  \
          hdr_cache_entry * hce = HCE(p); \
          if (EXPECT(HCE_VALID_FOR(hce, p), TRUE)) { \
            HC_HIT(); \
            hhdr = hce -> hce_hdr; \
          } else { \
            hhdr = HEADER_CACHE_MISS(p, hce, source); \
            if (NULL == hhdr) break;  \
          } \
        }
typedef struct bi {
    hdr * index[BOTTOM_SZ];
    struct bi * asc_link;
    struct bi * desc_link;
    word key;
#ifdef HASH_TL
    struct bi * hash_link;
#endif
} bottom_index;
#define MAX_JUMP (HBLKSIZE - 1)
#define HDR_FROM_BI(bi, p) \
                ((bi)->index[((word)(p) >> LOG_HBLKSIZE) & (BOTTOM_SZ - 1)])
#ifndef HASH_TL
#define BI(p) (GC_top_index \
              [(word)(p) >> (LOG_BOTTOM_SZ + LOG_HBLKSIZE)])
#define HDR_INNER(p) HDR_FROM_BI(BI(p),p)
#ifdef SMALL_CONFIG
#define HDR(p) GC_find_header((ptr_t)(p))
#else
#define HDR(p) HDR_INNER(p)
#endif
#define GET_BI(p, bottom_indx) (void)((bottom_indx) = BI(p))
#define GET_HDR(p, hhdr) (void)((hhdr) = HDR(p))
#define SET_HDR(p, hhdr) (void)(HDR_INNER(p) = (hhdr))
#define GET_HDR_ADDR(p, ha) (void)((ha) = &HDR_INNER(p))
#else
#define TL_HASH(hi) ((hi) & (TOP_SZ - 1))
#define GET_BI(p, bottom_indx) \
        do { \
          REGISTER word hi = (word)(p) >> (LOG_BOTTOM_SZ + LOG_HBLKSIZE); \
          REGISTER bottom_index * _bi = GC_top_index[TL_HASH(hi)]; \
          while (_bi -> key != hi && _bi != GC_all_nils) \
              _bi = _bi -> hash_link; \
          (bottom_indx) = _bi; \
        } while (0)
#define GET_HDR_ADDR(p, ha) \
        do { \
          REGISTER bottom_index * bi; \
          GET_BI(p, bi); \
          (ha) = &HDR_FROM_BI(bi, p); \
        } while (0)
#define GET_HDR(p, hhdr) \
        do { \
          REGISTER hdr ** _ha; \
          GET_HDR_ADDR(p, _ha); \
          (hhdr) = *_ha; \
        } while (0)
#define SET_HDR(p, hhdr) \
        do { \
          REGISTER hdr ** _ha; \
          GET_HDR_ADDR(p, _ha); \
          *_ha = (hhdr); \
        } while (0)
#define HDR(p) GC_find_header((ptr_t)(p))
#endif
#define IS_FORWARDING_ADDR_OR_NIL(hhdr) ((size_t) (hhdr) <= MAX_JUMP)
#define FORWARDED_ADDR(h, hhdr) ((struct hblk *)(h) - (size_t)(hhdr))
EXTERN_C_END
#endif
#endif
#ifndef GC_ATTR_NO_SANITIZE_ADDR
#ifndef ADDRESS_SANITIZER
#define GC_ATTR_NO_SANITIZE_ADDR
#elif GC_CLANG_PREREQ(3, 8)
#define GC_ATTR_NO_SANITIZE_ADDR __attribute__((no_sanitize("address")))
#else
#define GC_ATTR_NO_SANITIZE_ADDR __attribute__((no_sanitize_address))
#endif
#endif
#ifndef GC_ATTR_NO_SANITIZE_MEMORY
#ifndef MEMORY_SANITIZER
#define GC_ATTR_NO_SANITIZE_MEMORY
#elif GC_CLANG_PREREQ(3, 8)
#define GC_ATTR_NO_SANITIZE_MEMORY __attribute__((no_sanitize("memory")))
#else
#define GC_ATTR_NO_SANITIZE_MEMORY __attribute__((no_sanitize_memory))
#endif
#endif
#ifndef GC_ATTR_NO_SANITIZE_THREAD
#ifndef THREAD_SANITIZER
#define GC_ATTR_NO_SANITIZE_THREAD
#elif GC_CLANG_PREREQ(3, 8)
#define GC_ATTR_NO_SANITIZE_THREAD __attribute__((no_sanitize("thread")))
#else
#define GC_ATTR_NO_SANITIZE_THREAD __attribute__((no_sanitize_thread))
#endif
#endif
#ifndef GC_ATTR_UNUSED
#if GC_GNUC_PREREQ(3, 4)
#define GC_ATTR_UNUSED __attribute__((__unused__))
#else
#define GC_ATTR_UNUSED
#endif
#endif
#ifdef HAVE_CONFIG_H
#define GC_INLINE static inline
#elif defined(_MSC_VER) || defined(__INTEL_COMPILER) || defined(__DMC__) \
        || (GC_GNUC_PREREQ(3, 0) && defined(__STRICT_ANSI__)) \
        || defined(__WATCOMC__)
#define GC_INLINE static __inline
#elif GC_GNUC_PREREQ(3, 0) || defined(__sun)
#define GC_INLINE static inline
#else
#define GC_INLINE static
#endif
#ifndef GC_ATTR_NOINLINE
#if GC_GNUC_PREREQ(4, 0)
#define GC_ATTR_NOINLINE __attribute__((__noinline__))
#elif _MSC_VER >= 1400
#define GC_ATTR_NOINLINE __declspec(noinline)
#else
#define GC_ATTR_NOINLINE
#endif
#endif
#ifndef GC_API_OSCALL
#if defined(__GNUC__)
#if GC_GNUC_PREREQ(4, 0) && !defined(GC_NO_VISIBILITY)
#define GC_API_OSCALL extern __attribute__((__visibility__("default")))
#else
#define GC_API_OSCALL extern
#endif
#else
#define GC_API_OSCALL GC_API
#endif
#endif
#ifndef GC_API_PRIV
#define GC_API_PRIV GC_API
#endif
#if defined(THREADS) && !defined(NN_PLATFORM_CTR)
#ifndef GC_ATOMIC_OPS_H
#define GC_ATOMIC_OPS_H
#ifdef GC_BUILTIN_ATOMIC
#ifdef __cplusplus
    extern "C" {
#endif
  typedef GC_word AO_t;
#ifdef GC_PRIVATE_H
#define AO_INLINE GC_INLINE
#else
#define AO_INLINE static __inline
#endif
  typedef unsigned char AO_TS_t;
#define AO_TS_CLEAR 0
#define AO_TS_INITIALIZER (AO_TS_t)AO_TS_CLEAR
#if defined(__GCC_ATOMIC_TEST_AND_SET_TRUEVAL) && !defined(CPPCHECK)
#define AO_TS_SET __GCC_ATOMIC_TEST_AND_SET_TRUEVAL
#else
#define AO_TS_SET (AO_TS_t)1
#endif
#define AO_CLEAR(p) __atomic_clear(p, __ATOMIC_RELEASE)
#define AO_test_and_set_acquire(p) __atomic_test_and_set(p, __ATOMIC_ACQUIRE)
#define AO_HAVE_test_and_set_acquire
#define AO_compiler_barrier() __atomic_signal_fence(__ATOMIC_SEQ_CST)
#define AO_nop_full() __atomic_thread_fence(__ATOMIC_SEQ_CST)
#define AO_HAVE_nop_full
#define AO_fetch_and_add(p, v) __atomic_fetch_add(p, v, __ATOMIC_RELAXED)
#define AO_HAVE_fetch_and_add
#define AO_fetch_and_add1(p) AO_fetch_and_add(p, 1)
#define AO_HAVE_fetch_and_add1
#define AO_or(p, v) (void)__atomic_or_fetch(p, v, __ATOMIC_RELAXED)
#define AO_HAVE_or
#define AO_load(p) __atomic_load_n(p, __ATOMIC_RELAXED)
#define AO_HAVE_load
#define AO_load_acquire(p) __atomic_load_n(p, __ATOMIC_ACQUIRE)
#define AO_HAVE_load_acquire
#define AO_load_acquire_read(p) AO_load_acquire(p)
#define AO_HAVE_load_acquire_read
#define AO_store(p, v) __atomic_store_n(p, v, __ATOMIC_RELAXED)
#define AO_HAVE_store
#define AO_store_release(p, v) __atomic_store_n(p, v, __ATOMIC_RELEASE)
#define AO_HAVE_store_release
#define AO_store_release_write(p, v) AO_store_release(p, v)
#define AO_HAVE_store_release_write
#define AO_char_load(p) __atomic_load_n(p, __ATOMIC_RELAXED)
#define AO_HAVE_char_load
#define AO_char_store(p, v) __atomic_store_n(p, v, __ATOMIC_RELAXED)
#define AO_HAVE_char_store
#ifdef AO_REQUIRE_CAS
    AO_INLINE int
    AO_compare_and_swap(volatile AO_t *p, AO_t ov, AO_t nv)
    {
      return (int)__atomic_compare_exchange_n(p, &ov, nv, 0,
                                        __ATOMIC_RELAXED, __ATOMIC_RELAXED);
    }
    AO_INLINE int
    AO_compare_and_swap_release(volatile AO_t *p, AO_t ov, AO_t nv)
    {
      return (int)__atomic_compare_exchange_n(p, &ov, nv, 0,
                                        __ATOMIC_RELEASE, __ATOMIC_RELAXED);
    }
#define AO_HAVE_compare_and_swap_release
#endif
#ifdef __cplusplus
    }
#endif
#ifndef NO_LOCKFREE_AO_OR
#define HAVE_LOCKFREE_AO_OR 1
#endif
#else
#include "atomic_ops.h"
#if (!defined(AO_HAVE_load) || !defined(AO_HAVE_store)) && !defined(CPPCHECK)
#error AO_load or AO_store is missing; probably old version of atomic_ops
#endif
#endif
#endif
#ifndef AO_HAVE_compiler_barrier
#define AO_HAVE_compiler_barrier 1
#endif
#endif
#if defined(MSWIN32) || defined(MSWINCE) || defined(CYGWIN32)
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN 1
#endif
#define NOSERVICE
#include <windows.h>
#include <winbase.h>
#endif
#ifndef GC_LOCKS_H
#define GC_LOCKS_H
#ifdef THREADS
#ifdef PCR
#include <base/PCR_Base.h>
#include <th/PCR_Th.h>
#endif
   EXTERN_C_BEGIN
#ifdef PCR
     GC_EXTERN PCR_Th_ML GC_allocate_ml;
#if defined(CPPCHECK)
#define DCL_LOCK_STATE
#else
#define DCL_LOCK_STATE \
         PCR_ERes GC_fastLockRes; PCR_sigset_t GC_old_sig_mask
#endif
#define UNCOND_LOCK() PCR_Th_ML_Acquire(&GC_allocate_ml)
#define UNCOND_UNLOCK() PCR_Th_ML_Release(&GC_allocate_ml)
#elif defined(NN_PLATFORM_CTR) || defined(NINTENDO_SWITCH)
      extern void GC_lock(void);
      extern void GC_unlock(void);
#define UNCOND_LOCK() GC_lock()
#define UNCOND_UNLOCK() GC_unlock()
#endif
#if (!defined(AO_HAVE_test_and_set_acquire) || defined(GC_RTEMS_PTHREADS) \
       || defined(SN_TARGET_PS3) \
       || defined(GC_WIN32_THREADS) || defined(BASE_ATOMIC_OPS_EMULATED) \
       || defined(LINT2)) && defined(GC_PTHREADS)
#define USE_PTHREAD_LOCKS
#undef USE_SPIN_LOCK
#if defined(LINT2) && !defined(NO_PTHREAD_TRYLOCK)
#define NO_PTHREAD_TRYLOCK
#endif
#endif
#if defined(GC_WIN32_THREADS) && !defined(USE_PTHREAD_LOCKS)
#define NO_THREAD (DWORD)(-1)
     GC_EXTERN CRITICAL_SECTION GC_allocate_ml;
#ifdef GC_ASSERTIONS
       GC_EXTERN DWORD GC_lock_holder;
#define SET_LOCK_HOLDER() GC_lock_holder = GetCurrentThreadId()
#define UNSET_LOCK_HOLDER() GC_lock_holder = NO_THREAD
#define I_HOLD_LOCK() (!GC_need_to_lock \
                           || GC_lock_holder == GetCurrentThreadId())
#ifdef THREAD_SANITIZER
#define I_DONT_HOLD_LOCK() TRUE
#else
#define I_DONT_HOLD_LOCK() (!GC_need_to_lock \
                           || GC_lock_holder != GetCurrentThreadId())
#endif
#define UNCOND_LOCK() \
                { GC_ASSERT(I_DONT_HOLD_LOCK()); \
                  EnterCriticalSection(&GC_allocate_ml); \
                  SET_LOCK_HOLDER(); }
#define UNCOND_UNLOCK() \
                { GC_ASSERT(I_HOLD_LOCK()); UNSET_LOCK_HOLDER(); \
                  LeaveCriticalSection(&GC_allocate_ml); }
#else
#define UNCOND_LOCK() EnterCriticalSection(&GC_allocate_ml)
#define UNCOND_UNLOCK() LeaveCriticalSection(&GC_allocate_ml)
#endif
#elif defined(GC_PTHREADS)
     EXTERN_C_END
#include <pthread.h>
     EXTERN_C_BEGIN
#if !defined(GC_WIN32_PTHREADS)
#define NUMERIC_THREAD_ID(id) ((unsigned long)(id))
#define THREAD_EQUAL(id1, id2) ((id1) == (id2))
#define NUMERIC_THREAD_ID_UNIQUE
#elif defined(__WINPTHREADS_VERSION_MAJOR)
#define NUMERIC_THREAD_ID(id) ((unsigned long)(id))
#define THREAD_EQUAL(id1, id2) ((id1) == (id2))
#ifndef _WIN64
#define NUMERIC_THREAD_ID_UNIQUE
#endif
#else
#define NUMERIC_THREAD_ID(id) ((unsigned long)(word)(id.p))
#define THREAD_EQUAL(id1, id2) ((id1.p == id2.p) && (id1.x == id2.x))
#undef NUMERIC_THREAD_ID_UNIQUE
#endif
#define NO_THREAD ((unsigned long)(-1l))
#ifdef SN_TARGET_PSP2
       EXTERN_C_END
#include "psp2-support.h"
       EXTERN_C_BEGIN
       GC_EXTERN WapiMutex GC_allocate_ml_PSP2;
#define UNCOND_LOCK() { int res; GC_ASSERT(I_DONT_HOLD_LOCK()); \
                              res = PSP2_MutexLock(&GC_allocate_ml_PSP2); \
                              GC_ASSERT(0 == res); (void)res; \
                              SET_LOCK_HOLDER(); }
#define UNCOND_UNLOCK() { int res; GC_ASSERT(I_HOLD_LOCK()); \
                              UNSET_LOCK_HOLDER(); \
                              res = PSP2_MutexUnlock(&GC_allocate_ml_PSP2); \
                              GC_ASSERT(0 == res); (void)res; }
#elif (!defined(THREAD_LOCAL_ALLOC) || defined(USE_SPIN_LOCK)) \
          && !defined(USE_PTHREAD_LOCKS)
#undef USE_SPIN_LOCK
#define USE_SPIN_LOCK
      GC_EXTERN volatile AO_TS_t GC_allocate_lock;
      GC_INNER void GC_lock(void);
#ifdef GC_ASSERTIONS
#define UNCOND_LOCK() \
              { GC_ASSERT(I_DONT_HOLD_LOCK()); \
                if (AO_test_and_set_acquire(&GC_allocate_lock) == AO_TS_SET) \
                  GC_lock(); \
                SET_LOCK_HOLDER(); }
#define UNCOND_UNLOCK() \
              { GC_ASSERT(I_HOLD_LOCK()); UNSET_LOCK_HOLDER(); \
                AO_CLEAR(&GC_allocate_lock); }
#else
#define UNCOND_LOCK() \
              { if (AO_test_and_set_acquire(&GC_allocate_lock) == AO_TS_SET) \
                  GC_lock(); }
#define UNCOND_UNLOCK() AO_CLEAR(&GC_allocate_lock)
#endif
#else
#ifndef USE_PTHREAD_LOCKS
#define USE_PTHREAD_LOCKS
#endif
#endif
#ifdef USE_PTHREAD_LOCKS
       EXTERN_C_END
#include <pthread.h>
       EXTERN_C_BEGIN
       GC_EXTERN pthread_mutex_t GC_allocate_ml;
#ifdef GC_ASSERTIONS
         GC_INNER void GC_lock(void);
#define UNCOND_LOCK() { GC_ASSERT(I_DONT_HOLD_LOCK()); \
                                GC_lock(); SET_LOCK_HOLDER(); }
#define UNCOND_UNLOCK() \
                { GC_ASSERT(I_HOLD_LOCK()); UNSET_LOCK_HOLDER(); \
                  pthread_mutex_unlock(&GC_allocate_ml); }
#else
#if defined(NO_PTHREAD_TRYLOCK)
#define UNCOND_LOCK() pthread_mutex_lock(&GC_allocate_ml)
#else
           GC_INNER void GC_lock(void);
#define UNCOND_LOCK() \
              { if (0 != pthread_mutex_trylock(&GC_allocate_ml)) \
                  GC_lock(); }
#endif
#define UNCOND_UNLOCK() pthread_mutex_unlock(&GC_allocate_ml)
#endif
#endif
#ifdef GC_ASSERTIONS
       GC_EXTERN unsigned long GC_lock_holder;
#define SET_LOCK_HOLDER() \
                GC_lock_holder = NUMERIC_THREAD_ID(pthread_self())
#define UNSET_LOCK_HOLDER() GC_lock_holder = NO_THREAD
#define I_HOLD_LOCK() \
                (!GC_need_to_lock \
                 || GC_lock_holder == NUMERIC_THREAD_ID(pthread_self()))
#if !defined(NUMERIC_THREAD_ID_UNIQUE) || defined(THREAD_SANITIZER)
#define I_DONT_HOLD_LOCK() TRUE
#else
#define I_DONT_HOLD_LOCK() \
                (!GC_need_to_lock \
                 || GC_lock_holder != NUMERIC_THREAD_ID(pthread_self()))
#endif
#endif
#ifndef GC_WIN32_THREADS
       GC_EXTERN volatile GC_bool GC_collecting;
#ifdef AO_HAVE_char_store
#define ENTER_GC() AO_char_store((unsigned char*)&GC_collecting, TRUE)
#define EXIT_GC() AO_char_store((unsigned char*)&GC_collecting, FALSE)
#else
#define ENTER_GC() (void)(GC_collecting = TRUE)
#define EXIT_GC() (void)(GC_collecting = FALSE)
#endif
#endif
#endif
#if defined(GC_ALWAYS_MULTITHREADED) \
      && (defined(USE_PTHREAD_LOCKS) || defined(USE_SPIN_LOCK))
#define GC_need_to_lock TRUE
#define set_need_to_lock() (void)0
#else
#if defined(GC_ALWAYS_MULTITHREADED) && !defined(CPPCHECK)
#error Runtime initialization of GC lock is needed!
#endif
#undef GC_ALWAYS_MULTITHREADED
     GC_EXTERN GC_bool GC_need_to_lock;
#ifdef THREAD_SANITIZER
#define set_need_to_lock() \
                (void)(*(GC_bool volatile *)&GC_need_to_lock \
                        ? FALSE \
                        : (GC_need_to_lock = TRUE))
#else
#define set_need_to_lock() (void)(GC_need_to_lock = TRUE)
#endif
#endif
   EXTERN_C_END
#else
#define LOCK() (void)0
#define UNLOCK() (void)0
#ifdef GC_ASSERTIONS
#define I_HOLD_LOCK() TRUE
#define I_DONT_HOLD_LOCK() TRUE
#endif
#endif
#if defined(UNCOND_LOCK) && !defined(LOCK)
#if (defined(LINT2) && defined(USE_PTHREAD_LOCKS)) \
     || defined(GC_ALWAYS_MULTITHREADED)
#define LOCK() UNCOND_LOCK()
#define UNLOCK() UNCOND_UNLOCK()
#else
#define LOCK() do { if (GC_need_to_lock) UNCOND_LOCK(); } while (0)
#define UNLOCK() do { if (GC_need_to_lock) UNCOND_UNLOCK(); } while (0)
#endif
#endif
#ifndef ENTER_GC
#define ENTER_GC()
#define EXIT_GC()
#endif
#ifndef DCL_LOCK_STATE
#define DCL_LOCK_STATE
#endif
#endif
#define GC_WORD_MAX (~(word)0)
#ifdef STACK_GROWS_DOWN
#define COOLER_THAN >
#define HOTTER_THAN <
#define MAKE_COOLER(x,y) if ((word)((x) + (y)) > (word)(x)) {(x) += (y);} \
                            else (x) = (ptr_t)GC_WORD_MAX
#define MAKE_HOTTER(x,y) (x) -= (y)
#else
#define COOLER_THAN <
#define HOTTER_THAN >
#define MAKE_COOLER(x,y) if ((word)((x) - (y)) < (word)(x)) {(x) -= (y);} \
                            else (x) = 0
#define MAKE_HOTTER(x,y) (x) += (y)
#endif
#if defined(AMIGA) && defined(__SASC)
#define GC_FAR __far
#else
#define GC_FAR
#endif
EXTERN_C_BEGIN
#ifndef GC_NO_FINALIZATION
#define GC_INVOKE_FINALIZERS() GC_notify_or_invoke_finalizers()
  GC_INNER void GC_notify_or_invoke_finalizers(void);
  GC_INNER void GC_finalize(void);
#ifndef GC_TOGGLE_REFS_NOT_NEEDED
    GC_INNER void GC_process_togglerefs(void);
#endif
#ifndef SMALL_CONFIG
    GC_INNER void GC_print_finalization_stats(void);
#endif
#else
#define GC_INVOKE_FINALIZERS() (void)0
#endif
#if !defined(DONT_ADD_BYTE_AT_END)
#ifdef LINT2
#define EXTRA_BYTES ((size_t)(GC_all_interior_pointers? 1 : 0))
#else
#define EXTRA_BYTES (size_t)GC_all_interior_pointers
#endif
#define MAX_EXTRA_BYTES 1
#else
#define EXTRA_BYTES 0
#define MAX_EXTRA_BYTES 0
#endif
#ifndef LARGE_CONFIG
#define MINHINCR 16
#define MAXHINCR 2048
#else
#define MINHINCR 64
#define MAXHINCR 4096
#endif
#define BL_LIMIT GC_black_list_spacing
#ifdef NEED_CALLINFO
    struct callinfo {
        word ci_pc;
#if NARGS > 0
            word ci_arg[NARGS];
#endif
#if (NFRAMES * (NARGS + 1)) % 2 == 1
            word ci_dummy;
#endif
    };
#endif
#ifdef SAVE_CALL_CHAIN
  GC_INNER void GC_save_callers(struct callinfo info[NFRAMES]);
  GC_INNER void GC_print_callers(struct callinfo info[NFRAMES]);
#endif
EXTERN_C_END
#ifndef NO_CLOCK
#ifdef BSD_TIME
#undef CLOCK_TYPE
#undef GET_TIME
#undef MS_TIME_DIFF
#define CLOCK_TYPE struct timeval
#define CLOCK_TYPE_INITIALIZER { 0, 0 }
#define GET_TIME(x) \
                do { \
                  struct rusage rusage; \
                  getrusage(RUSAGE_SELF, &rusage); \
                  x = rusage.ru_utime; \
                } while (0)
#define MS_TIME_DIFF(a,b) ((unsigned long)((long)(a.tv_sec-b.tv_sec) * 1000 \
                + (long)(a.tv_usec - b.tv_usec) / 1000 \
                - (a.tv_usec < b.tv_usec \
                   && (long)(a.tv_usec - b.tv_usec) % 1000 != 0 ? 1 : 0)))
#define NS_FRAC_TIME_DIFF(a, b) ((unsigned long) \
                ((a.tv_usec < b.tv_usec \
                  && (long)(a.tv_usec - b.tv_usec) % 1000 != 0 ? 1000L : 0) \
                 + (long)(a.tv_usec - b.tv_usec) % 1000) * 1000)
#elif defined(MSWIN32) || defined(MSWINCE) || defined(WINXP_USE_PERF_COUNTER)
#if defined(MSWINRT_FLAVOR) || defined(WINXP_USE_PERF_COUNTER)
#define CLOCK_TYPE ULONGLONG
#define GET_TIME(x) \
                do { \
                  LARGE_INTEGER freq, tc; \
                  if (!QueryPerformanceFrequency(&freq) \
                      || !QueryPerformanceCounter(&tc)) \
                    ABORT("QueryPerformanceCounter requires WinXP+"); \
                  x = (CLOCK_TYPE)((double)tc.QuadPart/freq.QuadPart * 1e9); \
                } while (0)
#define MS_TIME_DIFF(a, b) ((unsigned long)(((a) - (b)) / 1000000UL))
#define NS_FRAC_TIME_DIFF(a, b) ((unsigned long)(((a) - (b)) % 1000000UL))
#else
#define CLOCK_TYPE DWORD
#define GET_TIME(x) (void)(x = GetTickCount())
#define MS_TIME_DIFF(a, b) ((unsigned long)((a) - (b)))
#define NS_FRAC_TIME_DIFF(a, b) 0UL
#endif
#elif defined(NN_PLATFORM_CTR)
#define CLOCK_TYPE long long
  EXTERN_C_BEGIN
  CLOCK_TYPE n3ds_get_system_tick(void);
  CLOCK_TYPE n3ds_convert_tick_to_ms(CLOCK_TYPE tick);
  EXTERN_C_END
#define GET_TIME(x) (void)(x = n3ds_get_system_tick())
#define MS_TIME_DIFF(a,b) ((unsigned long)n3ds_convert_tick_to_ms((a)-(b)))
#define NS_FRAC_TIME_DIFF(a, b) 0UL
#elif defined(NINTENDO_SWITCH) \
      || (((defined(LINUX) && defined(__USE_POSIX199309)) \
           || defined(CYGWIN32)) && defined(_POSIX_TIMERS))
#include <time.h>
#define HAVE_CLOCK_GETTIME 1
#define CLOCK_TYPE struct timespec
#define CLOCK_TYPE_INITIALIZER { 0, 0 }
#if defined(_POSIX_MONOTONIC_CLOCK) && !defined(NINTENDO_SWITCH)
#define GET_TIME(x) \
                do { \
                  if (clock_gettime(CLOCK_MONOTONIC, &x) == -1) \
                    ABORT("clock_gettime failed"); \
                } while (0)
#else
#define GET_TIME(x) \
                do { \
                  if (clock_gettime(CLOCK_REALTIME, &x) == -1) \
                    ABORT("clock_gettime failed"); \
                } while (0)
#endif
#define MS_TIME_DIFF(a, b) \
     \
    ((unsigned long)((a).tv_nsec + (1000000L*1000 - (b).tv_nsec)) / 1000000UL \
     + ((unsigned long)((a).tv_sec - (b).tv_sec) * 1000UL) - 1000UL)
#define NS_FRAC_TIME_DIFF(a, b) \
    ((unsigned long)((a).tv_nsec + (1000000L*1000 - (b).tv_nsec)) % 1000000UL)
#else
#include <time.h>
#if defined(FREEBSD) && !defined(CLOCKS_PER_SEC)
#include <machine/limits.h>
#define CLOCKS_PER_SEC CLK_TCK
#endif
#if !defined(CLOCKS_PER_SEC)
#define CLOCKS_PER_SEC 1000000
#endif
#define CLOCK_TYPE clock_t
#define GET_TIME(x) (void)(x = clock())
#define MS_TIME_DIFF(a,b) (CLOCKS_PER_SEC % 1000 == 0 ? \
        (unsigned long)((a) - (b)) / (unsigned long)(CLOCKS_PER_SEC / 1000) \
        : ((unsigned long)((a) - (b)) * 1000) / (unsigned long)CLOCKS_PER_SEC)
#define NS_FRAC_TIME_DIFF(a, b) (CLOCKS_PER_SEC <= 1000 ? 0UL \
    : (unsigned long)(CLOCKS_PER_SEC <= (clock_t)1000000UL \
        ? (((a) - (b)) * ((clock_t)1000000UL / CLOCKS_PER_SEC) % 1000) * 1000 \
        : (CLOCKS_PER_SEC <= (clock_t)1000000UL * 1000 \
            ? ((a) - (b)) * ((clock_t)1000000UL * 1000 / CLOCKS_PER_SEC) \
            : (((a) - (b)) * (clock_t)1000000UL * 1000) / CLOCKS_PER_SEC) \
          % (clock_t)1000000UL))
#endif
#ifndef CLOCK_TYPE_INITIALIZER
#define CLOCK_TYPE_INITIALIZER 0
#endif
#endif
#if defined(SPARC) && defined(SUNOS4) \
     || (defined(M68K) && defined(NEXT)) || defined(VAX)
#define BCOPY_EXISTS
#elif defined(AMIGA) || defined(DARWIN)
#include <string.h>
#define BCOPY_EXISTS
#elif defined(MACOS) && defined(POWERPC)
#include <MacMemory.h>
#define bcopy(x,y,n) BlockMoveData(x, y, n)
#define bzero(x,n) BlockZero(x, n)
#define BCOPY_EXISTS
#endif
#if !defined(BCOPY_EXISTS) || defined(CPPCHECK)
#include <string.h>
#define BCOPY(x,y,n) memcpy(y, x, (size_t)(n))
#define BZERO(x,n)  memset(x, 0, (size_t)(n))
#else
#define BCOPY(x,y,n) bcopy((void *)(x),(void *)(y),(size_t)(n))
#define BZERO(x,n) bzero((void *)(x),(size_t)(n))
#endif
#ifdef PCR
#include "th/PCR_ThCtl.h"
#endif
EXTERN_C_BEGIN
#ifdef PCR
#define STOP_WORLD() \
        PCR_ThCtl_SetExclusiveMode(PCR_ThCtl_ExclusiveMode_stopNormal, \
                                   PCR_allSigsBlocked, \
                                   PCR_waitForever)
#define START_WORLD() \
        PCR_ThCtl_SetExclusiveMode(PCR_ThCtl_ExclusiveMode_null, \
                                   PCR_allSigsBlocked, \
                                   PCR_waitForever)
#else
#if defined(NN_PLATFORM_CTR) || defined(NINTENDO_SWITCH) \
       || defined(GC_WIN32_THREADS) || defined(GC_PTHREADS)
      GC_INNER void GC_stop_world(void);
      GC_INNER void GC_start_world(void);
#define STOP_WORLD() GC_stop_world()
#define START_WORLD() GC_start_world()
#else
#define STOP_WORLD() GC_ASSERT(GC_blocked_sp == NULL)
#define START_WORLD()
#endif
#endif
#ifdef THREADS
  GC_EXTERN GC_on_thread_event_proc GC_on_thread_event;
#endif
#if defined(SMALL_CONFIG) || defined(PCR)
#define GC_on_abort(msg) (void)0
#else
    GC_API_PRIV GC_abort_func GC_on_abort;
#endif
#if defined(CPPCHECK)
#define ABORT(msg) { GC_on_abort(msg); abort(); }
#elif defined(PCR)
#define ABORT(s) PCR_Base_Panic(s)
#else
#if defined(MSWIN_XBOX1) && !defined(DebugBreak)
#define DebugBreak() __debugbreak()
#elif defined(MSWINCE) && !defined(DebugBreak) \
       && (!defined(UNDER_CE) || (defined(__MINGW32CE__) && !defined(ARM32)))
#define DebugBreak() _exit(-1)
#endif
#if defined(MSWIN32) && (defined(NO_DEBUGGING) || defined(LINT2))
#define ABORT(msg) (GC_on_abort(msg), _exit(-1))
#elif defined(MSWINCE) && defined(NO_DEBUGGING)
#define ABORT(msg) (GC_on_abort(msg), ExitProcess(-1))
#elif defined(MSWIN32) || defined(MSWINCE)
#if defined(_CrtDbgBreak) && defined(_DEBUG) && defined(_MSC_VER)
#define ABORT(msg) { GC_on_abort(msg); \
                            _CrtDbgBreak() ; }
#else
#define ABORT(msg) { GC_on_abort(msg); DebugBreak(); }
#endif
#else
#define ABORT(msg) (GC_on_abort(msg), abort())
#endif
#endif
#define ABORT_ARG1(C_msg, C_fmt, arg1) \
                MACRO_BLKSTMT_BEGIN \
                  GC_ERRINFO_PRINTF(C_msg  C_fmt "\n", arg1); \
                  ABORT(C_msg); \
                MACRO_BLKSTMT_END
#define ABORT_ARG2(C_msg, C_fmt, arg1, arg2) \
                MACRO_BLKSTMT_BEGIN \
                  GC_ERRINFO_PRINTF(C_msg  C_fmt "\n", arg1, arg2); \
                  ABORT(C_msg); \
                MACRO_BLKSTMT_END
#define ABORT_ARG3(C_msg, C_fmt, arg1, arg2, arg3) \
                MACRO_BLKSTMT_BEGIN \
                  GC_ERRINFO_PRINTF(C_msg  C_fmt "\n", \
                                    arg1, arg2, arg3); \
                  ABORT(C_msg); \
                MACRO_BLKSTMT_END
#define ABORT_RET(msg) \
              if ((signed_word)GC_current_warn_proc == -1) {} else ABORT(msg)
#ifdef PCR
#define EXIT() PCR_Base_Exit(1,PCR_waitForever)
#else
#define EXIT() (GC_on_abort(NULL), exit(1 ))
#endif
#define WARN(msg, arg) \
    (*GC_current_warn_proc)(( char *)("GC Warning: " msg), \
                            (word)(arg))
GC_EXTERN GC_warn_proc GC_current_warn_proc;
#ifndef WARN_PRIdPTR
#define WARN_PRIdPTR "ld"
#endif
#define TRUSTED_STRING(s) (char*)COVERT_DATAFLOW(s)
#ifdef GC_READ_ENV_FILE
  GC_INNER char * GC_envfile_getenv(const char *name);
#define GETENV(name) GC_envfile_getenv(name)
#elif defined(NO_GETENV) && !defined(CPPCHECK)
#define GETENV(name) NULL
#elif defined(EMPTY_GETENV_RESULTS)
  GC_INLINE char * fixed_getenv(const char *name)
  {
    char *value = getenv(name);
    return value != NULL && *value != '\0' ? value : NULL;
  }
#define GETENV(name) fixed_getenv(name)
#else
#define GETENV(name) getenv(name)
#endif
EXTERN_C_END
#if defined(DARWIN)
#include <mach/thread_status.h>
#ifndef MAC_OS_X_VERSION_MAX_ALLOWED
#include <AvailabilityMacros.h>
#endif
#if defined(POWERPC)
#if CPP_WORDSZ == 32
#define GC_THREAD_STATE_T          ppc_thread_state_t
#else
#define GC_THREAD_STATE_T          ppc_thread_state64_t
#define GC_MACH_THREAD_STATE       PPC_THREAD_STATE64
#define GC_MACH_THREAD_STATE_COUNT PPC_THREAD_STATE64_COUNT
#endif
#elif defined(I386) || defined(X86_64)
#if CPP_WORDSZ == 32
#if defined(i386_THREAD_STATE_COUNT) && !defined(x86_THREAD_STATE32_COUNT)
#define GC_THREAD_STATE_T                i386_thread_state_t
#define GC_MACH_THREAD_STATE             i386_THREAD_STATE
#define GC_MACH_THREAD_STATE_COUNT       i386_THREAD_STATE_COUNT
#else
#define GC_THREAD_STATE_T                x86_thread_state32_t
#define GC_MACH_THREAD_STATE             x86_THREAD_STATE32
#define GC_MACH_THREAD_STATE_COUNT       x86_THREAD_STATE32_COUNT
#endif
#else
#define GC_THREAD_STATE_T          x86_thread_state64_t
#define GC_MACH_THREAD_STATE       x86_THREAD_STATE64
#define GC_MACH_THREAD_STATE_COUNT x86_THREAD_STATE64_COUNT
#endif
#elif defined(ARM32) && defined(ARM_UNIFIED_THREAD_STATE) \
       && !defined(CPPCHECK)
#define GC_THREAD_STATE_T            arm_unified_thread_state_t
#define GC_MACH_THREAD_STATE         ARM_UNIFIED_THREAD_STATE
#define GC_MACH_THREAD_STATE_COUNT   ARM_UNIFIED_THREAD_STATE_COUNT
#elif defined(ARM32)
#define GC_THREAD_STATE_T            arm_thread_state_t
#ifdef ARM_MACHINE_THREAD_STATE_COUNT
#define GC_MACH_THREAD_STATE       ARM_MACHINE_THREAD_STATE
#define GC_MACH_THREAD_STATE_COUNT ARM_MACHINE_THREAD_STATE_COUNT
#endif
#elif defined(AARCH64)
#define GC_THREAD_STATE_T            arm_thread_state64_t
#define GC_MACH_THREAD_STATE         ARM_THREAD_STATE64
#define GC_MACH_THREAD_STATE_COUNT   ARM_THREAD_STATE64_COUNT
#elif !defined(CPPCHECK)
#error define GC_THREAD_STATE_T
#endif
#ifndef GC_MACH_THREAD_STATE
#define GC_MACH_THREAD_STATE         MACHINE_THREAD_STATE
#define GC_MACH_THREAD_STATE_COUNT   MACHINE_THREAD_STATE_COUNT
#endif
#if CPP_WORDSZ == 32
#define GC_MACH_HEADER   mach_header
#define GC_MACH_SECTION  section
#define GC_GETSECTBYNAME getsectbynamefromheader
#else
#define GC_MACH_HEADER   mach_header_64
#define GC_MACH_SECTION  section_64
#define GC_GETSECTBYNAME getsectbynamefromheader_64
#endif
#if __DARWIN_UNIX03
#define THREAD_FLD_NAME(x) __ ## x
#else
#define THREAD_FLD_NAME(x) x
#endif
#if defined(ARM32) && defined(ARM_UNIFIED_THREAD_STATE)
#define THREAD_FLD(x) ts_32.THREAD_FLD_NAME(x)
#else
#define THREAD_FLD(x) THREAD_FLD_NAME(x)
#endif
#endif
#include <setjmp.h>
#if __STDC_VERSION__ >= 201112L
#include <assert.h>
#endif
EXTERN_C_BEGIN
#if CPP_WORDSZ == 32
#define WORDS_TO_BYTES(x) ((x)<<2)
#define BYTES_TO_WORDS(x) ((x)>>2)
#define LOGWL             ((word)5)
#define modWORDSZ(n) ((n) & 0x1f)
#if ALIGNMENT != 4
#define UNALIGNED_PTRS
#endif
#endif
#if CPP_WORDSZ == 64
#define WORDS_TO_BYTES(x)   ((x)<<3)
#define BYTES_TO_WORDS(x)   ((x)>>3)
#define LOGWL               ((word)6)
#define modWORDSZ(n) ((n) & 0x3f)
#if ALIGNMENT != 8
#define UNALIGNED_PTRS
#endif
#endif
#define GRANULE_BYTES GC_GRANULE_BYTES
#define TINY_FREELISTS GC_TINY_FREELISTS
#define WORDSZ ((word)CPP_WORDSZ)
#define SIGNB  ((word)1 << (WORDSZ-1))
#define BYTES_PER_WORD      ((word)(sizeof (word)))
#define divWORDSZ(n) ((n) >> LOGWL)
#if GRANULE_BYTES == 8
#define BYTES_TO_GRANULES(n) ((n)>>3)
#define GRANULES_TO_BYTES(n) ((n)<<3)
#if CPP_WORDSZ == 64
#define GRANULES_TO_WORDS(n) (n)
#elif CPP_WORDSZ == 32
#define GRANULES_TO_WORDS(n) ((n)<<1)
#else
#define GRANULES_TO_WORDS(n) BYTES_TO_WORDS(GRANULES_TO_BYTES(n))
#endif
#elif GRANULE_BYTES == 16
#define BYTES_TO_GRANULES(n) ((n)>>4)
#define GRANULES_TO_BYTES(n) ((n)<<4)
#if CPP_WORDSZ == 64
#define GRANULES_TO_WORDS(n) ((n)<<1)
#elif CPP_WORDSZ == 32
#define GRANULES_TO_WORDS(n) ((n)<<2)
#else
#define GRANULES_TO_WORDS(n) BYTES_TO_WORDS(GRANULES_TO_BYTES(n))
#endif
#else
#error Bad GRANULE_BYTES value
#endif
#ifndef HBLKSIZE
#if defined(LARGE_CONFIG) || !defined(SMALL_CONFIG)
#ifdef ALPHA
#define CPP_LOG_HBLKSIZE 13
#elif defined(SN_TARGET_PSP2)
#define CPP_LOG_HBLKSIZE 16
#else
#define CPP_LOG_HBLKSIZE 12
#endif
#else
#define CPP_LOG_HBLKSIZE 10
#endif
#else
#if HBLKSIZE == 512
#define CPP_LOG_HBLKSIZE 9
#elif HBLKSIZE == 1024
#define CPP_LOG_HBLKSIZE 10
#elif HBLKSIZE == 2048
#define CPP_LOG_HBLKSIZE 11
#elif HBLKSIZE == 4096
#define CPP_LOG_HBLKSIZE 12
#elif HBLKSIZE == 8192
#define CPP_LOG_HBLKSIZE 13
#elif HBLKSIZE == 16384
#define CPP_LOG_HBLKSIZE 14
#elif !defined(CPPCHECK)
#error Bad HBLKSIZE value
#endif
#undef HBLKSIZE
#endif
#define CPP_HBLKSIZE (1 << CPP_LOG_HBLKSIZE)
#define LOG_HBLKSIZE   ((size_t)CPP_LOG_HBLKSIZE)
#define HBLKSIZE ((size_t)CPP_HBLKSIZE)
#define GC_SQRT_SIZE_MAX ((((size_t)1) << (WORDSZ / 2)) - 1)
#define CPP_MAXOBJBYTES (CPP_HBLKSIZE/2)
#define MAXOBJBYTES ((size_t)CPP_MAXOBJBYTES)
#define CPP_MAXOBJWORDS BYTES_TO_WORDS(CPP_MAXOBJBYTES)
#define MAXOBJWORDS ((size_t)CPP_MAXOBJWORDS)
#define CPP_MAXOBJGRANULES BYTES_TO_GRANULES(CPP_MAXOBJBYTES)
#define MAXOBJGRANULES ((size_t)CPP_MAXOBJGRANULES)
#define divHBLKSZ(n) ((n) >> LOG_HBLKSIZE)
#define HBLK_PTR_DIFF(p,q) divHBLKSZ((ptr_t)p - (ptr_t)q)
#define modHBLKSZ(n) ((n) & (HBLKSIZE-1))
#define HBLKPTR(objptr) ((struct hblk *)(((word)(objptr)) \
                                          & ~(word)(HBLKSIZE-1)))
#define HBLKDISPL(objptr) (((size_t) (objptr)) & (HBLKSIZE-1))
#define ROUNDUP_GRANULE_SIZE(lb)  \
            (SIZET_SAT_ADD(lb, GRANULE_BYTES - 1) & ~(GRANULE_BYTES - 1))
#define ROUNDED_UP_GRANULES(lb)  \
        BYTES_TO_GRANULES(SIZET_SAT_ADD(lb, GRANULE_BYTES - 1 + EXTRA_BYTES))
#if MAX_EXTRA_BYTES == 0
#define SMALL_OBJ(bytes) EXPECT((bytes) <= (MAXOBJBYTES), TRUE)
#else
#define SMALL_OBJ(bytes) \
            (EXPECT((bytes) <= (MAXOBJBYTES - MAX_EXTRA_BYTES), TRUE) \
             || (bytes) <= MAXOBJBYTES - EXTRA_BYTES)
#endif
#define ADD_SLOP(lb)  \
                SIZET_SAT_ADD(lb, EXTRA_BYTES)
#ifndef LOG_PHT_ENTRIES
#ifdef LARGE_CONFIG
#if CPP_WORDSZ == 32
#define LOG_PHT_ENTRIES 20
#else
#define LOG_PHT_ENTRIES 21
#endif
#elif !defined(SMALL_CONFIG)
#define LOG_PHT_ENTRIES  18
#else
#define LOG_PHT_ENTRIES  15
#endif
#endif
#define PHT_ENTRIES ((word)1 << LOG_PHT_ENTRIES)
#define PHT_SIZE (PHT_ENTRIES >> LOGWL)
typedef word page_hash_table[PHT_SIZE];
#define PHT_HASH(addr) ((((word)(addr)) >> LOG_HBLKSIZE) & (PHT_ENTRIES - 1))
#define get_pht_entry_from_index(bl, index) \
                (((bl)[divWORDSZ(index)] >> modWORDSZ(index)) & 1)
#define set_pht_entry_from_index(bl, index) \
                (void)((bl)[divWORDSZ(index)] |= (word)1 << modWORDSZ(index))
#if defined(THREADS) && defined(AO_HAVE_or)
#define set_pht_entry_from_index_concurrent(bl, index) \
                AO_or((volatile AO_t *)&(bl)[divWORDSZ(index)], \
                      (AO_t)((word)1 << modWORDSZ(index)))
#else
#define set_pht_entry_from_index_concurrent(bl, index) \
                set_pht_entry_from_index(bl, index)
#endif
#define HBLKMASK   (HBLKSIZE-1)
#define MARK_BITS_PER_HBLK (HBLKSIZE/GRANULE_BYTES)
union word_ptr_ao_u {
  word w;
  signed_word sw;
  void *vp;
#ifdef PARALLEL_MARK
    volatile AO_t ao;
#endif
};
struct hblkhdr {
    struct hblk * hb_next;
    struct hblk * hb_prev;
    struct hblk * hb_block;
    unsigned char hb_obj_kind;
    unsigned char hb_flags;
#define IGNORE_OFF_PAGE  1
#define WAS_UNMAPPED 2
#define FREE_BLK 4
#ifdef ENABLE_DISCLAIM
#define HAS_DISCLAIM 8
#define MARK_UNCONDITIONALLY 0x10
#endif
#ifdef MARK_BIT_PER_GRANULE
#define LARGE_BLOCK 0x20
#endif
    unsigned short hb_last_reclaimed;
#ifdef MARK_BIT_PER_OBJ
      unsigned32 hb_inv_sz;
#define LARGE_INV_SZ (1 << 16)
#endif
    word hb_sz;
    word hb_descr;
#ifdef MARK_BIT_PER_GRANULE
      unsigned short * hb_map;
#endif
#ifdef PARALLEL_MARK
      volatile AO_t hb_n_marks;
#else
      size_t hb_n_marks;
#endif
#ifdef USE_MARK_BYTES
#define MARK_BITS_SZ (MARK_BITS_PER_HBLK + 1)
      union {
        char _hb_marks[MARK_BITS_SZ];
        word dummy;
      } _mark_byte_union;
#define hb_marks _mark_byte_union._hb_marks
#else
#define MARK_BITS_SZ (MARK_BITS_PER_HBLK/CPP_WORDSZ + 1)
      word hb_marks[MARK_BITS_SZ];
#endif
};
#define ANY_INDEX 23
#define HBLK_WORDS (HBLKSIZE/sizeof(word))
#define HBLK_GRANULES (HBLKSIZE/GRANULE_BYTES)
#define HBLK_OBJS(sz_in_bytes) (HBLKSIZE/(sz_in_bytes))
struct hblk {
    char hb_body[HBLKSIZE];
};
#define HBLK_IS_FREE(hdr) (((hdr) -> hb_flags & FREE_BLK) != 0)
#define OBJ_SZ_TO_BLOCKS(lb) divHBLKSZ((lb) + HBLKSIZE-1)
#define OBJ_SZ_TO_BLOCKS_CHECKED(lb)  \
                                divHBLKSZ(SIZET_SAT_ADD(lb, HBLKSIZE - 1))
#define obj_link(p) (*(void  **)(p))
#define LOG_MAX_MARK_PROCS 6
#define MAX_MARK_PROCS (1 << LOG_MAX_MARK_PROCS)
#ifdef LARGE_CONFIG
#define MAX_ROOT_SETS 8192
#elif !defined(SMALL_CONFIG)
#define MAX_ROOT_SETS 2048
#else
#define MAX_ROOT_SETS 512
#endif
#define MAX_EXCLUSIONS (MAX_ROOT_SETS/4)
struct exclusion {
    ptr_t e_start;
    ptr_t e_end;
};
struct roots {
        ptr_t r_start;
        ptr_t r_end;
#if !defined(MSWIN32) && !defined(MSWINCE) && !defined(CYGWIN32)
          struct roots * r_next;
#endif
        GC_bool r_tmp;
};
#if !defined(MSWIN32) && !defined(MSWINCE) && !defined(CYGWIN32)
#define LOG_RT_SIZE 6
#define RT_SIZE (1 << LOG_RT_SIZE)
#endif
#if !defined(MAX_HEAP_SECTS) && (defined(CYGWIN32) || defined(MSWIN32) \
                    || defined(MSWINCE) || defined(USE_PROC_FOR_LIBRARIES))
#ifdef LARGE_CONFIG
#if CPP_WORDSZ > 32
#define MAX_HEAP_SECTS 81920
#else
#define MAX_HEAP_SECTS 7680
#endif
#elif defined(SMALL_CONFIG) && !defined(USE_PROC_FOR_LIBRARIES)
#if defined(PARALLEL_MARK) && (defined(MSWIN32) || defined(CYGWIN32))
#define MAX_HEAP_SECTS 384
#else
#define MAX_HEAP_SECTS 128
#endif
#elif CPP_WORDSZ > 32
#define MAX_HEAP_SECTS 1024
#else
#define MAX_HEAP_SECTS 512
#endif
#endif
typedef struct GC_ms_entry {
    ptr_t mse_start;
    union word_ptr_ao_u mse_descr;
} mse;
typedef int mark_state_t;
struct disappearing_link;
struct finalizable_object;
struct dl_hashtbl_s {
    struct disappearing_link **head;
    word entries;
    unsigned log_size;
};
struct fnlz_roots_s {
  struct finalizable_object **fo_head;
  struct finalizable_object *finalize_now;
};
union toggle_ref_u {
  void *strong_ref;
  GC_hidden_pointer weak_ref;
};
typedef struct {
    word ed_bitmap;
    GC_bool ed_continued;
} typed_ext_descr_t;
struct HeapSect {
    ptr_t hs_start;
    size_t hs_bytes;
};
struct _GC_arrays {
  word _heapsize;
  word _requested_heapsize;
  ptr_t _last_heap_addr;
  word _large_free_bytes;
  word _large_allocd_bytes;
  word _max_large_allocd_bytes;
  word _bytes_allocd_before_gc;
#define GC_our_mem_bytes GC_arrays._our_mem_bytes
  word _our_mem_bytes;
#ifndef SEPARATE_GLOBALS
#define GC_bytes_allocd GC_arrays._bytes_allocd
    word _bytes_allocd;
#endif
  word _bytes_dropped;
  word _bytes_finalized;
  word _bytes_freed;
  word _finalizer_bytes_freed;
  bottom_index *_all_bottom_indices;
  bottom_index *_all_bottom_indices_end;
  ptr_t _scratch_free_ptr;
  hdr *_hdr_free_list;
  ptr_t _scratch_end_ptr;
#if defined(IRIX5) || (defined(USE_PROC_FOR_LIBRARIES) && !defined(LINUX))
#define USE_SCRATCH_LAST_END_PTR
#define GC_scratch_last_end_ptr GC_arrays._scratch_last_end_ptr
    ptr_t _scratch_last_end_ptr;
#endif
  mse *_mark_stack;
  mse *_mark_stack_limit;
#ifdef PARALLEL_MARK
    mse *volatile _mark_stack_top;
#else
    mse *_mark_stack_top;
#endif
  word _composite_in_use;
  word _atomic_in_use;
#ifdef USE_MUNMAP
#define GC_unmapped_bytes GC_arrays._unmapped_bytes
    word _unmapped_bytes;
#ifdef COUNT_UNMAPPED_REGIONS
#define GC_num_unmapped_regions GC_arrays._num_unmapped_regions
      signed_word _num_unmapped_regions;
#endif
#else
#define GC_unmapped_bytes 0
#endif
  bottom_index * _all_nils;
#define GC_scan_ptr GC_arrays._scan_ptr
  struct hblk * _scan_ptr;
#ifdef PARALLEL_MARK
#define GC_main_local_mark_stack GC_arrays._main_local_mark_stack
    mse *_main_local_mark_stack;
#define GC_first_nonempty GC_arrays._first_nonempty
    volatile AO_t _first_nonempty;
#endif
#define GC_mark_stack_size GC_arrays._mark_stack_size
  size_t _mark_stack_size;
#define GC_mark_state GC_arrays._mark_state
  mark_state_t _mark_state;
#define GC_mark_stack_too_small GC_arrays._mark_stack_too_small
  GC_bool _mark_stack_too_small;
#define GC_objects_are_marked GC_arrays._objects_are_marked
  GC_bool _objects_are_marked;
#ifdef ENABLE_TRACE
#define GC_trace_addr GC_arrays._trace_addr
    ptr_t _trace_addr;
#endif
#define GC_capacity_heap_sects GC_arrays._capacity_heap_sects
  size_t _capacity_heap_sects;
#define GC_n_heap_sects GC_arrays._n_heap_sects
  word _n_heap_sects;
#if defined(MSWIN32) || defined(MSWINCE) || defined(CYGWIN32)
#define GC_n_heap_bases GC_arrays._n_heap_bases
    word _n_heap_bases;
#endif
#ifdef USE_PROC_FOR_LIBRARIES
#define GC_n_memory GC_arrays._n_memory
    word _n_memory;
#endif
#ifdef GC_GCJ_SUPPORT
#define GC_gcjobjfreelist GC_arrays._gcjobjfreelist
    ptr_t *_gcjobjfreelist;
#endif
#define GC_fo_entries GC_arrays._fo_entries
  word _fo_entries;
#ifndef GC_NO_FINALIZATION
#define GC_dl_hashtbl GC_arrays._dl_hashtbl
#define GC_fnlz_roots GC_arrays._fnlz_roots
#define GC_log_fo_table_size GC_arrays._log_fo_table_size
#ifndef GC_LONG_REFS_NOT_NEEDED
#define GC_ll_hashtbl GC_arrays._ll_hashtbl
      struct dl_hashtbl_s _ll_hashtbl;
#endif
    struct dl_hashtbl_s _dl_hashtbl;
    struct fnlz_roots_s _fnlz_roots;
    unsigned _log_fo_table_size;
#ifndef GC_TOGGLE_REFS_NOT_NEEDED
#define GC_toggleref_arr GC_arrays._toggleref_arr
#define GC_toggleref_array_size GC_arrays._toggleref_array_size
#define GC_toggleref_array_capacity GC_arrays._toggleref_array_capacity
      union toggle_ref_u *_toggleref_arr;
      size_t _toggleref_array_size;
      size_t _toggleref_array_capacity;
#endif
#endif
#ifdef TRACE_BUF
#define GC_trace_buf_ptr GC_arrays._trace_buf_ptr
    int _trace_buf_ptr;
#endif
#ifdef ENABLE_DISCLAIM
#define GC_finalized_kind GC_arrays._finalized_kind
    int _finalized_kind;
#endif
#define n_root_sets GC_arrays._n_root_sets
#define GC_excl_table_entries GC_arrays._excl_table_entries
  int _n_root_sets;
  size_t _excl_table_entries;
#ifdef THREADS
#define GC_roots_were_cleared GC_arrays._roots_were_cleared
    GC_bool _roots_were_cleared;
#endif
#define GC_explicit_typing_initialized GC_arrays._explicit_typing_initialized
#define GC_ed_size GC_arrays._ed_size
#define GC_avail_descr GC_arrays._avail_descr
#define GC_ext_descriptors GC_arrays._ext_descriptors
#ifdef AO_HAVE_load_acquire
    volatile AO_t _explicit_typing_initialized;
#else
    GC_bool _explicit_typing_initialized;
#endif
  size_t _ed_size;
  size_t _avail_descr;
  typed_ext_descr_t *_ext_descriptors;
  GC_mark_proc _mark_procs[MAX_MARK_PROCS];
  char _modws_valid_offsets[sizeof(word)];
#if !defined(MSWIN32) && !defined(MSWINCE) && !defined(CYGWIN32)
#define GC_root_index GC_arrays._root_index
    struct roots * _root_index[RT_SIZE];
#endif
#ifdef SAVE_CALL_CHAIN
#define GC_last_stack GC_arrays._last_stack
    struct callinfo _last_stack[NFRAMES];
#endif
#ifndef SEPARATE_GLOBALS
#define GC_objfreelist GC_arrays._objfreelist
    void *_objfreelist[MAXOBJGRANULES+1];
#define GC_aobjfreelist GC_arrays._aobjfreelist
    void *_aobjfreelist[MAXOBJGRANULES+1];
#endif
  void *_uobjfreelist[MAXOBJGRANULES+1];
#ifdef GC_ATOMIC_UNCOLLECTABLE
#define GC_auobjfreelist GC_arrays._auobjfreelist
    void *_auobjfreelist[MAXOBJGRANULES+1];
#endif
  size_t _size_map[MAXOBJBYTES+1];
#ifdef MARK_BIT_PER_GRANULE
#define GC_obj_map GC_arrays._obj_map
    unsigned short * _obj_map[MAXOBJGRANULES + 1];
#define MAP_LEN BYTES_TO_GRANULES(HBLKSIZE)
#endif
#define VALID_OFFSET_SZ HBLKSIZE
  char _valid_offsets[VALID_OFFSET_SZ];
#ifndef GC_DISABLE_INCREMENTAL
#define GC_grungy_pages GC_arrays._grungy_pages
    page_hash_table _grungy_pages;
#define GC_dirty_pages GC_arrays._dirty_pages
    volatile page_hash_table _dirty_pages;
#endif
#if (defined(CHECKSUMS) && (defined(GWW_VDB) || defined(SOFT_VDB))) \
     || defined(PROC_VDB)
#define GC_written_pages GC_arrays._written_pages
    page_hash_table _written_pages;
#endif
#define GC_heap_sects GC_arrays._heap_sects
  struct HeapSect *_heap_sects;
#if defined(USE_PROC_FOR_LIBRARIES)
#define GC_our_memory GC_arrays._our_memory
    struct HeapSect _our_memory[MAX_HEAP_SECTS];
#endif
#if defined(MSWIN32) || defined(MSWINCE) || defined(CYGWIN32)
#define GC_heap_bases GC_arrays._heap_bases
    ptr_t _heap_bases[MAX_HEAP_SECTS];
#endif
#ifdef MSWINCE
#define GC_heap_lengths GC_arrays._heap_lengths
    word _heap_lengths[MAX_HEAP_SECTS];
#endif
  struct roots _static_roots[MAX_ROOT_SETS];
  struct exclusion _excl_table[MAX_EXCLUSIONS];
  bottom_index * _top_index[TOP_SZ];
};
GC_API_PRIV GC_FAR struct _GC_arrays GC_arrays;
#define GC_all_nils GC_arrays._all_nils
#define GC_atomic_in_use GC_arrays._atomic_in_use
#define GC_bytes_allocd_before_gc GC_arrays._bytes_allocd_before_gc
#define GC_bytes_dropped GC_arrays._bytes_dropped
#define GC_bytes_finalized GC_arrays._bytes_finalized
#define GC_bytes_freed GC_arrays._bytes_freed
#define GC_composite_in_use GC_arrays._composite_in_use
#define GC_excl_table GC_arrays._excl_table
#define GC_finalizer_bytes_freed GC_arrays._finalizer_bytes_freed
#define GC_heapsize GC_arrays._heapsize
#define GC_large_allocd_bytes GC_arrays._large_allocd_bytes
#define GC_large_free_bytes GC_arrays._large_free_bytes
#define GC_last_heap_addr GC_arrays._last_heap_addr
#define GC_mark_stack GC_arrays._mark_stack
#define GC_mark_stack_limit GC_arrays._mark_stack_limit
#define GC_mark_stack_top GC_arrays._mark_stack_top
#define GC_mark_procs GC_arrays._mark_procs
#define GC_max_large_allocd_bytes GC_arrays._max_large_allocd_bytes
#define GC_modws_valid_offsets GC_arrays._modws_valid_offsets
#define GC_requested_heapsize GC_arrays._requested_heapsize
#define GC_all_bottom_indices GC_arrays._all_bottom_indices
#define GC_all_bottom_indices_end GC_arrays._all_bottom_indices_end
#define GC_scratch_free_ptr GC_arrays._scratch_free_ptr
#define GC_hdr_free_list GC_arrays._hdr_free_list
#define GC_scratch_end_ptr GC_arrays._scratch_end_ptr
#define GC_size_map GC_arrays._size_map
#define GC_static_roots GC_arrays._static_roots
#define GC_top_index GC_arrays._top_index
#define GC_uobjfreelist GC_arrays._uobjfreelist
#define GC_valid_offsets GC_arrays._valid_offsets
#define beginGC_arrays ((ptr_t)(&GC_arrays))
#define endGC_arrays (((ptr_t)(&GC_arrays)) + (sizeof GC_arrays))
#define USED_HEAP_SIZE (GC_heapsize - GC_large_free_bytes)
#ifndef MAXOBJKINDS
#define MAXOBJKINDS 16
#endif
GC_EXTERN struct obj_kind {
   void **ok_freelist;
   struct hblk **ok_reclaim_list;
   word ok_descriptor;
   GC_bool ok_relocate_descr;
   GC_bool ok_init;
#ifdef ENABLE_DISCLAIM
     GC_bool ok_mark_unconditionally;
     int (GC_CALLBACK *ok_disclaim_proc)(void * );
#define OK_DISCLAIM_INITZ , FALSE, 0
#else
#define OK_DISCLAIM_INITZ
#endif
} GC_obj_kinds[MAXOBJKINDS];
#define beginGC_obj_kinds ((ptr_t)(&GC_obj_kinds))
#define endGC_obj_kinds (beginGC_obj_kinds + (sizeof GC_obj_kinds))
#ifdef SEPARATE_GLOBALS
  extern word GC_bytes_allocd;
  extern ptr_t GC_objfreelist[MAXOBJGRANULES+1];
#define beginGC_objfreelist ((ptr_t)(&GC_objfreelist))
#define endGC_objfreelist (beginGC_objfreelist + sizeof(GC_objfreelist))
  extern ptr_t GC_aobjfreelist[MAXOBJGRANULES+1];
#define beginGC_aobjfreelist ((ptr_t)(&GC_aobjfreelist))
#define endGC_aobjfreelist (beginGC_aobjfreelist + sizeof(GC_aobjfreelist))
#endif
#define PTRFREE 0
#define NORMAL  1
#define UNCOLLECTABLE 2
#ifdef GC_ATOMIC_UNCOLLECTABLE
#define AUNCOLLECTABLE 3
#define IS_UNCOLLECTABLE(k) (((k) & ~1) == UNCOLLECTABLE)
#define GC_N_KINDS_INITIAL_VALUE 4
#else
#define IS_UNCOLLECTABLE(k) ((k) == UNCOLLECTABLE)
#define GC_N_KINDS_INITIAL_VALUE 3
#endif
GC_EXTERN unsigned GC_n_kinds;
GC_EXTERN size_t GC_page_size;
#define ROUNDUP_PAGESIZE(lb)  \
            (SIZET_SAT_ADD(lb, GC_page_size - 1) & ~(GC_page_size - 1))
#ifdef MMAP_SUPPORTED
#define ROUNDUP_PAGESIZE_IF_MMAP(lb) ROUNDUP_PAGESIZE(lb)
#else
#define ROUNDUP_PAGESIZE_IF_MMAP(lb) (lb)
#endif
#if defined(MSWIN32) || defined(MSWINCE) || defined(CYGWIN32)
  GC_EXTERN SYSTEM_INFO GC_sysinfo;
  GC_INNER GC_bool GC_is_heap_base(void *p);
#endif
GC_EXTERN word GC_black_list_spacing;
#ifdef GC_GCJ_SUPPORT
  extern struct hblk * GC_hblkfreelist[];
  extern word GC_free_bytes[];
#endif
GC_EXTERN word GC_root_size;
GC_EXTERN GC_bool GC_debugging_started;
struct blocking_data {
    GC_fn_type fn;
    void * client_data;
};
struct GC_traced_stack_sect_s {
  ptr_t saved_stack_ptr;
#ifdef IA64
    ptr_t saved_backing_store_ptr;
    ptr_t backing_store_end;
#endif
  struct GC_traced_stack_sect_s *prev;
};
#ifdef THREADS
  GC_INNER void GC_push_all_stack_sections(ptr_t lo, ptr_t hi,
                        struct GC_traced_stack_sect_s *traced_stack_sect);
  GC_EXTERN word GC_total_stacksize;
#else
  GC_EXTERN ptr_t GC_blocked_sp;
  GC_EXTERN struct GC_traced_stack_sect_s *GC_traced_stack_sect;
#endif
#ifdef IA64
  GC_INNER void GC_push_all_register_sections(ptr_t bs_lo, ptr_t bs_hi,
                  int eager, struct GC_traced_stack_sect_s *traced_stack_sect);
#endif
#ifdef USE_MARK_BYTES
#define mark_bit_from_hdr(hhdr,n) ((hhdr)->hb_marks[n])
#define set_mark_bit_from_hdr(hhdr,n) ((hhdr)->hb_marks[n] = 1)
#define clear_mark_bit_from_hdr(hhdr,n) ((hhdr)->hb_marks[n] = 0)
#else
#if defined(PARALLEL_MARK) || (defined(THREAD_SANITIZER) && defined(THREADS))
#define OR_WORD(addr, bits) AO_or((volatile AO_t *)(addr), (AO_t)(bits))
#else
#define OR_WORD(addr, bits) (void)(*(addr) |= (bits))
#endif
#define mark_bit_from_hdr(hhdr,n) \
              (((hhdr)->hb_marks[divWORDSZ(n)] >> modWORDSZ(n)) & (word)1)
#define set_mark_bit_from_hdr(hhdr,n) \
              OR_WORD((hhdr)->hb_marks+divWORDSZ(n), (word)1 << modWORDSZ(n))
#define clear_mark_bit_from_hdr(hhdr,n) \
              ((hhdr)->hb_marks[divWORDSZ(n)] &= ~((word)1 << modWORDSZ(n)))
#endif
#ifdef MARK_BIT_PER_OBJ
#define MARK_BIT_NO(offset, sz) (((word)(offset))/(sz))
#define MARK_BIT_OFFSET(sz) 1
#define IF_PER_OBJ(x) x
#define FINAL_MARK_BIT(sz) ((sz) > MAXOBJBYTES? 1 : HBLK_OBJS(sz))
#else
#define MARK_BIT_NO(offset, sz) BYTES_TO_GRANULES((word)(offset))
#define MARK_BIT_OFFSET(sz) BYTES_TO_GRANULES(sz)
#define IF_PER_OBJ(x)
#define FINAL_MARK_BIT(sz) \
                ((sz) > MAXOBJBYTES ? MARK_BITS_PER_HBLK \
                                : BYTES_TO_GRANULES((sz) * HBLK_OBJS(sz)))
#endif
GC_INNER ptr_t GC_approx_sp(void);
GC_INNER GC_bool GC_should_collect(void);
void GC_apply_to_all_blocks(void (*fn)(struct hblk *h, word client_data),
                            word client_data);
GC_INNER struct hblk * GC_next_block(struct hblk *h, GC_bool allow_free);
GC_INNER struct hblk * GC_prev_block(struct hblk * h);
GC_INNER void GC_mark_init(void);
GC_INNER void GC_clear_marks(void);
GC_INNER void GC_invalidate_mark_state(void);
GC_INNER GC_bool GC_mark_some(ptr_t cold_gc_frame);
GC_INNER void GC_initiate_gc(void);
GC_INNER GC_bool GC_collection_in_progress(void);
#define GC_PUSH_ALL_SYM(sym) \
                GC_push_all(( void *)&(sym), \
                            ( void *)(&(sym) + 1))
GC_INNER void GC_push_all_stack(ptr_t b, ptr_t t);
#ifdef NO_VDB_FOR_STATIC_ROOTS
#define GC_push_conditional_static(b, t, all) \
                ((void)(all), GC_push_all(b, t))
#else
  GC_INNER void GC_push_conditional_static(void *b, void *t, GC_bool all);
#endif
#if defined(WRAP_MARK_SOME) && defined(PARALLEL_MARK)
  GC_INNER void GC_push_conditional_eager(void *bottom, void *top,
                                          GC_bool all);
#endif
GC_INNER void GC_push_roots(GC_bool all, ptr_t cold_gc_frame);
GC_API_PRIV GC_push_other_roots_proc GC_push_other_roots;
#ifdef THREADS
  void GC_push_thread_structures(void);
#endif
GC_EXTERN void (*GC_push_typed_structures)(void);
GC_INNER void GC_with_callee_saves_pushed(void (*fn)(ptr_t, void *),
                                          volatile ptr_t arg);
#if defined(SPARC) || defined(IA64)
  ptr_t GC_save_regs_in_stack(void);
#endif
#if defined(AMIGA) || defined(MACOS) || defined(GC_DARWIN_THREADS)
  void GC_push_one(word p);
#endif
#ifdef GC_WIN32_THREADS
  GC_INNER void GC_push_many_regs(const word *regs, unsigned count);
#endif
#if defined(PRINT_BLACK_LIST) || defined(KEEP_BACK_PTRS)
  GC_INNER void GC_mark_and_push_stack(ptr_t p, ptr_t source);
#else
  GC_INNER void GC_mark_and_push_stack(ptr_t p);
#endif
GC_INNER void GC_clear_hdr_marks(hdr * hhdr);
GC_INNER void GC_set_hdr_marks(hdr * hhdr);
GC_INNER void GC_set_fl_marks(ptr_t p);
#if defined(GC_ASSERTIONS) && defined(THREAD_LOCAL_ALLOC)
  void GC_check_fl_marks(void **);
#endif
void GC_add_roots_inner(ptr_t b, ptr_t e, GC_bool tmp);
#ifdef USE_PROC_FOR_LIBRARIES
  GC_INNER void GC_remove_roots_subregion(ptr_t b, ptr_t e);
#endif
GC_INNER void GC_exclude_static_roots_inner(void *start, void *finish);
#if defined(DYNAMIC_LOADING) || defined(MSWIN32) || defined(MSWINCE) \
    || defined(CYGWIN32) || defined(PCR)
  GC_INNER void GC_register_dynamic_libraries(void);
#endif
GC_INNER void GC_cond_register_dynamic_libraries(void);
ptr_t GC_get_main_stack_base(void);
#ifdef IA64
  GC_INNER ptr_t GC_get_register_stack_base(void);
#endif
void GC_register_data_segments(void);
#ifdef THREADS
  GC_INNER void GC_thr_init(void);
  GC_INNER void GC_init_parallel(void);
#else
  GC_INNER GC_bool GC_is_static_root(void *p);
#ifdef TRACE_BUF
    void GC_add_trace_entry(char *kind, word arg1, word arg2);
#endif
#endif
#ifdef PRINT_BLACK_LIST
  GC_INNER void GC_add_to_black_list_normal(word p, ptr_t source);
#define GC_ADD_TO_BLACK_LIST_NORMAL(bits, source) \
                if (GC_all_interior_pointers) { \
                  GC_add_to_black_list_stack((word)(bits), (source)); \
                } else \
                  GC_add_to_black_list_normal((word)(bits), (source))
  GC_INNER void GC_add_to_black_list_stack(word p, ptr_t source);
#define GC_ADD_TO_BLACK_LIST_STACK(bits, source) \
            GC_add_to_black_list_stack((word)(bits), (source))
#else
  GC_INNER void GC_add_to_black_list_normal(word p);
#define GC_ADD_TO_BLACK_LIST_NORMAL(bits, source) \
                if (GC_all_interior_pointers) { \
                  GC_add_to_black_list_stack((word)(bits)); \
                } else \
                  GC_add_to_black_list_normal((word)(bits))
  GC_INNER void GC_add_to_black_list_stack(word p);
#define GC_ADD_TO_BLACK_LIST_STACK(bits, source) \
            GC_add_to_black_list_stack((word)(bits))
#endif
struct hblk * GC_is_black_listed(struct hblk * h, word len);
GC_INNER void GC_promote_black_lists(void);
GC_INNER void GC_unpromote_black_lists(void);
GC_INNER ptr_t GC_scratch_alloc(size_t bytes);
#ifdef GWW_VDB
#else
#define GC_scratch_recycle_no_gww GC_scratch_recycle_inner
#endif
GC_INNER void GC_scratch_recycle_inner(void *ptr, size_t bytes);
#ifdef MARK_BIT_PER_GRANULE
  GC_INNER GC_bool GC_add_map_entry(size_t sz);
#endif
GC_INNER void GC_register_displacement_inner(size_t offset);
GC_INNER void GC_new_hblk(size_t size_in_granules, int kind);
GC_INNER ptr_t GC_build_fl(struct hblk *h, size_t words, GC_bool clear,
                           ptr_t list);
GC_INNER struct hblk * GC_allochblk(size_t size_in_bytes, int kind,
                                    unsigned flags);
GC_INNER ptr_t GC_alloc_large(size_t lb, int k, unsigned flags);
GC_INNER void GC_freehblk(struct hblk * p);
GC_INNER GC_bool GC_expand_hp_inner(word n);
GC_INNER void GC_start_reclaim(GC_bool abort_if_found);
GC_INNER void GC_continue_reclaim(word sz, int kind);
GC_INNER GC_bool GC_reclaim_all(GC_stop_func stop_func, GC_bool ignore_old);
GC_INNER ptr_t GC_reclaim_generic(struct hblk * hbp, hdr *hhdr, size_t sz,
                                  GC_bool init, ptr_t list,
                                  signed_word *count);
GC_INNER GC_bool GC_block_empty(hdr * hhdr);
GC_INNER int GC_CALLBACK GC_never_stop_func(void);
GC_INNER GC_bool GC_try_to_collect_inner(GC_stop_func f);
#define GC_gcollect_inner() \
                (void)GC_try_to_collect_inner(GC_never_stop_func)
#ifdef THREADS
  GC_EXTERN GC_bool GC_in_thread_creation;
#endif
GC_EXTERN GC_bool GC_is_initialized;
GC_INNER void GC_collect_a_little_inner(int n);
GC_INNER void * GC_generic_malloc_inner(size_t lb, int k);
#if defined(DBG_HDRS_ALL) || defined(GC_GCJ_SUPPORT) \
    || !defined(GC_NO_FINALIZATION)
  GC_INNER void * GC_generic_malloc_inner_ignore_off_page(size_t lb, int k);
#endif
GC_INNER GC_bool GC_collect_or_expand(word needed_blocks,
                                      GC_bool ignore_off_page, GC_bool retry);
GC_INNER ptr_t GC_allocobj(size_t sz, int kind);
#ifdef GC_ADD_CALLER
#ifdef GC_HAVE_RETURN_ADDR_PARENT
#define GC_DBG_EXTRAS GC_RETURN_ADDR_PARENT, NULL, 0
#else
#define GC_DBG_EXTRAS GC_RETURN_ADDR, NULL, 0
#endif
#else
#define GC_DBG_EXTRAS "unknown", 0
#endif
#ifdef GC_COLLECT_AT_MALLOC
  extern size_t GC_dbg_collect_at_malloc_min_lb;
#define GC_DBG_COLLECT_AT_MALLOC(lb) \
                (void)((lb) >= GC_dbg_collect_at_malloc_min_lb ? \
                            (GC_gcollect(), 0) : 0)
#else
#define GC_DBG_COLLECT_AT_MALLOC(lb) (void)0
#endif
#if defined(THREAD_LOCAL_ALLOC) && defined(GC_GCJ_SUPPORT)
    GC_INNER void * GC_core_gcj_malloc(size_t, void *);
#endif
GC_INNER void GC_init_headers(void);
GC_INNER struct hblkhdr * GC_install_header(struct hblk *h);
GC_INNER GC_bool GC_install_counts(struct hblk * h, size_t sz);
GC_INNER void GC_remove_header(struct hblk * h);
GC_INNER void GC_remove_counts(struct hblk * h, size_t sz);
GC_INNER hdr * GC_find_header(ptr_t h);
#ifdef USE_PROC_FOR_LIBRARIES
  GC_INNER void GC_add_to_our_memory(ptr_t p, size_t bytes);
#else
#define GC_add_to_our_memory(p, bytes) \
                (GC_our_mem_bytes += (bytes), (void)(p))
#endif
GC_INNER void GC_print_all_errors(void);
GC_EXTERN void (*GC_check_heap)(void);
GC_EXTERN void (*GC_print_all_smashed)(void);
GC_EXTERN void (*GC_print_heap_obj)(ptr_t p);
#if defined(LINUX) && defined(__ELF__) && !defined(SMALL_CONFIG)
  void GC_print_address_map(void);
#endif
#ifndef SHORT_DBG_HDRS
  GC_EXTERN GC_bool GC_findleak_delay_free;
  GC_INNER GC_bool GC_check_leaked(ptr_t base);
#endif
GC_EXTERN GC_bool GC_have_errors;
#define VERBOSE 2
#if !defined(NO_CLOCK) || !defined(SMALL_CONFIG)
  extern int GC_print_stats;
#else
#define GC_print_stats 0
#endif
#ifdef KEEP_BACK_PTRS
  GC_EXTERN long GC_backtraces;
  GC_INNER void GC_generate_random_backtrace_no_gc(void);
#endif
#ifdef LINT2
#define GC_RAND_MAX (~0U >> 1)
  GC_API_PRIV long GC_random(void);
#endif
GC_EXTERN GC_bool GC_print_back_height;
#ifdef MAKE_BACK_GRAPH
  void GC_print_back_graph_stats(void);
#endif
#ifdef THREADS
  GC_INNER void GC_free_inner(void * p);
#endif
#ifdef DBG_HDRS_ALL
  GC_INNER void * GC_debug_generic_malloc_inner(size_t lb, int k);
  GC_INNER void * GC_debug_generic_malloc_inner_ignore_off_page(size_t lb,
                                                                int k);
#define GC_INTERNAL_MALLOC GC_debug_generic_malloc_inner
#define GC_INTERNAL_MALLOC_IGNORE_OFF_PAGE \
               GC_debug_generic_malloc_inner_ignore_off_page
#ifdef THREADS
    GC_INNER void GC_debug_free_inner(void * p);
#define GC_INTERNAL_FREE GC_debug_free_inner
#else
#define GC_INTERNAL_FREE GC_debug_free
#endif
#else
#define GC_INTERNAL_MALLOC GC_generic_malloc_inner
#define GC_INTERNAL_MALLOC_IGNORE_OFF_PAGE \
               GC_generic_malloc_inner_ignore_off_page
#ifdef THREADS
#define GC_INTERNAL_FREE GC_free_inner
#else
#define GC_INTERNAL_FREE GC_free
#endif
#endif
#ifdef USE_MUNMAP
  GC_INNER void GC_unmap_old(void);
  GC_INNER void GC_merge_unmapped(void);
  GC_INNER void GC_unmap(ptr_t start, size_t bytes);
  GC_INNER void GC_remap(ptr_t start, size_t bytes);
  GC_INNER void GC_unmap_gap(ptr_t start1, size_t bytes1, ptr_t start2,
                             size_t bytes2);
  GC_INLINE ptr_t GC_unmap_end(ptr_t start, size_t bytes)
  {
     return (ptr_t)((word)(start + bytes) & ~(GC_page_size - 1));
  }
#endif
#ifdef CAN_HANDLE_FORK
  GC_EXTERN int GC_handle_fork;
#endif
#ifdef GC_DISABLE_INCREMENTAL
#define GC_incremental FALSE
#define GC_auto_incremental FALSE
#define GC_manual_vdb FALSE
#define GC_dirty(p) (void)(p)
#define REACHABLE_AFTER_DIRTY(p) (void)(p)
#else
  GC_EXTERN GC_bool GC_incremental;
  GC_INNER void GC_read_dirty(GC_bool output_unneeded);
  GC_INNER GC_bool GC_page_was_dirty(struct hblk *h);
  GC_INNER void GC_remove_protection(struct hblk *h, word nblocks,
                                   GC_bool pointerfree);
#if !defined(NO_VDB_FOR_STATIC_ROOTS) && !defined(PROC_VDB)
    GC_INNER GC_bool GC_is_vdb_for_static_roots(void);
#endif
#ifdef CAN_HANDLE_FORK
#if defined(PROC_VDB) || defined(SOFT_VDB)
      GC_INNER void GC_dirty_update_child(void);
#else
#define GC_dirty_update_child() (void)0
#endif
#endif
  GC_INNER GC_bool GC_dirty_init(void);
  GC_EXTERN GC_bool GC_manual_vdb;
#define GC_auto_incremental (GC_incremental && !GC_manual_vdb)
  GC_INNER void GC_dirty_inner(const void *p);
#define GC_dirty(p) (GC_manual_vdb ? GC_dirty_inner(p) : (void)0)
#define REACHABLE_AFTER_DIRTY(p) GC_reachable_here(p)
#endif
#define GC_base_C(p) ((const void *)GC_base(( void *)(p)))
void GC_print_block_list(void);
void GC_print_hblkfreelist(void);
void GC_print_heap_sects(void);
void GC_print_static_roots(void);
#ifdef KEEP_BACK_PTRS
   GC_INNER void GC_store_back_pointer(ptr_t source, ptr_t dest);
   GC_INNER void GC_marked_for_finalization(ptr_t dest);
#define GC_STORE_BACK_PTR(source, dest) GC_store_back_pointer(source, dest)
#define GC_MARKED_FOR_FINALIZATION(dest) GC_marked_for_finalization(dest)
#else
#define GC_STORE_BACK_PTR(source, dest) (void)(source)
#define GC_MARKED_FOR_FINALIZATION(dest)
#endif
void GC_noop6(word, word, word, word, word, word);
GC_API void GC_CALL GC_noop1(word);
#ifndef GC_ATTR_FORMAT_PRINTF
#if GC_GNUC_PREREQ(3, 0)
#define GC_ATTR_FORMAT_PRINTF(spec_argnum, first_checked) \
        __attribute__((__format__(__printf__, spec_argnum, first_checked)))
#else
#define GC_ATTR_FORMAT_PRINTF(spec_argnum, first_checked)
#endif
#endif
GC_API_PRIV void GC_printf(const char * format, ...)
                        GC_ATTR_FORMAT_PRINTF(1, 2);
GC_API_PRIV void GC_err_printf(const char * format, ...)
                        GC_ATTR_FORMAT_PRINTF(1, 2);
GC_API_PRIV void GC_log_printf(const char * format, ...)
                        GC_ATTR_FORMAT_PRINTF(1, 2);
#ifndef GC_ANDROID_LOG
#define GC_PRINT_STATS_FLAG (GC_print_stats != 0)
#define GC_INFOLOG_PRINTF GC_COND_LOG_PRINTF
#define GC_verbose_log_printf GC_log_printf
#else
  extern GC_bool GC_quiet;
#define GC_PRINT_STATS_FLAG (!GC_quiet)
#ifndef GC_INFOLOG_PRINTF
#define GC_INFOLOG_PRINTF if (GC_quiet) {} else GC_info_log_printf
#endif
  GC_INNER void GC_info_log_printf(const char *format, ...)
                        GC_ATTR_FORMAT_PRINTF(1, 2);
  GC_INNER void GC_verbose_log_printf(const char *format, ...)
                        GC_ATTR_FORMAT_PRINTF(1, 2);
#endif
#if defined(SMALL_CONFIG) || defined(GC_ANDROID_LOG)
#define GC_ERRINFO_PRINTF GC_INFOLOG_PRINTF
#else
#define GC_ERRINFO_PRINTF GC_log_printf
#endif
#define GC_COND_LOG_PRINTF \
                if (EXPECT(!GC_print_stats, TRUE)) {} else GC_log_printf
#define GC_VERBOSE_LOG_PRINTF \
    if (EXPECT(GC_print_stats != VERBOSE, TRUE)) {} else GC_verbose_log_printf
#ifndef GC_DBGLOG_PRINTF
#define GC_DBGLOG_PRINTF if (!GC_PRINT_STATS_FLAG) {} else GC_log_printf
#endif
void GC_err_puts(const char *s);
#define TO_KiB_UL(v) ((unsigned long)(((v) + ((1 << 9) - 1)) >> 10))
GC_EXTERN unsigned GC_fail_count;
GC_EXTERN long GC_large_alloc_warn_interval;
GC_EXTERN signed_word GC_bytes_found;
#ifndef GC_GET_HEAP_USAGE_NOT_NEEDED
  GC_EXTERN word GC_reclaimed_bytes_before_gc;
#endif
#ifdef USE_MUNMAP
  GC_EXTERN int GC_unmap_threshold;
  GC_EXTERN GC_bool GC_force_unmap_on_gcollect;
#endif
#ifdef MSWIN32
  GC_EXTERN GC_bool GC_no_win32_dlls;
  GC_EXTERN GC_bool GC_wnt;
#endif
#ifdef THREADS
#if (defined(MSWIN32) && !defined(CONSOLE_LOG)) || defined(MSWINCE)
    GC_EXTERN CRITICAL_SECTION GC_write_cs;
#ifdef GC_ASSERTIONS
      GC_EXTERN GC_bool GC_write_disabled;
#endif
#endif
#if defined(GC_DISABLE_INCREMENTAL) || defined(HAVE_LOCKFREE_AO_OR)
#define GC_acquire_dirty_lock() (void)0
#define GC_release_dirty_lock() (void)0
#else
#define GC_acquire_dirty_lock() \
        do {  \
        } while (AO_test_and_set_acquire(&GC_fault_handler_lock) == AO_TS_SET)
#define GC_release_dirty_lock() AO_CLEAR(&GC_fault_handler_lock)
    GC_EXTERN volatile AO_TS_t GC_fault_handler_lock;
#endif
#ifdef MSWINCE
    GC_EXTERN GC_bool GC_dont_query_stack_min;
#endif
#elif defined(IA64)
  GC_EXTERN ptr_t GC_save_regs_ret_val;
#endif
#ifdef THREAD_LOCAL_ALLOC
  GC_EXTERN GC_bool GC_world_stopped;
  GC_INNER void GC_mark_thread_local_free_lists(void);
#endif
#if defined(GLIBC_2_19_TSX_BUG) && defined(THREADS)
  GC_INNER int GC_parse_version(int *pminor, const char *pverstr);
#endif
#if defined(MPROTECT_VDB) && defined(GWW_VDB)
    GC_INNER GC_bool GC_gww_dirty_init(void);
#endif
#if defined(CHECKSUMS) || defined(PROC_VDB)
  GC_INNER GC_bool GC_page_was_ever_dirty(struct hblk * h);
#endif
#ifdef CHECKSUMS
#if defined(MPROTECT_VDB) && !defined(DARWIN)
    void GC_record_fault(struct hblk * h);
#endif
  void GC_check_dirty(void);
#endif
GC_INNER void GC_default_print_heap_obj_proc(ptr_t p);
GC_INNER void GC_setpagesize(void);
GC_INNER void GC_initialize_offsets(void);
GC_INNER void GC_bl_init(void);
GC_INNER void GC_bl_init_no_interiors(void);
GC_INNER void GC_start_debugging_inner(void);
GC_INNER void *GC_store_debug_info_inner(void *p, word sz, const char *str,
                                         int linenum);
#ifdef REDIRECT_MALLOC
#ifdef GC_LINUX_THREADS
    GC_INNER GC_bool GC_text_mapping(char *nm, ptr_t *startp, ptr_t *endp);
#endif
#elif defined(USE_WINALLOC)
  GC_INNER void GC_add_current_malloc_heap(void);
#endif
#ifdef MAKE_BACK_GRAPH
  GC_INNER void GC_build_back_graph(void);
  GC_INNER void GC_traverse_back_graph(void);
#endif
#ifdef MSWIN32
  GC_INNER void GC_init_win32(void);
#endif
#if !defined(MSWIN32) && !defined(MSWINCE) && !defined(CYGWIN32)
  GC_INNER void * GC_roots_present(ptr_t);
#endif
#ifdef GC_WIN32_THREADS
  GC_INNER void GC_get_next_stack(char *start, char * limit, char **lo,
                                  char **hi);
#if defined(MPROTECT_VDB) && !defined(CYGWIN32)
    GC_INNER void GC_set_write_fault_handler(void);
#endif
#if defined(WRAP_MARK_SOME) && !defined(GC_PTHREADS)
    GC_INNER GC_bool GC_started_thread_while_stopped(void);
#endif
#endif
#ifdef THREADS
  GC_INNER void GC_reset_finalizer_nested(void);
  GC_INNER unsigned char *GC_check_finalizer_nested(void);
  GC_INNER void GC_do_blocking_inner(ptr_t data, void * context);
  GC_INNER void GC_push_all_stacks(void);
#ifdef USE_PROC_FOR_LIBRARIES
    GC_INNER GC_bool GC_segment_is_thread_stack(ptr_t lo, ptr_t hi);
#endif
#ifdef IA64
    GC_INNER ptr_t GC_greatest_stack_base_below(ptr_t bound);
#endif
#endif
#ifdef DYNAMIC_LOADING
  GC_INNER GC_bool GC_register_main_static_data(void);
#ifdef DARWIN
    GC_INNER void GC_init_dyld(void);
#endif
#endif
#ifdef SEARCH_FOR_DATA_START
  GC_INNER void GC_init_linux_data_start(void);
  void * GC_find_limit(void *, int);
#endif
#if defined(NETBSD) && defined(__ELF__)
  GC_INNER void GC_init_netbsd_elf(void);
  void * GC_find_limit(void *, int);
#endif
#ifdef UNIX_LIKE
  GC_INNER void GC_set_and_save_fault_handler(void (*handler)(int));
#endif
#ifdef NEED_PROC_MAPS
#if defined(DYNAMIC_LOADING) && defined(USE_PROC_FOR_LIBRARIES)
    GC_INNER const char *GC_parse_map_entry(const char *maps_ptr,
                                            ptr_t *start, ptr_t *end,
                                            const char **prot,
                                            unsigned *maj_dev,
                                            const char **mapping_name);
#endif
#if defined(IA64) || defined(INCLUDE_LINUX_THREAD_DESCR)
    GC_INNER GC_bool GC_enclosing_mapping(ptr_t addr,
                                          ptr_t *startp, ptr_t *endp);
#endif
  GC_INNER const char *GC_get_maps(void);
#endif
#ifdef GC_ASSERTIONS
#define GC_ASSERT(expr) \
              do { \
                if (!(expr)) { \
                  GC_err_printf("Assertion failure: %s:%d\n", \
                                __FILE__, __LINE__); \
                  ABORT("assertion failure"); \
                } \
              } while (0)
  GC_INNER word GC_compute_large_free_bytes(void);
  GC_INNER word GC_compute_root_size(void);
#else
#define GC_ASSERT(expr)
#endif
#if _MSC_VER >= 1700
#define GC_STATIC_ASSERT(expr) \
                static_assert(expr, "static assertion failed: " #expr)
#elif defined(static_assert) && __STDC_VERSION__ >= 201112L
#define GC_STATIC_ASSERT(expr) static_assert(expr, #expr)
#elif defined(mips) && !defined(__GNUC__)
#define GC_STATIC_ASSERT(expr) \
    do { if (0) { char j[(expr)? 1 : -1]; j[0]='\0'; j[0]=j[0]; } } while(0)
#else
#define GC_STATIC_ASSERT(expr) (void)sizeof(char[(expr)? 1 : -1])
#endif
#if GC_GNUC_PREREQ(4, 0)
#define NONNULL_ARG_NOT_NULL(arg) (*(volatile void **)&(arg) != NULL)
#else
#define NONNULL_ARG_NOT_NULL(arg) (NULL != (arg))
#endif
#define COND_DUMP_CHECKS \
          do { \
            GC_ASSERT(GC_compute_large_free_bytes() == GC_large_free_bytes); \
            GC_ASSERT(GC_compute_root_size() == GC_root_size); \
          } while (0)
#ifndef NO_DEBUGGING
  GC_EXTERN GC_bool GC_dump_regularly;
#define COND_DUMP if (EXPECT(GC_dump_regularly, FALSE)) { \
                        GC_dump_named(NULL); \
                   } else COND_DUMP_CHECKS
#else
#define COND_DUMP COND_DUMP_CHECKS
#endif
#if defined(PARALLEL_MARK)
#define GC_markers_m1 GC_parallel
  GC_EXTERN GC_bool GC_parallel_mark_disabled;
  GC_INNER void GC_wait_for_markers_init(void);
  GC_INNER void GC_acquire_mark_lock(void);
  GC_INNER void GC_release_mark_lock(void);
  GC_INNER void GC_notify_all_builder(void);
  GC_INNER void GC_wait_for_reclaim(void);
  GC_EXTERN signed_word GC_fl_builder_count;
  GC_INNER void GC_notify_all_marker(void);
  GC_INNER void GC_wait_marker(void);
  GC_EXTERN word GC_mark_no;
  GC_INNER void GC_help_marker(word my_mark_no);
  GC_INNER void GC_start_mark_threads_inner(void);
#endif
#if defined(GC_PTHREADS) && !defined(GC_WIN32_THREADS) && !defined(NACL) \
    && !defined(GC_DARWIN_THREADS) && !defined(SIG_SUSPEND)
#if (defined(GC_LINUX_THREADS) || defined(GC_DGUX386_THREADS)) \
     && !defined(GC_USESIGRT_SIGNALS)
#if defined(SPARC) && !defined(SIGPWR)
#define SIG_SUSPEND SIGLOST
#else
#define SIG_SUSPEND SIGPWR
#endif
#elif defined(GC_OPENBSD_THREADS)
#ifndef GC_OPENBSD_UTHREADS
#define SIG_SUSPEND SIGXFSZ
#endif
#elif defined(_SIGRTMIN) && !defined(CPPCHECK)
#define SIG_SUSPEND _SIGRTMIN + 6
#else
#define SIG_SUSPEND SIGRTMIN + 6
#endif
#endif
#if defined(GC_PTHREADS) && !defined(GC_SEM_INIT_PSHARED)
#define GC_SEM_INIT_PSHARED 0
#endif
#if (defined(UNIX_LIKE) || (defined(NEED_FIND_LIMIT) && defined(CYGWIN32))) \
    && !defined(GC_NO_SIGSETJMP)
#if defined(SUNOS5SIGS) && !defined(FREEBSD) && !defined(LINUX)
    EXTERN_C_END
#include <sys/siginfo.h>
    EXTERN_C_BEGIN
#endif
#define SETJMP(env) sigsetjmp(env, 1)
#define LONGJMP(env, val) siglongjmp(env, val)
#define JMP_BUF sigjmp_buf
#else
#ifdef ECOS
#define SETJMP(env) hal_setjmp(env)
#else
#define SETJMP(env) setjmp(env)
#endif
#define LONGJMP(env, val) longjmp(env, val)
#define JMP_BUF jmp_buf
#endif
#if defined(HEURISTIC2) || defined(SEARCH_FOR_DATA_START) \
    || ((defined(SVR4) || defined(AIX) || defined(DGUX) \
         || (defined(LINUX) && defined(SPARC))) && !defined(PCR))
#define NEED_FIND_LIMIT
#endif
#if defined(DATASTART_USES_BSDGETDATASTART)
  EXTERN_C_END
#include <machine/trap.h>
  EXTERN_C_BEGIN
#if !defined(PCR)
#define NEED_FIND_LIMIT
#endif
  GC_INNER ptr_t GC_FreeBSDGetDataStart(size_t, ptr_t);
#define DATASTART_IS_FUNC
#endif
#if (defined(NETBSD) || defined(OPENBSD)) && defined(__ELF__) \
    && !defined(NEED_FIND_LIMIT)
#define NEED_FIND_LIMIT
#endif
#if defined(IA64) && !defined(NEED_FIND_LIMIT)
#define NEED_FIND_LIMIT
#endif
#if defined(NEED_FIND_LIMIT) \
     || (defined(USE_PROC_FOR_LIBRARIES) && defined(THREADS))
  GC_EXTERN JMP_BUF GC_jmp_buf;
  GC_INNER void GC_setup_temporary_fault_handler(void);
  GC_INNER void GC_reset_fault_handler(void);
#endif
#if defined(CANCEL_SAFE)
#if defined(GC_ASSERTIONS) \
     && (defined(USE_COMPILER_TLS) \
         || (defined(LINUX) && !defined(ARM32) && GC_GNUC_PREREQ(3, 3) \
             || defined(HPUX) ))
    extern __thread unsigned char GC_cancel_disable_count;
#define NEED_CANCEL_DISABLE_COUNT
#define INCR_CANCEL_DISABLE() ++GC_cancel_disable_count
#define DECR_CANCEL_DISABLE() --GC_cancel_disable_count
#define ASSERT_CANCEL_DISABLED() GC_ASSERT(GC_cancel_disable_count > 0)
#else
#define INCR_CANCEL_DISABLE()
#define DECR_CANCEL_DISABLE()
#define ASSERT_CANCEL_DISABLED() (void)0
#endif
#define DISABLE_CANCEL(state) \
        do { pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, &state); \
          INCR_CANCEL_DISABLE(); } while (0)
#define RESTORE_CANCEL(state) \
        do { ASSERT_CANCEL_DISABLED(); \
          pthread_setcancelstate(state, NULL); \
          DECR_CANCEL_DISABLE(); } while (0)
#else
#define DISABLE_CANCEL(state) (void)0
#define RESTORE_CANCEL(state) (void)0
#define ASSERT_CANCEL_DISABLED() (void)0
#endif
EXTERN_C_END
#endif
#ifdef KEEP_BACK_PTRS
#ifndef GC_BACKPTR_H
#define GC_BACKPTR_H
#ifndef GC_H
#endif
#ifdef __cplusplus
  extern "C" {
#endif
typedef enum {
    GC_UNREFERENCED,
    GC_NO_SPACE,
    GC_REFD_FROM_ROOT,
    GC_REFD_FROM_REG,
    GC_REFD_FROM_HEAP,
    GC_FINALIZER_REFD
} GC_ref_kind;
GC_API GC_ref_kind GC_CALL GC_get_back_ptr_info(void * ,
                                void ** , size_t * )
                                GC_ATTR_NONNULL(1);
GC_API void * GC_CALL GC_generate_random_heap_address(void);
GC_API void * GC_CALL GC_generate_random_valid_address(void);
GC_API void GC_CALL GC_generate_random_backtrace(void);
GC_API void GC_CALL GC_print_backtrace(void *) GC_ATTR_NONNULL(1);
#ifdef __cplusplus
  }
#endif
#endif
#endif
EXTERN_C_BEGIN
#if CPP_WORDSZ == 32
#define START_FLAG (word)0xfedcedcb
#define END_FLAG (word)0xbcdecdef
#else
#define START_FLAG GC_WORD_C(0xFEDCEDCBfedcedcb)
#define END_FLAG GC_WORD_C(0xBCDECDEFbcdecdef)
#endif
#if defined(KEEP_BACK_PTRS) || defined(PRINT_BLACK_LIST) \
    || defined(MAKE_BACK_GRAPH)
#define NOT_MARKED (ptr_t)0
#define MARKED_FOR_FINALIZATION ((ptr_t)(word)2)
#define MARKED_FROM_REGISTER ((ptr_t)(word)4)
#endif
typedef struct {
#if defined(KEEP_BACK_PTRS) || defined(MAKE_BACK_GRAPH)
#if ALIGNMENT == 1
#define HIDE_BACK_PTR(p) GC_HIDE_POINTER(~1 & (word)(p))
#else
#define HIDE_BACK_PTR(p) GC_HIDE_POINTER(p)
#endif
#ifdef KEEP_BACK_PTRS
      GC_hidden_pointer oh_back_ptr;
#endif
#ifdef MAKE_BACK_GRAPH
      GC_hidden_pointer oh_bg_ptr;
#endif
#if defined(KEEP_BACK_PTRS) != defined(MAKE_BACK_GRAPH)
      word oh_dummy;
#endif
#endif
  const char * oh_string;
  signed_word oh_int;
#ifdef NEED_CALLINFO
    struct callinfo oh_ci[NFRAMES];
#endif
#ifndef SHORT_DBG_HDRS
    word oh_sz;
    word oh_sf;
#endif
} oh;
#ifdef SHORT_DBG_HDRS
#define DEBUG_BYTES (sizeof (oh))
#define UNCOLLECTABLE_DEBUG_BYTES DEBUG_BYTES
#else
#define UNCOLLECTABLE_DEBUG_BYTES (sizeof (oh) + sizeof (word))
#define DEBUG_BYTES (UNCOLLECTABLE_DEBUG_BYTES - EXTRA_BYTES)
#endif
#define SIMPLE_ROUNDED_UP_WORDS(n) BYTES_TO_WORDS((n) + WORDS_TO_BYTES(1) - 1)
#if defined(SAVE_CALL_CHAIN)
  struct callinfo;
  GC_INNER void GC_save_callers(struct callinfo info[NFRAMES]);
  GC_INNER void GC_print_callers(struct callinfo info[NFRAMES]);
#define ADD_CALL_CHAIN(base, ra) GC_save_callers(((oh *)(base)) -> oh_ci)
#define PRINT_CALL_CHAIN(base) GC_print_callers(((oh *)(base)) -> oh_ci)
#elif defined(GC_ADD_CALLER)
  struct callinfo;
  GC_INNER void GC_print_callers(struct callinfo info[NFRAMES]);
#define ADD_CALL_CHAIN(base, ra) ((oh *)(base)) -> oh_ci[0].ci_pc = (ra)
#define PRINT_CALL_CHAIN(base) GC_print_callers(((oh *)(base)) -> oh_ci)
#else
#define ADD_CALL_CHAIN(base, ra)
#define PRINT_CALL_CHAIN(base)
#endif
#ifdef GC_ADD_CALLER
#define OPT_RA ra,
#else
#define OPT_RA
#endif
#ifdef SHORT_DBG_HDRS
#define GC_has_other_debug_info(p) 1
#else
  GC_INNER int GC_has_other_debug_info(ptr_t p);
#endif
#if defined(KEEP_BACK_PTRS) || defined(MAKE_BACK_GRAPH)
#if defined(SHORT_DBG_HDRS) && !defined(CPPCHECK)
#error Non-ptr stored in object results in GC_HAS_DEBUG_INFO malfunction
#endif
#if defined(PARALLEL_MARK) && defined(KEEP_BACK_PTRS)
#define GC_HAS_DEBUG_INFO(p) \
                ((AO_load((volatile AO_t *)(p)) & 1) != 0 \
                 && GC_has_other_debug_info(p) > 0)
#else
#define GC_HAS_DEBUG_INFO(p) \
                ((*(word *)(p) & 1) && GC_has_other_debug_info(p) > 0)
#endif
#else
#define GC_HAS_DEBUG_INFO(p) (GC_has_other_debug_info(p) > 0)
#endif
EXTERN_C_END
#endif
#ifdef MAKE_BACK_GRAPH
#define MAX_IN  10
#if (!defined(DBG_HDRS_ALL) || (ALIGNMENT != CPP_WORDSZ/8) \
     ) && !defined(CPPCHECK)
#error The configuration does not support MAKE_BACK_GRAPH
#endif
#define FLAG_MANY 2
typedef struct back_edges_struct {
  word n_edges;
  unsigned short flags;
#define RETAIN 1
  unsigned short height_gc_no;
  signed_word height;
#define HEIGHT_UNKNOWN      (-2)
#define HEIGHT_IN_PROGRESS  (-1)
  ptr_t edges[MAX_IN];
  struct back_edges_struct *cont;
} back_edges;
#define MAX_BACK_EDGE_STRUCTS 100000
static back_edges *back_edge_space = 0;
STATIC int GC_n_back_edge_structs = 0;
static back_edges *avail_back_edges = 0;
static back_edges * new_back_edges(void)
{
  if (0 == back_edge_space) {
    size_t bytes_to_get = ROUNDUP_PAGESIZE_IF_MMAP(MAX_BACK_EDGE_STRUCTS
                                                   * sizeof(back_edges));
    GC_ASSERT(GC_page_size != 0);
    back_edge_space = (back_edges *)GET_MEM(bytes_to_get);
    if (NULL == back_edge_space)
      ABORT("Insufficient memory for back edges");
    GC_add_to_our_memory((ptr_t)back_edge_space, bytes_to_get);
  }
  if (0 != avail_back_edges) {
    back_edges * result = avail_back_edges;
    avail_back_edges = result -> cont;
    result -> cont = 0;
    return result;
  }
  if (GC_n_back_edge_structs >= MAX_BACK_EDGE_STRUCTS - 1) {
    ABORT("Needed too much space for back edges: adjust "
          "MAX_BACK_EDGE_STRUCTS");
  }
  return back_edge_space + (GC_n_back_edge_structs++);
}
static void deallocate_back_edges(back_edges *p)
{
   back_edges *last = p;
   while (0 != last -> cont) last = last -> cont;
   last -> cont = avail_back_edges;
   avail_back_edges = p;
}
#define INITIAL_IN_PROGRESS 10000
static ptr_t * in_progress_space = 0;
static size_t in_progress_size = 0;
static size_t n_in_progress = 0;
static void push_in_progress(ptr_t p)
{
  if (n_in_progress >= in_progress_size) {
    ptr_t * new_in_progress_space;
    GC_ASSERT(GC_page_size != 0);
    if (NULL == in_progress_space) {
      in_progress_size = ROUNDUP_PAGESIZE_IF_MMAP(INITIAL_IN_PROGRESS
                                                        * sizeof(ptr_t))
                                / sizeof(ptr_t);
      new_in_progress_space =
                        (ptr_t *)GET_MEM(in_progress_size * sizeof(ptr_t));
    } else {
      in_progress_size *= 2;
      new_in_progress_space = (ptr_t *)
                                GET_MEM(in_progress_size * sizeof(ptr_t));
      if (new_in_progress_space != NULL)
        BCOPY(in_progress_space, new_in_progress_space,
              n_in_progress * sizeof(ptr_t));
    }
    if (EXPECT(new_in_progress_space != NULL, TRUE))
      GC_add_to_our_memory((ptr_t)new_in_progress_space,
                           in_progress_size * sizeof(ptr_t));
#ifndef GWW_VDB
      GC_scratch_recycle_no_gww(in_progress_space,
                                n_in_progress * sizeof(ptr_t));
#elif defined(LINT2)
      GC_noop1((word)in_progress_space);
#endif
    in_progress_space = new_in_progress_space;
  }
  if (in_progress_space == 0)
      ABORT("MAKE_BACK_GRAPH: Out of in-progress space: "
            "Huge linear data structure?");
  in_progress_space[n_in_progress++] = p;
}
static GC_bool is_in_progress(ptr_t p)
{
  size_t i;
  for (i = 0; i < n_in_progress; ++i) {
    if (in_progress_space[i] == p) return TRUE;
  }
  return FALSE;
}
GC_INLINE void pop_in_progress(ptr_t p GC_ATTR_UNUSED)
{
  --n_in_progress;
  GC_ASSERT(in_progress_space[n_in_progress] == p);
}
#define GET_OH_BG_PTR(p) \
                (ptr_t)GC_REVEAL_POINTER(((oh *)(p)) -> oh_bg_ptr)
#define SET_OH_BG_PTR(p,q) (((oh *)(p)) -> oh_bg_ptr = GC_HIDE_POINTER(q))
static void ensure_struct(ptr_t p)
{
  ptr_t old_back_ptr = GET_OH_BG_PTR(p);
  if (!((word)old_back_ptr & FLAG_MANY)) {
    back_edges *be = new_back_edges();
    be -> flags = 0;
    if (0 == old_back_ptr) {
      be -> n_edges = 0;
    } else {
      be -> n_edges = 1;
      be -> edges[0] = old_back_ptr;
    }
    be -> height = HEIGHT_UNKNOWN;
    be -> height_gc_no = (unsigned short)(GC_gc_no - 1);
    GC_ASSERT((word)be >= (word)back_edge_space);
    SET_OH_BG_PTR(p, (word)be | FLAG_MANY);
  }
}
static void add_edge(ptr_t p, ptr_t q)
{
    ptr_t pred = GET_OH_BG_PTR(q);
    back_edges * be, *be_cont;
    word i;
    GC_ASSERT(p == GC_base(p) && q == GC_base(q));
    if (!GC_HAS_DEBUG_INFO(q) || !GC_HAS_DEBUG_INFO(p)) {
      return;
    }
    if (NULL == pred) {
      static unsigned random_number = 13;
#define GOT_LUCKY_NUMBER (((++random_number) & 0x7f) == 0)
        SET_OH_BG_PTR(q, p);
        if (GOT_LUCKY_NUMBER) ensure_struct(q);
        return;
    }
    {
      back_edges *e = (back_edges *)((word)pred & ~FLAG_MANY);
      word n_edges;
      word total;
      int local = 0;
      if (((word)pred & FLAG_MANY) != 0) {
        n_edges = e -> n_edges;
      } else if (((word)COVERT_DATAFLOW(pred) & 1) == 0) {
        n_edges = 1;
        local = -1;
      } else {
        n_edges = 0;
      }
      for (total = 0; total < n_edges; ++total) {
        if (local == MAX_IN) {
          e = e -> cont;
          local = 0;
        }
        if (local >= 0)
          pred = e -> edges[local++];
        if (pred == p)
          return;
      }
    }
    ensure_struct(q);
    be = (back_edges *)((word)GET_OH_BG_PTR(q) & ~FLAG_MANY);
    for (i = be -> n_edges, be_cont = be; i > MAX_IN; i -= MAX_IN)
        be_cont = be_cont -> cont;
    if (i == MAX_IN) {
        be_cont -> cont = new_back_edges();
        be_cont = be_cont -> cont;
        i = 0;
    }
    be_cont -> edges[i] = p;
    be -> n_edges++;
#ifdef DEBUG_PRINT_BIG_N_EDGES
      if (GC_print_stats == VERBOSE && be -> n_edges == 100) {
        GC_err_printf("The following object has big in-degree:\n");
        GC_print_heap_obj(q);
      }
#endif
}
typedef void (*per_object_func)(ptr_t p, size_t n_bytes, word gc_descr);
static void per_object_helper(struct hblk *h, word fn)
{
  hdr * hhdr = HDR(h);
  size_t sz = (size_t)hhdr->hb_sz;
  word descr = hhdr -> hb_descr;
  per_object_func f = (per_object_func)fn;
  size_t i = 0;
  do {
    f((ptr_t)(h -> hb_body + i), sz, descr);
    i += sz;
  } while (i + sz <= BYTES_TO_WORDS(HBLKSIZE));
}
GC_INLINE void GC_apply_to_each_object(per_object_func f)
{
  GC_apply_to_all_blocks(per_object_helper, (word)f);
}
static void reset_back_edge(ptr_t p, size_t n_bytes GC_ATTR_UNUSED,
                            word gc_descr GC_ATTR_UNUSED)
{
  if (GC_HAS_DEBUG_INFO(p)) {
    ptr_t old_back_ptr = GET_OH_BG_PTR(p);
    if ((word)old_back_ptr & FLAG_MANY) {
      back_edges *be = (back_edges *)((word)old_back_ptr & ~FLAG_MANY);
      if (!(be -> flags & RETAIN)) {
        deallocate_back_edges(be);
        SET_OH_BG_PTR(p, 0);
      } else {
        GC_ASSERT(GC_is_marked(p));
          be -> n_edges = 0;
          if (0 != be -> cont) {
            deallocate_back_edges(be -> cont);
            be -> cont = 0;
          }
        GC_ASSERT(GC_is_marked(p));
          be -> flags &= ~RETAIN;
      }
    } else  {
      SET_OH_BG_PTR(p, 0);
    }
  }
}
static void add_back_edges(ptr_t p, size_t n_bytes, word gc_descr)
{
  word *currentp = (word *)(p + sizeof(oh));
    if((gc_descr & GC_DS_TAGS) != GC_DS_LENGTH) {
      gc_descr = n_bytes;
    }
  while ((word)currentp < (word)(p + gc_descr)) {
    word current = *currentp++;
    FIXUP_POINTER(current);
    if (current >= (word)GC_least_plausible_heap_addr &&
        current <= (word)GC_greatest_plausible_heap_addr) {
       ptr_t target = (ptr_t)GC_base((void *)current);
       if (0 != target) {
         add_edge(p, target);
       }
    }
  }
}
GC_INNER void GC_build_back_graph(void)
{
  GC_ASSERT(I_HOLD_LOCK());
  GC_apply_to_each_object(add_back_edges);
}
static word backwards_height(ptr_t p)
{
  word result;
  ptr_t pred = GET_OH_BG_PTR(p);
  back_edges *be;
  if (NULL == pred)
    return 1;
  if (((word)pred & FLAG_MANY) == 0) {
    if (is_in_progress(p)) return 0;
    push_in_progress(p);
    result = backwards_height(pred) + 1;
    pop_in_progress(p);
    return result;
  }
  be = (back_edges *)((word)pred & ~FLAG_MANY);
  if (be -> height >= 0 && be -> height_gc_no == (unsigned short)GC_gc_no)
      return be -> height;
    if (be -> height == HEIGHT_IN_PROGRESS) return 0;
  result = (be -> height > 0? be -> height : 1);
  be -> height = HEIGHT_IN_PROGRESS;
  {
      back_edges *e = be;
      word n_edges;
      word total;
      int local = 0;
      if (((word)pred & FLAG_MANY) != 0) {
        n_edges = e -> n_edges;
      } else if (((word)pred & 1) == 0) {
        n_edges = 1;
        local = -1;
      } else {
        n_edges = 0;
      }
      for (total = 0; total < n_edges; ++total) {
        word this_height;
        if (local == MAX_IN) {
          e = e -> cont;
          local = 0;
        }
        if (local >= 0)
          pred = e -> edges[local++];
        if (GC_is_marked(pred) && ((word)GET_OH_BG_PTR(p) & FLAG_MANY) == 0) {
          GC_COND_LOG_PRINTF("Found bogus pointer from %p to %p\n",
                             (void *)pred, (void *)p);
          this_height = 1;
        } else {
          this_height = backwards_height(pred);
        }
        if (this_height >= result)
          result = this_height + 1;
      }
  }
  be -> height = result;
  be -> height_gc_no = (unsigned short)GC_gc_no;
  return result;
}
STATIC word GC_max_height = 0;
STATIC ptr_t GC_deepest_obj = NULL;
static void update_max_height(ptr_t p, size_t n_bytes GC_ATTR_UNUSED,
                              word gc_descr GC_ATTR_UNUSED)
{
  if (GC_is_marked(p) && GC_HAS_DEBUG_INFO(p)) {
    word p_height = 0;
    ptr_t p_deepest_obj = 0;
    ptr_t back_ptr;
    back_edges *be = 0;
    back_ptr = GET_OH_BG_PTR(p);
    if (0 != back_ptr && ((word)back_ptr & FLAG_MANY)) {
      be = (back_edges *)((word)back_ptr & ~FLAG_MANY);
      if (be -> height != HEIGHT_UNKNOWN) p_height = be -> height;
    }
    {
      ptr_t pred = GET_OH_BG_PTR(p);
      back_edges *e = (back_edges *)((word)pred & ~FLAG_MANY);
      word n_edges;
      word total;
      int local = 0;
      if (((word)pred & FLAG_MANY) != 0) {
        n_edges = e -> n_edges;
      } else if (pred != NULL && ((word)pred & 1) == 0) {
        n_edges = 1;
        local = -1;
      } else {
        n_edges = 0;
      }
      for (total = 0; total < n_edges; ++total) {
        if (local == MAX_IN) {
          e = e -> cont;
          local = 0;
        }
        if (local >= 0)
          pred = e -> edges[local++];
        if (!GC_is_marked(pred) && GC_HAS_DEBUG_INFO(pred)) {
          word this_height = backwards_height(pred);
          if (this_height > p_height) {
            p_height = this_height;
            p_deepest_obj = pred;
          }
        }
      }
    }
    if (p_height > 0) {
        if (be == 0) {
          ensure_struct(p);
          back_ptr = GET_OH_BG_PTR(p);
          be = (back_edges *)((word)back_ptr & ~FLAG_MANY);
        }
        be -> flags |= RETAIN;
        be -> height = p_height;
        be -> height_gc_no = (unsigned short)GC_gc_no;
    }
    if (p_height > GC_max_height) {
        GC_max_height = p_height;
        GC_deepest_obj = p_deepest_obj;
    }
  }
}
STATIC word GC_max_max_height = 0;
GC_INNER void GC_traverse_back_graph(void)
{
  GC_ASSERT(I_HOLD_LOCK());
  GC_max_height = 0;
  GC_apply_to_each_object(update_max_height);
  if (0 != GC_deepest_obj)
    GC_set_mark_bit(GC_deepest_obj);
}
void GC_print_back_graph_stats(void)
{
  GC_ASSERT(I_HOLD_LOCK());
  GC_printf("Maximum backwards height of reachable objects"
            " at GC #%lu is %lu\n",
            (unsigned long)GC_gc_no, (unsigned long)GC_max_height);
  if (GC_max_height > GC_max_max_height) {
    ptr_t obj = GC_deepest_obj;
    GC_max_max_height = GC_max_height;
    UNLOCK();
    GC_err_printf(
            "The following unreachable object is last in a longest chain "
            "of unreachable objects:\n");
    GC_print_heap_obj(obj);
    LOCK();
  }
  GC_COND_LOG_PRINTF("Needed max total of %d back-edge structs\n",
                     GC_n_back_edge_structs);
  GC_apply_to_each_object(reset_back_edge);
  GC_deepest_obj = 0;
}
#endif
STATIC word * GC_old_normal_bl = NULL;
STATIC word * GC_incomplete_normal_bl = NULL;
STATIC word * GC_old_stack_bl = NULL;
STATIC word * GC_incomplete_stack_bl = NULL;
STATIC word GC_total_stack_black_listed = 0;
GC_INNER word GC_black_list_spacing = MINHINCR * HBLKSIZE;
STATIC void GC_clear_bl(word *);
GC_INNER void GC_default_print_heap_obj_proc(ptr_t p)
{
    ptr_t base = (ptr_t)GC_base(p);
    int kind = HDR(base)->hb_obj_kind;
    GC_err_printf("object at %p of appr. %lu bytes (%s)\n",
                  (void *)base, (unsigned long)GC_size(base),
                  kind == PTRFREE ? "atomic" :
                    IS_UNCOLLECTABLE(kind) ? "uncollectable" : "composite");
}
GC_INNER void (*GC_print_heap_obj)(ptr_t p) = GC_default_print_heap_obj_proc;
#ifdef PRINT_BLACK_LIST
  STATIC void GC_print_blacklisted_ptr(word p, ptr_t source,
                                       const char *kind_str)
  {
    ptr_t base = (ptr_t)GC_base(source);
    if (0 == base) {
        GC_err_printf("Black listing (%s) %p referenced from %p in %s\n",
                      kind_str, (void *)p, (void *)source,
                      NULL != source ? "root set" : "register");
    } else {
        GC_err_printf("Black listing (%s) %p referenced from %p in"
                      " object at %p of appr. %lu bytes\n",
                      kind_str, (void *)p, (void *)source,
                      (void *)base, (unsigned long)GC_size(base));
    }
  }
#endif
GC_INNER void GC_bl_init_no_interiors(void)
{
  if (GC_incomplete_normal_bl == 0) {
    GC_old_normal_bl = (word *)GC_scratch_alloc(sizeof(page_hash_table));
    GC_incomplete_normal_bl = (word *)GC_scratch_alloc(
                                                  sizeof(page_hash_table));
    if (GC_old_normal_bl == 0 || GC_incomplete_normal_bl == 0) {
      GC_err_printf("Insufficient memory for black list\n");
      EXIT();
    }
    GC_clear_bl(GC_old_normal_bl);
    GC_clear_bl(GC_incomplete_normal_bl);
  }
}
GC_INNER void GC_bl_init(void)
{
    if (!GC_all_interior_pointers) {
      GC_bl_init_no_interiors();
    }
    GC_ASSERT(NULL == GC_old_stack_bl && NULL == GC_incomplete_stack_bl);
    GC_old_stack_bl = (word *)GC_scratch_alloc(sizeof(page_hash_table));
    GC_incomplete_stack_bl = (word *)GC_scratch_alloc(sizeof(page_hash_table));
    if (GC_old_stack_bl == 0 || GC_incomplete_stack_bl == 0) {
        GC_err_printf("Insufficient memory for black list\n");
        EXIT();
    }
    GC_clear_bl(GC_old_stack_bl);
    GC_clear_bl(GC_incomplete_stack_bl);
}
STATIC void GC_clear_bl(word *doomed)
{
    BZERO(doomed, sizeof(page_hash_table));
}
STATIC void GC_copy_bl(word *old, word *dest)
{
    BCOPY(old, dest, sizeof(page_hash_table));
}
static word total_stack_black_listed(void);
GC_INNER void GC_promote_black_lists(void)
{
    word * very_old_normal_bl = GC_old_normal_bl;
    word * very_old_stack_bl = GC_old_stack_bl;
    GC_old_normal_bl = GC_incomplete_normal_bl;
    GC_old_stack_bl = GC_incomplete_stack_bl;
    if (!GC_all_interior_pointers) {
      GC_clear_bl(very_old_normal_bl);
    }
    GC_clear_bl(very_old_stack_bl);
    GC_incomplete_normal_bl = very_old_normal_bl;
    GC_incomplete_stack_bl = very_old_stack_bl;
    GC_total_stack_black_listed = total_stack_black_listed();
    GC_VERBOSE_LOG_PRINTF(
                "%lu bytes in heap blacklisted for interior pointers\n",
                (unsigned long)GC_total_stack_black_listed);
    if (GC_total_stack_black_listed != 0) {
        GC_black_list_spacing =
                HBLKSIZE*(GC_heapsize/GC_total_stack_black_listed);
    }
    if (GC_black_list_spacing < 3 * HBLKSIZE) {
        GC_black_list_spacing = 3 * HBLKSIZE;
    }
    if (GC_black_list_spacing > MAXHINCR * HBLKSIZE) {
        GC_black_list_spacing = MAXHINCR * HBLKSIZE;
    }
}
GC_INNER void GC_unpromote_black_lists(void)
{
    if (!GC_all_interior_pointers) {
      GC_copy_bl(GC_old_normal_bl, GC_incomplete_normal_bl);
    }
    GC_copy_bl(GC_old_stack_bl, GC_incomplete_stack_bl);
}
#if defined(PARALLEL_MARK) && defined(THREAD_SANITIZER)
#define backlist_set_pht_entry_from_index(db, index) \
                        set_pht_entry_from_index_concurrent(db, index)
#else
#define backlist_set_pht_entry_from_index(bl, index) \
                        set_pht_entry_from_index(bl, index)
#endif
#ifdef PRINT_BLACK_LIST
  GC_INNER void GC_add_to_black_list_normal(word p, ptr_t source)
#else
  GC_INNER void GC_add_to_black_list_normal(word p)
#endif
{
  if (GC_modws_valid_offsets[p & (sizeof(word)-1)]) {
    word index = PHT_HASH((word)p);
    if (HDR(p) == 0 || get_pht_entry_from_index(GC_old_normal_bl, index)) {
#ifdef PRINT_BLACK_LIST
        if (!get_pht_entry_from_index(GC_incomplete_normal_bl, index)) {
          GC_print_blacklisted_ptr(p, source, "normal");
        }
#endif
      backlist_set_pht_entry_from_index(GC_incomplete_normal_bl, index);
    }
  }
}
#ifdef PRINT_BLACK_LIST
  GC_INNER void GC_add_to_black_list_stack(word p, ptr_t source)
#else
  GC_INNER void GC_add_to_black_list_stack(word p)
#endif
{
  word index = PHT_HASH((word)p);
  if (HDR(p) == 0 || get_pht_entry_from_index(GC_old_stack_bl, index)) {
#ifdef PRINT_BLACK_LIST
      if (!get_pht_entry_from_index(GC_incomplete_stack_bl, index)) {
        GC_print_blacklisted_ptr(p, source, "stack");
      }
#endif
    backlist_set_pht_entry_from_index(GC_incomplete_stack_bl, index);
  }
}
struct hblk * GC_is_black_listed(struct hblk *h, word len)
{
    word index = PHT_HASH((word)h);
    word i;
    word nblocks;
    if (!GC_all_interior_pointers
        && (get_pht_entry_from_index(GC_old_normal_bl, index)
            || get_pht_entry_from_index(GC_incomplete_normal_bl, index))) {
      return (h+1);
    }
    nblocks = divHBLKSZ(len);
    for (i = 0;;) {
        if (GC_old_stack_bl[divWORDSZ(index)] == 0
            && GC_incomplete_stack_bl[divWORDSZ(index)] == 0) {
          i += WORDSZ - modWORDSZ(index);
        } else {
          if (get_pht_entry_from_index(GC_old_stack_bl, index)
              || get_pht_entry_from_index(GC_incomplete_stack_bl, index)) {
            return(h+i+1);
          }
          i++;
        }
        if (i >= nblocks) break;
        index = PHT_HASH((word)(h+i));
    }
    return(0);
}
STATIC word GC_number_stack_black_listed(struct hblk *start,
                                         struct hblk *endp1)
{
    struct hblk * h;
    word result = 0;
    for (h = start; (word)h < (word)endp1; h++) {
        word index = PHT_HASH((word)h);
        if (get_pht_entry_from_index(GC_old_stack_bl, index)) result++;
    }
    return(result);
}
static word total_stack_black_listed(void)
{
    unsigned i;
    word total = 0;
    for (i = 0; i < GC_n_heap_sects; i++) {
        struct hblk * start = (struct hblk *) GC_heap_sects[i].hs_start;
        struct hblk * endp1 = start + divHBLKSZ(GC_heap_sects[i].hs_bytes);
        total += GC_number_stack_black_listed(start, endp1);
    }
    return(total * HBLKSIZE);
}
#ifdef CHECKSUMS
#define NSUMS 10000
#define OFFSET 0x10000
typedef struct {
        GC_bool new_valid;
        word old_sum;
        word new_sum;
        struct hblk * block;
} page_entry;
page_entry GC_sums[NSUMS];
STATIC word GC_faulted[NSUMS] = { 0 };
STATIC size_t GC_n_faulted = 0;
#if defined(MPROTECT_VDB) && !defined(DARWIN)
  void GC_record_fault(struct hblk * h)
  {
    word page = (word)h & ~(GC_page_size - 1);
    GC_ASSERT(GC_page_size != 0);
    if (GC_n_faulted >= NSUMS) ABORT("write fault log overflowed");
    GC_faulted[GC_n_faulted++] = page;
  }
#endif
STATIC GC_bool GC_was_faulted(struct hblk *h)
{
    size_t i;
    word page = (word)h & ~(GC_page_size - 1);
    for (i = 0; i < GC_n_faulted; ++i) {
        if (GC_faulted[i] == page) return TRUE;
    }
    return FALSE;
}
STATIC word GC_checksum(struct hblk *h)
{
    word *p = (word *)h;
    word *lim = (word *)(h+1);
    word result = 0;
    while ((word)p < (word)lim) {
        result += *p++;
    }
    return(result | 0x80000000 );
}
int GC_n_dirty_errors = 0;
int GC_n_faulted_dirty_errors = 0;
unsigned long GC_n_clean = 0;
unsigned long GC_n_dirty = 0;
STATIC void GC_update_check_page(struct hblk *h, int index)
{
    page_entry *pe = GC_sums + index;
    hdr * hhdr = HDR(h);
    struct hblk *b;
    if (pe -> block != 0 && pe -> block != h + OFFSET) ABORT("goofed");
    pe -> old_sum = pe -> new_sum;
    pe -> new_sum = GC_checksum(h);
#if !defined(MSWIN32) && !defined(MSWINCE)
        if (pe -> new_sum != 0x80000000 && !GC_page_was_ever_dirty(h)) {
            GC_err_printf("GC_page_was_ever_dirty(%p) is wrong\n", (void *)h);
        }
#endif
    if (GC_page_was_dirty(h)) {
        GC_n_dirty++;
    } else {
        GC_n_clean++;
    }
    b = h;
    while (IS_FORWARDING_ADDR_OR_NIL(hhdr) && hhdr != 0) {
        b -= (word)hhdr;
        hhdr = HDR(b);
    }
    if (pe -> new_valid
        && hhdr != 0 && hhdr -> hb_descr != 0
        && pe -> old_sum != pe -> new_sum) {
        if (!GC_page_was_dirty(h) || !GC_page_was_ever_dirty(h)) {
            GC_bool was_faulted = GC_was_faulted(h);
            GC_n_dirty_errors++;
            if (was_faulted) GC_n_faulted_dirty_errors++;
        }
    }
    pe -> new_valid = TRUE;
    pe -> block = h + OFFSET;
}
word GC_bytes_in_used_blocks = 0;
STATIC void GC_add_block(struct hblk *h, word dummy GC_ATTR_UNUSED)
{
   hdr * hhdr = HDR(h);
   GC_bytes_in_used_blocks += (hhdr->hb_sz + HBLKSIZE-1) & ~(HBLKSIZE-1);
}
STATIC void GC_check_blocks(void)
{
    word bytes_in_free_blocks = GC_large_free_bytes;
    GC_bytes_in_used_blocks = 0;
    GC_apply_to_all_blocks(GC_add_block, (word)0);
    GC_COND_LOG_PRINTF("GC_bytes_in_used_blocks= %lu,"
                       " bytes_in_free_blocks= %lu, heapsize= %lu\n",
                       (unsigned long)GC_bytes_in_used_blocks,
                       (unsigned long)bytes_in_free_blocks,
                       (unsigned long)GC_heapsize);
    if (GC_bytes_in_used_blocks + bytes_in_free_blocks != GC_heapsize) {
        GC_err_printf("LOST SOME BLOCKS!!\n");
    }
}
void GC_check_dirty(void)
{
    int index;
    unsigned i;
    struct hblk *h;
    ptr_t start;
    GC_check_blocks();
    GC_n_dirty_errors = 0;
    GC_n_faulted_dirty_errors = 0;
    GC_n_clean = 0;
    GC_n_dirty = 0;
    index = 0;
    for (i = 0; i < GC_n_heap_sects; i++) {
        start = GC_heap_sects[i].hs_start;
        for (h = (struct hblk *)start;
             (word)h < (word)(start + GC_heap_sects[i].hs_bytes); h++) {
             GC_update_check_page(h, index);
             index++;
             if (index >= NSUMS) goto out;
        }
    }
out:
    GC_COND_LOG_PRINTF("Checked %lu clean and %lu dirty pages\n",
                       GC_n_clean, GC_n_dirty);
    if (GC_n_dirty_errors > 0) {
        GC_err_printf("Found %d dirty bit errors (%d were faulted)\n",
                      GC_n_dirty_errors, GC_n_faulted_dirty_errors);
    }
    for (i = 0; i < GC_n_faulted; ++i) {
        GC_faulted[i] = 0;
    }
    GC_n_faulted = 0;
}
#endif
#ifndef GC_PMARK_H
#define GC_PMARK_H
#if defined(HAVE_CONFIG_H) && !defined(GC_PRIVATE_H)
#endif
#ifndef GC_BUILD
#define GC_BUILD
#endif
#if (defined(__linux__) || defined(__GLIBC__) || defined(__GNU__)) \
    && !defined(_GNU_SOURCE) && defined(GC_PTHREADS) \
    && !defined(GC_NO_PTHREAD_SIGMASK)
#define _GNU_SOURCE 1
#endif
#if defined(KEEP_BACK_PTRS) || defined(PRINT_BLACK_LIST)
#endif
EXTERN_C_BEGIN
#ifndef MARK_DESCR_OFFSET
#define MARK_DESCR_OFFSET sizeof(word)
#endif
#define BITMAP_BITS (WORDSZ - GC_DS_TAG_BITS)
#define PROC(descr) \
      (GC_mark_procs[((descr) >> GC_DS_TAG_BITS) & (GC_MAX_MARK_PROCS-1)])
#define ENV(descr) \
      ((descr) >> (GC_DS_TAG_BITS + GC_LOG_MAX_MARK_PROCS))
#define MAX_ENV \
      (((word)1 << (WORDSZ - GC_DS_TAG_BITS - GC_LOG_MAX_MARK_PROCS)) - 1)
GC_EXTERN unsigned GC_n_mark_procs;
#define GC_MARK_STACK_DISCARDS (INITIAL_MARK_STACK_SIZE/8)
#ifdef PARALLEL_MARK
#endif
GC_INNER mse * GC_signal_mark_stack_overflow(mse *msp);
GC_INLINE mse * GC_push_obj(ptr_t obj, hdr * hhdr,  mse * mark_stack_top,
                            mse * mark_stack_limit)
{
  word descr = hhdr -> hb_descr;
  GC_ASSERT(!HBLK_IS_FREE(hhdr));
  if (descr != 0) {
    mark_stack_top++;
    if ((word)mark_stack_top >= (word)mark_stack_limit) {
      mark_stack_top = GC_signal_mark_stack_overflow(mark_stack_top);
    }
    mark_stack_top -> mse_start = obj;
    mark_stack_top -> mse_descr.w = descr;
  }
  return mark_stack_top;
}
#define PUSH_CONTENTS(current, mark_stack_top, mark_stack_limit, source) \
  do { \
    hdr * my_hhdr; \
    HC_GET_HDR(current, my_hhdr, source);  \
    mark_stack_top = GC_push_contents_hdr(current, mark_stack_top, \
                                          mark_stack_limit, \
                                          source, my_hhdr, TRUE); \
  } while (0)
#ifdef USE_MARK_BYTES
#if defined(PARALLEL_MARK) && defined(AO_HAVE_char_store) \
     && !defined(BASE_ATOMIC_OPS_EMULATED)
#define SET_MARK_BIT_EXIT_IF_SET(hhdr, bit_no) \
      {  \
        volatile unsigned char * mark_byte_addr = \
                        (unsigned char *)(hhdr)->hb_marks + (bit_no); \
         \
        if (AO_char_load(mark_byte_addr) != 0) \
          break;  \
        AO_char_store(mark_byte_addr, 1); \
      }
#else
#define SET_MARK_BIT_EXIT_IF_SET(hhdr, bit_no) \
      {  \
        char * mark_byte_addr = (char *)(hhdr)->hb_marks + (bit_no); \
        if (*mark_byte_addr != 0) break;  \
        *mark_byte_addr = 1; \
      }
#endif
#else
#ifdef PARALLEL_MARK
#ifdef THREAD_SANITIZER
#define OR_WORD_EXIT_IF_SET(addr, bits) \
        {  \
          if (!((word)AO_load((volatile AO_t *)(addr)) & (bits))) { \
                 \
            AO_or((volatile AO_t *)(addr), (AO_t)(bits)); \
          } else { \
            break;  \
          } \
        }
#else
#define OR_WORD_EXIT_IF_SET(addr, bits) \
        {  \
          if (!(*(addr) & (bits))) { \
            AO_or((volatile AO_t *)(addr), (AO_t)(bits)); \
          } else { \
            break;  \
          } \
        }
#endif
#else
#define OR_WORD_EXIT_IF_SET(addr, bits) \
        {  \
           word old = *(addr); \
           word my_bits = (bits); \
           if ((old & my_bits) != 0) \
             break;  \
           *(addr) = old | my_bits; \
        }
#endif
#define SET_MARK_BIT_EXIT_IF_SET(hhdr, bit_no) \
    {  \
        word * mark_word_addr = (hhdr)->hb_marks + divWORDSZ(bit_no); \
        OR_WORD_EXIT_IF_SET(mark_word_addr, \
                (word)1 << modWORDSZ(bit_no));  \
    }
#endif
#ifdef PARALLEL_MARK
#define INCR_MARKS(hhdr) \
                AO_store(&hhdr->hb_n_marks, AO_load(&hhdr->hb_n_marks) + 1)
#else
#define INCR_MARKS(hhdr) (void)(++hhdr->hb_n_marks)
#endif
#ifdef ENABLE_TRACE
#define TRACE(source, cmd) \
        if (GC_trace_addr != 0 && (ptr_t)(source) == GC_trace_addr) cmd
#define TRACE_TARGET(target, cmd) \
        if (GC_trace_addr != 0 && (target) == *(ptr_t *)GC_trace_addr) cmd
#else
#define TRACE(source, cmd)
#define TRACE_TARGET(source, cmd)
#endif
#if defined(I386) && defined(__GNUC__) && !defined(NACL)
#define LONG_MULT(hprod, lprod, x, y) \
    do { \
        __asm__ __volatile__("mull %2" : "=a"(lprod), "=d"(hprod) \
                             : "g"(y), "0"(x)); \
    } while (0)
#else
#if defined(__int64) && !defined(__GNUC__) && !defined(CPPCHECK)
#define ULONG_MULT_T unsigned __int64
#else
#define ULONG_MULT_T unsigned long long
#endif
#define LONG_MULT(hprod, lprod, x, y) \
    do { \
        ULONG_MULT_T prod = (ULONG_MULT_T)(x) * (ULONG_MULT_T)(y); \
        GC_STATIC_ASSERT(sizeof(x) + sizeof(y) <= sizeof(prod)); \
        hprod = prod >> 32; \
        lprod = (unsigned32)prod; \
    } while (0)
#endif
GC_INLINE mse * GC_push_contents_hdr(ptr_t current, mse * mark_stack_top,
                                     mse * mark_stack_limit, ptr_t source,
                                     hdr * hhdr, GC_bool do_offset_check)
{
  do {
    size_t displ = HBLKDISPL(current);
    ptr_t base = current;
#ifdef MARK_BIT_PER_GRANULE
      size_t gran_displ = BYTES_TO_GRANULES(displ);
      size_t gran_offset = hhdr -> hb_map[gran_displ];
      size_t byte_offset = displ & (GRANULE_BYTES - 1);
      if (EXPECT((gran_offset | byte_offset) != 0, FALSE))
#else
      unsigned32 gran_displ;
      unsigned32 inv_sz = hhdr -> hb_inv_sz;
#endif
    {
#ifdef MARK_BIT_PER_GRANULE
        if ((hhdr -> hb_flags & LARGE_BLOCK) != 0)
#else
        if (EXPECT(inv_sz == LARGE_INV_SZ, FALSE))
#endif
      {
        size_t obj_displ;
        base = (ptr_t)hhdr->hb_block;
        obj_displ = current - base;
        if (obj_displ != displ) {
          GC_ASSERT(obj_displ < hhdr -> hb_sz);
        } else if (do_offset_check && !GC_valid_offsets[obj_displ]) {
          GC_ADD_TO_BLACK_LIST_NORMAL(current, source);
          break;
        }
        GC_ASSERT(hhdr -> hb_sz > HBLKSIZE
                  || hhdr -> hb_block == HBLKPTR(current));
        GC_ASSERT((word)hhdr->hb_block <= (word)current);
        gran_displ = 0;
      } else {
#ifdef MARK_BIT_PER_GRANULE
          size_t obj_displ = GRANULES_TO_BYTES(gran_offset) + byte_offset;
#else
          unsigned32 low_prod;
          LONG_MULT(gran_displ, low_prod, (unsigned32)displ, inv_sz);
          if ((low_prod >> 16) != 0)
#endif
        {
#if defined(MARK_BIT_PER_OBJ) \
             && !defined(MARK_BIT_PER_GRANULE)
            size_t obj_displ;
            GC_STATIC_ASSERT(HBLKSIZE <= (1 << 15));
            obj_displ = (((low_prod >> 16) + 1) * (size_t)hhdr->hb_sz) >> 16;
#endif
          if (do_offset_check && !GC_valid_offsets[obj_displ]) {
            GC_ADD_TO_BLACK_LIST_NORMAL(current, source);
            break;
          }
#ifdef MARK_BIT_PER_GRANULE
            gran_displ -= gran_offset;
#endif
          base -= obj_displ;
        }
      }
    }
#ifdef MARK_BIT_PER_GRANULE
      GC_ASSERT(hhdr == GC_find_header(base));
      GC_ASSERT(gran_displ % BYTES_TO_GRANULES(hhdr -> hb_sz) == 0);
#else
      GC_ASSERT(gran_displ <= HBLK_OBJS(hhdr -> hb_sz));
#endif
    TRACE(source, GC_log_printf("GC #%lu: passed validity tests\n",
                                (unsigned long)GC_gc_no));
    SET_MARK_BIT_EXIT_IF_SET(hhdr, gran_displ);
    TRACE(source, GC_log_printf("GC #%lu: previously unmarked\n",
                                (unsigned long)GC_gc_no));
    TRACE_TARGET(base, GC_log_printf("GC #%lu: marking %p from %p instead\n",
                                     (unsigned long)GC_gc_no, (void *)base,
                                     (void *)source));
    INCR_MARKS(hhdr);
    GC_STORE_BACK_PTR(source, base);
    mark_stack_top = GC_push_obj(base, hhdr, mark_stack_top,
                                 mark_stack_limit);
  } while (0);
  return mark_stack_top;
}
#if defined(PRINT_BLACK_LIST) || defined(KEEP_BACK_PTRS)
#define PUSH_ONE_CHECKED_STACK(p, source) \
        GC_mark_and_push_stack((ptr_t)(p), (ptr_t)(source))
#else
#define PUSH_ONE_CHECKED_STACK(p, source) \
        GC_mark_and_push_stack((ptr_t)(p))
#endif
#ifdef NEED_FIXUP_POINTER
#define GC_PUSH_ONE_STACK(p, source) \
    do { \
      if ((word)(p) >= (word)GC_least_plausible_heap_addr \
          && (word)(p) < (word)GC_greatest_plausible_heap_addr) { \
         PUSH_ONE_CHECKED_STACK(p, source); \
      } \
      FIXUP_POINTER(p); \
      if ((word)(p) >= (word)GC_least_plausible_heap_addr \
          && (word)(p) < (word)GC_greatest_plausible_heap_addr) { \
         PUSH_ONE_CHECKED_STACK(p, source); \
      } \
    } while (0)
#else
#define GC_PUSH_ONE_STACK(p, source) \
    do { \
      if ((word)(p) >= (word)GC_least_plausible_heap_addr \
          && (word)(p) < (word)GC_greatest_plausible_heap_addr) { \
         PUSH_ONE_CHECKED_STACK(p, source); \
      } \
    } while (0)
#endif
#define GC_PUSH_ONE_HEAP(p,source,mark_stack_top) \
    do { \
      FIXUP_POINTER(p); \
      if ((word)(p) >= (word)GC_least_plausible_heap_addr \
          && (word)(p) < (word)GC_greatest_plausible_heap_addr) \
        mark_stack_top = GC_mark_and_push((void *)(p), mark_stack_top, \
                                GC_mark_stack_limit, (void * *)(source)); \
    } while (0)
GC_INNER mse * GC_mark_from(mse * top, mse * bottom, mse *limit);
#define MARK_FROM_MARK_STACK() \
        GC_mark_stack_top = GC_mark_from(GC_mark_stack_top, \
                                         GC_mark_stack, \
                                         GC_mark_stack + GC_mark_stack_size);
#define GC_mark_stack_empty() ((word)GC_mark_stack_top < (word)GC_mark_stack)
#define GC_MARK_FO(real_ptr, mark_proc) \
  do { \
    (*(mark_proc))(real_ptr); \
    while (!GC_mark_stack_empty()) MARK_FROM_MARK_STACK(); \
    if (GC_mark_state != MS_NONE) { \
        GC_set_mark_bit(real_ptr); \
        while (!GC_mark_some((ptr_t)0)) {  } \
    } \
  } while (0)
#define MS_NONE 0
#define MS_PUSH_RESCUERS 1
#define MS_PUSH_UNCOLLECTABLE 2
#define MS_ROOTS_PUSHED 3
#define MS_PARTIALLY_INVALID 4
#define MS_INVALID 5
EXTERN_C_END
#endif
#ifdef GC_GCJ_SUPPORT
#ifndef GC_GCJ_H
#define GC_GCJ_H
#ifndef GC_H
#endif
#ifdef __cplusplus
  extern "C" {
#endif
GC_API void GC_CALL GC_init_gcj_malloc(int ,
                                void *  );
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
        GC_gcj_malloc(size_t ,
                      void * );
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
        GC_debug_gcj_malloc(size_t ,
                            void * ,
                            GC_EXTRA_PARAMS);
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
        GC_gcj_malloc_ignore_off_page(size_t ,
                                void * );
GC_API int GC_gcj_kind;
GC_API int GC_gcj_debug_kind;
#ifdef GC_DEBUG
#define GC_GCJ_MALLOC(s,d) GC_debug_gcj_malloc(s,d,GC_EXTRAS)
#define GC_GCJ_MALLOC_IGNORE_OFF_PAGE(s,d) GC_debug_gcj_malloc(s,d,GC_EXTRAS)
#else
#define GC_GCJ_MALLOC(s,d) GC_gcj_malloc(s,d)
#define GC_GCJ_MALLOC_IGNORE_OFF_PAGE(s,d) GC_gcj_malloc_ignore_off_page(s,d)
#endif
#ifdef __cplusplus
  }
#endif
#endif
int GC_gcj_kind = 0;
int GC_gcj_debug_kind = 0;
STATIC struct GC_ms_entry * GC_gcj_fake_mark_proc(word * addr GC_ATTR_UNUSED,
                        struct GC_ms_entry *mark_stack_ptr,
                        struct GC_ms_entry * mark_stack_limit GC_ATTR_UNUSED,
                        word env GC_ATTR_UNUSED)
{
    ABORT_RET("No client gcj mark proc is specified");
    return mark_stack_ptr;
}
GC_API void GC_CALL GC_init_gcj_malloc(int mp_index,
                                       void * mp)
{
#ifndef GC_IGNORE_GCJ_INFO
      GC_bool ignore_gcj_info;
#endif
    DCL_LOCK_STATE;
    if (mp == 0)
      mp = (void *)(word)GC_gcj_fake_mark_proc;
    GC_init();
    LOCK();
    if (GC_gcjobjfreelist != NULL) {
      UNLOCK();
      return;
    }
#ifdef GC_IGNORE_GCJ_INFO
#define ignore_gcj_info TRUE
#else
      ignore_gcj_info = (0 != GETENV("GC_IGNORE_GCJ_INFO"));
#endif
    if (ignore_gcj_info) {
      GC_COND_LOG_PRINTF("Gcj-style type information is disabled!\n");
    }
    GC_ASSERT(GC_mark_procs[mp_index] == (GC_mark_proc)0);
    GC_mark_procs[mp_index] = (GC_mark_proc)(word)mp;
    if ((unsigned)mp_index >= GC_n_mark_procs)
        ABORT("GC_init_gcj_malloc: bad index");
    GC_gcjobjfreelist = (ptr_t *)GC_new_free_list_inner();
    if (ignore_gcj_info) {
        GC_gcj_kind = GC_new_kind_inner((void **)GC_gcjobjfreelist,
                                         GC_DS_LENGTH,
                                        TRUE, TRUE);
        GC_gcj_debug_kind = GC_gcj_kind;
    } else {
        GC_gcj_kind = GC_new_kind_inner(
                        (void **)GC_gcjobjfreelist,
                        (((word)(-(signed_word)MARK_DESCR_OFFSET
                                 - GC_INDIR_PER_OBJ_BIAS))
                         | GC_DS_PER_OBJECT),
                        FALSE, TRUE);
        GC_gcj_debug_kind = GC_new_kind_inner(GC_new_free_list_inner(),
                                GC_MAKE_PROC(mp_index,
                                             1 ),
                                FALSE, TRUE);
    }
    UNLOCK();
#undef ignore_gcj_info
}
#define GENERAL_MALLOC_INNER(lb,k) \
    GC_clear_stack(GC_generic_malloc_inner(lb, k))
#define GENERAL_MALLOC_INNER_IOP(lb,k) \
    GC_clear_stack(GC_generic_malloc_inner_ignore_off_page(lb, k))
static void maybe_finalize(void)
{
   static word last_finalized_no = 0;
   DCL_LOCK_STATE;
   if (GC_gc_no == last_finalized_no ||
       !EXPECT(GC_is_initialized, TRUE)) return;
   UNLOCK();
   GC_INVOKE_FINALIZERS();
   LOCK();
   last_finalized_no = GC_gc_no;
}
#ifdef THREAD_LOCAL_ALLOC
  GC_INNER void * GC_core_gcj_malloc(size_t lb,
                                     void * ptr_to_struct_containing_descr)
#else
  GC_API GC_ATTR_MALLOC void * GC_CALL GC_gcj_malloc(size_t lb,
                                      void * ptr_to_struct_containing_descr)
#endif
{
    ptr_t op;
    DCL_LOCK_STATE;
    GC_DBG_COLLECT_AT_MALLOC(lb);
    if(SMALL_OBJ(lb)) {
        word lg;
        LOCK();
        lg = GC_size_map[lb];
        op = GC_gcjobjfreelist[lg];
        if(EXPECT(0 == op, FALSE)) {
            maybe_finalize();
            op = (ptr_t)GENERAL_MALLOC_INNER((word)lb, GC_gcj_kind);
            if (0 == op) {
                GC_oom_func oom_fn = GC_oom_fn;
                UNLOCK();
                return((*oom_fn)(lb));
            }
        } else {
            GC_gcjobjfreelist[lg] = (ptr_t)obj_link(op);
            GC_bytes_allocd += GRANULES_TO_BYTES((word)lg);
        }
        GC_ASSERT(((void **)op)[1] == 0);
    } else {
        LOCK();
        maybe_finalize();
        op = (ptr_t)GENERAL_MALLOC_INNER((word)lb, GC_gcj_kind);
        if (0 == op) {
            GC_oom_func oom_fn = GC_oom_fn;
            UNLOCK();
            return((*oom_fn)(lb));
        }
    }
    *(void **)op = ptr_to_struct_containing_descr;
    UNLOCK();
    GC_dirty(op);
    REACHABLE_AFTER_DIRTY(ptr_to_struct_containing_descr);
    return (void *)op;
}
GC_API GC_ATTR_MALLOC void * GC_CALL GC_debug_gcj_malloc(size_t lb,
                void * ptr_to_struct_containing_descr, GC_EXTRA_PARAMS)
{
    void * result;
    DCL_LOCK_STATE;
    LOCK();
    maybe_finalize();
    result = GC_generic_malloc_inner(SIZET_SAT_ADD(lb, DEBUG_BYTES),
                                     GC_gcj_debug_kind);
    if (result == 0) {
        GC_oom_func oom_fn = GC_oom_fn;
        UNLOCK();
        GC_err_printf("GC_debug_gcj_malloc(%lu, %p) returning NULL (%s:%d)\n",
                (unsigned long)lb, ptr_to_struct_containing_descr, s, i);
        return((*oom_fn)(lb));
    }
    *((void **)((ptr_t)result + sizeof(oh))) = ptr_to_struct_containing_descr;
    if (!GC_debugging_started) {
        GC_start_debugging_inner();
    }
    ADD_CALL_CHAIN(result, ra);
    result = GC_store_debug_info_inner(result, (word)lb, s, i);
    UNLOCK();
    GC_dirty(result);
    REACHABLE_AFTER_DIRTY(ptr_to_struct_containing_descr);
    return result;
}
GC_API GC_ATTR_MALLOC void * GC_CALL GC_gcj_malloc_ignore_off_page(size_t lb,
                                     void * ptr_to_struct_containing_descr)
{
    ptr_t op;
    DCL_LOCK_STATE;
    GC_DBG_COLLECT_AT_MALLOC(lb);
    if(SMALL_OBJ(lb)) {
        word lg;
        LOCK();
        lg = GC_size_map[lb];
        op = GC_gcjobjfreelist[lg];
        if (EXPECT(0 == op, FALSE)) {
            maybe_finalize();
            op = (ptr_t)GENERAL_MALLOC_INNER_IOP(lb, GC_gcj_kind);
            if (0 == op) {
                GC_oom_func oom_fn = GC_oom_fn;
                UNLOCK();
                return((*oom_fn)(lb));
            }
        } else {
            GC_gcjobjfreelist[lg] = (ptr_t)obj_link(op);
            GC_bytes_allocd += GRANULES_TO_BYTES((word)lg);
        }
    } else {
        LOCK();
        maybe_finalize();
        op = (ptr_t)GENERAL_MALLOC_INNER_IOP(lb, GC_gcj_kind);
        if (0 == op) {
            GC_oom_func oom_fn = GC_oom_fn;
            UNLOCK();
            return((*oom_fn)(lb));
        }
    }
    *(void **)op = ptr_to_struct_containing_descr;
    UNLOCK();
    GC_dirty(op);
    REACHABLE_AFTER_DIRTY(ptr_to_struct_containing_descr);
    return (void *)op;
}
#endif
GC_INNER hdr * GC_find_header(ptr_t h)
{
#ifdef HASH_TL
        hdr * result;
        GET_HDR(h, result);
        return(result);
#else
        return(HDR_INNER(h));
#endif
}
GC_INNER hdr *
#ifdef PRINT_BLACK_LIST
  GC_header_cache_miss(ptr_t p, hdr_cache_entry *hce, ptr_t source)
#else
  GC_header_cache_miss(ptr_t p, hdr_cache_entry *hce)
#endif
{
  hdr *hhdr;
  HC_MISS();
  GET_HDR(p, hhdr);
  if (IS_FORWARDING_ADDR_OR_NIL(hhdr)) {
    if (GC_all_interior_pointers) {
      if (hhdr != 0) {
        ptr_t current = p;
        current = (ptr_t)HBLKPTR(current);
        do {
            current = current - HBLKSIZE*(word)hhdr;
            hhdr = HDR(current);
        } while(IS_FORWARDING_ADDR_OR_NIL(hhdr));
        if (hhdr -> hb_flags & IGNORE_OFF_PAGE)
            return 0;
        if (HBLK_IS_FREE(hhdr)
            || p - current >= (ptrdiff_t)(hhdr->hb_sz)) {
            GC_ADD_TO_BLACK_LIST_NORMAL(p, source);
            return 0;
        }
      } else {
        GC_ADD_TO_BLACK_LIST_NORMAL(p, source);
      }
      GC_ASSERT(hhdr == 0 || !HBLK_IS_FREE(hhdr));
      return hhdr;
    } else {
      if (hhdr == 0) {
        GC_ADD_TO_BLACK_LIST_NORMAL(p, source);
      }
      return 0;
    }
  } else {
    if (HBLK_IS_FREE(hhdr)) {
      GC_ADD_TO_BLACK_LIST_NORMAL(p, source);
      return 0;
    } else {
      hce -> block_addr = (word)(p) >> LOG_HBLKSIZE;
      hce -> hce_hdr = hhdr;
      return hhdr;
    }
  }
}
GC_INNER ptr_t GC_scratch_alloc(size_t bytes)
{
    ptr_t result = GC_scratch_free_ptr;
    size_t bytes_to_get;
    bytes = ROUNDUP_GRANULE_SIZE(bytes);
    for (;;) {
        GC_ASSERT((word)GC_scratch_end_ptr >= (word)result);
        if (bytes <= (word)GC_scratch_end_ptr - (word)result) {
            GC_scratch_free_ptr = result + bytes;
            return result;
        }
        GC_ASSERT(GC_page_size != 0);
        if (bytes >= MINHINCR * HBLKSIZE) {
            bytes_to_get = ROUNDUP_PAGESIZE_IF_MMAP(bytes);
            result = (ptr_t)GET_MEM(bytes_to_get);
            if (result != NULL) {
              GC_add_to_our_memory(result, bytes_to_get);
#ifdef USE_SCRATCH_LAST_END_PTR
                GC_scratch_last_end_ptr = result + bytes;
#endif
            }
            return result;
        }
        bytes_to_get = ROUNDUP_PAGESIZE_IF_MMAP(MINHINCR * HBLKSIZE);
        result = (ptr_t)GET_MEM(bytes_to_get);
        if (EXPECT(NULL == result, FALSE)) {
            WARN("Out of memory - trying to allocate requested amount"
                 " (%" WARN_PRIdPTR " bytes)...\n", (word)bytes);
            bytes_to_get = ROUNDUP_PAGESIZE_IF_MMAP(bytes);
            result = (ptr_t)GET_MEM(bytes_to_get);
            if (result != NULL) {
              GC_add_to_our_memory(result, bytes_to_get);
#ifdef USE_SCRATCH_LAST_END_PTR
                GC_scratch_last_end_ptr = result + bytes;
#endif
            }
            return result;
        }
        GC_add_to_our_memory(result, bytes_to_get);
        GC_scratch_free_ptr = result;
        GC_scratch_end_ptr = GC_scratch_free_ptr + bytes_to_get;
#ifdef USE_SCRATCH_LAST_END_PTR
          GC_scratch_last_end_ptr = GC_scratch_end_ptr;
#endif
    }
}
static hdr * alloc_hdr(void)
{
    hdr * result;
    if (NULL == GC_hdr_free_list) {
        result = (hdr *)GC_scratch_alloc(sizeof(hdr));
    } else {
        result = GC_hdr_free_list;
        GC_hdr_free_list = (hdr *) result -> hb_next;
    }
    return(result);
}
GC_INLINE void free_hdr(hdr * hhdr)
{
    hhdr -> hb_next = (struct hblk *) GC_hdr_free_list;
    GC_hdr_free_list = hhdr;
}
#ifdef COUNT_HDR_CACHE_HITS
  word GC_hdr_cache_hits = 0;
  word GC_hdr_cache_misses = 0;
#endif
GC_INNER void GC_init_headers(void)
{
    unsigned i;
    GC_ASSERT(NULL == GC_all_nils);
    GC_all_nils = (bottom_index *)GC_scratch_alloc(sizeof(bottom_index));
    if (GC_all_nils == NULL) {
      GC_err_printf("Insufficient memory for GC_all_nils\n");
      EXIT();
    }
    BZERO(GC_all_nils, sizeof(bottom_index));
    for (i = 0; i < TOP_SZ; i++) {
        GC_top_index[i] = GC_all_nils;
    }
}
static GC_bool get_index(word addr)
{
    word hi = (word)(addr) >> (LOG_BOTTOM_SZ + LOG_HBLKSIZE);
    bottom_index * r;
    bottom_index * p;
    bottom_index ** prev;
    bottom_index *pi;
    word i;
    GC_ASSERT(I_HOLD_LOCK());
#ifdef HASH_TL
      i = TL_HASH(hi);
      pi = p = GC_top_index[i];
      while(p != GC_all_nils) {
          if (p -> key == hi) return(TRUE);
          p = p -> hash_link;
      }
#else
      if (GC_top_index[hi] != GC_all_nils)
        return TRUE;
      i = hi;
#endif
    r = (bottom_index *)GC_scratch_alloc(sizeof(bottom_index));
    if (EXPECT(NULL == r, FALSE))
      return FALSE;
    BZERO(r, sizeof(bottom_index));
    r -> key = hi;
#ifdef HASH_TL
      r -> hash_link = pi;
#endif
      prev = &GC_all_bottom_indices;
      pi = 0;
      while ((p = *prev) != 0 && p -> key < hi) {
        pi = p;
        prev = &(p -> asc_link);
      }
      r -> desc_link = pi;
      if (0 == p) {
        GC_all_bottom_indices_end = r;
      } else {
        p -> desc_link = r;
      }
      r -> asc_link = p;
      *prev = r;
      GC_top_index[i] = r;
    return(TRUE);
}
GC_INNER struct hblkhdr * GC_install_header(struct hblk *h)
{
    hdr * result;
    if (!get_index((word) h)) return(0);
    result = alloc_hdr();
    if (result) {
      SET_HDR(h, result);
#ifdef USE_MUNMAP
        result -> hb_last_reclaimed = (unsigned short)GC_gc_no;
#endif
    }
    return(result);
}
GC_INNER GC_bool GC_install_counts(struct hblk *h, size_t sz)
{
    struct hblk * hbp;
    for (hbp = h; (word)hbp < (word)h + sz; hbp += BOTTOM_SZ) {
        if (!get_index((word)hbp))
            return FALSE;
        if ((word)hbp > GC_WORD_MAX - (word)BOTTOM_SZ * HBLKSIZE)
            break;
    }
    if (!get_index((word)h + sz - 1))
        return FALSE;
    for (hbp = h + 1; (word)hbp < (word)h + sz; hbp += 1) {
        word i = HBLK_PTR_DIFF(hbp, h);
        SET_HDR(hbp, (hdr *)(i > MAX_JUMP? MAX_JUMP : i));
    }
    return TRUE;
}
GC_INNER void GC_remove_header(struct hblk *h)
{
    hdr **ha;
    GET_HDR_ADDR(h, ha);
    free_hdr(*ha);
    *ha = 0;
}
GC_INNER void GC_remove_counts(struct hblk *h, size_t sz)
{
    struct hblk * hbp;
    for (hbp = h+1; (word)hbp < (word)h + sz; hbp += 1) {
        SET_HDR(hbp, 0);
    }
}
void GC_apply_to_all_blocks(void (*fn)(struct hblk *h, word client_data),
                            word client_data)
{
    signed_word j;
    bottom_index * index_p;
    for (index_p = GC_all_bottom_indices; index_p != 0;
         index_p = index_p -> asc_link) {
        for (j = BOTTOM_SZ-1; j >= 0;) {
            if (!IS_FORWARDING_ADDR_OR_NIL(index_p->index[j])) {
                if (!HBLK_IS_FREE(index_p->index[j])) {
                    (*fn)(((struct hblk *)
                              (((index_p->key << LOG_BOTTOM_SZ) + (word)j)
                               << LOG_HBLKSIZE)),
                          client_data);
                }
                j--;
             } else if (index_p->index[j] == 0) {
                j--;
             } else {
                j -= (signed_word)(index_p->index[j]);
             }
         }
     }
}
GC_INNER struct hblk * GC_next_block(struct hblk *h, GC_bool allow_free)
{
    REGISTER bottom_index * bi;
    REGISTER word j = ((word)h >> LOG_HBLKSIZE) & (BOTTOM_SZ-1);
    GC_ASSERT(I_HOLD_LOCK());
    GET_BI(h, bi);
    if (bi == GC_all_nils) {
        REGISTER word hi = (word)h >> (LOG_BOTTOM_SZ + LOG_HBLKSIZE);
        bi = GC_all_bottom_indices;
        while (bi != 0 && bi -> key < hi) bi = bi -> asc_link;
        j = 0;
    }
    while (bi != 0) {
        while (j < BOTTOM_SZ) {
            hdr * hhdr = bi -> index[j];
            if (IS_FORWARDING_ADDR_OR_NIL(hhdr)) {
                j++;
            } else {
                if (allow_free || !HBLK_IS_FREE(hhdr)) {
                    return ((struct hblk *)
                              (((bi -> key << LOG_BOTTOM_SZ) + j)
                               << LOG_HBLKSIZE));
                } else {
                    j += divHBLKSZ(hhdr -> hb_sz);
                }
            }
        }
        j = 0;
        bi = bi -> asc_link;
    }
    return(0);
}
GC_INNER struct hblk * GC_prev_block(struct hblk *h)
{
    bottom_index * bi;
    signed_word j = ((word)h >> LOG_HBLKSIZE) & (BOTTOM_SZ-1);
    GC_ASSERT(I_HOLD_LOCK());
    GET_BI(h, bi);
    if (bi == GC_all_nils) {
        word hi = (word)h >> (LOG_BOTTOM_SZ + LOG_HBLKSIZE);
        bi = GC_all_bottom_indices_end;
        while (bi != 0 && bi -> key > hi) bi = bi -> desc_link;
        j = BOTTOM_SZ - 1;
    }
    while(bi != 0) {
        while (j >= 0) {
            hdr * hhdr = bi -> index[j];
            if (0 == hhdr) {
                --j;
            } else if (IS_FORWARDING_ADDR_OR_NIL(hhdr)) {
                j -= (signed_word)hhdr;
            } else {
                return((struct hblk *)
                          (((bi -> key << LOG_BOTTOM_SZ) + j)
                               << LOG_HBLKSIZE));
            }
        }
        j = BOTTOM_SZ - 1;
        bi = bi -> desc_link;
    }
    return(0);
}
#include <stdio.h>
#ifndef SMALL_CONFIG
  STATIC ptr_t GC_build_fl_clear2(struct hblk *h, ptr_t ofl)
  {
    word * p = (word *)(h -> hb_body);
    word * lim = (word *)(h + 1);
    p[0] = (word)ofl;
    p[1] = 0;
    p[2] = (word)p;
    p[3] = 0;
    p += 4;
    for (; (word)p < (word)lim; p += 4) {
        p[0] = (word)(p-2);
        p[1] = 0;
        p[2] = (word)p;
        p[3] = 0;
    }
    return((ptr_t)(p-2));
  }
  STATIC ptr_t GC_build_fl_clear4(struct hblk *h, ptr_t ofl)
  {
    word * p = (word *)(h -> hb_body);
    word * lim = (word *)(h + 1);
    p[0] = (word)ofl;
    p[1] = 0;
    p[2] = 0;
    p[3] = 0;
    p += 4;
    for (; (word)p < (word)lim; p += 4) {
        GC_PREFETCH_FOR_WRITE((ptr_t)(p + 64));
        p[0] = (word)(p-4);
        p[1] = 0;
        CLEAR_DOUBLE(p+2);
    }
    return((ptr_t)(p-4));
  }
  STATIC ptr_t GC_build_fl2(struct hblk *h, ptr_t ofl)
  {
    word * p = (word *)(h -> hb_body);
    word * lim = (word *)(h + 1);
    p[0] = (word)ofl;
    p[2] = (word)p;
    p += 4;
    for (; (word)p < (word)lim; p += 4) {
        p[0] = (word)(p-2);
        p[2] = (word)p;
    }
    return((ptr_t)(p-2));
  }
  STATIC ptr_t GC_build_fl4(struct hblk *h, ptr_t ofl)
  {
    word * p = (word *)(h -> hb_body);
    word * lim = (word *)(h + 1);
    p[0] = (word)ofl;
    p[4] = (word)p;
    p += 8;
    for (; (word)p < (word)lim; p += 8) {
        GC_PREFETCH_FOR_WRITE((ptr_t)(p + 64));
        p[0] = (word)(p-4);
        p[4] = (word)p;
    }
    return((ptr_t)(p-4));
  }
#endif
GC_INNER ptr_t GC_build_fl(struct hblk *h, size_t sz, GC_bool clear,
                           ptr_t list)
{
  word *p, *prev;
  word *last_object;
    GC_PREFETCH_FOR_WRITE((ptr_t)h);
    GC_PREFETCH_FOR_WRITE((ptr_t)h + 128);
    GC_PREFETCH_FOR_WRITE((ptr_t)h + 256);
    GC_PREFETCH_FOR_WRITE((ptr_t)h + 378);
#ifndef SMALL_CONFIG
    switch (sz) {
        case 2: if (clear) {
                    return GC_build_fl_clear2(h, list);
                } else {
                    return GC_build_fl2(h, list);
                }
        case 4: if (clear) {
                    return GC_build_fl_clear4(h, list);
                } else {
                    return GC_build_fl4(h, list);
                }
        default:
                break;
    }
#endif
    if (clear) BZERO(h, HBLKSIZE);
    p = (word *)(h -> hb_body) + sz;
    prev = (word *)(h -> hb_body);
    last_object = (word *)((char *)h + HBLKSIZE);
    last_object -= sz;
    while ((word)p <= (word)last_object) {
        obj_link(p) = (ptr_t)prev;
        prev = p;
        p += sz;
    }
    p -= sz;
    *(ptr_t *)h = list;
    return ((ptr_t)p);
}
GC_INNER void GC_new_hblk(size_t gran, int kind)
{
  struct hblk *h;
  GC_bool clear = GC_obj_kinds[kind].ok_init;
  GC_STATIC_ASSERT((sizeof (struct hblk)) == HBLKSIZE);
  GC_ASSERT(I_HOLD_LOCK());
  if (GC_debugging_started) clear = TRUE;
    h = GC_allochblk(GRANULES_TO_BYTES(gran), kind, 0);
    if (h == 0) return;
      if (IS_UNCOLLECTABLE(kind)) GC_set_hdr_marks(HDR(h));
      GC_obj_kinds[kind].ok_freelist[gran] =
        GC_build_fl(h, GRANULES_TO_WORDS(gran), clear,
                    (ptr_t)GC_obj_kinds[kind].ok_freelist[gran]);
}
GC_API void GC_CALL GC_register_displacement(size_t offset)
{
    DCL_LOCK_STATE;
    LOCK();
    GC_register_displacement_inner(offset);
    UNLOCK();
}
GC_INNER void GC_register_displacement_inner(size_t offset)
{
    GC_ASSERT(I_HOLD_LOCK());
    if (offset >= VALID_OFFSET_SZ) {
        ABORT("Bad argument to GC_register_displacement");
    }
    if (!GC_valid_offsets[offset]) {
      GC_valid_offsets[offset] = TRUE;
      GC_modws_valid_offsets[offset % sizeof(word)] = TRUE;
    }
}
#ifdef MARK_BIT_PER_GRANULE
  GC_INNER GC_bool GC_add_map_entry(size_t granules)
  {
    unsigned displ;
    unsigned short * new_map;
    if (granules > BYTES_TO_GRANULES(MAXOBJBYTES)) granules = 0;
    if (GC_obj_map[granules] != 0) {
        return(TRUE);
    }
    new_map = (unsigned short *)GC_scratch_alloc(MAP_LEN * sizeof(short));
    if (new_map == 0) return(FALSE);
    GC_COND_LOG_PRINTF(
                "Adding block map for size of %u granules (%u bytes)\n",
                (unsigned)granules, (unsigned)GRANULES_TO_BYTES(granules));
    if (granules == 0) {
      for (displ = 0; displ < BYTES_TO_GRANULES(HBLKSIZE); displ++) {
        new_map[displ] = 1;
      }
    } else {
      for (displ = 0; displ < BYTES_TO_GRANULES(HBLKSIZE); displ++) {
        new_map[displ] = (unsigned short)(displ % granules);
      }
    }
    GC_obj_map[granules] = new_map;
    return(TRUE);
  }
#endif
GC_INNER void GC_initialize_offsets(void)
{
  unsigned i;
  if (GC_all_interior_pointers) {
    for (i = 0; i < VALID_OFFSET_SZ; ++i)
      GC_valid_offsets[i] = TRUE;
  } else {
    BZERO(GC_valid_offsets, sizeof(GC_valid_offsets));
    for (i = 0; i < sizeof(word); ++i)
      GC_modws_valid_offsets[i] = FALSE;
  }
}
STATIC void GC_CALLBACK GC_default_same_obj_print_proc(void * p, void * q)
{
    ABORT_ARG2("GC_same_obj test failed",
               ": %p and %p are not in the same object", p, q);
}
void (GC_CALLBACK *GC_same_obj_print_proc) (void *, void *)
                = GC_default_same_obj_print_proc;
GC_API void * GC_CALL GC_same_obj(void *p, void *q)
{
    struct hblk *h;
    hdr *hhdr;
    ptr_t base, limit;
    word sz;
    if (!EXPECT(GC_is_initialized, TRUE)) GC_init();
    hhdr = HDR((word)p);
    if (hhdr == 0) {
        if (divHBLKSZ((word)p) != divHBLKSZ((word)q)
            && HDR((word)q) != 0) {
            goto fail;
        }
        return(p);
    }
    if (IS_FORWARDING_ADDR_OR_NIL(hhdr)) {
        h = HBLKPTR(p) - (word)hhdr;
        hhdr = HDR(h);
        while (IS_FORWARDING_ADDR_OR_NIL(hhdr)) {
           h = FORWARDED_ADDR(h, hhdr);
           hhdr = HDR(h);
        }
        limit = (ptr_t)h + hhdr -> hb_sz;
        if ((word)p >= (word)limit || (word)q >= (word)limit
            || (word)q < (word)h) {
            goto fail;
        }
        return(p);
    }
    sz = hhdr -> hb_sz;
    if (sz > MAXOBJBYTES) {
      base = (ptr_t)HBLKPTR(p);
      limit = base + sz;
      if ((word)p >= (word)limit) {
        goto fail;
      }
    } else {
      size_t offset;
      size_t pdispl = HBLKDISPL(p);
      offset = pdispl % sz;
      if (HBLKPTR(p) != HBLKPTR(q)) goto fail;
      base = (ptr_t)p - offset;
      limit = base + sz;
    }
    if ((word)q >= (word)limit || (word)q < (word)base) {
        goto fail;
    }
    return(p);
fail:
    (*GC_same_obj_print_proc)((ptr_t)p, (ptr_t)q);
    return(p);
}
STATIC void GC_CALLBACK GC_default_is_valid_displacement_print_proc (void *p)
{
    ABORT_ARG1("GC_is_valid_displacement test failed", ": %p not valid", p);
}
void (GC_CALLBACK *GC_is_valid_displacement_print_proc)(void *) =
        GC_default_is_valid_displacement_print_proc;
GC_API void * GC_CALL GC_is_valid_displacement(void *p)
{
    hdr *hhdr;
    word pdispl;
    word offset;
    struct hblk *h;
    word sz;
    if (!EXPECT(GC_is_initialized, TRUE)) GC_init();
    hhdr = HDR((word)p);
    if (hhdr == 0) return(p);
    h = HBLKPTR(p);
    if (GC_all_interior_pointers) {
        while (IS_FORWARDING_ADDR_OR_NIL(hhdr)) {
           h = FORWARDED_ADDR(h, hhdr);
           hhdr = HDR(h);
        }
    } else if (IS_FORWARDING_ADDR_OR_NIL(hhdr)) {
        goto fail;
    }
    sz = hhdr -> hb_sz;
    pdispl = HBLKDISPL(p);
    offset = pdispl % sz;
    if ((sz > MAXOBJBYTES && (word)p >= (word)h + sz)
        || !GC_valid_offsets[offset]
        || ((word)p + (sz - offset) > (word)(h + 1)
            && !IS_FORWARDING_ADDR_OR_NIL(HDR(h + 1)))) {
        goto fail;
    }
    return(p);
fail:
    (*GC_is_valid_displacement_print_proc)((ptr_t)p);
    return(p);
}
STATIC void GC_CALLBACK GC_default_is_visible_print_proc(void * p)
{
    ABORT_ARG1("GC_is_visible test failed", ": %p not GC-visible", p);
}
void (GC_CALLBACK *GC_is_visible_print_proc)(void * p) =
                GC_default_is_visible_print_proc;
#ifndef THREADS
   STATIC GC_bool GC_on_stack(void *p)
   {
#ifdef STACK_GROWS_DOWN
       if ((word)p >= (word)GC_approx_sp()
           && (word)p < (word)GC_stackbottom) {
         return(TRUE);
       }
#else
       if ((word)p <= (word)GC_approx_sp()
           && (word)p > (word)GC_stackbottom) {
         return(TRUE);
       }
#endif
     return(FALSE);
   }
#endif
GC_API void * GC_CALL GC_is_visible(void *p)
{
    hdr *hhdr;
    if ((word)p & (ALIGNMENT - 1)) goto fail;
    if (!EXPECT(GC_is_initialized, TRUE)) GC_init();
#ifdef THREADS
        hhdr = HDR((word)p);
        if (hhdr != 0 && GC_base(p) == 0) {
            goto fail;
        } else {
            return(p);
        }
#else
          if (GC_on_stack(p)) return(p);
        hhdr = HDR((word)p);
        if (hhdr == 0) {
            if (GC_is_static_root(p)) return(p);
#if defined(DYNAMIC_LOADING) || defined(MSWIN32) \
                || defined(MSWINCE) || defined(CYGWIN32) || defined(PCR)
              GC_register_dynamic_libraries();
              if (GC_is_static_root(p))
                return(p);
#endif
            goto fail;
        } else {
            word descr;
            ptr_t base = (ptr_t)GC_base(p);
            if (NULL == base) goto fail;
            if (HBLKPTR(base) != HBLKPTR(p))
                hhdr = HDR(base);
            descr = hhdr -> hb_descr;
    retry:
            switch(descr & GC_DS_TAGS) {
                case GC_DS_LENGTH:
                    if ((word)p - (word)base > descr) goto fail;
                    break;
                case GC_DS_BITMAP:
                    if ((word)p - (word)base >= WORDS_TO_BYTES(BITMAP_BITS)
                        || ((word)p & (sizeof(word) - 1))) goto fail;
                    if (!(((word)1 << (WORDSZ - ((ptr_t)p - (ptr_t)base) - 1))
                          & descr)) goto fail;
                    break;
                case GC_DS_PROC:
                    break;
                case GC_DS_PER_OBJECT:
                    if ((signed_word)descr >= 0) {
                      descr = *(word *)((ptr_t)base + (descr & ~GC_DS_TAGS));
                    } else {
                      ptr_t type_descr = *(ptr_t *)base;
                      descr = *(word *)(type_descr
                                        - (descr - (word)(GC_DS_PER_OBJECT
                                           - GC_INDIR_PER_OBJ_BIAS)));
                    }
                    goto retry;
            }
            return(p);
        }
#endif
fail:
    (*GC_is_visible_print_proc)((ptr_t)p);
    return(p);
}
GC_API void * GC_CALL GC_pre_incr (void **p, ptrdiff_t how_much)
{
    void * initial = *p;
    void * result = GC_same_obj((void *)((ptr_t)initial + how_much), initial);
    if (!GC_all_interior_pointers) {
        (void) GC_is_valid_displacement(result);
    }
    return (*p = result);
}
GC_API void * GC_CALL GC_post_incr (void **p, ptrdiff_t how_much)
{
    void * initial = *p;
    void * result = GC_same_obj((void *)((ptr_t)initial + how_much), initial);
    if (!GC_all_interior_pointers) {
        (void) GC_is_valid_displacement(result);
    }
    *p = result;
    return(initial);
}
#ifndef GC_INLINE_H
#define GC_INLINE_H
#if GC_GNUC_PREREQ(3, 0)
#define GC_EXPECT(expr, outcome) __builtin_expect(expr,outcome)
#else
#define GC_EXPECT(expr, outcome) (expr)
#endif
#ifndef GC_ASSERT
#ifdef NDEBUG
#define GC_ASSERT(expr)
#else
#include <assert.h>
#define GC_ASSERT(expr) assert(expr)
#endif
#endif
#ifdef __cplusplus
  extern "C" {
#endif
#ifndef GC_PREFETCH_FOR_WRITE
#if GC_GNUC_PREREQ(3, 0) && !defined(GC_NO_PREFETCH_FOR_WRITE)
#define GC_PREFETCH_FOR_WRITE(x) __builtin_prefetch((x), 1)
#else
#define GC_PREFETCH_FOR_WRITE(x) (void)0
#endif
#endif
#define GC_I_PTRFREE 0
#define GC_I_NORMAL 1
GC_API void GC_CALL GC_generic_malloc_many(size_t , int ,
                                           void ** );
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
        GC_malloc_kind(size_t , int );
#ifdef GC_THREADS
  GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
        GC_malloc_kind_global(size_t , int );
#else
#define GC_malloc_kind_global GC_malloc_kind
#endif
#if defined(GC_THREADS) && defined(AO_HAVE_store)
#define GC_FAST_M_AO_STORE(my_fl, next) \
                AO_store((volatile AO_t *)(my_fl), (AO_t)(next))
#else
#define GC_FAST_M_AO_STORE(my_fl, next) (void)(*(my_fl) = (next))
#endif
#define GC_FAST_MALLOC_GRANS(result,granules,tiny_fl,num_direct, \
                              kind,default_expr,init) \
  do { \
    if (GC_EXPECT((granules) >= GC_TINY_FREELISTS,0)) { \
        result = (default_expr); \
    } else { \
        void **my_fl = (tiny_fl) + (granules); \
        void *my_entry=*my_fl; \
        void *next; \
    \
        for (;;) { \
            if (GC_EXPECT((GC_word)my_entry \
                          > (num_direct) + GC_TINY_FREELISTS + 1, 1)) { \
                next = *(void **)(my_entry); \
                result = (void *)my_entry; \
                GC_FAST_M_AO_STORE(my_fl, next); \
                init; \
                GC_PREFETCH_FOR_WRITE(next); \
                if ((kind) != GC_I_PTRFREE) { \
                    GC_end_stubborn_change(my_fl); \
                    GC_reachable_here(next); \
                } \
                GC_ASSERT(GC_size(result) >= (granules)*GC_GRANULE_BYTES); \
                GC_ASSERT((kind) == GC_I_PTRFREE \
                          || ((GC_word *)result)[1] == 0); \
                break; \
            } \
             \
            if ((GC_signed_word)my_entry - (GC_signed_word)(num_direct) <= 0 \
                     \
                    && my_entry != 0 ) { \
                 \
                GC_FAST_M_AO_STORE(my_fl, (char *)my_entry \
                                          + (granules) + 1); \
                result = (default_expr); \
                break; \
            } else { \
                 \
                GC_generic_malloc_many(((granules) == 0? GC_GRANULE_BYTES : \
                                        GC_RAW_BYTES_FROM_INDEX(granules)), \
                                       kind, my_fl); \
                my_entry = *my_fl; \
                if (my_entry == 0) { \
                    result = (*GC_get_oom_fn())((granules)*GC_GRANULE_BYTES); \
                    break; \
                } \
            } \
        } \
    } \
  } while (0)
#define GC_WORDS_TO_WHOLE_GRANULES(n) \
        GC_WORDS_TO_GRANULES((n) + GC_GRANULE_WORDS - 1)
#define GC_MALLOC_WORDS_KIND(result,n,tiny_fl,kind,init) \
    do { \
      size_t granules = GC_WORDS_TO_WHOLE_GRANULES(n); \
      GC_FAST_MALLOC_GRANS(result, granules, tiny_fl, 0, kind, \
                           GC_malloc_kind(granules*GC_GRANULE_BYTES, kind), \
                           init); \
    } while (0)
#define GC_MALLOC_WORDS(result,n,tiny_fl) \
        GC_MALLOC_WORDS_KIND(result, n, tiny_fl, GC_I_NORMAL, \
                             *(void **)(result) = 0)
#define GC_MALLOC_ATOMIC_WORDS(result,n,tiny_fl) \
        GC_MALLOC_WORDS_KIND(result, n, tiny_fl, GC_I_PTRFREE, (void)0)
#define GC_CONS(result, first, second, tiny_fl) \
    do { \
      void *l = (void *)(first); \
      void *r = (void *)(second); \
      GC_MALLOC_WORDS_KIND(result, 2, tiny_fl, GC_I_NORMAL, (void)0); \
      if ((result) != 0 ) { \
        *(void **)(result) = l; \
        GC_PTR_STORE_AND_DIRTY((void **)(result) + 1, r); \
        GC_reachable_here(l); \
      } \
    } while (0)
GC_API void GC_CALL GC_print_free_list(int ,
                                       size_t );
#ifdef __cplusplus
  }
#endif
#endif
#include <stdio.h>
#ifdef GC_USE_ENTIRE_HEAP
  int GC_use_entire_heap = TRUE;
#else
  int GC_use_entire_heap = FALSE;
#endif
#define MAX_BLACK_LIST_ALLOC (2*HBLKSIZE)
#define UNIQUE_THRESHOLD 32
#define HUGE_THRESHOLD 256
#define FL_COMPRESSION 8
#define N_HBLK_FLS ((HUGE_THRESHOLD - UNIQUE_THRESHOLD) / FL_COMPRESSION \
                     + UNIQUE_THRESHOLD)
#ifndef GC_GCJ_SUPPORT
  STATIC
#endif
  struct hblk * GC_hblkfreelist[N_HBLK_FLS+1] = { 0 };
#ifndef GC_GCJ_SUPPORT
  STATIC
#endif
  word GC_free_bytes[N_HBLK_FLS+1] = { 0 };
GC_INLINE int GC_enough_large_bytes_left(void)
{
    int n;
    word bytes = GC_large_allocd_bytes;
    GC_ASSERT(GC_max_large_allocd_bytes <= GC_heapsize);
    for (n = N_HBLK_FLS; n >= 0; --n) {
        bytes += GC_free_bytes[n];
        if (bytes >= GC_max_large_allocd_bytes) return n;
    }
    return 0;
}
STATIC int GC_hblk_fl_from_blocks(word blocks_needed)
{
    if (blocks_needed <= UNIQUE_THRESHOLD) return (int)blocks_needed;
    if (blocks_needed >= HUGE_THRESHOLD) return N_HBLK_FLS;
    return (int)(blocks_needed - UNIQUE_THRESHOLD)/FL_COMPRESSION
                                        + UNIQUE_THRESHOLD;
}
#define PHDR(hhdr) HDR((hhdr) -> hb_prev)
#define NHDR(hhdr) HDR((hhdr) -> hb_next)
#ifdef USE_MUNMAP
#define IS_MAPPED(hhdr) (((hhdr) -> hb_flags & WAS_UNMAPPED) == 0)
#else
#define IS_MAPPED(hhdr) TRUE
#endif
#if !defined(NO_DEBUGGING) || defined(GC_ASSERTIONS)
  GC_INNER word GC_compute_large_free_bytes(void)
  {
      word total_free = 0;
      unsigned i;
      for (i = 0; i <= N_HBLK_FLS; ++i) {
        struct hblk * h;
        hdr * hhdr;
        for (h = GC_hblkfreelist[i]; h != 0; h = hhdr->hb_next) {
          hhdr = HDR(h);
          total_free += hhdr->hb_sz;
        }
      }
      return total_free;
  }
#endif
#if !defined(NO_DEBUGGING)
void GC_print_hblkfreelist(void)
{
    unsigned i;
    word total;
    for (i = 0; i <= N_HBLK_FLS; ++i) {
      struct hblk * h = GC_hblkfreelist[i];
      if (0 != h) GC_printf("Free list %u (total size %lu):\n",
                            i, (unsigned long)GC_free_bytes[i]);
      while (h ) {
        hdr * hhdr = HDR(h);
        GC_printf("\t%p size %lu %s black listed\n",
                (void *)h, (unsigned long) hhdr -> hb_sz,
                GC_is_black_listed(h, HBLKSIZE) != 0 ? "start" :
                GC_is_black_listed(h, hhdr -> hb_sz) != 0 ? "partially" :
                                                        "not");
        h = hhdr -> hb_next;
      }
    }
    GC_printf("GC_large_free_bytes: %lu\n",
              (unsigned long)GC_large_free_bytes);
    if ((total = GC_compute_large_free_bytes()) != GC_large_free_bytes)
          GC_err_printf("GC_large_free_bytes INCONSISTENT!! Should be: %lu\n",
                        (unsigned long)total);
}
static int free_list_index_of(hdr *wanted)
{
    int i;
    for (i = 0; i <= N_HBLK_FLS; ++i) {
      struct hblk * h;
      hdr * hhdr;
      for (h = GC_hblkfreelist[i]; h != 0; h = hhdr -> hb_next) {
        hhdr = HDR(h);
        if (hhdr == wanted) return i;
      }
    }
    return -1;
}
GC_API void GC_CALL GC_dump_regions(void)
{
    unsigned i;
    for (i = 0; i < GC_n_heap_sects; ++i) {
        ptr_t start = GC_heap_sects[i].hs_start;
        size_t bytes = GC_heap_sects[i].hs_bytes;
        ptr_t end = start + bytes;
        ptr_t p;
          while (i+1 < GC_n_heap_sects && GC_heap_sects[i+1].hs_start == end) {
            ++i;
            end = GC_heap_sects[i].hs_start + GC_heap_sects[i].hs_bytes;
          }
        GC_printf("***Section from %p to %p\n", (void *)start, (void *)end);
        for (p = start; (word)p < (word)end; ) {
            hdr *hhdr = HDR(p);
            if (IS_FORWARDING_ADDR_OR_NIL(hhdr)) {
                GC_printf("\t%p Missing header!!(%p)\n",
                          (void *)p, (void *)hhdr);
                p += HBLKSIZE;
                continue;
            }
            if (HBLK_IS_FREE(hhdr)) {
                int correct_index = GC_hblk_fl_from_blocks(
                                        divHBLKSZ(hhdr -> hb_sz));
                int actual_index;
                GC_printf("\t%p\tfree block of size 0x%lx bytes%s\n",
                          (void *)p, (unsigned long)(hhdr -> hb_sz),
                          IS_MAPPED(hhdr) ? "" : " (unmapped)");
                actual_index = free_list_index_of(hhdr);
                if (-1 == actual_index) {
                    GC_printf("\t\tBlock not on free list %d!!\n",
                              correct_index);
                } else if (correct_index != actual_index) {
                    GC_printf("\t\tBlock on list %d, should be on %d!!\n",
                              actual_index, correct_index);
                }
                p += hhdr -> hb_sz;
            } else {
                GC_printf("\t%p\tused for blocks of size 0x%lx bytes\n",
                          (void *)p, (unsigned long)(hhdr -> hb_sz));
                p += HBLKSIZE * OBJ_SZ_TO_BLOCKS(hhdr -> hb_sz);
            }
        }
    }
}
#endif
static GC_bool setup_header(hdr * hhdr, struct hblk *block, size_t byte_sz,
                            int kind, unsigned flags)
{
    word descr;
#ifdef MARK_BIT_PER_GRANULE
      if (byte_sz > MAXOBJBYTES)
        flags |= LARGE_BLOCK;
#endif
#ifdef ENABLE_DISCLAIM
      if (GC_obj_kinds[kind].ok_disclaim_proc)
        flags |= HAS_DISCLAIM;
      if (GC_obj_kinds[kind].ok_mark_unconditionally)
        flags |= MARK_UNCONDITIONALLY;
#endif
      hhdr -> hb_sz = byte_sz;
      hhdr -> hb_obj_kind = (unsigned char)kind;
      hhdr -> hb_flags = (unsigned char)flags;
      hhdr -> hb_block = block;
      descr = GC_obj_kinds[kind].ok_descriptor;
      if (GC_obj_kinds[kind].ok_relocate_descr) descr += byte_sz;
      hhdr -> hb_descr = descr;
#ifdef MARK_BIT_PER_OBJ
      if (byte_sz > MAXOBJBYTES) {
         hhdr -> hb_inv_sz = LARGE_INV_SZ;
      } else {
        word inv_sz;
#if CPP_WORDSZ == 64
          inv_sz = ((word)1 << 32)/byte_sz;
          if (((inv_sz*byte_sz) >> 32) == 0) ++inv_sz;
#else
          GC_ASSERT(byte_sz >= 4);
          inv_sz = ((unsigned)1 << 31)/byte_sz;
          inv_sz *= 2;
          while (inv_sz*byte_sz > byte_sz) ++inv_sz;
#endif
#ifdef INV_SZ_COMPUTATION_CHECK
          GC_ASSERT(((1ULL << 32) + byte_sz - 1) / byte_sz == inv_sz);
#endif
        hhdr -> hb_inv_sz = inv_sz;
      }
#endif
#ifdef MARK_BIT_PER_GRANULE
    {
      size_t granules = BYTES_TO_GRANULES(byte_sz);
      if (EXPECT(!GC_add_map_entry(granules), FALSE)) {
        hhdr -> hb_sz = HBLKSIZE;
        hhdr -> hb_descr = 0;
        hhdr -> hb_flags |= LARGE_BLOCK;
        hhdr -> hb_map = 0;
        return FALSE;
      }
      hhdr -> hb_map = GC_obj_map[(hhdr -> hb_flags & LARGE_BLOCK) != 0 ?
                                    0 : granules];
    }
#endif
    GC_clear_hdr_marks(hhdr);
    hhdr -> hb_last_reclaimed = (unsigned short)GC_gc_no;
    return(TRUE);
}
STATIC void GC_remove_from_fl_at(hdr *hhdr, int index)
{
    GC_ASSERT(((hhdr -> hb_sz) & (HBLKSIZE-1)) == 0);
    if (hhdr -> hb_prev == 0) {
        GC_ASSERT(HDR(GC_hblkfreelist[index]) == hhdr);
        GC_hblkfreelist[index] = hhdr -> hb_next;
    } else {
        hdr *phdr;
        GET_HDR(hhdr -> hb_prev, phdr);
        phdr -> hb_next = hhdr -> hb_next;
    }
    GC_ASSERT(GC_free_bytes[index] >= hhdr -> hb_sz);
    GC_free_bytes[index] -= hhdr -> hb_sz;
    if (0 != hhdr -> hb_next) {
        hdr * nhdr;
        GC_ASSERT(!IS_FORWARDING_ADDR_OR_NIL(NHDR(hhdr)));
        GET_HDR(hhdr -> hb_next, nhdr);
        nhdr -> hb_prev = hhdr -> hb_prev;
    }
}
GC_INLINE void GC_remove_from_fl(hdr *hhdr)
{
  GC_remove_from_fl_at(hhdr, GC_hblk_fl_from_blocks(divHBLKSZ(hhdr->hb_sz)));
}
static struct hblk * get_block_ending_at(struct hblk *h)
{
    struct hblk * p = h - 1;
    hdr * phdr;
    GET_HDR(p, phdr);
    while (0 != phdr && IS_FORWARDING_ADDR_OR_NIL(phdr)) {
        p = FORWARDED_ADDR(p,phdr);
        phdr = HDR(p);
    }
    if (0 != phdr) {
        return p;
    }
    p = GC_prev_block(h - 1);
    if (p) {
        phdr = HDR(p);
        if ((ptr_t)p + phdr -> hb_sz == (ptr_t)h) {
            return p;
        }
    }
    return NULL;
}
STATIC struct hblk * GC_free_block_ending_at(struct hblk *h)
{
    struct hblk * p = get_block_ending_at(h);
    if (p ) {
      hdr * phdr = HDR(p);
      if (HBLK_IS_FREE(phdr)) {
        return p;
      }
    }
    return 0;
}
STATIC void GC_add_to_fl(struct hblk *h, hdr *hhdr)
{
    int index = GC_hblk_fl_from_blocks(divHBLKSZ(hhdr -> hb_sz));
    struct hblk *second = GC_hblkfreelist[index];
#if defined(GC_ASSERTIONS) && !defined(USE_MUNMAP)
      struct hblk *next = (struct hblk *)((word)h + hhdr -> hb_sz);
      hdr * nexthdr = HDR(next);
      struct hblk *prev = GC_free_block_ending_at(h);
      hdr * prevhdr = HDR(prev);
      GC_ASSERT(nexthdr == 0 || !HBLK_IS_FREE(nexthdr)
                || (GC_heapsize & SIGNB) != 0);
      GC_ASSERT(prev == 0 || !HBLK_IS_FREE(prevhdr)
                || (GC_heapsize & SIGNB) != 0);
#endif
    GC_ASSERT(((hhdr -> hb_sz) & (HBLKSIZE-1)) == 0);
    GC_hblkfreelist[index] = h;
    GC_free_bytes[index] += hhdr -> hb_sz;
    GC_ASSERT(GC_free_bytes[index] <= GC_large_free_bytes);
    hhdr -> hb_next = second;
    hhdr -> hb_prev = 0;
    if (second ) {
      hdr * second_hdr;
      GET_HDR(second, second_hdr);
      second_hdr -> hb_prev = h;
    }
    hhdr -> hb_flags |= FREE_BLK;
}
#ifdef USE_MUNMAP
#ifndef MUNMAP_THRESHOLD
#define MUNMAP_THRESHOLD 6
#endif
GC_INNER int GC_unmap_threshold = MUNMAP_THRESHOLD;
#ifdef COUNT_UNMAPPED_REGIONS
  static int calc_num_unmapped_regions_delta(struct hblk *h, hdr *hhdr)
  {
    struct hblk * prev = get_block_ending_at(h);
    struct hblk * next;
    GC_bool prev_unmapped = FALSE;
    GC_bool next_unmapped = FALSE;
    next = GC_next_block((struct hblk *)((ptr_t)h + hhdr->hb_sz), TRUE);
    if ((ptr_t)next != GC_unmap_end((ptr_t)h, (size_t)hhdr->hb_sz)) {
      next = NULL;
    }
    if (prev != NULL) {
      hdr * prevhdr = HDR(prev);
      prev_unmapped = !IS_MAPPED(prevhdr);
    }
    if (next != NULL) {
      hdr * nexthdr = HDR(next);
      next_unmapped = !IS_MAPPED(nexthdr);
    }
    if (prev_unmapped && next_unmapped) {
      return IS_MAPPED(hhdr) ? -1 : 1;
    }
    if (!prev_unmapped && !next_unmapped) {
      return IS_MAPPED(hhdr) ? 1 : -1;
    }
    return 0;
  }
#endif
GC_INLINE void GC_adjust_num_unmapped(struct hblk *h GC_ATTR_UNUSED,
                                      hdr *hhdr GC_ATTR_UNUSED)
{
#ifdef COUNT_UNMAPPED_REGIONS
    GC_num_unmapped_regions += calc_num_unmapped_regions_delta(h, hhdr);
#endif
}
GC_INNER void GC_unmap_old(void)
{
    int i;
    if (GC_unmap_threshold == 0)
      return;
#ifdef COUNT_UNMAPPED_REGIONS
    if (GC_num_unmapped_regions >= GC_UNMAPPED_REGIONS_SOFT_LIMIT)
      return;
#endif
    for (i = 0; i <= N_HBLK_FLS; ++i) {
      struct hblk * h;
      hdr * hhdr;
      for (h = GC_hblkfreelist[i]; 0 != h; h = hhdr -> hb_next) {
        hhdr = HDR(h);
        if (!IS_MAPPED(hhdr)) continue;
        if ((unsigned short)(GC_gc_no - hhdr->hb_last_reclaimed) >
                (unsigned short)GC_unmap_threshold) {
#ifdef COUNT_UNMAPPED_REGIONS
            int delta = calc_num_unmapped_regions_delta(h, hhdr);
            signed_word regions = GC_num_unmapped_regions + delta;
            if (delta >= 0 && regions >= GC_UNMAPPED_REGIONS_SOFT_LIMIT) {
              GC_COND_LOG_PRINTF("Unmapped regions limit reached!\n");
              return;
            }
            GC_num_unmapped_regions = regions;
#endif
          GC_unmap((ptr_t)h, (size_t)hhdr->hb_sz);
          hhdr -> hb_flags |= WAS_UNMAPPED;
        }
      }
    }
}
GC_INNER void GC_merge_unmapped(void)
{
    int i;
    for (i = 0; i <= N_HBLK_FLS; ++i) {
      struct hblk *h = GC_hblkfreelist[i];
      while (h != 0) {
        struct hblk *next;
        hdr *hhdr, *nexthdr;
        word size, nextsize;
        GET_HDR(h, hhdr);
        size = hhdr->hb_sz;
        next = (struct hblk *)((word)h + size);
        GET_HDR(next, nexthdr);
          if (0 != nexthdr && HBLK_IS_FREE(nexthdr)
              && (signed_word) (size + (nextsize = nexthdr->hb_sz)) > 0
                 ) {
            if (IS_MAPPED(hhdr) && !IS_MAPPED(nexthdr)) {
                if (size > nextsize) {
                  GC_adjust_num_unmapped(next, nexthdr);
                  GC_remap((ptr_t)next, nextsize);
                } else {
                  GC_adjust_num_unmapped(h, hhdr);
                  GC_unmap((ptr_t)h, size);
                  GC_unmap_gap((ptr_t)h, size, (ptr_t)next, nextsize);
                  hhdr -> hb_flags |= WAS_UNMAPPED;
                }
            } else if (IS_MAPPED(nexthdr) && !IS_MAPPED(hhdr)) {
              if (size > nextsize) {
                GC_adjust_num_unmapped(next, nexthdr);
                GC_unmap((ptr_t)next, nextsize);
                GC_unmap_gap((ptr_t)h, size, (ptr_t)next, nextsize);
              } else {
                GC_adjust_num_unmapped(h, hhdr);
                GC_remap((ptr_t)h, size);
                hhdr -> hb_flags &= ~WAS_UNMAPPED;
                hhdr -> hb_last_reclaimed = nexthdr -> hb_last_reclaimed;
              }
            } else if (!IS_MAPPED(hhdr) && !IS_MAPPED(nexthdr)) {
                GC_unmap_gap((ptr_t)h, size, (ptr_t)next, nextsize);
            }
            GC_remove_from_fl_at(hhdr, i);
            GC_remove_from_fl(nexthdr);
            hhdr -> hb_sz += nexthdr -> hb_sz;
            GC_remove_header(next);
            GC_add_to_fl(h, hhdr);
            h = GC_hblkfreelist[i];
          } else  {
            h = hhdr -> hb_next;
          }
      }
    }
}
#endif
STATIC struct hblk * GC_get_first_part(struct hblk *h, hdr *hhdr,
                                       size_t bytes, int index)
{
    word total_size = hhdr -> hb_sz;
    struct hblk * rest;
    hdr * rest_hdr;
    GC_ASSERT((total_size & (HBLKSIZE-1)) == 0);
    GC_remove_from_fl_at(hhdr, index);
    if (total_size == bytes) return h;
    rest = (struct hblk *)((word)h + bytes);
    rest_hdr = GC_install_header(rest);
    if (0 == rest_hdr) {
        WARN("Header allocation failed: dropping block\n", 0);
        return(0);
    }
    rest_hdr -> hb_sz = total_size - bytes;
    rest_hdr -> hb_flags = 0;
#ifdef GC_ASSERTIONS
        hhdr -> hb_flags &= ~FREE_BLK;
#endif
    GC_add_to_fl(rest, rest_hdr);
    return h;
}
STATIC void GC_split_block(struct hblk *h, hdr *hhdr, struct hblk *n,
                           hdr *nhdr, int index )
{
    word total_size = hhdr -> hb_sz;
    word h_size = (word)n - (word)h;
    struct hblk *prev = hhdr -> hb_prev;
    struct hblk *next = hhdr -> hb_next;
      nhdr -> hb_prev = prev;
      nhdr -> hb_next = next;
      nhdr -> hb_sz = total_size - h_size;
      nhdr -> hb_flags = 0;
      if (prev ) {
        HDR(prev) -> hb_next = n;
      } else {
        GC_hblkfreelist[index] = n;
      }
      if (next ) {
        HDR(next) -> hb_prev = n;
      }
      GC_ASSERT(GC_free_bytes[index] > h_size);
      GC_free_bytes[index] -= h_size;
#ifdef USE_MUNMAP
      hhdr -> hb_last_reclaimed = (unsigned short)GC_gc_no;
#endif
    hhdr -> hb_sz = h_size;
    GC_add_to_fl(h, hhdr);
    nhdr -> hb_flags |= FREE_BLK;
}
STATIC struct hblk *
GC_allochblk_nth(size_t sz , int kind, unsigned flags, int n,
                 int may_split);
#define AVOID_SPLIT_REMAPPED 2
GC_INNER struct hblk *
GC_allochblk(size_t sz, int kind, unsigned flags)
{
    word blocks;
    int start_list;
    struct hblk *result;
    int may_split;
    int split_limit;
    GC_ASSERT(I_HOLD_LOCK());
    GC_ASSERT((sz & (GRANULE_BYTES - 1)) == 0);
    blocks = OBJ_SZ_TO_BLOCKS_CHECKED(sz);
    if ((signed_word)(blocks * HBLKSIZE) < 0) {
      return 0;
    }
    start_list = GC_hblk_fl_from_blocks(blocks);
    result = GC_allochblk_nth(sz, kind, flags, start_list, FALSE);
    if (0 != result) return result;
    may_split = TRUE;
    if (GC_use_entire_heap || GC_dont_gc
        || USED_HEAP_SIZE < GC_requested_heapsize
        || GC_incremental || !GC_should_collect()) {
        split_limit = N_HBLK_FLS;
    } else if (GC_finalizer_bytes_freed > (GC_heapsize >> 4)) {
          split_limit = 0;
    } else {
          split_limit = GC_enough_large_bytes_left();
#ifdef USE_MUNMAP
            if (split_limit > 0)
              may_split = AVOID_SPLIT_REMAPPED;
#endif
    }
    if (start_list < UNIQUE_THRESHOLD) {
      ++start_list;
    }
    for (; start_list <= split_limit; ++start_list) {
        result = GC_allochblk_nth(sz, kind, flags, start_list, may_split);
        if (0 != result)
            break;
    }
    return result;
}
STATIC long GC_large_alloc_warn_suppressed = 0;
STATIC struct hblk *
GC_allochblk_nth(size_t sz, int kind, unsigned flags, int n, int may_split)
{
    struct hblk *hbp;
    hdr * hhdr;
    struct hblk *thishbp;
    hdr * thishdr;
    signed_word size_needed = HBLKSIZE * OBJ_SZ_TO_BLOCKS_CHECKED(sz);
        for (hbp = GC_hblkfreelist[n];; hbp = hhdr -> hb_next) {
            signed_word size_avail;
            if (hbp ) {
            } else {
              return NULL;
            }
            GET_HDR(hbp, hhdr);
            size_avail = (signed_word)hhdr->hb_sz;
            if (size_avail < size_needed) continue;
            if (size_avail != size_needed) {
              if (!may_split) continue;
              thishbp = hhdr -> hb_next;
              if (thishbp ) {
                signed_word next_size;
                GET_HDR(thishbp, thishdr);
                next_size = (signed_word)(thishdr -> hb_sz);
                if (next_size < size_avail
                    && next_size >= size_needed
                    && !GC_is_black_listed(thishbp, (word)size_needed)) {
                    continue;
                }
              }
            }
            if (!IS_UNCOLLECTABLE(kind) && (kind != PTRFREE
                        || size_needed > (signed_word)MAX_BLACK_LIST_ALLOC)) {
              struct hblk * lasthbp = hbp;
              ptr_t search_end = (ptr_t)hbp + size_avail - size_needed;
              signed_word orig_avail = size_avail;
              signed_word eff_size_needed = (flags & IGNORE_OFF_PAGE) != 0 ?
                                                (signed_word)HBLKSIZE
                                                : size_needed;
              while ((word)lasthbp <= (word)search_end
                     && (thishbp = GC_is_black_listed(lasthbp,
                                            (word)eff_size_needed)) != 0) {
                lasthbp = thishbp;
              }
              size_avail -= (ptr_t)lasthbp - (ptr_t)hbp;
              thishbp = lasthbp;
              if (size_avail >= size_needed) {
                if (thishbp != hbp) {
#ifdef USE_MUNMAP
                    if (may_split == AVOID_SPLIT_REMAPPED && !IS_MAPPED(hhdr))
                      continue;
#endif
                  thishdr = GC_install_header(thishbp);
                  if (0 != thishdr) {
#ifdef USE_MUNMAP
                      if (!IS_MAPPED(hhdr)) {
                        GC_adjust_num_unmapped(hbp, hhdr);
                        GC_remap((ptr_t)hbp, (size_t)hhdr->hb_sz);
                        hhdr -> hb_flags &= ~WAS_UNMAPPED;
                      }
#endif
                      GC_split_block(hbp, hhdr, thishbp, thishdr, n);
                      hbp = thishbp;
                      hhdr = thishdr;
                  }
                }
              } else if (size_needed > (signed_word)BL_LIMIT
                         && orig_avail - size_needed
                            > (signed_word)BL_LIMIT) {
                if (++GC_large_alloc_warn_suppressed
                    >= GC_large_alloc_warn_interval) {
                  WARN("Repeated allocation of very large block "
                       "(appr. size %" WARN_PRIdPTR "):\n"
                       "\tMay lead to memory leak and poor performance\n",
                       size_needed);
                  GC_large_alloc_warn_suppressed = 0;
                }
                size_avail = orig_avail;
              } else if (size_avail == 0
                         && size_needed == (signed_word)HBLKSIZE
                         && IS_MAPPED(hhdr)) {
                if (!GC_find_leak) {
                  static unsigned count = 0;
                  if ((++count & 3) == 0) {
                      word total_size = hhdr -> hb_sz;
                      struct hblk * limit = hbp + divHBLKSZ(total_size);
                      struct hblk * h;
                      struct hblk * prev = hhdr -> hb_prev;
                      GC_large_free_bytes -= total_size;
                      GC_bytes_dropped += total_size;
                      GC_remove_from_fl_at(hhdr, n);
                      for (h = hbp; (word)h < (word)limit; h++) {
                        if (h != hbp) {
                          hhdr = GC_install_header(h);
                        }
                        if (NULL != hhdr) {
                          (void)setup_header(hhdr, h, HBLKSIZE, PTRFREE, 0);
                          if (GC_debugging_started) {
                            BZERO(h, HBLKSIZE);
                          }
                        }
                      }
                      hbp = prev;
                      if (0 == hbp) {
                        return GC_allochblk_nth(sz, kind, flags, n, may_split);
                      }
                      hhdr = HDR(hbp);
                  }
                }
              }
            }
            if( size_avail >= size_needed ) {
#ifdef USE_MUNMAP
                  if (!IS_MAPPED(hhdr)) {
                    GC_adjust_num_unmapped(hbp, hhdr);
                    GC_remap((ptr_t)hbp, (size_t)hhdr->hb_sz);
                    hhdr -> hb_flags &= ~WAS_UNMAPPED;
                  }
#endif
                hbp = GC_get_first_part(hbp, hhdr, size_needed, n);
                break;
            }
        }
    if (0 == hbp) return 0;
        if (!GC_install_counts(hbp, (word)size_needed)) return(0);
        if (!setup_header(hhdr, hbp, sz, kind, flags)) {
            GC_remove_counts(hbp, (word)size_needed);
            return(0);
        }
#ifndef GC_DISABLE_INCREMENTAL
        GC_ASSERT((size_needed & (HBLKSIZE-1)) == 0);
        GC_remove_protection(hbp, divHBLKSZ(size_needed),
                             (hhdr -> hb_descr == 0) );
#endif
    GC_fail_count = 0;
    GC_large_free_bytes -= size_needed;
    GC_ASSERT(IS_MAPPED(hhdr));
    return( hbp );
}
GC_INNER void GC_freehblk(struct hblk *hbp)
{
    struct hblk *next, *prev;
    hdr *hhdr, *prevhdr, *nexthdr;
    word size;
    GET_HDR(hbp, hhdr);
    size = HBLKSIZE * OBJ_SZ_TO_BLOCKS(hhdr->hb_sz);
    if ((size & SIGNB) != 0)
      ABORT("Deallocating excessively large block.  Too large an allocation?");
    GC_remove_counts(hbp, size);
    hhdr->hb_sz = size;
#ifdef USE_MUNMAP
      hhdr -> hb_last_reclaimed = (unsigned short)GC_gc_no;
#endif
      if (HBLK_IS_FREE(hhdr)) {
        ABORT_ARG1("Duplicate large block deallocation",
                   " of %p", (void *)hbp);
      }
    GC_ASSERT(IS_MAPPED(hhdr));
    hhdr -> hb_flags |= FREE_BLK;
    next = (struct hblk *)((ptr_t)hbp + size);
    GET_HDR(next, nexthdr);
    prev = GC_free_block_ending_at(hbp);
      if(0 != nexthdr && HBLK_IS_FREE(nexthdr) && IS_MAPPED(nexthdr)
         && (signed_word)(hhdr -> hb_sz + nexthdr -> hb_sz) > 0
         ) {
        GC_remove_from_fl(nexthdr);
        hhdr -> hb_sz += nexthdr -> hb_sz;
        GC_remove_header(next);
      }
      if (prev ) {
        prevhdr = HDR(prev);
        if (IS_MAPPED(prevhdr)
            && (signed_word)(hhdr -> hb_sz + prevhdr -> hb_sz) > 0) {
          GC_remove_from_fl(prevhdr);
          prevhdr -> hb_sz += hhdr -> hb_sz;
#ifdef USE_MUNMAP
            prevhdr -> hb_last_reclaimed = (unsigned short)GC_gc_no;
#endif
          GC_remove_header(hbp);
          hbp = prev;
          hhdr = prevhdr;
        }
      }
    GC_large_free_bytes += size;
    GC_add_to_fl(hbp, hhdr);
}
#include <stdio.h>
#if !defined(MACOS) && !defined(MSWINCE)
#include <signal.h>
#if !defined(GC_NO_TYPES) && !defined(SN_TARGET_PSP2) \
     && !defined(__CC_ARM)
#include <sys/types.h>
#endif
#endif
word GC_non_gc_bytes = 0;
word GC_gc_no = 0;
#ifndef NO_CLOCK
  static unsigned long full_gc_total_time = 0;
  static unsigned full_gc_total_ns_frac = 0;
  static GC_bool measure_performance = FALSE;
  GC_API void GC_CALL GC_start_performance_measurement(void)
  {
    measure_performance = TRUE;
  }
  GC_API unsigned long GC_CALL GC_get_full_gc_total_time(void)
  {
    return full_gc_total_time;
  }
#endif
#ifndef GC_DISABLE_INCREMENTAL
  GC_INNER GC_bool GC_incremental = FALSE;
  STATIC GC_bool GC_should_start_incremental_collection = FALSE;
#endif
GC_API int GC_CALL GC_is_incremental_mode(void)
{
  return (int)GC_incremental;
}
#ifdef THREADS
  int GC_parallel = FALSE;
#endif
#if defined(GC_FULL_FREQ) && !defined(CPPCHECK)
  int GC_full_freq = GC_FULL_FREQ;
#else
  int GC_full_freq = 19;
#endif
STATIC GC_bool GC_need_full_gc = FALSE;
#ifdef THREAD_LOCAL_ALLOC
  GC_INNER GC_bool GC_world_stopped = FALSE;
#endif
STATIC GC_bool GC_disable_automatic_collection = FALSE;
GC_API void GC_CALL GC_set_disable_automatic_collection(int value)
{
  DCL_LOCK_STATE;
  LOCK();
  GC_disable_automatic_collection = (GC_bool)value;
  UNLOCK();
}
GC_API int GC_CALL GC_get_disable_automatic_collection(void)
{
  int value;
  DCL_LOCK_STATE;
  LOCK();
  value = (int)GC_disable_automatic_collection;
  UNLOCK();
  return value;
}
STATIC word GC_used_heap_size_after_full = 0;
EXTERN_C_BEGIN
extern const char * const GC_copyright[];
EXTERN_C_END
const char * const GC_copyright[] =
{"Copyright 1988, 1989 Hans-J. Boehm and Alan J. Demers ",
"Copyright (c) 1991-1995 by Xerox Corporation.  All rights reserved. ",
"Copyright (c) 1996-1998 by Silicon Graphics.  All rights reserved. ",
"Copyright (c) 1999-2009 by Hewlett-Packard Company.  All rights reserved. ",
"Copyright (c) 2008-2021 Ivan Maidanski ",
"THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY",
" EXPRESSED OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.",
"See source code for details." };
#ifndef GC_NO_VERSION_VAR
  EXTERN_C_BEGIN
  extern const unsigned GC_version;
  EXTERN_C_END
  const unsigned GC_version = ((GC_VERSION_MAJOR << 16) |
                        (GC_VERSION_MINOR << 8) | GC_VERSION_MICRO);
#endif
GC_API unsigned GC_CALL GC_get_version(void)
{
  return (GC_VERSION_MAJOR << 16) | (GC_VERSION_MINOR << 8) |
          GC_VERSION_MICRO;
}
#ifdef GC_DONT_EXPAND
  int GC_dont_expand = TRUE;
#else
  int GC_dont_expand = FALSE;
#endif
#if defined(GC_FREE_SPACE_DIVISOR) && !defined(CPPCHECK)
  word GC_free_space_divisor = GC_FREE_SPACE_DIVISOR;
#else
  word GC_free_space_divisor = 3;
#endif
GC_INNER int GC_CALLBACK GC_never_stop_func(void)
{
  return(0);
}
#if defined(GC_TIME_LIMIT) && !defined(CPPCHECK)
  unsigned long GC_time_limit = GC_TIME_LIMIT;
#elif defined(PARALLEL_MARK)
  unsigned long GC_time_limit = GC_TIME_UNLIMITED;
#else
  unsigned long GC_time_limit = 50;
#endif
#ifndef NO_CLOCK
  STATIC unsigned long GC_time_lim_nsec = 0;
#define TV_NSEC_LIMIT (1000UL * 1000)
  GC_API void GC_CALL GC_set_time_limit_tv(struct GC_timeval_s tv)
  {
    GC_ASSERT(tv.tv_ms <= GC_TIME_UNLIMITED);
    GC_ASSERT(tv.tv_nsec < TV_NSEC_LIMIT);
    GC_time_limit = tv.tv_ms;
    GC_time_lim_nsec = tv.tv_nsec;
  }
  GC_API struct GC_timeval_s GC_CALL GC_get_time_limit_tv(void)
  {
    struct GC_timeval_s tv;
    tv.tv_ms = GC_time_limit;
    tv.tv_nsec = GC_time_lim_nsec;
    return tv;
  }
  STATIC CLOCK_TYPE GC_start_time = CLOCK_TYPE_INITIALIZER;
#endif
STATIC int GC_n_attempts = 0;
STATIC GC_stop_func GC_default_stop_func = GC_never_stop_func;
GC_API void GC_CALL GC_set_stop_func(GC_stop_func stop_func)
{
  DCL_LOCK_STATE;
  GC_ASSERT(NONNULL_ARG_NOT_NULL(stop_func));
  LOCK();
  GC_default_stop_func = stop_func;
  UNLOCK();
}
GC_API GC_stop_func GC_CALL GC_get_stop_func(void)
{
  GC_stop_func stop_func;
  DCL_LOCK_STATE;
  LOCK();
  stop_func = GC_default_stop_func;
  UNLOCK();
  return stop_func;
}
#if defined(GC_DISABLE_INCREMENTAL) || defined(NO_CLOCK)
#define GC_timeout_stop_func GC_default_stop_func
#else
  STATIC int GC_CALLBACK GC_timeout_stop_func (void)
  {
    CLOCK_TYPE current_time;
    static unsigned count = 0;
    unsigned long time_diff, nsec_diff;
    if ((*GC_default_stop_func)())
      return(1);
    if ((count++ & 3) != 0) return(0);
    GET_TIME(current_time);
    time_diff = MS_TIME_DIFF(current_time,GC_start_time);
    nsec_diff = NS_FRAC_TIME_DIFF(current_time, GC_start_time);
#if defined(CPPCHECK)
      GC_noop1((word)&nsec_diff);
#endif
    if (time_diff >= GC_time_limit
        && (time_diff > GC_time_limit || nsec_diff >= GC_time_lim_nsec)) {
      GC_COND_LOG_PRINTF("Abandoning stopped marking after %lu ms %lu ns"
                         " (attempt %d)\n",
                         time_diff, nsec_diff, GC_n_attempts);
      return 1;
    }
    return(0);
  }
#endif
#ifdef THREADS
  GC_INNER word GC_total_stacksize = 0;
#endif
static size_t min_bytes_allocd_minimum = 1;
GC_API void GC_CALL GC_set_min_bytes_allocd(size_t value)
{
    GC_ASSERT(value > 0);
    min_bytes_allocd_minimum = value;
}
GC_API size_t GC_CALL GC_get_min_bytes_allocd(void)
{
    return min_bytes_allocd_minimum;
}
static word min_bytes_allocd(void)
{
    word result;
    word stack_size;
    word total_root_size;
    word scan_size;
#ifdef THREADS
      if (GC_need_to_lock) {
        stack_size = GC_total_stacksize;
#ifdef DEBUG_THREADS
          GC_log_printf("Total stacks size: %lu\n",
                        (unsigned long)stack_size);
#endif
      } else
#endif
     {
#ifdef STACK_NOT_SCANNED
        stack_size = 0;
#elif defined(STACK_GROWS_UP)
        stack_size = GC_approx_sp() - GC_stackbottom;
#else
        stack_size = GC_stackbottom - GC_approx_sp();
#endif
    }
    total_root_size = 2 * stack_size + GC_root_size;
    scan_size = 2 * GC_composite_in_use + GC_atomic_in_use / 4
                + total_root_size;
    result = scan_size / GC_free_space_divisor;
    if (GC_incremental) {
      result /= 2;
    }
    return result > min_bytes_allocd_minimum
            ? result : min_bytes_allocd_minimum;
}
STATIC word GC_non_gc_bytes_at_gc = 0;
STATIC word GC_adj_bytes_allocd(void)
{
    signed_word result;
    signed_word expl_managed = (signed_word)GC_non_gc_bytes
                                - (signed_word)GC_non_gc_bytes_at_gc;
    result = (signed_word)GC_bytes_allocd
             + (signed_word)GC_bytes_dropped
             - (signed_word)GC_bytes_freed
             + (signed_word)GC_finalizer_bytes_freed
             - expl_managed;
    if (result > (signed_word)GC_bytes_allocd) {
        result = GC_bytes_allocd;
    }
    result += GC_bytes_finalized;
    if (result < (signed_word)(GC_bytes_allocd >> 3)) {
        return(GC_bytes_allocd >> 3);
    } else {
        return(result);
    }
}
STATIC void GC_clear_a_few_frames(void)
{
#ifndef CLEAR_NWORDS
#define CLEAR_NWORDS 64
#endif
    volatile word frames[CLEAR_NWORDS];
    BZERO((word *)frames, CLEAR_NWORDS * sizeof(word));
}
STATIC word GC_collect_at_heapsize = GC_WORD_MAX;
GC_API void GC_CALL GC_start_incremental_collection(void)
{
#ifndef GC_DISABLE_INCREMENTAL
    DCL_LOCK_STATE;
    if (!GC_incremental) return;
    LOCK();
    GC_should_start_incremental_collection = TRUE;
    ENTER_GC();
    GC_collect_a_little_inner(1);
    EXIT_GC();
    UNLOCK();
#endif
}
GC_INNER GC_bool GC_should_collect(void)
{
    static word last_min_bytes_allocd;
    static word last_gc_no;
    GC_ASSERT(I_HOLD_LOCK());
    if (last_gc_no != GC_gc_no) {
      last_min_bytes_allocd = min_bytes_allocd();
      last_gc_no = GC_gc_no;
    }
#ifndef GC_DISABLE_INCREMENTAL
    if (GC_should_start_incremental_collection) {
      GC_should_start_incremental_collection = FALSE;
      return TRUE;
    }
#endif
    if (GC_disable_automatic_collection) return FALSE;
    return(GC_adj_bytes_allocd() >= last_min_bytes_allocd
           || GC_heapsize >= GC_collect_at_heapsize);
}
 GC_start_callback_proc GC_start_call_back = 0;
GC_API void GC_CALL GC_set_start_callback(GC_start_callback_proc fn)
{
    DCL_LOCK_STATE;
    LOCK();
    GC_start_call_back = fn;
    UNLOCK();
}
GC_API GC_start_callback_proc GC_CALL GC_get_start_callback(void)
{
    GC_start_callback_proc fn;
    DCL_LOCK_STATE;
    LOCK();
    fn = GC_start_call_back;
    UNLOCK();
    return fn;
}
GC_INLINE void GC_notify_full_gc(void)
{
    if (GC_start_call_back != 0) {
        (*GC_start_call_back)();
    }
}
STATIC GC_bool GC_is_full_gc = FALSE;
STATIC GC_bool GC_stopped_mark(GC_stop_func stop_func);
STATIC void GC_finish_collection(void);
STATIC void GC_maybe_gc(void)
{
    GC_ASSERT(I_HOLD_LOCK());
    ASSERT_CANCEL_DISABLED();
    if (GC_should_collect()) {
        static int n_partial_gcs = 0;
        if (!GC_incremental) {
            GC_try_to_collect_inner(GC_never_stop_func);
            n_partial_gcs = 0;
            return;
        } else {
#ifdef PARALLEL_MARK
            if (GC_parallel)
              GC_wait_for_reclaim();
#endif
          if (GC_need_full_gc || n_partial_gcs >= GC_full_freq) {
            GC_COND_LOG_PRINTF(
                "***>Full mark for collection #%lu after %lu allocd bytes\n",
                (unsigned long)GC_gc_no + 1, (unsigned long)GC_bytes_allocd);
            GC_promote_black_lists();
            (void)GC_reclaim_all((GC_stop_func)0, TRUE);
            GC_notify_full_gc();
            GC_clear_marks();
            n_partial_gcs = 0;
            GC_is_full_gc = TRUE;
          } else {
            n_partial_gcs++;
          }
        }
#ifndef NO_CLOCK
          if (GC_time_limit != GC_TIME_UNLIMITED) { GET_TIME(GC_start_time); }
#endif
        if (GC_stopped_mark(GC_time_limit == GC_TIME_UNLIMITED?
                            GC_never_stop_func : GC_timeout_stop_func)) {
#ifdef SAVE_CALL_CHAIN
                GC_save_callers(GC_last_stack);
#endif
            GC_finish_collection();
        } else {
            if (!GC_is_full_gc) {
                GC_n_attempts++;
            }
        }
    }
}
STATIC GC_on_collection_event_proc GC_on_collection_event = 0;
GC_API void GC_CALL GC_set_on_collection_event(GC_on_collection_event_proc fn)
{
    DCL_LOCK_STATE;
    LOCK();
    GC_on_collection_event = fn;
    UNLOCK();
}
GC_API GC_on_collection_event_proc GC_CALL GC_get_on_collection_event(void)
{
    GC_on_collection_event_proc fn;
    DCL_LOCK_STATE;
    LOCK();
    fn = GC_on_collection_event;
    UNLOCK();
    return fn;
}
GC_INNER GC_bool GC_try_to_collect_inner(GC_stop_func stop_func)
{
#ifndef NO_CLOCK
      CLOCK_TYPE start_time = CLOCK_TYPE_INITIALIZER;
      GC_bool start_time_valid;
#endif
    ASSERT_CANCEL_DISABLED();
    GC_ASSERT(I_HOLD_LOCK());
    if (GC_dont_gc || (*stop_func)()) return FALSE;
    if (GC_on_collection_event)
      GC_on_collection_event(GC_EVENT_START);
    if (GC_incremental && GC_collection_in_progress()) {
      GC_COND_LOG_PRINTF(
            "GC_try_to_collect_inner: finishing collection in progress\n");
        while(GC_collection_in_progress()) {
            if ((*stop_func)()) {
              return(FALSE);
            }
            ENTER_GC();
            GC_collect_a_little_inner(1);
            EXIT_GC();
        }
    }
    GC_notify_full_gc();
#ifndef NO_CLOCK
      start_time_valid = FALSE;
      if ((GC_print_stats | (int)measure_performance) != 0) {
        if (GC_print_stats)
          GC_log_printf("Initiating full world-stop collection!\n");
        start_time_valid = TRUE;
        GET_TIME(start_time);
      }
#endif
    GC_promote_black_lists();
#ifdef PARALLEL_MARK
          if (GC_parallel)
            GC_wait_for_reclaim();
#endif
        if ((GC_find_leak || stop_func != GC_never_stop_func)
            && !GC_reclaim_all(stop_func, FALSE)) {
            return(FALSE);
        }
    GC_invalidate_mark_state();
    GC_clear_marks();
#ifdef SAVE_CALL_CHAIN
        GC_save_callers(GC_last_stack);
#endif
    GC_is_full_gc = TRUE;
    if (!GC_stopped_mark(stop_func)) {
      if (!GC_incremental) {
        GC_invalidate_mark_state();
        GC_unpromote_black_lists();
      }
      return(FALSE);
    }
    GC_finish_collection();
#ifndef NO_CLOCK
      if (start_time_valid) {
        CLOCK_TYPE current_time;
        unsigned long time_diff, ns_frac_diff;
        GET_TIME(current_time);
        time_diff = MS_TIME_DIFF(current_time, start_time);
        ns_frac_diff = NS_FRAC_TIME_DIFF(current_time, start_time);
        if (measure_performance) {
          full_gc_total_time += time_diff;
          full_gc_total_ns_frac += (unsigned)ns_frac_diff;
          if (full_gc_total_ns_frac >= 1000000U) {
            full_gc_total_ns_frac -= 1000000U;
            full_gc_total_time++;
          }
        }
        if (GC_print_stats)
          GC_log_printf("Complete collection took %lu ms %lu ns\n",
                        time_diff, ns_frac_diff);
      }
#endif
    if (GC_on_collection_event)
      GC_on_collection_event(GC_EVENT_END);
    return(TRUE);
}
#ifndef GC_RATE
#define GC_RATE 10
#endif
#ifndef MAX_PRIOR_ATTEMPTS
#define MAX_PRIOR_ATTEMPTS 1
#endif
STATIC int GC_deficit = 0;
STATIC int GC_rate = GC_RATE;
GC_API void GC_CALL GC_set_rate(int value)
{
    GC_ASSERT(value > 0);
    GC_rate = value;
}
GC_API int GC_CALL GC_get_rate(void)
{
    return GC_rate;
}
static int max_prior_attempts = MAX_PRIOR_ATTEMPTS;
GC_API void GC_CALL GC_set_max_prior_attempts(int value)
{
    GC_ASSERT(value >= 0);
    max_prior_attempts = value;
}
GC_API int GC_CALL GC_get_max_prior_attempts(void)
{
    return max_prior_attempts;
}
GC_INNER void GC_collect_a_little_inner(int n)
{
    IF_CANCEL(int cancel_state;)
    GC_ASSERT(I_HOLD_LOCK());
    if (GC_dont_gc) return;
    DISABLE_CANCEL(cancel_state);
    if (GC_incremental && GC_collection_in_progress()) {
        int i;
        int max_deficit = GC_rate * n;
#ifdef PARALLEL_MARK
            if (GC_time_limit != GC_TIME_UNLIMITED)
                GC_parallel_mark_disabled = TRUE;
#endif
        for (i = GC_deficit; i < max_deficit; i++) {
            if (GC_mark_some(NULL))
                break;
        }
#ifdef PARALLEL_MARK
            GC_parallel_mark_disabled = FALSE;
#endif
        if (i < max_deficit) {
#ifdef SAVE_CALL_CHAIN
                GC_save_callers(GC_last_stack);
#endif
#ifdef PARALLEL_MARK
                if (GC_parallel)
                    GC_wait_for_reclaim();
#endif
            if (GC_n_attempts < max_prior_attempts
                && GC_time_limit != GC_TIME_UNLIMITED) {
#ifndef NO_CLOCK
                    GET_TIME(GC_start_time);
#endif
                if (GC_stopped_mark(GC_timeout_stop_func)) {
                    GC_finish_collection();
                } else {
                    GC_n_attempts++;
                }
            } else {
                (void)GC_stopped_mark(GC_never_stop_func);
                GC_finish_collection();
            }
        }
        if (GC_deficit > 0) {
            GC_deficit -= max_deficit;
            if (GC_deficit < 0)
                GC_deficit = 0;
        }
    } else {
        GC_maybe_gc();
    }
    RESTORE_CANCEL(cancel_state);
}
GC_INNER void (*GC_check_heap)(void) = 0;
GC_INNER void (*GC_print_all_smashed)(void) = 0;
GC_API int GC_CALL GC_collect_a_little(void)
{
    int result;
    DCL_LOCK_STATE;
    LOCK();
    ENTER_GC();
    GC_collect_a_little_inner(1);
    EXIT_GC();
    result = (int)GC_collection_in_progress();
    UNLOCK();
    if (!result && GC_debugging_started) GC_print_all_smashed();
    return(result);
}
#ifndef NO_CLOCK
  static unsigned world_stopped_total_time = 0;
  static unsigned world_stopped_total_divisor = 0;
#ifndef MAX_TOTAL_TIME_DIVISOR
#define MAX_TOTAL_TIME_DIVISOR 1000
#endif
#endif
#ifdef USE_MUNMAP
#define IF_USE_MUNMAP(x) x
#define COMMA_IF_USE_MUNMAP(x) , x
#else
#define IF_USE_MUNMAP(x)
#define COMMA_IF_USE_MUNMAP(x)
#endif
STATIC GC_bool GC_stopped_mark(GC_stop_func stop_func)
{
    int i;
#ifndef NO_CLOCK
      CLOCK_TYPE start_time = CLOCK_TYPE_INITIALIZER;
#endif
    GC_ASSERT(I_HOLD_LOCK());
#if !defined(REDIRECT_MALLOC) && defined(USE_WINALLOC)
        GC_add_current_malloc_heap();
#endif
#if defined(REGISTER_LIBRARIES_EARLY)
        GC_cond_register_dynamic_libraries();
#endif
#ifndef NO_CLOCK
      if (GC_PRINT_STATS_FLAG)
        GET_TIME(start_time);
#endif
#if !defined(GC_NO_FINALIZATION) && !defined(GC_TOGGLE_REFS_NOT_NEEDED)
      GC_process_togglerefs();
#endif
#ifdef THREADS
      if (GC_on_collection_event)
        GC_on_collection_event(GC_EVENT_PRE_STOP_WORLD);
#endif
    STOP_WORLD();
#ifdef THREADS
      if (GC_on_collection_event)
        GC_on_collection_event(GC_EVENT_POST_STOP_WORLD);
#endif
#ifdef THREAD_LOCAL_ALLOC
      GC_world_stopped = TRUE;
#endif
    GC_COND_LOG_PRINTF(
              "\n--> Marking for collection #%lu after %lu allocated bytes\n",
              (unsigned long)GC_gc_no + 1, (unsigned long) GC_bytes_allocd);
#ifdef MAKE_BACK_GRAPH
      if (GC_print_back_height) {
        GC_build_back_graph();
      }
#endif
        if (GC_on_collection_event)
          GC_on_collection_event(GC_EVENT_MARK_START);
            GC_clear_a_few_frames();
            GC_noop6(0,0,0,0,0,0);
        GC_initiate_gc();
#ifdef PARALLEL_MARK
          if (stop_func != GC_never_stop_func)
            GC_parallel_mark_disabled = TRUE;
#endif
        for (i = 0; !(*stop_func)(); i++) {
          if (GC_mark_some(GC_approx_sp())) {
#ifdef PARALLEL_MARK
              if (GC_parallel && GC_parallel_mark_disabled) {
                GC_COND_LOG_PRINTF("Stopped marking done after %d iterations"
                                   " with disabled parallel marker\n", i);
              }
#endif
            i = -1;
            break;
          }
        }
#ifdef PARALLEL_MARK
          GC_parallel_mark_disabled = FALSE;
#endif
        if (i >= 0) {
          GC_COND_LOG_PRINTF("Abandoned stopped marking after"
                             " %d iterations\n", i);
          GC_deficit = i;
#ifdef THREAD_LOCAL_ALLOC
            GC_world_stopped = FALSE;
#endif
#ifdef THREADS
            if (GC_on_collection_event)
              GC_on_collection_event(GC_EVENT_PRE_START_WORLD);
#endif
          START_WORLD();
#ifdef THREADS
            if (GC_on_collection_event)
              GC_on_collection_event(GC_EVENT_POST_START_WORLD);
#endif
          return FALSE;
        }
    GC_gc_no++;
#ifdef USE_MUNMAP
      GC_ASSERT(GC_heapsize >= GC_unmapped_bytes);
#endif
    GC_ASSERT(GC_our_mem_bytes >= GC_heapsize);
    GC_DBGLOG_PRINTF("GC #%lu freed %ld bytes, heap %lu KiB ("
                     IF_USE_MUNMAP("+ %lu KiB unmapped ")
                     "+ %lu KiB internal)\n",
                     (unsigned long)GC_gc_no, (long)GC_bytes_found,
                     TO_KiB_UL(GC_heapsize - GC_unmapped_bytes)
                     COMMA_IF_USE_MUNMAP(TO_KiB_UL(GC_unmapped_bytes)),
                     TO_KiB_UL(GC_our_mem_bytes - GC_heapsize));
    if (GC_debugging_started) {
      (*GC_check_heap)();
    }
    if (GC_on_collection_event) {
      GC_on_collection_event(GC_EVENT_MARK_END);
#ifdef THREADS
        GC_on_collection_event(GC_EVENT_PRE_START_WORLD);
#endif
    }
#ifdef THREAD_LOCAL_ALLOC
      GC_world_stopped = FALSE;
#endif
    START_WORLD();
#ifdef THREADS
      if (GC_on_collection_event)
        GC_on_collection_event(GC_EVENT_POST_START_WORLD);
#endif
#ifndef NO_CLOCK
      if (GC_PRINT_STATS_FLAG) {
        unsigned long time_diff;
        unsigned total_time, divisor;
        CLOCK_TYPE current_time;
        GET_TIME(current_time);
        time_diff = MS_TIME_DIFF(current_time,start_time);
        total_time = world_stopped_total_time;
        divisor = world_stopped_total_divisor;
        if ((int)total_time < 0 || divisor >= MAX_TOTAL_TIME_DIVISOR) {
          total_time >>= 1;
          divisor >>= 1;
        }
        total_time += time_diff < (((unsigned)-1) >> 1) ?
                        (unsigned)time_diff : ((unsigned)-1) >> 1;
        world_stopped_total_time = total_time;
        world_stopped_total_divisor = ++divisor;
        GC_ASSERT(divisor != 0);
        GC_log_printf("World-stopped marking took %lu ms %lu ns"
                      " (%u ms in average)\n",
                      time_diff, NS_FRAC_TIME_DIFF(current_time, start_time),
                      total_time / divisor);
      }
#endif
    return(TRUE);
}
GC_INNER void GC_set_fl_marks(ptr_t q)
{
    if (q ) {
      struct hblk *h = HBLKPTR(q);
      struct hblk *last_h = h;
      hdr *hhdr = HDR(h);
      IF_PER_OBJ(word sz = hhdr->hb_sz;)
      for (;;) {
        word bit_no = MARK_BIT_NO((ptr_t)q - (ptr_t)h, sz);
        if (!mark_bit_from_hdr(hhdr, bit_no)) {
          set_mark_bit_from_hdr(hhdr, bit_no);
          ++hhdr -> hb_n_marks;
        }
        q = (ptr_t)obj_link(q);
        if (q == NULL)
          break;
        h = HBLKPTR(q);
        if (h != last_h) {
          last_h = h;
          hhdr = HDR(h);
          IF_PER_OBJ(sz = hhdr->hb_sz;)
        }
      }
    }
}
#if defined(GC_ASSERTIONS) && defined(THREAD_LOCAL_ALLOC)
  void GC_check_fl_marks(void **pfreelist)
  {
#if defined(AO_HAVE_load_acquire_read) && !defined(THREAD_SANITIZER)
      AO_t *list = (AO_t *)AO_load_acquire_read((AO_t *)pfreelist);
      AO_t *prev;
      AO_t *p;
      if ((word)list <= HBLKSIZE) return;
      prev = (AO_t *)pfreelist;
      for (p = list; p != NULL;) {
        AO_t *next;
        if (!GC_is_marked(p)) {
          ABORT_ARG2("Unmarked local free list entry",
                     ": object %p on list %p", (void *)p, (void *)list);
        }
        next = (AO_t *)AO_load_acquire_read(p);
        if (AO_load(prev) != (AO_t)p)
          break;
        prev = p;
        p = next;
      }
#else
      (void)pfreelist;
#endif
  }
#endif
STATIC void GC_clear_fl_marks(ptr_t q)
{
      struct hblk *h = HBLKPTR(q);
      struct hblk *last_h = h;
      hdr *hhdr = HDR(h);
      word sz = hhdr->hb_sz;
      for (;;) {
        word bit_no = MARK_BIT_NO((ptr_t)q - (ptr_t)h, sz);
        if (mark_bit_from_hdr(hhdr, bit_no)) {
          size_t n_marks = hhdr -> hb_n_marks;
          GC_ASSERT(n_marks != 0);
          clear_mark_bit_from_hdr(hhdr, bit_no);
          n_marks--;
#ifdef PARALLEL_MARK
            if (0 != n_marks || !GC_parallel) {
              hhdr -> hb_n_marks = n_marks;
            }
#else
            hhdr -> hb_n_marks = n_marks;
#endif
        }
        GC_bytes_found -= sz;
        q = (ptr_t)obj_link(q);
        if (q == NULL)
          break;
        h = HBLKPTR(q);
        if (h != last_h) {
          last_h = h;
          hhdr = HDR(h);
          sz = hhdr->hb_sz;
        }
      }
}
#if defined(GC_ASSERTIONS) && defined(THREAD_LOCAL_ALLOC)
  void GC_check_tls(void);
#endif
GC_on_heap_resize_proc GC_on_heap_resize = 0;
GC_INLINE int GC_compute_heap_usage_percent(void)
{
  word used = GC_composite_in_use + GC_atomic_in_use;
  word heap_sz = GC_heapsize - GC_unmapped_bytes;
#if defined(CPPCHECK)
    word limit = (GC_WORD_MAX >> 1) / 50;
#else
    const word limit = GC_WORD_MAX / 100;
#endif
  return used >= heap_sz ? 0 : used < limit ?
                (int)((used * 100) / heap_sz) : (int)(used / (heap_sz / 100));
}
STATIC void GC_finish_collection(void)
{
#ifndef NO_CLOCK
      CLOCK_TYPE start_time = CLOCK_TYPE_INITIALIZER;
      CLOCK_TYPE finalize_time = CLOCK_TYPE_INITIALIZER;
#endif
    GC_ASSERT(I_HOLD_LOCK());
#if defined(GC_ASSERTIONS) \
       && defined(THREAD_LOCAL_ALLOC) && !defined(DBG_HDRS_ALL)
        GC_check_tls();
#endif
#ifndef NO_CLOCK
      if (GC_print_stats)
        GET_TIME(start_time);
#endif
    if (GC_on_collection_event)
      GC_on_collection_event(GC_EVENT_RECLAIM_START);
#ifndef GC_GET_HEAP_USAGE_NOT_NEEDED
      if (GC_bytes_found > 0)
        GC_reclaimed_bytes_before_gc += (word)GC_bytes_found;
#endif
    GC_bytes_found = 0;
#if defined(LINUX) && defined(__ELF__) && !defined(SMALL_CONFIG)
        if (GETENV("GC_PRINT_ADDRESS_MAP") != 0) {
          GC_print_address_map();
        }
#endif
    COND_DUMP;
    if (GC_find_leak) {
      word size;
      unsigned kind;
      ptr_t q;
      for (kind = 0; kind < GC_n_kinds; kind++) {
        for (size = 1; size <= MAXOBJGRANULES; size++) {
          q = (ptr_t)GC_obj_kinds[kind].ok_freelist[size];
          if (q != NULL)
            GC_set_fl_marks(q);
        }
      }
      GC_start_reclaim(TRUE);
    }
#ifndef GC_NO_FINALIZATION
      GC_finalize();
#endif
#ifndef NO_CLOCK
      if (GC_print_stats)
        GET_TIME(finalize_time);
#endif
    if (GC_print_back_height) {
#ifdef MAKE_BACK_GRAPH
        GC_traverse_back_graph();
#elif !defined(SMALL_CONFIG)
        GC_err_printf("Back height not available: "
                      "Rebuild collector with -DMAKE_BACK_GRAPH\n");
#endif
    }
    {
      word size;
      ptr_t q;
      unsigned kind;
      for (kind = 0; kind < GC_n_kinds; kind++) {
        for (size = 1; size <= MAXOBJGRANULES; size++) {
          q = (ptr_t)GC_obj_kinds[kind].ok_freelist[size];
          if (q != NULL)
            GC_clear_fl_marks(q);
        }
      }
    }
    GC_VERBOSE_LOG_PRINTF("Bytes recovered before sweep - f.l. count = %ld\n",
                          (long)GC_bytes_found);
    GC_start_reclaim(FALSE);
    GC_DBGLOG_PRINTF("In-use heap: %d%% (%lu KiB pointers + %lu KiB other)\n",
                     GC_compute_heap_usage_percent(),
                     TO_KiB_UL(GC_composite_in_use),
                     TO_KiB_UL(GC_atomic_in_use));
    if (GC_is_full_gc) {
        GC_used_heap_size_after_full = USED_HEAP_SIZE;
        GC_need_full_gc = FALSE;
    } else {
        GC_need_full_gc = USED_HEAP_SIZE - GC_used_heap_size_after_full
                            > min_bytes_allocd();
    }
    GC_VERBOSE_LOG_PRINTF("Immediately reclaimed %ld bytes, heapsize:"
                          " %lu bytes" IF_USE_MUNMAP(" (%lu unmapped)") "\n",
                          (long)GC_bytes_found,
                          (unsigned long)GC_heapsize
                          COMMA_IF_USE_MUNMAP((unsigned long)
                                              GC_unmapped_bytes));
    GC_n_attempts = 0;
    GC_is_full_gc = FALSE;
    GC_bytes_allocd_before_gc += GC_bytes_allocd;
    GC_non_gc_bytes_at_gc = GC_non_gc_bytes;
    GC_bytes_allocd = 0;
    GC_bytes_dropped = 0;
    GC_bytes_freed = 0;
    GC_finalizer_bytes_freed = 0;
    IF_USE_MUNMAP(GC_unmap_old());
    if (GC_on_collection_event)
      GC_on_collection_event(GC_EVENT_RECLAIM_END);
#ifndef NO_CLOCK
      if (GC_print_stats) {
        CLOCK_TYPE done_time;
        GET_TIME(done_time);
#if !defined(SMALL_CONFIG) && !defined(GC_NO_FINALIZATION)
          GC_print_finalization_stats();
#endif
        GC_log_printf("Finalize and initiate sweep took %lu ms %lu ns"
                      " + %lu ms %lu ns\n",
                      MS_TIME_DIFF(finalize_time, start_time),
                      NS_FRAC_TIME_DIFF(finalize_time, start_time),
                      MS_TIME_DIFF(done_time, finalize_time),
                      NS_FRAC_TIME_DIFF(done_time, finalize_time));
      }
#elif !defined(SMALL_CONFIG) && !defined(GC_NO_FINALIZATION)
      if (GC_print_stats)
        GC_print_finalization_stats();
#endif
}
STATIC GC_bool GC_try_to_collect_general(GC_stop_func stop_func,
                                         GC_bool force_unmap GC_ATTR_UNUSED)
{
    GC_bool result;
    IF_USE_MUNMAP(int old_unmap_threshold;)
    IF_CANCEL(int cancel_state;)
    DCL_LOCK_STATE;
    if (!EXPECT(GC_is_initialized, TRUE)) GC_init();
    if (GC_debugging_started) GC_print_all_smashed();
    GC_INVOKE_FINALIZERS();
    LOCK();
    DISABLE_CANCEL(cancel_state);
#ifdef USE_MUNMAP
      old_unmap_threshold = GC_unmap_threshold;
      if (force_unmap ||
          (GC_force_unmap_on_gcollect && old_unmap_threshold > 0))
        GC_unmap_threshold = 1;
#endif
    ENTER_GC();
      GC_noop6(0,0,0,0,0,0);
    result = GC_try_to_collect_inner(stop_func != 0 ? stop_func :
                                     GC_default_stop_func);
    EXIT_GC();
    IF_USE_MUNMAP(GC_unmap_threshold = old_unmap_threshold);
    RESTORE_CANCEL(cancel_state);
    UNLOCK();
    if (result) {
        if (GC_debugging_started) GC_print_all_smashed();
        GC_INVOKE_FINALIZERS();
    }
    return(result);
}
GC_API int GC_CALL GC_try_to_collect(GC_stop_func stop_func)
{
    GC_ASSERT(NONNULL_ARG_NOT_NULL(stop_func));
    return (int)GC_try_to_collect_general(stop_func, FALSE);
}
GC_API void GC_CALL GC_gcollect(void)
{
    (void)GC_try_to_collect_general(0, FALSE);
    if (GC_have_errors) GC_print_all_errors();
}
STATIC word GC_heapsize_at_forced_unmap = 0;
GC_API void GC_CALL GC_gcollect_and_unmap(void)
{
    GC_heapsize_at_forced_unmap = GC_heapsize;
    (void)GC_try_to_collect_general(GC_never_stop_func, TRUE);
}
#ifdef USE_PROC_FOR_LIBRARIES
  GC_INNER void GC_add_to_our_memory(ptr_t p, size_t bytes)
  {
    GC_ASSERT(p != NULL);
    if (GC_n_memory >= MAX_HEAP_SECTS)
      ABORT("Too many GC-allocated memory sections: Increase MAX_HEAP_SECTS");
    GC_our_memory[GC_n_memory].hs_start = p;
    GC_our_memory[GC_n_memory].hs_bytes = bytes;
    GC_n_memory++;
    GC_our_mem_bytes += bytes;
  }
#endif
STATIC void GC_add_to_heap(struct hblk *p, size_t bytes)
{
    hdr * phdr;
    word endp;
    size_t old_capacity = 0;
    void *old_heap_sects = NULL;
#ifdef GC_ASSERTIONS
      unsigned i;
#endif
    GC_ASSERT((word)p % HBLKSIZE == 0);
    GC_ASSERT(bytes % HBLKSIZE == 0);
    GC_ASSERT(bytes > 0);
    GC_ASSERT(GC_all_nils != NULL);
    if (GC_n_heap_sects == GC_capacity_heap_sects) {
#ifndef INITIAL_HEAP_SECTS
#define INITIAL_HEAP_SECTS 32
#endif
      size_t new_capacity = GC_n_heap_sects > 0 ?
                (size_t)GC_n_heap_sects * 2 : INITIAL_HEAP_SECTS;
      void *new_heap_sects =
                GC_scratch_alloc(new_capacity * sizeof(struct HeapSect));
      if (EXPECT(NULL == new_heap_sects, FALSE)) {
        new_capacity = (size_t)GC_n_heap_sects + INITIAL_HEAP_SECTS;
        new_heap_sects =
                GC_scratch_alloc(new_capacity * sizeof(struct HeapSect));
        if (NULL == new_heap_sects)
          ABORT("Insufficient memory for heap sections");
      }
      old_capacity = GC_capacity_heap_sects;
      old_heap_sects = GC_heap_sects;
      if (GC_n_heap_sects > 0)
        BCOPY(old_heap_sects, new_heap_sects,
              GC_n_heap_sects * sizeof(struct HeapSect));
      GC_capacity_heap_sects = new_capacity;
      GC_heap_sects = (struct HeapSect *)new_heap_sects;
      GC_COND_LOG_PRINTF("Grew heap sections array to %lu elements\n",
                         (unsigned long)new_capacity);
    }
    while ((word)p <= HBLKSIZE) {
        ++p;
        bytes -= HBLKSIZE;
        if (0 == bytes) return;
    }
    endp = (word)p + bytes;
    if (endp <= (word)p) {
        bytes -= HBLKSIZE;
        if (0 == bytes) return;
        endp -= HBLKSIZE;
    }
    phdr = GC_install_header(p);
    if (0 == phdr) {
        return;
    }
    GC_ASSERT(endp > (word)p && endp == (word)p + bytes);
#ifdef GC_ASSERTIONS
      for (i = 0; i < GC_n_heap_sects; i++) {
        word hs_start = (word)GC_heap_sects[i].hs_start;
        word hs_end = hs_start + GC_heap_sects[i].hs_bytes;
        word p_e = (word)p + bytes;
        GC_ASSERT(!((hs_start <= (word)p && (word)p < hs_end)
                    || (hs_start < p_e && p_e <= hs_end)
                    || ((word)p < hs_start && hs_end < p_e)));
      }
#endif
    GC_heap_sects[GC_n_heap_sects].hs_start = (ptr_t)p;
    GC_heap_sects[GC_n_heap_sects].hs_bytes = bytes;
    GC_n_heap_sects++;
    phdr -> hb_sz = bytes;
    phdr -> hb_flags = 0;
    GC_freehblk(p);
    GC_heapsize += bytes;
    GC_collect_at_heapsize += bytes;
    if (GC_collect_at_heapsize < GC_heapsize )
       GC_collect_at_heapsize = GC_WORD_MAX;
    if ((word)p <= (word)GC_least_plausible_heap_addr
        || GC_least_plausible_heap_addr == 0) {
        GC_least_plausible_heap_addr = (void *)((ptr_t)p - sizeof(word));
    }
    if ((word)p + bytes >= (word)GC_greatest_plausible_heap_addr) {
        GC_greatest_plausible_heap_addr = (void *)endp;
    }
    if (old_capacity > 0) {
#ifndef GWW_VDB
        GC_scratch_recycle_no_gww(old_heap_sects,
                                  old_capacity * sizeof(struct HeapSect));
#else
        GC_noop1((word)old_heap_sects);
#endif
    }
}
#if !defined(NO_DEBUGGING)
  void GC_print_heap_sects(void)
  {
    unsigned i;
    GC_printf("Total heap size: %lu" IF_USE_MUNMAP(" (%lu unmapped)") "\n",
              (unsigned long)GC_heapsize
              COMMA_IF_USE_MUNMAP((unsigned long)GC_unmapped_bytes));
    for (i = 0; i < GC_n_heap_sects; i++) {
      ptr_t start = GC_heap_sects[i].hs_start;
      size_t len = GC_heap_sects[i].hs_bytes;
      struct hblk *h;
      unsigned nbl = 0;
      for (h = (struct hblk *)start; (word)h < (word)(start + len); h++) {
        if (GC_is_black_listed(h, HBLKSIZE)) nbl++;
      }
      GC_printf("Section %d from %p to %p %u/%lu blacklisted\n",
                i, (void *)start, (void *)&start[len],
                nbl, (unsigned long)divHBLKSZ(len));
    }
  }
#endif
void * GC_least_plausible_heap_addr = (void *)GC_WORD_MAX;
void * GC_greatest_plausible_heap_addr = 0;
GC_INLINE word GC_max(word x, word y)
{
    return(x > y? x : y);
}
GC_INLINE word GC_min(word x, word y)
{
    return(x < y? x : y);
}
STATIC word GC_max_heapsize = 0;
GC_API void GC_CALL GC_set_max_heap_size(GC_word n)
{
    GC_max_heapsize = n;
}
GC_word GC_max_retries = 0;
GC_INNER void GC_scratch_recycle_inner(void *ptr, size_t bytes)
{
  size_t page_offset;
  size_t displ = 0;
  size_t recycled_bytes;
  if (NULL == ptr) return;
  GC_ASSERT(bytes != 0);
  GC_ASSERT(GC_page_size != 0);
  page_offset = (word)ptr & (GC_page_size - 1);
  if (page_offset != 0)
    displ = GC_page_size - page_offset;
  recycled_bytes = bytes > displ ? (bytes - displ) & ~(GC_page_size - 1) : 0;
  GC_COND_LOG_PRINTF("Recycle %lu/%lu scratch-allocated bytes at %p\n",
                (unsigned long)recycled_bytes, (unsigned long)bytes, ptr);
  if (recycled_bytes > 0)
    GC_add_to_heap((struct hblk *)((word)ptr + displ), recycled_bytes);
}
GC_INNER GC_bool GC_expand_hp_inner(word n)
{
    size_t bytes;
    struct hblk * space;
    word expansion_slop;
    GC_ASSERT(I_HOLD_LOCK());
    GC_ASSERT(GC_page_size != 0);
    if (n < MINHINCR) n = MINHINCR;
    bytes = ROUNDUP_PAGESIZE((size_t)n * HBLKSIZE);
    if (GC_max_heapsize != 0
        && (GC_max_heapsize < (word)bytes
            || GC_heapsize > GC_max_heapsize - (word)bytes)) {
        return(FALSE);
    }
    space = GET_MEM(bytes);
    if (EXPECT(NULL == space, FALSE)) {
        WARN("Failed to expand heap by %" WARN_PRIdPTR " bytes\n",
             (word)bytes);
        return(FALSE);
    }
    GC_add_to_our_memory((ptr_t)space, bytes);
    GC_INFOLOG_PRINTF("Grow heap to %lu KiB after %lu bytes allocated\n",
                      TO_KiB_UL(GC_heapsize + (word)bytes),
                      (unsigned long)GC_bytes_allocd);
    expansion_slop = min_bytes_allocd() + 4 * MAXHINCR * HBLKSIZE;
    if ((GC_last_heap_addr == 0 && !((word)space & SIGNB))
        || (GC_last_heap_addr != 0
            && (word)GC_last_heap_addr < (word)space)) {
        word new_limit = (word)space + (word)bytes + expansion_slop;
        if (new_limit > (word)space) {
          GC_greatest_plausible_heap_addr =
            (void *)GC_max((word)GC_greatest_plausible_heap_addr,
                           (word)new_limit);
        }
    } else {
        word new_limit = (word)space - expansion_slop;
        if (new_limit < (word)space) {
          GC_least_plausible_heap_addr =
            (void *)GC_min((word)GC_least_plausible_heap_addr,
                           (word)space - expansion_slop);
        }
    }
    GC_last_heap_addr = (ptr_t)space;
    GC_add_to_heap(space, bytes);
    GC_collect_at_heapsize =
        GC_heapsize + expansion_slop - 2 * MAXHINCR * HBLKSIZE;
    if (GC_collect_at_heapsize < GC_heapsize )
        GC_collect_at_heapsize = GC_WORD_MAX;
    if (GC_on_heap_resize)
        (*GC_on_heap_resize)(GC_heapsize);
    return(TRUE);
}
GC_API int GC_CALL GC_expand_hp(size_t bytes)
{
    int result;
    DCL_LOCK_STATE;
    if (!EXPECT(GC_is_initialized, TRUE)) GC_init();
    LOCK();
    result = (int)GC_expand_hp_inner(divHBLKSZ((word)bytes));
    if (result) GC_requested_heapsize += bytes;
    UNLOCK();
    return(result);
}
GC_INNER unsigned GC_fail_count = 0;
#if defined(GC_ALLOCD_BYTES_PER_FINALIZER) && !defined(CPPCHECK)
  STATIC word GC_allocd_bytes_per_finalizer = GC_ALLOCD_BYTES_PER_FINALIZER;
#else
  STATIC word GC_allocd_bytes_per_finalizer = 10000;
#endif
GC_API void GC_CALL GC_set_allocd_bytes_per_finalizer(GC_word value)
{
  GC_allocd_bytes_per_finalizer = value;
}
GC_API GC_word GC_CALL GC_get_allocd_bytes_per_finalizer(void)
{
  return GC_allocd_bytes_per_finalizer;
}
static word last_fo_entries = 0;
static word last_bytes_finalized = 0;
GC_INNER GC_bool GC_collect_or_expand(word needed_blocks,
                                      GC_bool ignore_off_page,
                                      GC_bool retry)
{
    GC_bool gc_not_stopped = TRUE;
    word blocks_to_get;
    IF_CANCEL(int cancel_state;)
    GC_ASSERT(I_HOLD_LOCK());
    DISABLE_CANCEL(cancel_state);
    if (!GC_incremental && !GC_dont_gc &&
        ((GC_dont_expand && GC_bytes_allocd > 0)
         || (GC_fo_entries > last_fo_entries
             && (last_bytes_finalized | GC_bytes_finalized) != 0
             && (GC_fo_entries - last_fo_entries)
                * GC_allocd_bytes_per_finalizer > GC_bytes_allocd)
         || GC_should_collect())) {
      gc_not_stopped = GC_try_to_collect_inner(
                        GC_bytes_allocd > 0 && (!GC_dont_expand || !retry) ?
                        GC_default_stop_func : GC_never_stop_func);
      if (gc_not_stopped == TRUE || !retry) {
        last_fo_entries = GC_fo_entries;
        last_bytes_finalized = GC_bytes_finalized;
        RESTORE_CANCEL(cancel_state);
        return(TRUE);
      }
    }
    blocks_to_get = (GC_heapsize - GC_heapsize_at_forced_unmap)
                        / (HBLKSIZE * GC_free_space_divisor)
                    + needed_blocks;
    if (blocks_to_get > MAXHINCR) {
      word slop;
      if (ignore_off_page) {
        slop = 4;
      } else {
        slop = 2 * divHBLKSZ(BL_LIMIT);
        if (slop > needed_blocks) slop = needed_blocks;
      }
      if (needed_blocks + slop > MAXHINCR) {
        blocks_to_get = needed_blocks + slop;
      } else {
        blocks_to_get = MAXHINCR;
      }
      if (blocks_to_get > divHBLKSZ(GC_WORD_MAX))
        blocks_to_get = divHBLKSZ(GC_WORD_MAX);
    }
    if (!GC_expand_hp_inner(blocks_to_get)
        && (blocks_to_get == needed_blocks
            || !GC_expand_hp_inner(needed_blocks))) {
      if (gc_not_stopped == FALSE) {
        GC_gcollect_inner();
        GC_ASSERT(GC_bytes_allocd == 0);
      } else if (GC_fail_count++ < GC_max_retries) {
        WARN("Out of Memory!  Trying to continue...\n", 0);
        GC_gcollect_inner();
      } else {
#if !defined(AMIGA) || !defined(GC_AMIGA_FASTALLOC)
          WARN("Out of Memory! Heap size: %" WARN_PRIdPTR " MiB."
               " Returning NULL!\n", (GC_heapsize - GC_unmapped_bytes) >> 20);
#endif
        RESTORE_CANCEL(cancel_state);
        return(FALSE);
      }
    } else if (GC_fail_count) {
      GC_COND_LOG_PRINTF("Memory available again...\n");
    }
    RESTORE_CANCEL(cancel_state);
    return(TRUE);
}
GC_INNER ptr_t GC_allocobj(size_t gran, int kind)
{
    void ** flh = &(GC_obj_kinds[kind].ok_freelist[gran]);
    GC_bool tried_minor = FALSE;
    GC_bool retry = FALSE;
    GC_ASSERT(I_HOLD_LOCK());
    if (gran == 0) return(0);
    while (*flh == 0) {
      ENTER_GC();
#ifndef GC_DISABLE_INCREMENTAL
        if (GC_incremental && GC_time_limit != GC_TIME_UNLIMITED) {
          GC_collect_a_little_inner(1);
        }
#endif
        GC_ASSERT(!GC_is_full_gc
                  || NULL == GC_obj_kinds[kind].ok_reclaim_list
                  || NULL == GC_obj_kinds[kind].ok_reclaim_list[gran]);
        GC_continue_reclaim(gran, kind);
      EXIT_GC();
#if defined(CPPCHECK)
        GC_noop1((word)&flh);
#endif
      if (NULL == *flh) {
        GC_new_hblk(gran, kind);
#if defined(CPPCHECK)
          GC_noop1((word)&flh);
#endif
        if (NULL == *flh) {
          ENTER_GC();
          if (GC_incremental && GC_time_limit == GC_TIME_UNLIMITED
              && !tried_minor) {
            GC_collect_a_little_inner(1);
            tried_minor = TRUE;
          } else {
            if (!GC_collect_or_expand(1, FALSE, retry)) {
              EXIT_GC();
              return(0);
            }
            retry = TRUE;
          }
          EXIT_GC();
        }
      }
    }
    GC_fail_count = 0;
    return (ptr_t)(*flh);
}
#ifndef MSWINCE
#include <errno.h>
#endif
#include <string.h>
#ifndef SHORT_DBG_HDRS
  GC_INNER int GC_has_other_debug_info(ptr_t p)
  {
    ptr_t body = (ptr_t)((oh *)p + 1);
    word sz = GC_size(p);
    if (HBLKPTR(p) != HBLKPTR((ptr_t)body)
        || sz < DEBUG_BYTES + EXTRA_BYTES) {
      return 0;
    }
    if (((oh *)p) -> oh_sf != (START_FLAG ^ (word)body)
        && ((word *)p)[BYTES_TO_WORDS(sz)-1] != (END_FLAG ^ (word)body)) {
      return 0;
    }
    if (((oh *)p)->oh_sz == sz) {
      return -1;
    }
    return 1;
  }
#endif
#ifdef LINT2
  long GC_random(void)
  {
    static unsigned seed = 1;
    seed = (seed * 1103515245U + 12345) & GC_RAND_MAX;
    return (long)seed;
  }
#endif
#ifdef KEEP_BACK_PTRS
#ifdef LINT2
#define RANDOM() GC_random()
#else
#include <stdlib.h>
#define GC_RAND_MAX RAND_MAX
#if defined(__GLIBC__) || defined(SOLARIS) \
     || defined(HPUX) || defined(IRIX5) || defined(OSF1)
#define RANDOM() random()
#else
#define RANDOM() (long)rand()
#endif
#endif
  GC_INNER void GC_store_back_pointer(ptr_t source, ptr_t dest)
  {
    if (GC_HAS_DEBUG_INFO(dest)) {
#ifdef PARALLEL_MARK
        AO_store((volatile AO_t *)&((oh *)dest)->oh_back_ptr,
                 (AO_t)HIDE_BACK_PTR(source));
#else
        ((oh *)dest) -> oh_back_ptr = HIDE_BACK_PTR(source);
#endif
    }
  }
  GC_INNER void GC_marked_for_finalization(ptr_t dest)
  {
    GC_store_back_pointer(MARKED_FOR_FINALIZATION, dest);
  }
  GC_API GC_ref_kind GC_CALL GC_get_back_ptr_info(void *dest, void **base_p,
                                                  size_t *offset_p)
  {
    oh * hdr = (oh *)GC_base(dest);
    ptr_t bp;
    ptr_t bp_base;
#ifdef LINT2
      if (!hdr) ABORT("Invalid GC_get_back_ptr_info argument");
#endif
    if (!GC_HAS_DEBUG_INFO((ptr_t) hdr)) return GC_NO_SPACE;
    bp = (ptr_t)GC_REVEAL_POINTER(hdr -> oh_back_ptr);
    if (MARKED_FOR_FINALIZATION == bp) return GC_FINALIZER_REFD;
    if (MARKED_FROM_REGISTER == bp) return GC_REFD_FROM_REG;
    if (NOT_MARKED == bp) return GC_UNREFERENCED;
#if ALIGNMENT == 1
      {
        ptr_t alternate_ptr = bp + 1;
        ptr_t target = *(ptr_t *)bp;
        ptr_t alternate_target = *(ptr_t *)alternate_ptr;
        if ((word)alternate_target >= (word)GC_least_plausible_heap_addr
            && (word)alternate_target <= (word)GC_greatest_plausible_heap_addr
            && ((word)target < (word)GC_least_plausible_heap_addr
                || (word)target > (word)GC_greatest_plausible_heap_addr)) {
            bp = alternate_ptr;
        }
      }
#endif
    bp_base = (ptr_t)GC_base(bp);
    if (NULL == bp_base) {
      *base_p = bp;
      *offset_p = 0;
      return GC_REFD_FROM_ROOT;
    } else {
      if (GC_HAS_DEBUG_INFO(bp_base)) bp_base += sizeof(oh);
      *base_p = bp_base;
      *offset_p = bp - bp_base;
      return GC_REFD_FROM_HEAP;
    }
  }
  GC_API void * GC_CALL GC_generate_random_heap_address(void)
  {
    size_t i;
    word heap_offset = RANDOM();
    if (GC_heapsize > GC_RAND_MAX) {
        heap_offset *= GC_RAND_MAX;
        heap_offset += RANDOM();
    }
    heap_offset %= GC_heapsize;
    for (i = 0;; ++i) {
        size_t size;
        if (i >= GC_n_heap_sects)
          ABORT("GC_generate_random_heap_address: size inconsistency");
        size = GC_heap_sects[i].hs_bytes;
        if (heap_offset < size) {
            break;
        } else {
            heap_offset -= size;
        }
    }
    return GC_heap_sects[i].hs_start + heap_offset;
  }
  GC_API void * GC_CALL GC_generate_random_valid_address(void)
  {
    ptr_t result;
    ptr_t base;
    do {
      result = (ptr_t)GC_generate_random_heap_address();
      base = (ptr_t)GC_base(result);
    } while (NULL == base || !GC_is_marked(base));
    return result;
  }
  GC_API void GC_CALL GC_print_backtrace(void *p)
  {
    void *current = p;
    int i;
    GC_ref_kind source;
    size_t offset;
    void *base;
    GC_print_heap_obj((ptr_t)GC_base(current));
    for (i = 0; ; ++i) {
      source = GC_get_back_ptr_info(current, &base, &offset);
      if (GC_UNREFERENCED == source) {
        GC_err_printf("Reference could not be found\n");
        goto out;
      }
      if (GC_NO_SPACE == source) {
        GC_err_printf("No debug info in object: Can't find reference\n");
        goto out;
      }
      GC_err_printf("Reachable via %d levels of pointers from ", i);
      switch(source) {
        case GC_REFD_FROM_ROOT:
          GC_err_printf("root at %p\n\n", base);
          goto out;
        case GC_REFD_FROM_REG:
          GC_err_printf("root in register\n\n");
          goto out;
        case GC_FINALIZER_REFD:
          GC_err_printf("list of finalizable objects\n\n");
          goto out;
        case GC_REFD_FROM_HEAP:
          GC_err_printf("offset %ld in object:\n", (long)offset);
          GC_print_heap_obj((ptr_t)GC_base(base));
          break;
        default:
          GC_err_printf("INTERNAL ERROR: UNEXPECTED SOURCE!!!!\n");
          goto out;
      }
      current = base;
    }
    out:;
  }
  GC_INNER void GC_generate_random_backtrace_no_gc(void)
  {
    void * current;
    current = GC_generate_random_valid_address();
    GC_printf("\n****Chosen address %p in object\n", current);
    GC_print_backtrace(current);
  }
  GC_API void GC_CALL GC_generate_random_backtrace(void)
  {
    if (GC_try_to_collect(GC_never_stop_func) == 0) {
      GC_err_printf("Cannot generate a backtrace: "
                    "garbage collection is disabled!\n");
      return;
    }
    GC_generate_random_backtrace_no_gc();
  }
#endif
#define CROSSES_HBLK(p, sz) \
        (((word)((p) + sizeof(oh) + (sz) - 1) ^ (word)(p)) >= HBLKSIZE)
GC_INNER void *GC_store_debug_info_inner(void *p, word sz GC_ATTR_UNUSED,
                                         const char *string, int linenum)
{
    word * result = (word *)((oh *)p + 1);
    GC_ASSERT(I_HOLD_LOCK());
    GC_ASSERT(GC_size(p) >= sizeof(oh) + sz);
    GC_ASSERT(!(SMALL_OBJ(sz) && CROSSES_HBLK((ptr_t)p, sz)));
#ifdef KEEP_BACK_PTRS
      ((oh *)p) -> oh_back_ptr = HIDE_BACK_PTR(NOT_MARKED);
#endif
#ifdef MAKE_BACK_GRAPH
      ((oh *)p) -> oh_bg_ptr = HIDE_BACK_PTR((ptr_t)0);
#endif
    ((oh *)p) -> oh_string = string;
    ((oh *)p) -> oh_int = linenum;
#ifndef SHORT_DBG_HDRS
      ((oh *)p) -> oh_sz = sz;
      ((oh *)p) -> oh_sf = START_FLAG ^ (word)result;
      ((word *)p)[BYTES_TO_WORDS(GC_size(p))-1] =
         result[SIMPLE_ROUNDED_UP_WORDS(sz)] = END_FLAG ^ (word)result;
#endif
    return result;
}
static void *store_debug_info(void *p, size_t lb,
                              const char *fn, GC_EXTRA_PARAMS)
{
    void *result;
    DCL_LOCK_STATE;
    if (NULL == p) {
        GC_err_printf("%s(%lu) returning NULL (%s:%d)\n",
                      fn, (unsigned long)lb, s, i);
        return NULL;
    }
    LOCK();
    if (!GC_debugging_started)
        GC_start_debugging_inner();
    ADD_CALL_CHAIN(p, ra);
    result = GC_store_debug_info_inner(p, (word)lb, s, i);
    UNLOCK();
    return result;
}
#ifndef SHORT_DBG_HDRS
  STATIC ptr_t GC_check_annotated_obj(oh *ohdr)
  {
    ptr_t body = (ptr_t)(ohdr + 1);
    word gc_sz = GC_size((ptr_t)ohdr);
    if (ohdr -> oh_sz + DEBUG_BYTES > gc_sz) {
        return((ptr_t)(&(ohdr -> oh_sz)));
    }
    if (ohdr -> oh_sf != (START_FLAG ^ (word)body)) {
        return((ptr_t)(&(ohdr -> oh_sf)));
    }
    if (((word *)ohdr)[BYTES_TO_WORDS(gc_sz)-1] != (END_FLAG ^ (word)body)) {
        return (ptr_t)(&((word *)ohdr)[BYTES_TO_WORDS(gc_sz)-1]);
    }
    if (((word *)body)[SIMPLE_ROUNDED_UP_WORDS(ohdr -> oh_sz)]
        != (END_FLAG ^ (word)body)) {
        return (ptr_t)(&((word *)body)[SIMPLE_ROUNDED_UP_WORDS(ohdr->oh_sz)]);
    }
    return(0);
  }
#endif
STATIC GC_describe_type_fn GC_describe_type_fns[MAXOBJKINDS] = {0};
GC_API void GC_CALL GC_register_describe_type_fn(int kind,
                                                 GC_describe_type_fn fn)
{
  GC_describe_type_fns[kind] = fn;
}
#define GET_OH_LINENUM(ohdr) ((int)(ohdr)->oh_int)
#ifndef SHORT_DBG_HDRS
#define IF_NOT_SHORTDBG_HDRS(x) x
#define COMMA_IFNOT_SHORTDBG_HDRS(x) , x
#else
#define IF_NOT_SHORTDBG_HDRS(x)
#define COMMA_IFNOT_SHORTDBG_HDRS(x)
#endif
STATIC void GC_print_obj(ptr_t p)
{
    oh * ohdr = (oh *)GC_base(p);
    ptr_t q;
    hdr * hhdr;
    int kind;
    const char *kind_str;
    char buffer[GC_TYPE_DESCR_LEN + 1];
    GC_ASSERT(I_DONT_HOLD_LOCK());
#ifdef LINT2
      if (!ohdr) ABORT("Invalid GC_print_obj argument");
#endif
    q = (ptr_t)(ohdr + 1);
    hhdr = GC_find_header(q);
    kind = hhdr -> hb_obj_kind;
    if (0 != GC_describe_type_fns[kind] && GC_is_marked(ohdr)) {
        buffer[GC_TYPE_DESCR_LEN] = 0;
        (GC_describe_type_fns[kind])(q, buffer);
        GC_ASSERT(buffer[GC_TYPE_DESCR_LEN] == 0);
        kind_str = buffer;
    } else {
        switch(kind) {
          case PTRFREE:
            kind_str = "PTRFREE";
            break;
          case NORMAL:
            kind_str = "NORMAL";
            break;
          case UNCOLLECTABLE:
            kind_str = "UNCOLLECTABLE";
            break;
#ifdef GC_ATOMIC_UNCOLLECTABLE
            case AUNCOLLECTABLE:
              kind_str = "ATOMIC_UNCOLLECTABLE";
              break;
#endif
          default:
            kind_str = NULL;
        }
    }
    if (NULL != kind_str) {
        GC_err_printf("%p (%s:%d," IF_NOT_SHORTDBG_HDRS(" sz= %lu,") " %s)\n",
                      (void *)((ptr_t)ohdr + sizeof(oh)),
                      ohdr->oh_string, GET_OH_LINENUM(ohdr)
                      COMMA_IFNOT_SHORTDBG_HDRS((unsigned long)ohdr->oh_sz),
                      kind_str);
    } else {
        GC_err_printf("%p (%s:%d," IF_NOT_SHORTDBG_HDRS(" sz= %lu,")
                      " kind= %d, descr= 0x%lx)\n",
                      (void *)((ptr_t)ohdr + sizeof(oh)),
                      ohdr->oh_string, GET_OH_LINENUM(ohdr)
                      COMMA_IFNOT_SHORTDBG_HDRS((unsigned long)ohdr->oh_sz),
                      kind, (unsigned long)hhdr->hb_descr);
    }
    PRINT_CALL_CHAIN(ohdr);
}
STATIC void GC_debug_print_heap_obj_proc(ptr_t p)
{
    GC_ASSERT(I_DONT_HOLD_LOCK());
    if (GC_HAS_DEBUG_INFO(p)) {
        GC_print_obj(p);
    } else {
        GC_default_print_heap_obj_proc(p);
    }
}
#ifndef SHORT_DBG_HDRS
  STATIC void GC_print_smashed_obj(const char *msg, void *p,
                                   ptr_t clobbered_addr)
  {
    oh * ohdr = (oh *)GC_base(p);
    GC_ASSERT(I_DONT_HOLD_LOCK());
#ifdef LINT2
      if (!ohdr) ABORT("Invalid GC_print_smashed_obj argument");
#endif
    if ((word)clobbered_addr <= (word)(&ohdr->oh_sz)
        || ohdr -> oh_string == 0) {
        GC_err_printf(
                "%s %p in or near object at %p(<smashed>, appr. sz= %lu)\n",
                msg, (void *)clobbered_addr, p,
                (unsigned long)(GC_size((ptr_t)ohdr) - DEBUG_BYTES));
    } else {
        GC_err_printf("%s %p in or near object at %p (%s:%d, sz= %lu)\n",
                msg, (void *)clobbered_addr, p,
                (word)(ohdr -> oh_string) < HBLKSIZE ? "(smashed string)" :
                ohdr -> oh_string[0] == '\0' ? "EMPTY(smashed?)" :
                                                ohdr -> oh_string,
                GET_OH_LINENUM(ohdr), (unsigned long)(ohdr -> oh_sz));
        PRINT_CALL_CHAIN(ohdr);
    }
  }
  STATIC void GC_check_heap_proc (void);
  STATIC void GC_print_all_smashed_proc (void);
#else
  STATIC void GC_do_nothing(void) {}
#endif
GC_INNER void GC_start_debugging_inner(void)
{
  GC_ASSERT(I_HOLD_LOCK());
#ifndef SHORT_DBG_HDRS
    GC_check_heap = GC_check_heap_proc;
    GC_print_all_smashed = GC_print_all_smashed_proc;
#else
    GC_check_heap = GC_do_nothing;
    GC_print_all_smashed = GC_do_nothing;
#endif
  GC_print_heap_obj = GC_debug_print_heap_obj_proc;
  GC_debugging_started = TRUE;
  GC_register_displacement_inner((word)sizeof(oh));
#if defined(CPPCHECK)
    GC_noop1(GC_debug_header_size);
#endif
}
const size_t GC_debug_header_size = sizeof(oh);
GC_API size_t GC_CALL GC_get_debug_header_size(void) {
  return sizeof(oh);
}
GC_API void GC_CALL GC_debug_register_displacement(size_t offset)
{
  DCL_LOCK_STATE;
  LOCK();
  GC_register_displacement_inner(offset);
  GC_register_displacement_inner((word)sizeof(oh) + offset);
  UNLOCK();
}
#ifdef GC_ADD_CALLER
#if defined(HAVE_DLADDR) && defined(GC_HAVE_RETURN_ADDR_PARENT)
#include <dlfcn.h>
    STATIC void GC_caller_func_offset(word ad, const char **symp, int *offp)
    {
      Dl_info caller;
      if (ad && dladdr((void *)ad, &caller) && caller.dli_sname != NULL) {
        *symp = caller.dli_sname;
        *offp = (int)((char *)ad - (char *)caller.dli_saddr);
      }
      if (NULL == *symp) {
        *symp = "unknown";
      }
    }
#else
#define GC_caller_func_offset(ad, symp, offp) (void)(*(symp) = "unknown")
#endif
#endif
GC_API GC_ATTR_MALLOC void * GC_CALL GC_debug_malloc(size_t lb,
                                                     GC_EXTRA_PARAMS)
{
    void * result;
    result = GC_malloc(SIZET_SAT_ADD(lb, DEBUG_BYTES));
#ifdef GC_ADD_CALLER
      if (s == NULL) {
        GC_caller_func_offset(ra, &s, &i);
      }
#endif
    return store_debug_info(result, lb, "GC_debug_malloc", OPT_RA s, i);
}
GC_API GC_ATTR_MALLOC void * GC_CALL
    GC_debug_malloc_ignore_off_page(size_t lb, GC_EXTRA_PARAMS)
{
    void * result = GC_malloc_ignore_off_page(SIZET_SAT_ADD(lb, DEBUG_BYTES));
    return store_debug_info(result, lb, "GC_debug_malloc_ignore_off_page",
                            OPT_RA s, i);
}
GC_API GC_ATTR_MALLOC void * GC_CALL
    GC_debug_malloc_atomic_ignore_off_page(size_t lb, GC_EXTRA_PARAMS)
{
    void * result = GC_malloc_atomic_ignore_off_page(
                                SIZET_SAT_ADD(lb, DEBUG_BYTES));
    return store_debug_info(result, lb,
                            "GC_debug_malloc_atomic_ignore_off_page",
                            OPT_RA s, i);
}
STATIC void * GC_debug_generic_malloc(size_t lb, int knd, GC_EXTRA_PARAMS)
{
    void * result = GC_generic_malloc(SIZET_SAT_ADD(lb, DEBUG_BYTES), knd);
    return store_debug_info(result, lb, "GC_debug_generic_malloc",
                            OPT_RA s, i);
}
#ifdef DBG_HDRS_ALL
  GC_INNER void * GC_debug_generic_malloc_inner(size_t lb, int k)
  {
    void * result;
    GC_ASSERT(I_HOLD_LOCK());
    result = GC_generic_malloc_inner(SIZET_SAT_ADD(lb, DEBUG_BYTES), k);
    if (NULL == result) {
        GC_err_printf("GC internal allocation (%lu bytes) returning NULL\n",
                       (unsigned long) lb);
        return(0);
    }
    if (!GC_debugging_started) {
        GC_start_debugging_inner();
    }
    ADD_CALL_CHAIN(result, GC_RETURN_ADDR);
    return (GC_store_debug_info_inner(result, (word)lb, "INTERNAL", 0));
  }
  GC_INNER void * GC_debug_generic_malloc_inner_ignore_off_page(size_t lb,
                                                                int k)
  {
    void * result;
    GC_ASSERT(I_HOLD_LOCK());
    result = GC_generic_malloc_inner_ignore_off_page(
                                SIZET_SAT_ADD(lb, DEBUG_BYTES), k);
    if (NULL == result) {
        GC_err_printf("GC internal allocation (%lu bytes) returning NULL\n",
                       (unsigned long) lb);
        return(0);
    }
    if (!GC_debugging_started) {
        GC_start_debugging_inner();
    }
    ADD_CALL_CHAIN(result, GC_RETURN_ADDR);
    return (GC_store_debug_info_inner(result, (word)lb, "INTERNAL", 0));
  }
#endif
#ifndef CPPCHECK
  GC_API void * GC_CALL GC_debug_malloc_stubborn(size_t lb, GC_EXTRA_PARAMS)
  {
    return GC_debug_malloc(lb, OPT_RA s, i);
  }
  GC_API void GC_CALL GC_debug_change_stubborn(
                                const void * p GC_ATTR_UNUSED) {}
#endif
GC_API void GC_CALL GC_debug_end_stubborn_change(const void *p)
{
    const void * q = GC_base_C(p);
    if (NULL == q) {
        ABORT_ARG1("GC_debug_end_stubborn_change: bad arg", ": %p", p);
    }
    GC_end_stubborn_change(q);
}
GC_API void GC_CALL GC_debug_ptr_store_and_dirty(void *p, const void *q)
{
    *(void **)GC_is_visible(p) = GC_is_valid_displacement((void *)q);
    GC_debug_end_stubborn_change(p);
    REACHABLE_AFTER_DIRTY(q);
}
GC_API GC_ATTR_MALLOC void * GC_CALL GC_debug_malloc_atomic(size_t lb,
                                                            GC_EXTRA_PARAMS)
{
    void * result = GC_malloc_atomic(SIZET_SAT_ADD(lb, DEBUG_BYTES));
    return store_debug_info(result, lb, "GC_debug_malloc_atomic",
                            OPT_RA s, i);
}
GC_API GC_ATTR_MALLOC char * GC_CALL GC_debug_strdup(const char *str,
                                                     GC_EXTRA_PARAMS)
{
  char *copy;
  size_t lb;
  if (str == NULL) {
    if (GC_find_leak)
      GC_err_printf("strdup(NULL) behavior is undefined\n");
    return NULL;
  }
  lb = strlen(str) + 1;
  copy = (char *)GC_debug_malloc_atomic(lb, OPT_RA s, i);
  if (copy == NULL) {
#ifndef MSWINCE
      errno = ENOMEM;
#endif
    return NULL;
  }
  BCOPY(str, copy, lb);
  return copy;
}
GC_API GC_ATTR_MALLOC char * GC_CALL GC_debug_strndup(const char *str,
                                                size_t size, GC_EXTRA_PARAMS)
{
  char *copy;
  size_t len = strlen(str);
  if (len > size)
    len = size;
  copy = (char *)GC_debug_malloc_atomic(len + 1, OPT_RA s, i);
  if (copy == NULL) {
#ifndef MSWINCE
      errno = ENOMEM;
#endif
    return NULL;
  }
  if (len > 0)
    BCOPY(str, copy, len);
  copy[len] = '\0';
  return copy;
}
#ifdef GC_REQUIRE_WCSDUP
#include <wchar.h>
  GC_API GC_ATTR_MALLOC wchar_t * GC_CALL GC_debug_wcsdup(const wchar_t *str,
                                                          GC_EXTRA_PARAMS)
  {
    size_t lb = (wcslen(str) + 1) * sizeof(wchar_t);
    wchar_t *copy = (wchar_t *)GC_debug_malloc_atomic(lb, OPT_RA s, i);
    if (copy == NULL) {
#ifndef MSWINCE
        errno = ENOMEM;
#endif
      return NULL;
    }
    BCOPY(str, copy, lb);
    return copy;
  }
#endif
GC_API GC_ATTR_MALLOC void * GC_CALL GC_debug_malloc_uncollectable(size_t lb,
                                                        GC_EXTRA_PARAMS)
{
    void * result = GC_malloc_uncollectable(
                                SIZET_SAT_ADD(lb, UNCOLLECTABLE_DEBUG_BYTES));
    return store_debug_info(result, lb, "GC_debug_malloc_uncollectable",
                            OPT_RA s, i);
}
#ifdef GC_ATOMIC_UNCOLLECTABLE
  GC_API GC_ATTR_MALLOC void * GC_CALL
        GC_debug_malloc_atomic_uncollectable(size_t lb, GC_EXTRA_PARAMS)
  {
    void * result = GC_malloc_atomic_uncollectable(
                                SIZET_SAT_ADD(lb, UNCOLLECTABLE_DEBUG_BYTES));
    return store_debug_info(result, lb,
                            "GC_debug_malloc_atomic_uncollectable",
                            OPT_RA s, i);
  }
#endif
#ifndef GC_FREED_MEM_MARKER
#if CPP_WORDSZ == 32
#define GC_FREED_MEM_MARKER 0xdeadbeef
#else
#define GC_FREED_MEM_MARKER GC_WORD_C(0xEFBEADDEdeadbeef)
#endif
#endif
GC_API void GC_CALL GC_debug_free(void * p)
{
    ptr_t base;
    if (0 == p) return;
    base = (ptr_t)GC_base(p);
    if (NULL == base) {
#if defined(REDIRECT_MALLOC) \
         && ((defined(NEED_CALLINFO) && defined(GC_HAVE_BUILTIN_BACKTRACE)) \
             || defined(GC_LINUX_THREADS) || defined(GC_SOLARIS_THREADS) \
             || defined(MSWIN32))
        if (!GC_is_heap_ptr(p)) return;
#endif
      ABORT_ARG1("Invalid pointer passed to free()", ": %p", p);
    }
    if ((ptr_t)p - (ptr_t)base != sizeof(oh)) {
#if defined(REDIRECT_FREE) && defined(USE_PROC_FOR_LIBRARIES)
#endif
      GC_err_printf(
               "GC_debug_free called on pointer %p w/o debugging info\n", p);
    } else {
#ifndef SHORT_DBG_HDRS
        ptr_t clobbered = GC_check_annotated_obj((oh *)base);
        word sz = GC_size(base);
        if (clobbered != 0) {
          GC_have_errors = TRUE;
          if (((oh *)base) -> oh_sz == sz) {
            GC_print_smashed_obj(
                  "GC_debug_free: found previously deallocated (?) object at",
                  p, clobbered);
            return;
          } else {
            GC_print_smashed_obj("GC_debug_free: found smashed location at",
                                 p, clobbered);
          }
        }
        ((oh *)base) -> oh_sz = sz;
#endif
    }
    if (GC_find_leak
#ifndef SHORT_DBG_HDRS
          && ((ptr_t)p - (ptr_t)base != sizeof(oh) || !GC_findleak_delay_free)
#endif
        ) {
      GC_free(base);
    } else {
      hdr * hhdr = HDR(p);
      if (hhdr -> hb_obj_kind == UNCOLLECTABLE
#ifdef GC_ATOMIC_UNCOLLECTABLE
            || hhdr -> hb_obj_kind == AUNCOLLECTABLE
#endif
          ) {
        GC_free(base);
      } else {
        word i;
        word sz = hhdr -> hb_sz;
        word obj_sz = BYTES_TO_WORDS(sz - sizeof(oh));
        for (i = 0; i < obj_sz; ++i)
          ((word *)p)[i] = GC_FREED_MEM_MARKER;
        GC_ASSERT((word *)p + i == (word *)(base + sz));
        LOCK();
        GC_bytes_freed += sz;
        UNLOCK();
      }
    }
}
#if defined(THREADS) && defined(DBG_HDRS_ALL)
  GC_INNER void GC_debug_free_inner(void * p)
  {
    ptr_t base = (ptr_t)GC_base(p);
    GC_ASSERT((ptr_t)p - (ptr_t)base == sizeof(oh));
#ifdef LINT2
      if (!base) ABORT("Invalid GC_debug_free_inner argument");
#endif
#ifndef SHORT_DBG_HDRS
      ((oh *)base) -> oh_sz = GC_size(base);
#endif
    GC_free_inner(base);
  }
#endif
GC_API void * GC_CALL GC_debug_realloc(void * p, size_t lb, GC_EXTRA_PARAMS)
{
    void * base;
    void * result;
    hdr * hhdr;
    if (p == 0) {
      return GC_debug_malloc(lb, OPT_RA s, i);
    }
    if (0 == lb)  {
      GC_debug_free(p);
      return NULL;
    }
#ifdef GC_ADD_CALLER
      if (s == NULL) {
        GC_caller_func_offset(ra, &s, &i);
      }
#endif
    base = GC_base(p);
    if (base == 0) {
        ABORT_ARG1("Invalid pointer passed to realloc()", ": %p", p);
    }
    if ((ptr_t)p - (ptr_t)base != sizeof(oh)) {
        GC_err_printf(
              "GC_debug_realloc called on pointer %p w/o debugging info\n", p);
        return(GC_realloc(p, lb));
    }
    hhdr = HDR(base);
    switch (hhdr -> hb_obj_kind) {
      case NORMAL:
        result = GC_debug_malloc(lb, OPT_RA s, i);
        break;
      case PTRFREE:
        result = GC_debug_malloc_atomic(lb, OPT_RA s, i);
        break;
      case UNCOLLECTABLE:
        result = GC_debug_malloc_uncollectable(lb, OPT_RA s, i);
        break;
#ifdef GC_ATOMIC_UNCOLLECTABLE
      case AUNCOLLECTABLE:
        result = GC_debug_malloc_atomic_uncollectable(lb, OPT_RA s, i);
        break;
#endif
      default:
        result = NULL;
        ABORT_RET("GC_debug_realloc: encountered bad kind");
    }
    if (result != NULL) {
      size_t old_sz;
#ifdef SHORT_DBG_HDRS
        old_sz = GC_size(base) - sizeof(oh);
#else
        old_sz = ((oh *)base) -> oh_sz;
#endif
      if (old_sz > 0)
        BCOPY(p, result, old_sz < lb ? old_sz : lb);
      GC_debug_free(p);
    }
    return(result);
}
GC_API GC_ATTR_MALLOC void * GC_CALL
    GC_debug_generic_or_special_malloc(size_t lb, int knd, GC_EXTRA_PARAMS)
{
    switch (knd) {
        case PTRFREE:
            return GC_debug_malloc_atomic(lb, OPT_RA s, i);
        case NORMAL:
            return GC_debug_malloc(lb, OPT_RA s, i);
        case UNCOLLECTABLE:
            return GC_debug_malloc_uncollectable(lb, OPT_RA s, i);
#ifdef GC_ATOMIC_UNCOLLECTABLE
        case AUNCOLLECTABLE:
            return GC_debug_malloc_atomic_uncollectable(lb, OPT_RA s, i);
#endif
        default:
            return GC_debug_generic_malloc(lb, knd, OPT_RA s, i);
    }
}
#ifndef SHORT_DBG_HDRS
#ifndef MAX_SMASHED
#define MAX_SMASHED 20
#endif
STATIC ptr_t GC_smashed[MAX_SMASHED] = {0};
STATIC unsigned GC_n_smashed = 0;
STATIC void GC_add_smashed(ptr_t smashed)
{
    GC_ASSERT(GC_is_marked(GC_base(smashed)));
    GC_smashed[GC_n_smashed] = smashed;
    if (GC_n_smashed < MAX_SMASHED - 1) ++GC_n_smashed;
    GC_have_errors = TRUE;
}
STATIC void GC_print_all_smashed_proc(void)
{
    unsigned i;
    GC_ASSERT(I_DONT_HOLD_LOCK());
    if (GC_n_smashed == 0) return;
    GC_err_printf("GC_check_heap_block: found %u smashed heap objects:\n",
                  GC_n_smashed);
    for (i = 0; i < GC_n_smashed; ++i) {
        ptr_t base = (ptr_t)GC_base(GC_smashed[i]);
#ifdef LINT2
          if (!base) ABORT("Invalid GC_smashed element");
#endif
        GC_print_smashed_obj("", base + sizeof(oh), GC_smashed[i]);
        GC_smashed[i] = 0;
    }
    GC_n_smashed = 0;
}
STATIC void GC_check_heap_block(struct hblk *hbp, word dummy GC_ATTR_UNUSED)
{
    struct hblkhdr * hhdr = HDR(hbp);
    word sz = hhdr -> hb_sz;
    word bit_no;
    char *p, *plim;
    p = hbp->hb_body;
    if (sz > MAXOBJBYTES) {
      plim = p;
    } else {
      plim = hbp->hb_body + HBLKSIZE - sz;
    }
    for (bit_no = 0; (word)p <= (word)plim;
         bit_no += MARK_BIT_OFFSET(sz), p += sz) {
      if (mark_bit_from_hdr(hhdr, bit_no) && GC_HAS_DEBUG_INFO((ptr_t)p)) {
        ptr_t clobbered = GC_check_annotated_obj((oh *)p);
        if (clobbered != 0)
          GC_add_smashed(clobbered);
      }
    }
}
STATIC void GC_check_heap_proc(void)
{
  GC_STATIC_ASSERT((sizeof(oh) & (GRANULE_BYTES - 1)) == 0);
  GC_apply_to_all_blocks(GC_check_heap_block, 0);
}
GC_INNER GC_bool GC_check_leaked(ptr_t base)
{
  word i;
  word obj_sz;
  word *p;
  if (
#if defined(KEEP_BACK_PTRS) || defined(MAKE_BACK_GRAPH)
        (*(word *)base & 1) != 0 &&
#endif
      GC_has_other_debug_info(base) >= 0)
    return TRUE;
  p = (word *)(base + sizeof(oh));
  obj_sz = BYTES_TO_WORDS(HDR(base)->hb_sz - sizeof(oh));
  for (i = 0; i < obj_sz; ++i)
    if (p[i] != GC_FREED_MEM_MARKER) {
        GC_set_mark_bit(base);
        GC_add_smashed((ptr_t)(&p[i]));
        break;
    }
  return FALSE;
}
#endif
#ifndef GC_NO_FINALIZATION
struct closure {
    GC_finalization_proc cl_fn;
    void * cl_data;
};
STATIC void * GC_make_closure(GC_finalization_proc fn, void * data)
{
    struct closure * result =
#ifdef DBG_HDRS_ALL
      (struct closure *) GC_debug_malloc(sizeof (struct closure),
                                         GC_EXTRAS);
#else
      (struct closure *) GC_malloc(sizeof (struct closure));
#endif
    if (result != 0) {
      result -> cl_fn = fn;
      result -> cl_data = data;
    }
    return((void *)result);
}
STATIC void GC_CALLBACK GC_debug_invoke_finalizer(void * obj, void * data)
{
    struct closure * cl = (struct closure *) data;
    (*(cl -> cl_fn))((void *)((char *)obj + sizeof(oh)), cl -> cl_data);
}
#define OFN_UNSET ((GC_finalization_proc)~(signed_word)0)
static void store_old(void *obj, GC_finalization_proc my_old_fn,
                      struct closure *my_old_cd, GC_finalization_proc *ofn,
                      void **ocd)
{
    if (0 != my_old_fn) {
      if (my_old_fn == OFN_UNSET) {
        return;
      }
      if (my_old_fn != GC_debug_invoke_finalizer) {
        GC_err_printf("Debuggable object at %p had a non-debug finalizer\n",
                      obj);
      } else {
        if (ofn) *ofn = my_old_cd -> cl_fn;
        if (ocd) *ocd = my_old_cd -> cl_data;
      }
    } else {
      if (ofn) *ofn = 0;
      if (ocd) *ocd = 0;
    }
}
GC_API void GC_CALL GC_debug_register_finalizer(void * obj,
                                        GC_finalization_proc fn,
                                        void * cd, GC_finalization_proc *ofn,
                                        void * *ocd)
{
    GC_finalization_proc my_old_fn = OFN_UNSET;
    void * my_old_cd;
    ptr_t base = (ptr_t)GC_base(obj);
    if (NULL == base) {
        if (ocd) *ocd = 0;
        if (ofn) *ofn = 0;
        return;
    }
    if ((ptr_t)obj - base != sizeof(oh)) {
        GC_err_printf("GC_debug_register_finalizer called with"
                      " non-base-pointer %p\n", obj);
    }
    if (0 == fn) {
      GC_register_finalizer(base, 0, 0, &my_old_fn, &my_old_cd);
    } else {
      cd = GC_make_closure(fn, cd);
      if (cd == 0) return;
      GC_register_finalizer(base, GC_debug_invoke_finalizer,
                            cd, &my_old_fn, &my_old_cd);
    }
    store_old(obj, my_old_fn, (struct closure *)my_old_cd, ofn, ocd);
}
GC_API void GC_CALL GC_debug_register_finalizer_no_order
                                    (void * obj, GC_finalization_proc fn,
                                     void * cd, GC_finalization_proc *ofn,
                                     void * *ocd)
{
    GC_finalization_proc my_old_fn = OFN_UNSET;
    void * my_old_cd;
    ptr_t base = (ptr_t)GC_base(obj);
    if (NULL == base) {
        if (ocd) *ocd = 0;
        if (ofn) *ofn = 0;
        return;
    }
    if ((ptr_t)obj - base != sizeof(oh)) {
        GC_err_printf("GC_debug_register_finalizer_no_order called with"
                      " non-base-pointer %p\n", obj);
    }
    if (0 == fn) {
      GC_register_finalizer_no_order(base, 0, 0, &my_old_fn, &my_old_cd);
    } else {
      cd = GC_make_closure(fn, cd);
      if (cd == 0) return;
      GC_register_finalizer_no_order(base, GC_debug_invoke_finalizer,
                                     cd, &my_old_fn, &my_old_cd);
    }
    store_old(obj, my_old_fn, (struct closure *)my_old_cd, ofn, ocd);
}
GC_API void GC_CALL GC_debug_register_finalizer_unreachable
                                    (void * obj, GC_finalization_proc fn,
                                     void * cd, GC_finalization_proc *ofn,
                                     void * *ocd)
{
    GC_finalization_proc my_old_fn = OFN_UNSET;
    void * my_old_cd;
    ptr_t base = (ptr_t)GC_base(obj);
    if (NULL == base) {
        if (ocd) *ocd = 0;
        if (ofn) *ofn = 0;
        return;
    }
    if ((ptr_t)obj - base != sizeof(oh)) {
        GC_err_printf("GC_debug_register_finalizer_unreachable called with"
                      " non-base-pointer %p\n", obj);
    }
    if (0 == fn) {
      GC_register_finalizer_unreachable(base, 0, 0, &my_old_fn, &my_old_cd);
    } else {
      cd = GC_make_closure(fn, cd);
      if (cd == 0) return;
      GC_register_finalizer_unreachable(base, GC_debug_invoke_finalizer,
                                        cd, &my_old_fn, &my_old_cd);
    }
    store_old(obj, my_old_fn, (struct closure *)my_old_cd, ofn, ocd);
}
GC_API void GC_CALL GC_debug_register_finalizer_ignore_self
                                    (void * obj, GC_finalization_proc fn,
                                     void * cd, GC_finalization_proc *ofn,
                                     void * *ocd)
{
    GC_finalization_proc my_old_fn = OFN_UNSET;
    void * my_old_cd;
    ptr_t base = (ptr_t)GC_base(obj);
    if (NULL == base) {
        if (ocd) *ocd = 0;
        if (ofn) *ofn = 0;
        return;
    }
    if ((ptr_t)obj - base != sizeof(oh)) {
        GC_err_printf("GC_debug_register_finalizer_ignore_self called with"
                      " non-base-pointer %p\n", obj);
    }
    if (0 == fn) {
      GC_register_finalizer_ignore_self(base, 0, 0, &my_old_fn, &my_old_cd);
    } else {
      cd = GC_make_closure(fn, cd);
      if (cd == 0) return;
      GC_register_finalizer_ignore_self(base, GC_debug_invoke_finalizer,
                                        cd, &my_old_fn, &my_old_cd);
    }
    store_old(obj, my_old_fn, (struct closure *)my_old_cd, ofn, ocd);
}
#endif
GC_API GC_ATTR_MALLOC void * GC_CALL GC_debug_malloc_replacement(size_t lb)
{
    return GC_debug_malloc(lb, GC_DBG_EXTRAS);
}
GC_API void * GC_CALL GC_debug_realloc_replacement(void *p, size_t lb)
{
    return GC_debug_realloc(p, lb, GC_DBG_EXTRAS);
}
#ifndef GC_NO_FINALIZATION
#ifndef GC_JAVAXFC_H
#define GC_JAVAXFC_H
#ifndef GC_H
#endif
#ifdef __cplusplus
  extern "C" {
#endif
GC_API void GC_CALL GC_finalize_all(void);
#ifdef GC_THREADS
#ifndef GC_SUSPEND_THREAD_ID
#define GC_SUSPEND_THREAD_ID void*
#endif
  GC_API void GC_CALL GC_suspend_thread(GC_SUSPEND_THREAD_ID);
  GC_API void GC_CALL GC_resume_thread(GC_SUSPEND_THREAD_ID);
  GC_API int GC_CALL GC_is_thread_suspended(GC_SUSPEND_THREAD_ID);
#endif
#ifdef __cplusplus
  }
#endif
#endif
typedef void (* finalization_mark_proc)(ptr_t );
#define HASH3(addr,size,log_size) \
        ((((word)(addr) >> 3) ^ ((word)(addr) >> (3 + (log_size)))) \
         & ((size) - 1))
#define HASH2(addr,log_size) HASH3(addr, (word)1 << (log_size), log_size)
struct hash_chain_entry {
    word hidden_key;
    struct hash_chain_entry * next;
};
struct disappearing_link {
    struct hash_chain_entry prolog;
#define dl_hidden_link prolog.hidden_key
#define dl_next(x) (struct disappearing_link *)((x) -> prolog.next)
#define dl_set_next(x, y) \
                (void)((x)->prolog.next = (struct hash_chain_entry *)(y))
    word dl_hidden_obj;
};
struct finalizable_object {
    struct hash_chain_entry prolog;
#define fo_hidden_base prolog.hidden_key
#define fo_next(x) (struct finalizable_object *)((x) -> prolog.next)
#define fo_set_next(x,y) ((x)->prolog.next = (struct hash_chain_entry *)(y))
    GC_finalization_proc fo_fn;
    ptr_t fo_client_data;
    word fo_object_size;
    finalization_mark_proc fo_mark_proc;
};
#ifdef AO_HAVE_store
#define SET_FINALIZE_NOW(fo) \
            AO_store((volatile AO_t *)&GC_fnlz_roots.finalize_now, (AO_t)(fo))
#else
#define SET_FINALIZE_NOW(fo) (void)(GC_fnlz_roots.finalize_now = (fo))
#endif
GC_API void GC_CALL GC_push_finalizer_structures(void)
{
  GC_ASSERT((word)(&GC_dl_hashtbl.head) % sizeof(word) == 0);
  GC_ASSERT((word)(&GC_fnlz_roots) % sizeof(word) == 0);
#ifndef GC_LONG_REFS_NOT_NEEDED
    GC_ASSERT((word)(&GC_ll_hashtbl.head) % sizeof(word) == 0);
    GC_PUSH_ALL_SYM(GC_ll_hashtbl.head);
#endif
  GC_PUSH_ALL_SYM(GC_dl_hashtbl.head);
  GC_PUSH_ALL_SYM(GC_fnlz_roots);
}
#ifndef GC_ON_GROW_LOG_SIZE_MIN
#define GC_ON_GROW_LOG_SIZE_MIN CPP_LOG_HBLKSIZE
#endif
STATIC void GC_grow_table(struct hash_chain_entry ***table,
                          unsigned *log_size_ptr, word *entries_ptr)
{
    word i;
    struct hash_chain_entry *p;
    unsigned log_old_size = *log_size_ptr;
    unsigned log_new_size = log_old_size + 1;
    word old_size = *table == NULL ? 0 : (word)1 << log_old_size;
    word new_size = (word)1 << log_new_size;
    struct hash_chain_entry **new_table;
    GC_ASSERT(I_HOLD_LOCK());
    if (log_old_size >= GC_ON_GROW_LOG_SIZE_MIN && !GC_incremental) {
      IF_CANCEL(int cancel_state;)
      DISABLE_CANCEL(cancel_state);
      (void)GC_try_to_collect_inner(GC_never_stop_func);
      RESTORE_CANCEL(cancel_state);
      if (*entries_ptr < ((word)1 << log_old_size) - (*entries_ptr >> 2))
        return;
    }
    new_table = (struct hash_chain_entry **)
                    GC_INTERNAL_MALLOC_IGNORE_OFF_PAGE(
                        (size_t)new_size * sizeof(struct hash_chain_entry *),
                        NORMAL);
    if (new_table == 0) {
        if (*table == 0) {
            ABORT("Insufficient space for initial table allocation");
        } else {
            return;
        }
    }
    for (i = 0; i < old_size; i++) {
      p = (*table)[i];
      while (p != 0) {
        ptr_t real_key = (ptr_t)GC_REVEAL_POINTER(p->hidden_key);
        struct hash_chain_entry *next = p -> next;
        size_t new_hash = HASH3(real_key, new_size, log_new_size);
        p -> next = new_table[new_hash];
        GC_dirty(p);
        new_table[new_hash] = p;
        p = next;
      }
    }
    *log_size_ptr = log_new_size;
    *table = new_table;
    GC_dirty(new_table);
}
GC_API int GC_CALL GC_register_disappearing_link(void * * link)
{
    ptr_t base;
    base = (ptr_t)GC_base(link);
    if (base == 0)
        ABORT("Bad arg to GC_register_disappearing_link");
    return(GC_general_register_disappearing_link(link, base));
}
STATIC int GC_register_disappearing_link_inner(
                        struct dl_hashtbl_s *dl_hashtbl, void **link,
                        const void *obj, const char *tbl_log_name)
{
    struct disappearing_link *curr_dl;
    size_t index;
    struct disappearing_link * new_dl;
    DCL_LOCK_STATE;
    if (EXPECT(GC_find_leak, FALSE)) return GC_UNIMPLEMENTED;
    LOCK();
    GC_ASSERT(obj != NULL && GC_base_C(obj) == obj);
    if (EXPECT(NULL == dl_hashtbl -> head, FALSE)
        || EXPECT(dl_hashtbl -> entries
                  > ((word)1 << dl_hashtbl -> log_size), FALSE)) {
        GC_grow_table((struct hash_chain_entry ***)&dl_hashtbl -> head,
                      &dl_hashtbl -> log_size, &dl_hashtbl -> entries);
        GC_COND_LOG_PRINTF("Grew %s table to %u entries\n", tbl_log_name,
                           1U << dl_hashtbl -> log_size);
    }
    index = HASH2(link, dl_hashtbl -> log_size);
    for (curr_dl = dl_hashtbl -> head[index]; curr_dl != 0;
         curr_dl = dl_next(curr_dl)) {
        if (curr_dl -> dl_hidden_link == GC_HIDE_POINTER(link)) {
            curr_dl -> dl_hidden_obj = GC_HIDE_POINTER(obj);
            UNLOCK();
            return GC_DUPLICATE;
        }
    }
    new_dl = (struct disappearing_link *)
        GC_INTERNAL_MALLOC(sizeof(struct disappearing_link),NORMAL);
    if (0 == new_dl) {
      GC_oom_func oom_fn = GC_oom_fn;
      UNLOCK();
      new_dl = (struct disappearing_link *)
                (*oom_fn)(sizeof(struct disappearing_link));
      if (0 == new_dl) {
        return GC_NO_MEMORY;
      }
      LOCK();
      index = HASH2(link, dl_hashtbl -> log_size);
      for (curr_dl = dl_hashtbl -> head[index]; curr_dl != 0;
           curr_dl = dl_next(curr_dl)) {
        if (curr_dl -> dl_hidden_link == GC_HIDE_POINTER(link)) {
          curr_dl -> dl_hidden_obj = GC_HIDE_POINTER(obj);
          UNLOCK();
#ifndef DBG_HDRS_ALL
            GC_free((void *)new_dl);
#endif
          return GC_DUPLICATE;
        }
      }
    }
    new_dl -> dl_hidden_obj = GC_HIDE_POINTER(obj);
    new_dl -> dl_hidden_link = GC_HIDE_POINTER(link);
    dl_set_next(new_dl, dl_hashtbl -> head[index]);
    GC_dirty(new_dl);
    dl_hashtbl -> head[index] = new_dl;
    dl_hashtbl -> entries++;
    GC_dirty(dl_hashtbl->head + index);
    UNLOCK();
    return GC_SUCCESS;
}
GC_API int GC_CALL GC_general_register_disappearing_link(void * * link,
                                                         const void * obj)
{
    if (((word)link & (ALIGNMENT-1)) != 0 || !NONNULL_ARG_NOT_NULL(link))
        ABORT("Bad arg to GC_general_register_disappearing_link");
    return GC_register_disappearing_link_inner(&GC_dl_hashtbl, link, obj,
                                               "dl");
}
#ifdef DBG_HDRS_ALL
#define FREE_DL_ENTRY(curr_dl) dl_set_next(curr_dl, NULL)
#else
#define FREE_DL_ENTRY(curr_dl) GC_free(curr_dl)
#endif
GC_INLINE struct disappearing_link *GC_unregister_disappearing_link_inner(
                                struct dl_hashtbl_s *dl_hashtbl, void **link)
{
    struct disappearing_link *curr_dl;
    struct disappearing_link *prev_dl = NULL;
    size_t index;
    GC_ASSERT(I_HOLD_LOCK());
    if (EXPECT(NULL == dl_hashtbl -> head, FALSE)) return NULL;
    index = HASH2(link, dl_hashtbl -> log_size);
    for (curr_dl = dl_hashtbl -> head[index]; curr_dl;
         curr_dl = dl_next(curr_dl)) {
        if (curr_dl -> dl_hidden_link == GC_HIDE_POINTER(link)) {
            if (NULL == prev_dl) {
                dl_hashtbl -> head[index] = dl_next(curr_dl);
                GC_dirty(dl_hashtbl->head + index);
            } else {
                dl_set_next(prev_dl, dl_next(curr_dl));
                GC_dirty(prev_dl);
            }
            dl_hashtbl -> entries--;
            break;
        }
        prev_dl = curr_dl;
    }
    return curr_dl;
}
GC_API int GC_CALL GC_unregister_disappearing_link(void * * link)
{
    struct disappearing_link *curr_dl;
    DCL_LOCK_STATE;
    if (((word)link & (ALIGNMENT-1)) != 0) return(0);
    LOCK();
    curr_dl = GC_unregister_disappearing_link_inner(&GC_dl_hashtbl, link);
    UNLOCK();
    if (NULL == curr_dl) return 0;
    FREE_DL_ENTRY(curr_dl);
    return 1;
}
#ifndef GC_TOGGLE_REFS_NOT_NEEDED
  typedef union toggle_ref_u GCToggleRef;
  STATIC GC_toggleref_func GC_toggleref_callback = 0;
  GC_INNER void GC_process_togglerefs(void)
  {
    size_t i;
    size_t new_size = 0;
    GC_bool needs_barrier = FALSE;
    GC_ASSERT(I_HOLD_LOCK());
    for (i = 0; i < GC_toggleref_array_size; ++i) {
      GCToggleRef r = GC_toggleref_arr[i];
      void *obj = r.strong_ref;
      if (((word)obj & 1) != 0) {
        obj = GC_REVEAL_POINTER(r.weak_ref);
      }
      if (NULL == obj) {
        continue;
      }
      switch (GC_toggleref_callback(obj)) {
      case GC_TOGGLE_REF_DROP:
        break;
      case GC_TOGGLE_REF_STRONG:
        GC_toggleref_arr[new_size++].strong_ref = obj;
        needs_barrier = TRUE;
        break;
      case GC_TOGGLE_REF_WEAK:
        GC_toggleref_arr[new_size++].weak_ref = GC_HIDE_POINTER(obj);
        break;
      default:
        ABORT("Bad toggle-ref status returned by callback");
      }
    }
    if (new_size < GC_toggleref_array_size) {
      BZERO(&GC_toggleref_arr[new_size],
            (GC_toggleref_array_size - new_size) * sizeof(GCToggleRef));
      GC_toggleref_array_size = new_size;
    }
    if (needs_barrier)
      GC_dirty(GC_toggleref_arr);
  }
  STATIC void GC_normal_finalize_mark_proc(ptr_t);
  static void push_and_mark_object(void *p)
  {
    GC_normal_finalize_mark_proc((ptr_t)p);
    while (!GC_mark_stack_empty()) {
      MARK_FROM_MARK_STACK();
    }
    GC_set_mark_bit(p);
    if (GC_mark_state != MS_NONE) {
      while (!GC_mark_some(0)) {
      }
    }
  }
  STATIC void GC_mark_togglerefs(void)
  {
    size_t i;
    if (NULL == GC_toggleref_arr)
      return;
    GC_set_mark_bit(GC_toggleref_arr);
    for (i = 0; i < GC_toggleref_array_size; ++i) {
      void *obj = GC_toggleref_arr[i].strong_ref;
      if (obj != NULL && ((word)obj & 1) == 0) {
        push_and_mark_object(obj);
      }
    }
  }
  STATIC void GC_clear_togglerefs(void)
  {
    size_t i;
    for (i = 0; i < GC_toggleref_array_size; ++i) {
      if ((GC_toggleref_arr[i].weak_ref & 1) != 0) {
        if (!GC_is_marked(GC_REVEAL_POINTER(GC_toggleref_arr[i].weak_ref))) {
          GC_toggleref_arr[i].weak_ref = 0;
        } else {
        }
      }
    }
  }
  GC_API void GC_CALL GC_set_toggleref_func(GC_toggleref_func fn)
  {
    DCL_LOCK_STATE;
    LOCK();
    GC_toggleref_callback = fn;
    UNLOCK();
  }
  GC_API GC_toggleref_func GC_CALL GC_get_toggleref_func(void)
  {
    GC_toggleref_func fn;
    DCL_LOCK_STATE;
    LOCK();
    fn = GC_toggleref_callback;
    UNLOCK();
    return fn;
  }
  static GC_bool ensure_toggleref_capacity(size_t capacity_inc)
  {
    GC_ASSERT(I_HOLD_LOCK());
    if (NULL == GC_toggleref_arr) {
      GC_toggleref_array_capacity = 32;
      GC_toggleref_arr = (GCToggleRef *)GC_INTERNAL_MALLOC_IGNORE_OFF_PAGE(
                        GC_toggleref_array_capacity * sizeof(GCToggleRef),
                        NORMAL);
      if (NULL == GC_toggleref_arr)
        return FALSE;
    }
    if (GC_toggleref_array_size + capacity_inc
        >= GC_toggleref_array_capacity) {
      GCToggleRef *new_array;
      while (GC_toggleref_array_capacity
              < GC_toggleref_array_size + capacity_inc) {
        GC_toggleref_array_capacity *= 2;
        if ((GC_toggleref_array_capacity
             & ((size_t)1 << (sizeof(size_t) * 8 - 1))) != 0)
          return FALSE;
      }
      new_array = (GCToggleRef *)GC_INTERNAL_MALLOC_IGNORE_OFF_PAGE(
                        GC_toggleref_array_capacity * sizeof(GCToggleRef),
                        NORMAL);
      if (NULL == new_array)
        return FALSE;
      if (EXPECT(GC_toggleref_array_size > 0, TRUE))
        BCOPY(GC_toggleref_arr, new_array,
              GC_toggleref_array_size * sizeof(GCToggleRef));
      GC_INTERNAL_FREE(GC_toggleref_arr);
      GC_toggleref_arr = new_array;
    }
    return TRUE;
  }
  GC_API int GC_CALL GC_toggleref_add(void *obj, int is_strong_ref)
  {
    int res = GC_SUCCESS;
    DCL_LOCK_STATE;
    GC_ASSERT(NONNULL_ARG_NOT_NULL(obj));
    LOCK();
    if (GC_toggleref_callback != 0) {
      if (!ensure_toggleref_capacity(1)) {
        res = GC_NO_MEMORY;
      } else {
        GC_toggleref_arr[GC_toggleref_array_size].strong_ref =
                        is_strong_ref ? obj : (void *)GC_HIDE_POINTER(obj);
        if (is_strong_ref)
          GC_dirty(GC_toggleref_arr + GC_toggleref_array_size);
        GC_toggleref_array_size++;
      }
    }
    UNLOCK();
    return res;
  }
#endif
STATIC GC_await_finalize_proc GC_object_finalized_proc = 0;
GC_API void GC_CALL GC_set_await_finalize_proc(GC_await_finalize_proc fn)
{
  DCL_LOCK_STATE;
  LOCK();
  GC_object_finalized_proc = fn;
  UNLOCK();
}
GC_API GC_await_finalize_proc GC_CALL GC_get_await_finalize_proc(void)
{
  GC_await_finalize_proc fn;
  DCL_LOCK_STATE;
  LOCK();
  fn = GC_object_finalized_proc;
  UNLOCK();
  return fn;
}
#ifndef GC_LONG_REFS_NOT_NEEDED
  GC_API int GC_CALL GC_register_long_link(void * * link, const void * obj)
  {
    if (((word)link & (ALIGNMENT-1)) != 0 || !NONNULL_ARG_NOT_NULL(link))
        ABORT("Bad arg to GC_register_long_link");
    return GC_register_disappearing_link_inner(&GC_ll_hashtbl, link, obj,
                                               "long dl");
  }
  GC_API int GC_CALL GC_unregister_long_link(void * * link)
  {
    struct disappearing_link *curr_dl;
    DCL_LOCK_STATE;
    if (((word)link & (ALIGNMENT-1)) != 0) return(0);
    LOCK();
    curr_dl = GC_unregister_disappearing_link_inner(&GC_ll_hashtbl, link);
    UNLOCK();
    if (NULL == curr_dl) return 0;
    FREE_DL_ENTRY(curr_dl);
    return 1;
  }
#endif
#ifndef GC_MOVE_DISAPPEARING_LINK_NOT_NEEDED
  STATIC int GC_move_disappearing_link_inner(
                                struct dl_hashtbl_s *dl_hashtbl,
                                void **link, void **new_link)
  {
    struct disappearing_link *curr_dl, *new_dl;
    struct disappearing_link *prev_dl = NULL;
    size_t curr_index, new_index;
    word curr_hidden_link, new_hidden_link;
    GC_ASSERT(I_HOLD_LOCK());
    if (EXPECT(NULL == dl_hashtbl -> head, FALSE)) return GC_NOT_FOUND;
    curr_index = HASH2(link, dl_hashtbl -> log_size);
    curr_hidden_link = GC_HIDE_POINTER(link);
    for (curr_dl = dl_hashtbl -> head[curr_index]; curr_dl;
         curr_dl = dl_next(curr_dl)) {
      if (curr_dl -> dl_hidden_link == curr_hidden_link)
        break;
      prev_dl = curr_dl;
    }
    if (EXPECT(NULL == curr_dl, FALSE)) {
      return GC_NOT_FOUND;
    } else if (link == new_link) {
      return GC_SUCCESS;
    }
    new_index = HASH2(new_link, dl_hashtbl -> log_size);
    new_hidden_link = GC_HIDE_POINTER(new_link);
    for (new_dl = dl_hashtbl -> head[new_index]; new_dl;
         new_dl = dl_next(new_dl)) {
      if (new_dl -> dl_hidden_link == new_hidden_link) {
        return GC_DUPLICATE;
      }
    }
    if (NULL == prev_dl) {
      dl_hashtbl -> head[curr_index] = dl_next(curr_dl);
    } else {
      dl_set_next(prev_dl, dl_next(curr_dl));
      GC_dirty(prev_dl);
    }
    curr_dl -> dl_hidden_link = new_hidden_link;
    dl_set_next(curr_dl, dl_hashtbl -> head[new_index]);
    dl_hashtbl -> head[new_index] = curr_dl;
    GC_dirty(curr_dl);
    GC_dirty(dl_hashtbl->head);
    return GC_SUCCESS;
  }
  GC_API int GC_CALL GC_move_disappearing_link(void **link, void **new_link)
  {
    int result;
    DCL_LOCK_STATE;
    if (((word)new_link & (ALIGNMENT-1)) != 0
        || !NONNULL_ARG_NOT_NULL(new_link))
      ABORT("Bad new_link arg to GC_move_disappearing_link");
    if (((word)link & (ALIGNMENT-1)) != 0)
      return GC_NOT_FOUND;
    LOCK();
    result = GC_move_disappearing_link_inner(&GC_dl_hashtbl, link, new_link);
    UNLOCK();
    return result;
  }
#ifndef GC_LONG_REFS_NOT_NEEDED
    GC_API int GC_CALL GC_move_long_link(void **link, void **new_link)
    {
      int result;
      DCL_LOCK_STATE;
      if (((word)new_link & (ALIGNMENT-1)) != 0
          || !NONNULL_ARG_NOT_NULL(new_link))
        ABORT("Bad new_link arg to GC_move_long_link");
      if (((word)link & (ALIGNMENT-1)) != 0)
        return GC_NOT_FOUND;
      LOCK();
      result = GC_move_disappearing_link_inner(&GC_ll_hashtbl, link, new_link);
      UNLOCK();
      return result;
    }
#endif
#endif
#if defined(_MSC_VER) && defined(I386)
  GC_ATTR_NOINLINE
#endif
STATIC void GC_normal_finalize_mark_proc(ptr_t p)
{
    GC_mark_stack_top = GC_push_obj(p, HDR(p), GC_mark_stack_top,
                                    GC_mark_stack + GC_mark_stack_size);
}
STATIC void GC_ignore_self_finalize_mark_proc(ptr_t p)
{
    hdr * hhdr = HDR(p);
    word descr = hhdr -> hb_descr;
    ptr_t q;
    ptr_t scan_limit;
    ptr_t target_limit = p + hhdr -> hb_sz - 1;
    if ((descr & GC_DS_TAGS) == GC_DS_LENGTH) {
       scan_limit = p + descr - sizeof(word);
    } else {
       scan_limit = target_limit + 1 - sizeof(word);
    }
    for (q = p; (word)q <= (word)scan_limit; q += ALIGNMENT) {
        word r = *(word *)q;
        if (r < (word)p || r > (word)target_limit) {
            GC_PUSH_ONE_HEAP(r, q, GC_mark_stack_top);
        }
    }
}
STATIC void GC_null_finalize_mark_proc(ptr_t p GC_ATTR_UNUSED) {}
STATIC void GC_unreachable_finalize_mark_proc(ptr_t p)
{
    GC_normal_finalize_mark_proc(p);
}
STATIC void GC_register_finalizer_inner(void * obj,
                                        GC_finalization_proc fn, void *cd,
                                        GC_finalization_proc *ofn, void **ocd,
                                        finalization_mark_proc mp)
{
    struct finalizable_object * curr_fo;
    size_t index;
    struct finalizable_object *new_fo = 0;
    hdr *hhdr = NULL;
    DCL_LOCK_STATE;
    if (EXPECT(GC_find_leak, FALSE)) return;
    LOCK();
    if (EXPECT(NULL == GC_fnlz_roots.fo_head, FALSE)
        || EXPECT(GC_fo_entries > ((word)1 << GC_log_fo_table_size), FALSE)) {
        GC_grow_table((struct hash_chain_entry ***)&GC_fnlz_roots.fo_head,
                      &GC_log_fo_table_size, &GC_fo_entries);
        GC_COND_LOG_PRINTF("Grew fo table to %u entries\n",
                           1U << GC_log_fo_table_size);
    }
    for (;;) {
      struct finalizable_object *prev_fo = NULL;
      GC_oom_func oom_fn;
      index = HASH2(obj, GC_log_fo_table_size);
      curr_fo = GC_fnlz_roots.fo_head[index];
      while (curr_fo != 0) {
        GC_ASSERT(GC_size(curr_fo) >= sizeof(struct finalizable_object));
        if (curr_fo -> fo_hidden_base == GC_HIDE_POINTER(obj)) {
          if (ocd) *ocd = (void *) (curr_fo -> fo_client_data);
          if (ofn) *ofn = curr_fo -> fo_fn;
          if (prev_fo == 0) {
            GC_fnlz_roots.fo_head[index] = fo_next(curr_fo);
          } else {
            fo_set_next(prev_fo, fo_next(curr_fo));
            GC_dirty(prev_fo);
          }
          if (fn == 0) {
            GC_fo_entries--;
#if !defined(THREADS) && !defined(DBG_HDRS_ALL)
              GC_free((void *)curr_fo);
#endif
          } else {
            curr_fo -> fo_fn = fn;
            curr_fo -> fo_client_data = (ptr_t)cd;
            curr_fo -> fo_mark_proc = mp;
            GC_dirty(curr_fo);
            if (prev_fo == 0) {
              GC_fnlz_roots.fo_head[index] = curr_fo;
            } else {
              fo_set_next(prev_fo, curr_fo);
              GC_dirty(prev_fo);
            }
          }
          if (NULL == prev_fo)
            GC_dirty(GC_fnlz_roots.fo_head + index);
          UNLOCK();
#ifndef DBG_HDRS_ALL
              GC_free((void *)new_fo);
#endif
          return;
        }
        prev_fo = curr_fo;
        curr_fo = fo_next(curr_fo);
      }
      if (EXPECT(new_fo != 0, FALSE)) {
        GC_ASSERT(fn != 0);
#ifdef LINT2
          if (NULL == hhdr) ABORT("Bad hhdr in GC_register_finalizer_inner");
#endif
        break;
      }
      if (fn == 0) {
        if (ocd) *ocd = 0;
        if (ofn) *ofn = 0;
        UNLOCK();
        return;
      }
      GET_HDR(obj, hhdr);
      if (EXPECT(0 == hhdr, FALSE)) {
        if (ocd) *ocd = 0;
        if (ofn) *ofn = 0;
        UNLOCK();
        return;
      }
      new_fo = (struct finalizable_object *)
        GC_INTERNAL_MALLOC(sizeof(struct finalizable_object),NORMAL);
      if (EXPECT(new_fo != 0, TRUE))
        break;
      oom_fn = GC_oom_fn;
      UNLOCK();
      new_fo = (struct finalizable_object *)
                (*oom_fn)(sizeof(struct finalizable_object));
      if (0 == new_fo) {
        return;
      }
      LOCK();
    }
    GC_ASSERT(GC_size(new_fo) >= sizeof(struct finalizable_object));
    if (ocd) *ocd = 0;
    if (ofn) *ofn = 0;
    new_fo -> fo_hidden_base = GC_HIDE_POINTER(obj);
    new_fo -> fo_fn = fn;
    new_fo -> fo_client_data = (ptr_t)cd;
    new_fo -> fo_object_size = hhdr -> hb_sz;
    new_fo -> fo_mark_proc = mp;
    fo_set_next(new_fo, GC_fnlz_roots.fo_head[index]);
    GC_dirty(new_fo);
    GC_fo_entries++;
    GC_fnlz_roots.fo_head[index] = new_fo;
    GC_dirty(GC_fnlz_roots.fo_head + index);
    UNLOCK();
}
GC_API void GC_CALL GC_register_finalizer(void * obj,
                                  GC_finalization_proc fn, void * cd,
                                  GC_finalization_proc *ofn, void ** ocd)
{
    GC_register_finalizer_inner(obj, fn, cd, ofn,
                                ocd, GC_normal_finalize_mark_proc);
}
GC_API void GC_CALL GC_register_finalizer_ignore_self(void * obj,
                               GC_finalization_proc fn, void * cd,
                               GC_finalization_proc *ofn, void ** ocd)
{
    GC_register_finalizer_inner(obj, fn, cd, ofn,
                                ocd, GC_ignore_self_finalize_mark_proc);
}
GC_API void GC_CALL GC_register_finalizer_no_order(void * obj,
                               GC_finalization_proc fn, void * cd,
                               GC_finalization_proc *ofn, void ** ocd)
{
    GC_register_finalizer_inner(obj, fn, cd, ofn,
                                ocd, GC_null_finalize_mark_proc);
}
static GC_bool need_unreachable_finalization = FALSE;
GC_API void GC_CALL GC_register_finalizer_unreachable(void * obj,
                               GC_finalization_proc fn, void * cd,
                               GC_finalization_proc *ofn, void ** ocd)
{
    need_unreachable_finalization = TRUE;
    GC_ASSERT(GC_java_finalization);
    GC_register_finalizer_inner(obj, fn, cd, ofn,
                                ocd, GC_unreachable_finalize_mark_proc);
}
#ifndef NO_DEBUGGING
  STATIC void GC_dump_finalization_links(
                                const struct dl_hashtbl_s *dl_hashtbl)
  {
    size_t dl_size = (size_t)1 << dl_hashtbl -> log_size;
    size_t i;
    if (NULL == dl_hashtbl -> head) return;
    for (i = 0; i < dl_size; i++) {
      struct disappearing_link *curr_dl;
      for (curr_dl = dl_hashtbl -> head[i]; curr_dl != 0;
           curr_dl = dl_next(curr_dl)) {
        ptr_t real_ptr = (ptr_t)GC_REVEAL_POINTER(curr_dl->dl_hidden_obj);
        ptr_t real_link = (ptr_t)GC_REVEAL_POINTER(curr_dl->dl_hidden_link);
        GC_printf("Object: %p, link: %p\n",
                  (void *)real_ptr, (void *)real_link);
      }
    }
  }
  GC_API void GC_CALL GC_dump_finalization(void)
  {
    struct finalizable_object * curr_fo;
    size_t i;
    size_t fo_size = GC_fnlz_roots.fo_head == NULL ? 0 :
                                (size_t)1 << GC_log_fo_table_size;
    GC_printf("Disappearing (short) links:\n");
    GC_dump_finalization_links(&GC_dl_hashtbl);
#ifndef GC_LONG_REFS_NOT_NEEDED
      GC_printf("Disappearing long links:\n");
      GC_dump_finalization_links(&GC_ll_hashtbl);
#endif
    GC_printf("Finalizers:\n");
    for (i = 0; i < fo_size; i++) {
      for (curr_fo = GC_fnlz_roots.fo_head[i];
           curr_fo != NULL; curr_fo = fo_next(curr_fo)) {
        ptr_t real_ptr = (ptr_t)GC_REVEAL_POINTER(curr_fo->fo_hidden_base);
        GC_printf("Finalizable object: %p\n", (void *)real_ptr);
      }
    }
  }
#endif
#ifndef SMALL_CONFIG
  STATIC word GC_old_dl_entries = 0;
#ifndef GC_LONG_REFS_NOT_NEEDED
    STATIC word GC_old_ll_entries = 0;
#endif
#endif
#ifndef THREADS
  STATIC int GC_finalizer_nested = 0;
  STATIC unsigned GC_finalizer_skipped = 0;
  STATIC unsigned char *GC_check_finalizer_nested(void)
  {
    unsigned nesting_level = *(unsigned char *)&GC_finalizer_nested;
    if (nesting_level) {
      if (++GC_finalizer_skipped < (1U << nesting_level)) return NULL;
      GC_finalizer_skipped = 0;
    }
    *(char *)&GC_finalizer_nested = (char)(nesting_level + 1);
    return (unsigned char *)&GC_finalizer_nested;
  }
#endif
GC_INLINE void GC_make_disappearing_links_disappear(
                                        struct dl_hashtbl_s* dl_hashtbl,
                                        GC_bool is_remove_dangling)
{
  size_t i;
  size_t dl_size = (size_t)1 << dl_hashtbl -> log_size;
  GC_bool needs_barrier = FALSE;
  GC_ASSERT(I_HOLD_LOCK());
  if (NULL == dl_hashtbl -> head) return;
  for (i = 0; i < dl_size; i++) {
    struct disappearing_link *curr_dl, *next_dl;
    struct disappearing_link *prev_dl = NULL;
    for (curr_dl = dl_hashtbl->head[i]; curr_dl != NULL; curr_dl = next_dl) {
      next_dl = dl_next(curr_dl);
      if (is_remove_dangling) {
        ptr_t real_link = (ptr_t)GC_base(GC_REVEAL_POINTER(
                                                curr_dl->dl_hidden_link));
        if (NULL == real_link || EXPECT(GC_is_marked(real_link), TRUE)) {
          prev_dl = curr_dl;
          continue;
        }
      } else {
        if (EXPECT(GC_is_marked((ptr_t)GC_REVEAL_POINTER(
                                        curr_dl->dl_hidden_obj)), TRUE)) {
          prev_dl = curr_dl;
          continue;
        }
        *(ptr_t *)GC_REVEAL_POINTER(curr_dl->dl_hidden_link) = NULL;
      }
      if (NULL == prev_dl) {
        dl_hashtbl -> head[i] = next_dl;
        needs_barrier = TRUE;
      } else {
        dl_set_next(prev_dl, next_dl);
        GC_dirty(prev_dl);
      }
      GC_clear_mark_bit(curr_dl);
      dl_hashtbl -> entries--;
    }
  }
  if (needs_barrier)
    GC_dirty(dl_hashtbl -> head);
}
GC_INNER void GC_finalize(void)
{
    struct finalizable_object * curr_fo, * prev_fo, * next_fo;
    ptr_t real_ptr;
    size_t i;
    size_t fo_size = GC_fnlz_roots.fo_head == NULL ? 0 :
                                (size_t)1 << GC_log_fo_table_size;
    GC_bool needs_barrier = FALSE;
    GC_ASSERT(I_HOLD_LOCK());
#ifndef SMALL_CONFIG
      GC_old_dl_entries = GC_dl_hashtbl.entries;
#ifndef GC_LONG_REFS_NOT_NEEDED
        GC_old_ll_entries = GC_ll_hashtbl.entries;
#endif
#endif
#ifndef GC_TOGGLE_REFS_NOT_NEEDED
      GC_mark_togglerefs();
#endif
    GC_make_disappearing_links_disappear(&GC_dl_hashtbl, FALSE);
    GC_ASSERT(GC_mark_state == MS_NONE);
    for (i = 0; i < fo_size; i++) {
      for (curr_fo = GC_fnlz_roots.fo_head[i];
           curr_fo != NULL; curr_fo = fo_next(curr_fo)) {
        GC_ASSERT(GC_size(curr_fo) >= sizeof(struct finalizable_object));
        real_ptr = (ptr_t)GC_REVEAL_POINTER(curr_fo->fo_hidden_base);
        if (!GC_is_marked(real_ptr)) {
            GC_MARKED_FOR_FINALIZATION(real_ptr);
            GC_MARK_FO(real_ptr, curr_fo -> fo_mark_proc);
            if (GC_is_marked(real_ptr)) {
                WARN("Finalization cycle involving %p\n", real_ptr);
            }
        }
      }
    }
    GC_bytes_finalized = 0;
    for (i = 0; i < fo_size; i++) {
      curr_fo = GC_fnlz_roots.fo_head[i];
      prev_fo = 0;
      while (curr_fo != 0) {
        real_ptr = (ptr_t)GC_REVEAL_POINTER(curr_fo->fo_hidden_base);
        if (!GC_is_marked(real_ptr)) {
            if (!GC_java_finalization) {
              GC_set_mark_bit(real_ptr);
            }
              next_fo = fo_next(curr_fo);
              if (NULL == prev_fo) {
                GC_fnlz_roots.fo_head[i] = next_fo;
                if (GC_object_finalized_proc) {
                  GC_dirty(GC_fnlz_roots.fo_head + i);
                } else {
                  needs_barrier = TRUE;
                }
              } else {
                fo_set_next(prev_fo, next_fo);
                GC_dirty(prev_fo);
              }
              GC_fo_entries--;
              if (GC_object_finalized_proc)
                GC_object_finalized_proc(real_ptr);
              fo_set_next(curr_fo, GC_fnlz_roots.finalize_now);
              GC_dirty(curr_fo);
              SET_FINALIZE_NOW(curr_fo);
              curr_fo -> fo_hidden_base =
                        (word)GC_REVEAL_POINTER(curr_fo -> fo_hidden_base);
              GC_bytes_finalized +=
                        curr_fo -> fo_object_size
                        + sizeof(struct finalizable_object);
            GC_ASSERT(GC_is_marked(GC_base(curr_fo)));
            curr_fo = next_fo;
        } else {
            prev_fo = curr_fo;
            curr_fo = fo_next(curr_fo);
        }
      }
    }
  if (GC_java_finalization) {
      for (curr_fo = GC_fnlz_roots.finalize_now;
           curr_fo != NULL; curr_fo = fo_next(curr_fo)) {
        real_ptr = (ptr_t)curr_fo -> fo_hidden_base;
        if (!GC_is_marked(real_ptr)) {
            if (curr_fo -> fo_mark_proc == GC_null_finalize_mark_proc) {
                GC_MARK_FO(real_ptr, GC_normal_finalize_mark_proc);
            }
            if (curr_fo -> fo_mark_proc != GC_unreachable_finalize_mark_proc) {
                GC_set_mark_bit(real_ptr);
            }
        }
      }
      if (need_unreachable_finalization) {
        curr_fo = GC_fnlz_roots.finalize_now;
        GC_ASSERT(NULL == curr_fo || GC_fnlz_roots.fo_head != NULL);
        prev_fo = NULL;
        while (curr_fo != NULL) {
          next_fo = fo_next(curr_fo);
          if (curr_fo -> fo_mark_proc == GC_unreachable_finalize_mark_proc) {
            real_ptr = (ptr_t)curr_fo -> fo_hidden_base;
            if (!GC_is_marked(real_ptr)) {
              GC_set_mark_bit(real_ptr);
            } else {
              if (NULL == prev_fo) {
                SET_FINALIZE_NOW(next_fo);
              } else {
                fo_set_next(prev_fo, next_fo);
                GC_dirty(prev_fo);
              }
              curr_fo -> fo_hidden_base =
                                GC_HIDE_POINTER(curr_fo -> fo_hidden_base);
              GC_bytes_finalized -=
                  curr_fo->fo_object_size + sizeof(struct finalizable_object);
              i = HASH2(real_ptr, GC_log_fo_table_size);
              fo_set_next(curr_fo, GC_fnlz_roots.fo_head[i]);
              GC_dirty(curr_fo);
              GC_fo_entries++;
              GC_fnlz_roots.fo_head[i] = curr_fo;
              curr_fo = prev_fo;
              needs_barrier = TRUE;
            }
          }
          prev_fo = curr_fo;
          curr_fo = next_fo;
        }
      }
  }
  if (needs_barrier)
    GC_dirty(GC_fnlz_roots.fo_head);
  GC_make_disappearing_links_disappear(&GC_dl_hashtbl, TRUE);
#ifndef GC_TOGGLE_REFS_NOT_NEEDED
    GC_clear_togglerefs();
#endif
#ifndef GC_LONG_REFS_NOT_NEEDED
    GC_make_disappearing_links_disappear(&GC_ll_hashtbl, FALSE);
    GC_make_disappearing_links_disappear(&GC_ll_hashtbl, TRUE);
#endif
  if (GC_fail_count) {
#ifdef THREADS
      GC_reset_finalizer_nested();
#else
      GC_finalizer_nested = 0;
#endif
  }
}
#ifndef JAVA_FINALIZATION_NOT_NEEDED
  STATIC void GC_enqueue_all_finalizers(void)
  {
    struct finalizable_object * next_fo;
    size_t i;
    size_t fo_size = GC_fnlz_roots.fo_head == NULL ? 0 :
                                (size_t)1 << GC_log_fo_table_size;
    GC_ASSERT(I_HOLD_LOCK());
    GC_bytes_finalized = 0;
    for (i = 0; i < fo_size; i++) {
      struct finalizable_object * curr_fo = GC_fnlz_roots.fo_head[i];
      GC_fnlz_roots.fo_head[i] = NULL;
      while (curr_fo != NULL) {
          ptr_t real_ptr = (ptr_t)GC_REVEAL_POINTER(curr_fo->fo_hidden_base);
          GC_MARK_FO(real_ptr, GC_normal_finalize_mark_proc);
          GC_set_mark_bit(real_ptr);
          next_fo = fo_next(curr_fo);
          fo_set_next(curr_fo, GC_fnlz_roots.finalize_now);
          GC_dirty(curr_fo);
          SET_FINALIZE_NOW(curr_fo);
          curr_fo -> fo_hidden_base =
                        (word)GC_REVEAL_POINTER(curr_fo -> fo_hidden_base);
          GC_bytes_finalized +=
                curr_fo -> fo_object_size + sizeof(struct finalizable_object);
          curr_fo = next_fo;
      }
    }
    GC_fo_entries = 0;
  }
  GC_API void GC_CALL GC_finalize_all(void)
  {
    DCL_LOCK_STATE;
    LOCK();
    while (GC_fo_entries > 0) {
      GC_enqueue_all_finalizers();
      UNLOCK();
      GC_invoke_finalizers();
      LOCK();
    }
    UNLOCK();
  }
#endif
GC_API int GC_CALL GC_should_invoke_finalizers(void)
{
#ifdef AO_HAVE_load
    return AO_load((volatile AO_t *)&GC_fnlz_roots.finalize_now) != 0;
#else
    return GC_fnlz_roots.finalize_now != NULL;
#endif
}
GC_API int GC_CALL GC_invoke_finalizers(void)
{
    int count = 0;
    word bytes_freed_before = 0;
    DCL_LOCK_STATE;
    while (GC_should_invoke_finalizers()) {
        struct finalizable_object * curr_fo;
#ifdef THREADS
            LOCK();
#endif
        if (count == 0) {
            bytes_freed_before = GC_bytes_freed;
        }
        curr_fo = GC_fnlz_roots.finalize_now;
#ifdef THREADS
            if (curr_fo != NULL)
                SET_FINALIZE_NOW(fo_next(curr_fo));
            UNLOCK();
            if (curr_fo == 0) break;
#else
            GC_fnlz_roots.finalize_now = fo_next(curr_fo);
#endif
        fo_set_next(curr_fo, 0);
        (*(curr_fo -> fo_fn))((ptr_t)(curr_fo -> fo_hidden_base),
                              curr_fo -> fo_client_data);
        curr_fo -> fo_client_data = 0;
        ++count;
    }
    if (count != 0
#if defined(THREADS) && !defined(THREAD_SANITIZER)
            && bytes_freed_before != GC_bytes_freed
#endif
       ) {
        LOCK();
        GC_finalizer_bytes_freed += (GC_bytes_freed - bytes_freed_before);
        UNLOCK();
    }
    return count;
}
static word last_finalizer_notification = 0;
GC_INNER void GC_notify_or_invoke_finalizers(void)
{
    GC_finalizer_notifier_proc notifier_fn = 0;
#if defined(KEEP_BACK_PTRS) || defined(MAKE_BACK_GRAPH)
      static word last_back_trace_gc_no = 1;
#endif
    DCL_LOCK_STATE;
#if defined(THREADS) && !defined(KEEP_BACK_PTRS) \
       && !defined(MAKE_BACK_GRAPH)
      if (!GC_should_invoke_finalizers())
        return;
#endif
    LOCK();
#if defined(KEEP_BACK_PTRS) || defined(MAKE_BACK_GRAPH)
      if (GC_gc_no > last_back_trace_gc_no) {
#ifdef KEEP_BACK_PTRS
          long i;
          last_back_trace_gc_no = GC_WORD_MAX;
          for (i = 0; i < GC_backtraces; ++i) {
              UNLOCK();
              GC_generate_random_backtrace_no_gc();
              LOCK();
          }
          last_back_trace_gc_no = GC_gc_no;
#endif
#ifdef MAKE_BACK_GRAPH
          if (GC_print_back_height) {
            GC_print_back_graph_stats();
          }
#endif
      }
#endif
    if (NULL == GC_fnlz_roots.finalize_now) {
      UNLOCK();
      return;
    }
    if (!GC_finalize_on_demand) {
      unsigned char *pnested = GC_check_finalizer_nested();
      UNLOCK();
      if (pnested != NULL) {
        (void) GC_invoke_finalizers();
        *pnested = 0;
#ifndef THREADS
          GC_ASSERT(NULL == GC_fnlz_roots.finalize_now);
#endif
      }
      return;
    }
    if (last_finalizer_notification != GC_gc_no) {
        notifier_fn = GC_finalizer_notifier;
        last_finalizer_notification = GC_gc_no;
    }
    UNLOCK();
    if (notifier_fn != 0)
        (*notifier_fn)();
}
#ifndef SMALL_CONFIG
#ifndef GC_LONG_REFS_NOT_NEEDED
#define IF_LONG_REFS_PRESENT_ELSE(x,y) (x)
#else
#define IF_LONG_REFS_PRESENT_ELSE(x,y) (y)
#endif
  GC_INNER void GC_print_finalization_stats(void)
  {
    struct finalizable_object *fo;
    unsigned long ready = 0;
    GC_log_printf("%lu finalization entries;"
                  " %lu/%lu short/long disappearing links alive\n",
                  (unsigned long)GC_fo_entries,
                  (unsigned long)GC_dl_hashtbl.entries,
                  (unsigned long)IF_LONG_REFS_PRESENT_ELSE(
                                                GC_ll_hashtbl.entries, 0));
    for (fo = GC_fnlz_roots.finalize_now; fo != NULL; fo = fo_next(fo))
      ++ready;
    GC_log_printf("%lu finalization-ready objects;"
                  " %ld/%ld short/long links cleared\n",
                  ready,
                  (long)GC_old_dl_entries - (long)GC_dl_hashtbl.entries,
                  (long)IF_LONG_REFS_PRESENT_ELSE(
                              GC_old_ll_entries - GC_ll_hashtbl.entries, 0));
  }
#endif
#endif
#ifdef ENABLE_DISCLAIM
#ifndef GC_DISCLAIM_H
#define GC_DISCLAIM_H
#ifdef __cplusplus
  extern "C" {
#endif
GC_API void GC_CALL GC_init_finalized_malloc(void);
typedef int (GC_CALLBACK * GC_disclaim_proc)(void * );
GC_API void GC_CALL GC_register_disclaim_proc(int ,
                                GC_disclaim_proc ,
                                int ) GC_ATTR_NONNULL(2);
struct GC_finalizer_closure {
    GC_finalization_proc proc;
    void *cd;
};
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
        GC_finalized_malloc(size_t ,
                const struct GC_finalizer_closure * ) GC_ATTR_NONNULL(2);
#ifdef __cplusplus
  }
#endif
#endif
#if defined(KEEP_BACK_PTRS) || defined(MAKE_BACK_GRAPH)
#define FINALIZER_CLOSURE_FLAG 0x2
#else
#define FINALIZER_CLOSURE_FLAG 0x1
#endif
STATIC int GC_CALLBACK GC_finalized_disclaim(void *obj)
{
    word fc_word = *(word *)obj;
    if ((fc_word & FINALIZER_CLOSURE_FLAG) != 0) {
        const struct GC_finalizer_closure *fc
                        = (struct GC_finalizer_closure *)(fc_word
                                        & ~(word)FINALIZER_CLOSURE_FLAG);
        GC_ASSERT(!GC_find_leak);
        (*fc->proc)((word *)obj + 1, fc->cd);
    }
    return 0;
}
GC_API void GC_CALL GC_init_finalized_malloc(void)
{
    DCL_LOCK_STATE;
    GC_init();
    LOCK();
    if (GC_finalized_kind != 0) {
        UNLOCK();
        return;
    }
    GC_register_displacement_inner(sizeof(word));
    GC_register_displacement_inner(FINALIZER_CLOSURE_FLAG);
    GC_register_displacement_inner(sizeof(oh) + FINALIZER_CLOSURE_FLAG);
    GC_finalized_kind = GC_new_kind_inner(GC_new_free_list_inner(),
                                          GC_DS_LENGTH, TRUE, TRUE);
    GC_ASSERT(GC_finalized_kind != 0);
    GC_register_disclaim_proc(GC_finalized_kind, GC_finalized_disclaim, TRUE);
    UNLOCK();
}
GC_API void GC_CALL GC_register_disclaim_proc(int kind, GC_disclaim_proc proc,
                                              int mark_unconditionally)
{
    GC_ASSERT((unsigned)kind < MAXOBJKINDS);
    GC_ASSERT(NONNULL_ARG_NOT_NULL(proc));
    if (!EXPECT(GC_find_leak, FALSE)) {
        GC_obj_kinds[kind].ok_disclaim_proc = proc;
        GC_obj_kinds[kind].ok_mark_unconditionally =
                                        (GC_bool)mark_unconditionally;
    }
}
GC_API GC_ATTR_MALLOC void * GC_CALL GC_finalized_malloc(size_t lb,
                                const struct GC_finalizer_closure *fclos)
{
    word *op;
    GC_ASSERT(GC_finalized_kind != 0);
    GC_ASSERT(NONNULL_ARG_NOT_NULL(fclos));
    GC_ASSERT(((word)fclos & FINALIZER_CLOSURE_FLAG) == 0);
    op = (word *)GC_malloc_kind(SIZET_SAT_ADD(lb, sizeof(word)),
                                GC_finalized_kind);
    if (EXPECT(NULL == op, FALSE))
        return NULL;
    *op = (word)fclos | FINALIZER_CLOSURE_FLAG;
    GC_dirty(op);
    REACHABLE_AFTER_DIRTY(fclos);
    return op + 1;
}
#endif
#include <stdio.h>
#include <string.h>
STATIC GC_bool GC_alloc_reclaim_list(struct obj_kind *kind)
{
    struct hblk ** result = (struct hblk **)
                GC_scratch_alloc((MAXOBJGRANULES+1) * sizeof(struct hblk *));
    if (result == 0) return(FALSE);
    BZERO(result, (MAXOBJGRANULES+1)*sizeof(struct hblk *));
    kind -> ok_reclaim_list = result;
    return(TRUE);
}
GC_INNER ptr_t GC_alloc_large(size_t lb, int k, unsigned flags)
{
    struct hblk * h;
    word n_blocks;
    ptr_t result;
    GC_bool retry = FALSE;
    GC_ASSERT(I_HOLD_LOCK());
    lb = ROUNDUP_GRANULE_SIZE(lb);
    n_blocks = OBJ_SZ_TO_BLOCKS_CHECKED(lb);
    if (!EXPECT(GC_is_initialized, TRUE)) {
      DCL_LOCK_STATE;
      UNLOCK();
      GC_init();
      LOCK();
    }
        if (GC_incremental && !GC_dont_gc) {
            ENTER_GC();
            GC_collect_a_little_inner((int)n_blocks);
            EXIT_GC();
        }
    h = GC_allochblk(lb, k, flags);
#ifdef USE_MUNMAP
        if (0 == h) {
            GC_merge_unmapped();
            h = GC_allochblk(lb, k, flags);
        }
#endif
    while (0 == h && GC_collect_or_expand(n_blocks, flags != 0, retry)) {
        h = GC_allochblk(lb, k, flags);
        retry = TRUE;
    }
    if (h == 0) {
        result = 0;
    } else {
        size_t total_bytes = n_blocks * HBLKSIZE;
        if (n_blocks > 1) {
            GC_large_allocd_bytes += total_bytes;
            if (GC_large_allocd_bytes > GC_max_large_allocd_bytes)
                GC_max_large_allocd_bytes = GC_large_allocd_bytes;
        }
        result = h -> hb_body;
    }
    return result;
}
STATIC ptr_t GC_alloc_large_and_clear(size_t lb, int k, unsigned flags)
{
    ptr_t result;
    GC_ASSERT(I_HOLD_LOCK());
    result = GC_alloc_large(lb, k, flags);
    if (result != NULL
          && (GC_debugging_started || GC_obj_kinds[k].ok_init)) {
        word n_blocks = OBJ_SZ_TO_BLOCKS(lb);
        BZERO(result, n_blocks * HBLKSIZE);
    }
    return result;
}
STATIC void GC_extend_size_map(size_t i)
{
  size_t orig_granule_sz = ROUNDED_UP_GRANULES(i);
  size_t granule_sz;
  size_t byte_sz = GRANULES_TO_BYTES(orig_granule_sz);
  size_t smaller_than_i = byte_sz - (byte_sz >> 3);
  size_t low_limit;
  size_t number_of_objs;
  GC_ASSERT(I_HOLD_LOCK());
  GC_ASSERT(0 == GC_size_map[i]);
  if (0 == GC_size_map[smaller_than_i]) {
    low_limit = byte_sz - (byte_sz >> 2);
    granule_sz = orig_granule_sz;
    while (GC_size_map[low_limit] != 0)
      low_limit++;
  } else {
    low_limit = smaller_than_i + 1;
    while (GC_size_map[low_limit] != 0)
      low_limit++;
    granule_sz = ROUNDED_UP_GRANULES(low_limit);
    granule_sz += granule_sz >> 3;
    if (granule_sz < orig_granule_sz)
      granule_sz = orig_granule_sz;
  }
  granule_sz = (granule_sz + 1) & ~1;
  if (granule_sz > MAXOBJGRANULES)
    granule_sz = MAXOBJGRANULES;
  number_of_objs = HBLK_GRANULES / granule_sz;
  GC_ASSERT(number_of_objs != 0);
  granule_sz = (HBLK_GRANULES / number_of_objs) & ~1;
  byte_sz = GRANULES_TO_BYTES(granule_sz) - EXTRA_BYTES;
  for (; low_limit <= byte_sz; low_limit++)
    GC_size_map[low_limit] = granule_sz;
}
GC_INNER void * GC_generic_malloc_inner(size_t lb, int k)
{
    void *op;
    GC_ASSERT(I_HOLD_LOCK());
    GC_ASSERT(k < MAXOBJKINDS);
    if (SMALL_OBJ(lb)) {
        struct obj_kind * kind = GC_obj_kinds + k;
        size_t lg = GC_size_map[lb];
        void ** opp = &(kind -> ok_freelist[lg]);
        op = *opp;
        if (EXPECT(0 == op, FALSE)) {
          if (lg == 0) {
            if (!EXPECT(GC_is_initialized, TRUE)) {
              DCL_LOCK_STATE;
              UNLOCK();
              GC_init();
              LOCK();
              lg = GC_size_map[lb];
            }
            if (0 == lg) {
              GC_extend_size_map(lb);
              lg = GC_size_map[lb];
              GC_ASSERT(lg != 0);
            }
            opp = &(kind -> ok_freelist[lg]);
            op = *opp;
          }
          if (0 == op) {
            if (0 == kind -> ok_reclaim_list &&
                !GC_alloc_reclaim_list(kind))
              return NULL;
            op = GC_allocobj(lg, k);
            if (0 == op)
              return NULL;
          }
        }
        *opp = obj_link(op);
        obj_link(op) = 0;
        GC_bytes_allocd += GRANULES_TO_BYTES((word)lg);
    } else {
        op = (ptr_t)GC_alloc_large_and_clear(ADD_SLOP(lb), k, 0);
        if (op != NULL)
            GC_bytes_allocd += lb;
    }
    return op;
}
#if defined(DBG_HDRS_ALL) || defined(GC_GCJ_SUPPORT) \
    || !defined(GC_NO_FINALIZATION)
  GC_INNER void * GC_generic_malloc_inner_ignore_off_page(size_t lb, int k)
  {
    word lb_adjusted;
    void * op;
    GC_ASSERT(I_HOLD_LOCK());
    if (lb <= HBLKSIZE)
        return GC_generic_malloc_inner(lb, k);
    GC_ASSERT(k < MAXOBJKINDS);
    lb_adjusted = ADD_SLOP(lb);
    op = GC_alloc_large_and_clear(lb_adjusted, k, IGNORE_OFF_PAGE);
    if (op != NULL)
        GC_bytes_allocd += lb_adjusted;
    return op;
  }
#endif
#ifdef GC_COLLECT_AT_MALLOC
#if defined(CPPCHECK)
    size_t GC_dbg_collect_at_malloc_min_lb = 16*1024;
#else
    size_t GC_dbg_collect_at_malloc_min_lb = (GC_COLLECT_AT_MALLOC);
#endif
#endif
GC_API GC_ATTR_MALLOC void * GC_CALL GC_generic_malloc(size_t lb, int k)
{
    void * result;
    DCL_LOCK_STATE;
    GC_ASSERT(k < MAXOBJKINDS);
    if (EXPECT(GC_have_errors, FALSE))
      GC_print_all_errors();
    GC_INVOKE_FINALIZERS();
    GC_DBG_COLLECT_AT_MALLOC(lb);
    if (SMALL_OBJ(lb)) {
        LOCK();
        result = GC_generic_malloc_inner(lb, k);
        UNLOCK();
    } else {
        size_t lg;
        size_t lb_rounded;
        word n_blocks;
        GC_bool init;
        lg = ROUNDED_UP_GRANULES(lb);
        lb_rounded = GRANULES_TO_BYTES(lg);
        n_blocks = OBJ_SZ_TO_BLOCKS(lb_rounded);
        init = GC_obj_kinds[k].ok_init;
        LOCK();
        result = (ptr_t)GC_alloc_large(lb_rounded, k, 0);
        if (0 != result) {
          if (GC_debugging_started) {
            BZERO(result, n_blocks * HBLKSIZE);
          } else {
#ifdef THREADS
                ((word *)result)[0] = 0;
                ((word *)result)[1] = 0;
                ((word *)result)[GRANULES_TO_WORDS(lg)-1] = 0;
                ((word *)result)[GRANULES_TO_WORDS(lg)-2] = 0;
#endif
          }
          GC_bytes_allocd += lb_rounded;
        }
        UNLOCK();
        if (init && !GC_debugging_started && 0 != result) {
            BZERO(result, n_blocks * HBLKSIZE);
        }
    }
    if (0 == result) {
        return((*GC_get_oom_fn())(lb));
    } else {
        return(result);
    }
}
GC_API GC_ATTR_MALLOC void * GC_CALL GC_malloc_kind_global(size_t lb, int k)
{
    GC_ASSERT(k < MAXOBJKINDS);
    if (SMALL_OBJ(lb)) {
        void *op;
        void **opp;
        size_t lg;
        DCL_LOCK_STATE;
        GC_DBG_COLLECT_AT_MALLOC(lb);
        LOCK();
        lg = GC_size_map[lb];
        opp = &GC_obj_kinds[k].ok_freelist[lg];
        op = *opp;
        if (EXPECT(op != NULL, TRUE)) {
            if (k == PTRFREE) {
                *opp = obj_link(op);
            } else {
                GC_ASSERT(0 == obj_link(op)
                          || ((word)obj_link(op)
                                <= (word)GC_greatest_plausible_heap_addr
                              && (word)obj_link(op)
                                >= (word)GC_least_plausible_heap_addr));
                *opp = obj_link(op);
                obj_link(op) = 0;
            }
            GC_bytes_allocd += GRANULES_TO_BYTES((word)lg);
            UNLOCK();
            return op;
        }
        UNLOCK();
    }
    return GC_clear_stack(GC_generic_malloc(lb, k));
}
#if defined(THREADS) && !defined(THREAD_LOCAL_ALLOC)
  GC_API GC_ATTR_MALLOC void * GC_CALL GC_malloc_kind(size_t lb, int k)
  {
    return GC_malloc_kind_global(lb, k);
  }
#endif
GC_API GC_ATTR_MALLOC void * GC_CALL GC_malloc_atomic(size_t lb)
{
    return GC_malloc_kind(lb, PTRFREE);
}
GC_API GC_ATTR_MALLOC void * GC_CALL GC_malloc(size_t lb)
{
    return GC_malloc_kind(lb, NORMAL);
}
GC_API GC_ATTR_MALLOC void * GC_CALL GC_generic_malloc_uncollectable(
                                                        size_t lb, int k)
{
    void *op;
    DCL_LOCK_STATE;
    GC_ASSERT(k < MAXOBJKINDS);
    if (SMALL_OBJ(lb)) {
        void **opp;
        size_t lg;
        GC_DBG_COLLECT_AT_MALLOC(lb);
        if (EXTRA_BYTES != 0 && lb != 0) lb--;
        LOCK();
        lg = GC_size_map[lb];
        opp = &GC_obj_kinds[k].ok_freelist[lg];
        op = *opp;
        if (EXPECT(op != NULL, TRUE)) {
            *opp = obj_link(op);
            obj_link(op) = 0;
            GC_bytes_allocd += GRANULES_TO_BYTES((word)lg);
            GC_non_gc_bytes += GRANULES_TO_BYTES((word)lg);
            UNLOCK();
        } else {
            UNLOCK();
            op = GC_generic_malloc(lb, k);
        }
        GC_ASSERT(0 == op || GC_is_marked(op));
    } else {
      op = GC_generic_malloc(lb, k);
      if (op ) {
        hdr * hhdr = HDR(op);
        GC_ASSERT(((word)op & (HBLKSIZE - 1)) == 0);
        LOCK();
        set_mark_bit_from_hdr(hhdr, 0);
#ifndef THREADS
          GC_ASSERT(hhdr -> hb_n_marks == 0);
#endif
        hhdr -> hb_n_marks = 1;
        UNLOCK();
      }
    }
    return op;
}
GC_API GC_ATTR_MALLOC void * GC_CALL GC_malloc_uncollectable(size_t lb)
{
  return GC_generic_malloc_uncollectable(lb, UNCOLLECTABLE);
}
#ifdef GC_ATOMIC_UNCOLLECTABLE
  GC_API GC_ATTR_MALLOC void * GC_CALL
        GC_malloc_atomic_uncollectable(size_t lb)
  {
    return GC_generic_malloc_uncollectable(lb, AUNCOLLECTABLE);
  }
#endif
#if defined(REDIRECT_MALLOC) && !defined(REDIRECT_MALLOC_IN_HEADER)
#ifndef MSWINCE
#include <errno.h>
#endif
#define GC_debug_malloc_replacement(lb) GC_debug_malloc(lb, GC_DBG_EXTRAS)
#if defined(CPPCHECK)
#define REDIRECT_MALLOC_F GC_malloc
#else
#define REDIRECT_MALLOC_F REDIRECT_MALLOC
#endif
  void * malloc(size_t lb)
  {
#if defined(I386) && defined(GC_SOLARIS_THREADS)
      if (!EXPECT(GC_is_initialized, TRUE)) return sbrk(lb);
#endif
    return (void *)REDIRECT_MALLOC_F(lb);
  }
#if defined(GC_LINUX_THREADS)
    STATIC ptr_t GC_libpthread_start = 0;
    STATIC ptr_t GC_libpthread_end = 0;
    STATIC ptr_t GC_libld_start = 0;
    STATIC ptr_t GC_libld_end = 0;
    STATIC void GC_init_lib_bounds(void)
    {
      IF_CANCEL(int cancel_state;)
      if (GC_libpthread_start != 0) return;
      DISABLE_CANCEL(cancel_state);
      GC_init();
      if (!GC_text_mapping("libpthread-",
                           &GC_libpthread_start, &GC_libpthread_end)) {
          WARN("Failed to find libpthread.so text mapping: Expect crash\n", 0);
            GC_libpthread_start = (ptr_t)1;
      }
      if (!GC_text_mapping("ld-", &GC_libld_start, &GC_libld_end)) {
          WARN("Failed to find ld.so text mapping: Expect crash\n", 0);
      }
      RESTORE_CANCEL(cancel_state);
    }
#endif
  void * calloc(size_t n, size_t lb)
  {
    if ((lb | n) > GC_SQRT_SIZE_MAX
        && lb && n > GC_SIZE_MAX / lb)
      return (*GC_get_oom_fn())(GC_SIZE_MAX);
#if defined(GC_LINUX_THREADS)
      {
        static GC_bool lib_bounds_set = FALSE;
        ptr_t caller = (ptr_t)__builtin_return_address(0);
        if (!EXPECT(lib_bounds_set, TRUE)) {
          GC_init_lib_bounds();
          lib_bounds_set = TRUE;
        }
        if (((word)caller >= (word)GC_libpthread_start
             && (word)caller < (word)GC_libpthread_end)
            || ((word)caller >= (word)GC_libld_start
                && (word)caller < (word)GC_libld_end))
          return GC_generic_malloc_uncollectable(n * lb, UNCOLLECTABLE);
      }
#endif
    return (void *)REDIRECT_MALLOC_F(n * lb);
  }
#ifndef strdup
    char *strdup(const char *s)
    {
      size_t lb = strlen(s) + 1;
      char *result = (char *)REDIRECT_MALLOC_F(lb);
      if (result == 0) {
        errno = ENOMEM;
        return 0;
      }
      BCOPY(s, result, lb);
      return result;
    }
#endif
#ifndef strndup
    char *strndup(const char *str, size_t size)
    {
      char *copy;
      size_t len = strlen(str);
      if (len > size)
        len = size;
      copy = (char *)REDIRECT_MALLOC_F(len + 1);
      if (copy == NULL) {
        errno = ENOMEM;
        return NULL;
      }
      if (EXPECT(len > 0, TRUE))
        BCOPY(str, copy, len);
      copy[len] = '\0';
      return copy;
    }
#endif
#undef GC_debug_malloc_replacement
#endif
GC_API void GC_CALL GC_free(void * p)
{
    struct hblk *h;
    hdr *hhdr;
    size_t sz;
    size_t ngranules;
    int knd;
    struct obj_kind * ok;
    DCL_LOCK_STATE;
    if (p ) {
    } else {
        return;
    }
#ifdef LOG_ALLOCS
      GC_log_printf("GC_free(%p) after GC #%lu\n",
                    p, (unsigned long)GC_gc_no);
#endif
    h = HBLKPTR(p);
    hhdr = HDR(h);
#if defined(REDIRECT_MALLOC) && \
        ((defined(NEED_CALLINFO) && defined(GC_HAVE_BUILTIN_BACKTRACE)) \
         || defined(GC_SOLARIS_THREADS) || defined(GC_LINUX_THREADS) \
         || defined(MSWIN32))
        if (0 == hhdr) return;
#endif
    GC_ASSERT(GC_base(p) == p);
    sz = (size_t)hhdr->hb_sz;
    ngranules = BYTES_TO_GRANULES(sz);
    knd = hhdr -> hb_obj_kind;
    ok = &GC_obj_kinds[knd];
    if (EXPECT(ngranules <= MAXOBJGRANULES, TRUE)) {
        void **flh;
        LOCK();
        GC_bytes_freed += sz;
        if (IS_UNCOLLECTABLE(knd)) GC_non_gc_bytes -= sz;
        if (ok -> ok_init && EXPECT(sz > sizeof(word), TRUE)) {
            BZERO((word *)p + 1, sz-sizeof(word));
        }
        flh = &(ok -> ok_freelist[ngranules]);
        obj_link(p) = *flh;
        *flh = (ptr_t)p;
        UNLOCK();
    } else {
        size_t nblocks = OBJ_SZ_TO_BLOCKS(sz);
        LOCK();
        GC_bytes_freed += sz;
        if (IS_UNCOLLECTABLE(knd)) GC_non_gc_bytes -= sz;
        if (nblocks > 1) {
          GC_large_allocd_bytes -= nblocks * HBLKSIZE;
        }
        GC_freehblk(h);
        UNLOCK();
    }
}
#ifdef THREADS
  GC_INNER void GC_free_inner(void * p)
  {
    struct hblk *h;
    hdr *hhdr;
    size_t sz;
    size_t ngranules;
    int knd;
    struct obj_kind * ok;
    h = HBLKPTR(p);
    hhdr = HDR(h);
    knd = hhdr -> hb_obj_kind;
    sz = (size_t)hhdr->hb_sz;
    ngranules = BYTES_TO_GRANULES(sz);
    ok = &GC_obj_kinds[knd];
    if (ngranules <= MAXOBJGRANULES) {
        void ** flh;
        GC_bytes_freed += sz;
        if (IS_UNCOLLECTABLE(knd)) GC_non_gc_bytes -= sz;
        if (ok -> ok_init && EXPECT(sz > sizeof(word), TRUE)) {
            BZERO((word *)p + 1, sz-sizeof(word));
        }
        flh = &(ok -> ok_freelist[ngranules]);
        obj_link(p) = *flh;
        *flh = (ptr_t)p;
    } else {
        size_t nblocks = OBJ_SZ_TO_BLOCKS(sz);
        GC_bytes_freed += sz;
        if (IS_UNCOLLECTABLE(knd)) GC_non_gc_bytes -= sz;
        if (nblocks > 1) {
          GC_large_allocd_bytes -= nblocks * HBLKSIZE;
        }
        GC_freehblk(h);
    }
  }
#endif
#if defined(REDIRECT_MALLOC) && !defined(REDIRECT_FREE)
#define REDIRECT_FREE GC_free
#endif
#if defined(REDIRECT_FREE) && !defined(REDIRECT_MALLOC_IN_HEADER)
#if defined(CPPCHECK)
#define REDIRECT_FREE_F GC_free
#else
#define REDIRECT_FREE_F REDIRECT_FREE
#endif
  void free(void * p)
  {
#ifndef IGNORE_FREE
#if defined(GC_LINUX_THREADS) && !defined(USE_PROC_FOR_LIBRARIES)
        ptr_t caller = (ptr_t)__builtin_return_address(0);
        if (((word)caller >= (word)GC_libpthread_start
             && (word)caller < (word)GC_libpthread_end)
            || ((word)caller >= (word)GC_libld_start
                && (word)caller < (word)GC_libld_end)) {
          GC_free(p);
          return;
        }
#endif
      REDIRECT_FREE_F(p);
#endif
  }
#endif
#include <stdio.h>
#include <string.h>
#ifndef MSWINCE
#include <errno.h>
#endif
#ifndef GC_ALLOC_PTRS_H
#define GC_ALLOC_PTRS_H
#ifdef __cplusplus
  extern "C" {
#endif
#ifndef GC_API_PRIV
#define GC_API_PRIV GC_API
#endif
#ifndef GC_APIVAR_CONST
#if defined(GC_BUILD) || !defined(GC_DLL)
#define GC_APIVAR_CONST const
#else
#define GC_APIVAR_CONST
#endif
#endif
GC_API_PRIV void ** GC_APIVAR_CONST GC_objfreelist_ptr;
GC_API_PRIV void ** GC_APIVAR_CONST GC_aobjfreelist_ptr;
GC_API_PRIV void ** GC_APIVAR_CONST GC_uobjfreelist_ptr;
#ifdef GC_ATOMIC_UNCOLLECTABLE
  GC_API_PRIV void ** GC_APIVAR_CONST GC_auobjfreelist_ptr;
#endif
GC_API_PRIV void GC_CALL GC_incr_bytes_allocd(size_t );
GC_API_PRIV void GC_CALL GC_incr_bytes_freed(size_t );
#ifdef __cplusplus
  }
#endif
#endif
void ** const GC_objfreelist_ptr = GC_objfreelist;
void ** const GC_aobjfreelist_ptr = GC_aobjfreelist;
void ** const GC_uobjfreelist_ptr = GC_uobjfreelist;
#ifdef GC_ATOMIC_UNCOLLECTABLE
    void ** const GC_auobjfreelist_ptr = GC_auobjfreelist;
#endif
GC_API int GC_CALL GC_get_kind_and_size(const void * p, size_t * psize)
{
    hdr * hhdr = HDR(p);
    if (psize != NULL) {
        *psize = (size_t)hhdr->hb_sz;
    }
    return hhdr -> hb_obj_kind;
}
GC_API GC_ATTR_MALLOC void * GC_CALL GC_generic_or_special_malloc(size_t lb,
                                                                  int knd)
{
    switch(knd) {
        case PTRFREE:
        case NORMAL:
            return GC_malloc_kind(lb, knd);
        case UNCOLLECTABLE:
#ifdef GC_ATOMIC_UNCOLLECTABLE
          case AUNCOLLECTABLE:
#endif
            return GC_generic_malloc_uncollectable(lb, knd);
        default:
            return GC_generic_malloc(lb, knd);
    }
}
GC_API void * GC_CALL GC_realloc(void * p, size_t lb)
{
    struct hblk * h;
    hdr * hhdr;
    void * result;
    size_t sz;
    size_t orig_sz;
    int obj_kind;
    if (p == 0) return(GC_malloc(lb));
    if (0 == lb)  {
#ifndef IGNORE_FREE
        GC_free(p);
#endif
      return NULL;
    }
    h = HBLKPTR(p);
    hhdr = HDR(h);
    sz = (size_t)hhdr->hb_sz;
    obj_kind = hhdr -> hb_obj_kind;
    orig_sz = sz;
    if (sz > MAXOBJBYTES) {
        word descr = GC_obj_kinds[obj_kind].ok_descriptor;
        sz = (sz + HBLKSIZE-1) & ~HBLKMASK;
        if (GC_obj_kinds[obj_kind].ok_relocate_descr)
          descr += sz;
#ifdef AO_HAVE_store
          GC_STATIC_ASSERT(sizeof(hhdr->hb_sz) == sizeof(AO_t));
          AO_store((volatile AO_t *)&hhdr->hb_sz, (AO_t)sz);
          AO_store((volatile AO_t *)&hhdr->hb_descr, (AO_t)descr);
#else
          {
            DCL_LOCK_STATE;
            LOCK();
            hhdr -> hb_sz = sz;
            hhdr -> hb_descr = descr;
            UNLOCK();
          }
#endif
#ifdef MARK_BIT_PER_OBJ
            GC_ASSERT(hhdr -> hb_inv_sz == LARGE_INV_SZ);
#endif
#ifdef MARK_BIT_PER_GRANULE
            GC_ASSERT((hhdr -> hb_flags & LARGE_BLOCK) != 0
                        && hhdr -> hb_map[ANY_INDEX] == 1);
#endif
          if (IS_UNCOLLECTABLE(obj_kind)) GC_non_gc_bytes += (sz - orig_sz);
    }
    if (ADD_SLOP(lb) <= sz) {
        if (lb >= (sz >> 1)) {
            if (orig_sz > lb) {
                BZERO(((ptr_t)p) + lb, orig_sz - lb);
            }
            return(p);
        }
        sz = lb;
    }
    result = GC_generic_or_special_malloc((word)lb, obj_kind);
    if (result != NULL) {
      BCOPY(p, result, sz);
#ifndef IGNORE_FREE
        GC_free(p);
#endif
    }
    return result;
}
#if defined(REDIRECT_MALLOC) && !defined(REDIRECT_REALLOC)
#define REDIRECT_REALLOC GC_realloc
#endif
#ifdef REDIRECT_REALLOC
#define GC_debug_realloc_replacement(p, lb) \
        GC_debug_realloc(p, lb, GC_DBG_EXTRAS)
#if !defined(REDIRECT_MALLOC_IN_HEADER)
    void * realloc(void * p, size_t lb)
    {
      return(REDIRECT_REALLOC(p, lb));
    }
#endif
#undef GC_debug_realloc_replacement
#endif
GC_API GC_ATTR_MALLOC void * GC_CALL
    GC_generic_malloc_ignore_off_page(size_t lb, int k)
{
    void *result;
    size_t lg;
    size_t lb_rounded;
    word n_blocks;
    GC_bool init;
    DCL_LOCK_STATE;
    if (SMALL_OBJ(lb))
        return GC_generic_malloc(lb, k);
    GC_ASSERT(k < MAXOBJKINDS);
    lg = ROUNDED_UP_GRANULES(lb);
    lb_rounded = GRANULES_TO_BYTES(lg);
    n_blocks = OBJ_SZ_TO_BLOCKS(lb_rounded);
    init = GC_obj_kinds[k].ok_init;
    if (EXPECT(GC_have_errors, FALSE))
      GC_print_all_errors();
    GC_INVOKE_FINALIZERS();
    GC_DBG_COLLECT_AT_MALLOC(lb);
    LOCK();
    result = (ptr_t)GC_alloc_large(ADD_SLOP(lb), k, IGNORE_OFF_PAGE);
    if (NULL == result) {
        GC_oom_func oom_fn = GC_oom_fn;
        UNLOCK();
        return (*oom_fn)(lb);
    }
    if (GC_debugging_started) {
        BZERO(result, n_blocks * HBLKSIZE);
    } else {
#ifdef THREADS
            ((word *)result)[0] = 0;
            ((word *)result)[1] = 0;
            ((word *)result)[GRANULES_TO_WORDS(lg)-1] = 0;
            ((word *)result)[GRANULES_TO_WORDS(lg)-2] = 0;
#endif
    }
    GC_bytes_allocd += lb_rounded;
    UNLOCK();
    if (init && !GC_debugging_started) {
        BZERO(result, n_blocks * HBLKSIZE);
    }
    return(result);
}
GC_API GC_ATTR_MALLOC void * GC_CALL GC_malloc_ignore_off_page(size_t lb)
{
    return GC_generic_malloc_ignore_off_page(lb, NORMAL);
}
GC_API GC_ATTR_MALLOC void * GC_CALL
    GC_malloc_atomic_ignore_off_page(size_t lb)
{
    return GC_generic_malloc_ignore_off_page(lb, PTRFREE);
}
void GC_CALL GC_incr_bytes_allocd(size_t n)
{
    GC_bytes_allocd += n;
}
void GC_CALL GC_incr_bytes_freed(size_t n)
{
    GC_bytes_freed += n;
}
GC_API size_t GC_CALL GC_get_expl_freed_bytes_since_gc(void)
{
    return (size_t)GC_bytes_freed;
}
#ifdef PARALLEL_MARK
    STATIC volatile AO_t GC_bytes_allocd_tmp = 0;
#endif
GC_API void GC_CALL GC_generic_malloc_many(size_t lb, int k, void **result)
{
    void *op;
    void *p;
    void **opp;
    size_t lw;
    size_t lg;
    signed_word my_bytes_allocd = 0;
    struct obj_kind * ok = &(GC_obj_kinds[k]);
    struct hblk ** rlh;
    DCL_LOCK_STATE;
    GC_ASSERT(lb != 0 && (lb & (GRANULE_BYTES-1)) == 0);
    if (!SMALL_OBJ(lb) || GC_manual_vdb) {
        op = GC_generic_malloc(lb, k);
        if (EXPECT(0 != op, TRUE))
            obj_link(op) = 0;
        *result = op;
#ifndef GC_DISABLE_INCREMENTAL
          if (GC_manual_vdb && GC_is_heap_ptr(result)) {
            GC_dirty_inner(result);
            REACHABLE_AFTER_DIRTY(op);
          }
#endif
        return;
    }
    GC_ASSERT(k < MAXOBJKINDS);
    lw = BYTES_TO_WORDS(lb);
    lg = BYTES_TO_GRANULES(lb);
    if (EXPECT(GC_have_errors, FALSE))
      GC_print_all_errors();
    GC_INVOKE_FINALIZERS();
    GC_DBG_COLLECT_AT_MALLOC(lb);
    if (!EXPECT(GC_is_initialized, TRUE)) GC_init();
    LOCK();
      if (GC_incremental && !GC_dont_gc) {
        ENTER_GC();
        GC_collect_a_little_inner(1);
        EXIT_GC();
      }
    rlh = ok -> ok_reclaim_list;
    if (rlh != NULL) {
        struct hblk * hbp;
        hdr * hhdr;
        while ((hbp = rlh[lg]) != NULL) {
            hhdr = HDR(hbp);
            rlh[lg] = hhdr -> hb_next;
            GC_ASSERT(hhdr -> hb_sz == lb);
            hhdr -> hb_last_reclaimed = (unsigned short) GC_gc_no;
#ifdef PARALLEL_MARK
              if (GC_parallel) {
                  signed_word my_bytes_allocd_tmp =
                                (signed_word)AO_load(&GC_bytes_allocd_tmp);
                  GC_ASSERT(my_bytes_allocd_tmp >= 0);
                  if (my_bytes_allocd_tmp != 0) {
                    (void)AO_fetch_and_add(&GC_bytes_allocd_tmp,
                                           (AO_t)(-my_bytes_allocd_tmp));
                    GC_bytes_allocd += my_bytes_allocd_tmp;
                  }
                  GC_acquire_mark_lock();
                  ++ GC_fl_builder_count;
                  UNLOCK();
                  GC_release_mark_lock();
              }
#endif
            op = GC_reclaim_generic(hbp, hhdr, lb,
                                    ok -> ok_init, 0, &my_bytes_allocd);
            if (op != 0) {
#ifdef PARALLEL_MARK
                if (GC_parallel) {
                  *result = op;
                  (void)AO_fetch_and_add(&GC_bytes_allocd_tmp,
                                         (AO_t)my_bytes_allocd);
                  GC_acquire_mark_lock();
                  -- GC_fl_builder_count;
                  if (GC_fl_builder_count == 0) GC_notify_all_builder();
#ifdef THREAD_SANITIZER
                    GC_release_mark_lock();
                    LOCK();
                    GC_bytes_found += my_bytes_allocd;
                    UNLOCK();
#else
                    GC_bytes_found += my_bytes_allocd;
                    GC_release_mark_lock();
#endif
                  (void) GC_clear_stack(0);
                  return;
                }
#endif
              GC_bytes_found += my_bytes_allocd;
              GC_bytes_allocd += my_bytes_allocd;
              goto out;
            }
#ifdef PARALLEL_MARK
              if (GC_parallel) {
                GC_acquire_mark_lock();
                -- GC_fl_builder_count;
                if (GC_fl_builder_count == 0) GC_notify_all_builder();
                GC_release_mark_lock();
                LOCK();
                rlh = ok -> ok_reclaim_list;
                if (NULL == rlh) break;
              }
#endif
        }
    }
      opp = &(GC_obj_kinds[k].ok_freelist[lg]);
      if ( (op = *opp) != 0 ) {
        *opp = 0;
        my_bytes_allocd = 0;
        for (p = op; p != 0; p = obj_link(p)) {
          my_bytes_allocd += lb;
          if ((word)my_bytes_allocd >= HBLKSIZE) {
            *opp = obj_link(p);
            obj_link(p) = 0;
            break;
          }
        }
        GC_bytes_allocd += my_bytes_allocd;
        goto out;
      }
    {
        struct hblk *h = GC_allochblk(lb, k, 0);
        if (h ) {
          if (IS_UNCOLLECTABLE(k)) GC_set_hdr_marks(HDR(h));
          GC_bytes_allocd += HBLKSIZE - HBLKSIZE % lb;
#ifdef PARALLEL_MARK
            if (GC_parallel) {
              GC_acquire_mark_lock();
              ++ GC_fl_builder_count;
              UNLOCK();
              GC_release_mark_lock();
              op = GC_build_fl(h, lw,
                        (ok -> ok_init || GC_debugging_started), 0);
              *result = op;
              GC_acquire_mark_lock();
              -- GC_fl_builder_count;
              if (GC_fl_builder_count == 0) GC_notify_all_builder();
              GC_release_mark_lock();
              (void) GC_clear_stack(0);
              return;
            }
#endif
          op = GC_build_fl(h, lw, (ok -> ok_init || GC_debugging_started), 0);
          goto out;
        }
    }
      op = GC_generic_malloc_inner(lb, k);
      if (0 != op) obj_link(op) = 0;
  out:
    *result = op;
    UNLOCK();
    (void) GC_clear_stack(0);
}
GC_API GC_ATTR_MALLOC void * GC_CALL GC_malloc_many(size_t lb)
{
    void *result;
    lb = SIZET_SAT_ADD(lb, EXTRA_BYTES + GRANULE_BYTES - 1)
            & ~(GRANULE_BYTES - 1);
    GC_generic_malloc_many(lb, NORMAL, &result);
    return result;
}
#include <limits.h>
GC_API GC_ATTR_MALLOC void * GC_CALL GC_memalign(size_t align, size_t lb)
{
    size_t new_lb;
    size_t offset;
    ptr_t result;
    if (align <= GRANULE_BYTES) return GC_malloc(lb);
    if (align >= HBLKSIZE/2 || lb >= HBLKSIZE/2) {
        if (align > HBLKSIZE) {
          return (*GC_get_oom_fn())(LONG_MAX-1024);
        }
        return GC_malloc(lb <= HBLKSIZE? HBLKSIZE : lb);
    }
    new_lb = SIZET_SAT_ADD(lb, align - 1);
    result = (ptr_t)GC_malloc(new_lb);
    offset = (word)result % align;
    if (offset != 0) {
        offset = align - offset;
        if (!GC_all_interior_pointers) {
            GC_STATIC_ASSERT(VALID_OFFSET_SZ <= HBLKSIZE);
            GC_ASSERT(offset < VALID_OFFSET_SZ);
            GC_register_displacement(offset);
        }
    }
    result += offset;
    GC_ASSERT((word)result % align == 0);
    return result;
}
GC_API int GC_CALL GC_posix_memalign(void **memptr, size_t align, size_t lb)
{
  size_t align_minus_one = align - 1;
  if (align < sizeof(void *) || (align_minus_one & align) != 0) {
#ifdef MSWINCE
      return ERROR_INVALID_PARAMETER;
#else
      return EINVAL;
#endif
  }
  if ((*memptr = GC_memalign(align, lb)) == NULL) {
#ifdef MSWINCE
      return ERROR_NOT_ENOUGH_MEMORY;
#else
      return ENOMEM;
#endif
  }
  return 0;
}
GC_API GC_ATTR_MALLOC char * GC_CALL GC_strdup(const char *s)
{
  char *copy;
  size_t lb;
  if (s == NULL) return NULL;
  lb = strlen(s) + 1;
  copy = (char *)GC_malloc_atomic(lb);
  if (NULL == copy) {
#ifndef MSWINCE
      errno = ENOMEM;
#endif
    return NULL;
  }
  BCOPY(s, copy, lb);
  return copy;
}
GC_API GC_ATTR_MALLOC char * GC_CALL GC_strndup(const char *str, size_t size)
{
  char *copy;
  size_t len = strlen(str);
  if (len > size)
    len = size;
  copy = (char *)GC_malloc_atomic(len + 1);
  if (copy == NULL) {
#ifndef MSWINCE
      errno = ENOMEM;
#endif
    return NULL;
  }
  if (EXPECT(len > 0, TRUE))
    BCOPY(str, copy, len);
  copy[len] = '\0';
  return copy;
}
#ifdef GC_REQUIRE_WCSDUP
#include <wchar.h>
  GC_API GC_ATTR_MALLOC wchar_t * GC_CALL GC_wcsdup(const wchar_t *str)
  {
    size_t lb = (wcslen(str) + 1) * sizeof(wchar_t);
    wchar_t *copy = (wchar_t *)GC_malloc_atomic(lb);
    if (copy == NULL) {
#ifndef MSWINCE
        errno = ENOMEM;
#endif
      return NULL;
    }
    BCOPY(str, copy, lb);
    return copy;
  }
#endif
#ifndef CPPCHECK
  GC_API void * GC_CALL GC_malloc_stubborn(size_t lb)
  {
    return GC_malloc(lb);
  }
  GC_API void GC_CALL GC_change_stubborn(const void *p GC_ATTR_UNUSED)
  {
  }
#endif
GC_API void GC_CALL GC_end_stubborn_change(const void *p)
{
  GC_dirty(p);
}
GC_API void GC_CALL GC_ptr_store_and_dirty(void *p, const void *q)
{
  *(const void **)p = q;
  GC_dirty(p);
  REACHABLE_AFTER_DIRTY(q);
}
#if defined(__MINGW32__) && !defined(__MINGW_EXCPT_DEFINE_PSDK) \
    && defined(__i386__)
#define __MINGW_EXCPT_DEFINE_PSDK 1
#endif
#include <stdio.h>
#if defined(MSWIN32) && defined(__GNUC__)
#include <excpt.h>
#endif
GC_ATTR_NOINLINE
void GC_noop6(word arg1 GC_ATTR_UNUSED, word arg2 GC_ATTR_UNUSED,
              word arg3 GC_ATTR_UNUSED, word arg4 GC_ATTR_UNUSED,
              word arg5 GC_ATTR_UNUSED, word arg6 GC_ATTR_UNUSED)
{
#if defined(AO_HAVE_compiler_barrier) && !defined(BASE_ATOMIC_OPS_EMULATED)
    AO_compiler_barrier();
#else
    GC_noop1(0);
#endif
}
volatile word GC_noop_sink;
GC_ATTR_NO_SANITIZE_THREAD
GC_API void GC_CALL GC_noop1(word x)
{
    GC_noop_sink = x;
}
GC_INNER struct obj_kind GC_obj_kinds[MAXOBJKINDS] = {
 { &GC_aobjfreelist[0], 0 ,
                 GC_DS_LENGTH, FALSE, FALSE
                 OK_DISCLAIM_INITZ },
  { &GC_objfreelist[0], 0,
                 GC_DS_LENGTH,
                TRUE , TRUE
                 OK_DISCLAIM_INITZ },
              { &GC_uobjfreelist[0], 0,
                 GC_DS_LENGTH, TRUE , TRUE
                 OK_DISCLAIM_INITZ },
#ifdef GC_ATOMIC_UNCOLLECTABLE
              { &GC_auobjfreelist[0], 0,
                 GC_DS_LENGTH, FALSE , FALSE
                 OK_DISCLAIM_INITZ },
#endif
};
#ifndef INITIAL_MARK_STACK_SIZE
#define INITIAL_MARK_STACK_SIZE (1*HBLKSIZE)
#endif
#if !defined(GC_DISABLE_INCREMENTAL)
  STATIC word GC_n_rescuing_pages = 0;
#endif
#ifdef PARALLEL_MARK
  GC_INNER GC_bool GC_parallel_mark_disabled = FALSE;
#endif
GC_INNER GC_bool GC_collection_in_progress(void)
{
    return(GC_mark_state != MS_NONE);
}
GC_INNER void GC_clear_hdr_marks(hdr *hhdr)
{
  size_t last_bit;
#ifdef AO_HAVE_load
    last_bit = FINAL_MARK_BIT((size_t)AO_load((volatile AO_t *)&hhdr->hb_sz));
#else
    last_bit = FINAL_MARK_BIT((size_t)hhdr->hb_sz);
#endif
    BZERO(hhdr -> hb_marks, sizeof(hhdr->hb_marks));
    set_mark_bit_from_hdr(hhdr, last_bit);
    hhdr -> hb_n_marks = 0;
}
GC_INNER void GC_set_hdr_marks(hdr *hhdr)
{
    unsigned i;
    size_t sz = (size_t)hhdr->hb_sz;
    unsigned n_marks = (unsigned)FINAL_MARK_BIT(sz);
#ifdef USE_MARK_BYTES
      for (i = 0; i <= n_marks; i += (unsigned)MARK_BIT_OFFSET(sz)) {
        hhdr -> hb_marks[i] = 1;
      }
#else
      for (i = 0; i < divWORDSZ(n_marks + WORDSZ); ++i) {
        hhdr -> hb_marks[i] = GC_WORD_MAX;
      }
#endif
#ifdef MARK_BIT_PER_OBJ
      hhdr -> hb_n_marks = n_marks;
#else
      hhdr -> hb_n_marks = HBLK_OBJS(sz);
#endif
}
static void clear_marks_for_block(struct hblk *h, word dummy GC_ATTR_UNUSED)
{
    hdr * hhdr = HDR(h);
    if (IS_UNCOLLECTABLE(hhdr -> hb_obj_kind)) return;
    GC_clear_hdr_marks(hhdr);
}
GC_API void GC_CALL GC_set_mark_bit(const void *p)
{
    struct hblk *h = HBLKPTR(p);
    hdr * hhdr = HDR(h);
    word bit_no = MARK_BIT_NO((ptr_t)p - (ptr_t)h, hhdr -> hb_sz);
    if (!mark_bit_from_hdr(hhdr, bit_no)) {
      set_mark_bit_from_hdr(hhdr, bit_no);
      ++hhdr -> hb_n_marks;
    }
}
GC_API void GC_CALL GC_clear_mark_bit(const void *p)
{
    struct hblk *h = HBLKPTR(p);
    hdr * hhdr = HDR(h);
    word bit_no = MARK_BIT_NO((ptr_t)p - (ptr_t)h, hhdr -> hb_sz);
    if (mark_bit_from_hdr(hhdr, bit_no)) {
      size_t n_marks = hhdr -> hb_n_marks;
      GC_ASSERT(n_marks != 0);
      clear_mark_bit_from_hdr(hhdr, bit_no);
      n_marks--;
#ifdef PARALLEL_MARK
        if (n_marks != 0 || !GC_parallel)
          hhdr -> hb_n_marks = n_marks;
#else
          hhdr -> hb_n_marks = n_marks;
#endif
    }
}
GC_API int GC_CALL GC_is_marked(const void *p)
{
    struct hblk *h = HBLKPTR(p);
    hdr * hhdr = HDR(h);
    word bit_no = MARK_BIT_NO((ptr_t)p - (ptr_t)h, hhdr -> hb_sz);
    return (int)mark_bit_from_hdr(hhdr, bit_no);
}
GC_INNER void GC_clear_marks(void)
{
    GC_apply_to_all_blocks(clear_marks_for_block, (word)0);
    GC_objects_are_marked = FALSE;
    GC_mark_state = MS_INVALID;
    GC_scan_ptr = NULL;
}
GC_INNER void GC_initiate_gc(void)
{
    GC_ASSERT(I_HOLD_LOCK());
#ifndef GC_DISABLE_INCREMENTAL
        if (GC_incremental) {
#ifdef CHECKSUMS
            GC_read_dirty(FALSE);
            GC_check_dirty();
#else
            GC_read_dirty(GC_mark_state == MS_INVALID);
#endif
        }
        GC_n_rescuing_pages = 0;
#endif
    if (GC_mark_state == MS_NONE) {
        GC_mark_state = MS_PUSH_RESCUERS;
    } else if (GC_mark_state != MS_INVALID) {
        ABORT("Unexpected state");
    }
    GC_scan_ptr = NULL;
}
#ifdef PARALLEL_MARK
    STATIC void GC_do_parallel_mark(void);
#endif
#ifdef GC_DISABLE_INCREMENTAL
#define GC_push_next_marked_dirty(h) GC_push_next_marked(h)
#else
  STATIC struct hblk * GC_push_next_marked_dirty(struct hblk *h);
#endif
STATIC struct hblk * GC_push_next_marked(struct hblk *h);
STATIC struct hblk * GC_push_next_marked_uncollectable(struct hblk *h);
static void alloc_mark_stack(size_t);
#ifdef WRAP_MARK_SOME
  STATIC GC_bool GC_mark_some_inner(ptr_t cold_gc_frame)
#else
  GC_INNER GC_bool GC_mark_some(ptr_t cold_gc_frame)
#endif
{
    switch(GC_mark_state) {
        case MS_NONE:
            break;
        case MS_PUSH_RESCUERS:
            if ((word)GC_mark_stack_top
                >= (word)(GC_mark_stack_limit - INITIAL_MARK_STACK_SIZE/2)) {
                GC_mark_stack_too_small = TRUE;
                MARK_FROM_MARK_STACK();
                break;
            } else {
                GC_scan_ptr = GC_push_next_marked_dirty(GC_scan_ptr);
                if (NULL == GC_scan_ptr) {
#if !defined(GC_DISABLE_INCREMENTAL)
                    GC_COND_LOG_PRINTF("Marked from %lu dirty pages\n",
                                       (unsigned long)GC_n_rescuing_pages);
#endif
                    GC_push_roots(FALSE, cold_gc_frame);
                    GC_objects_are_marked = TRUE;
                    if (GC_mark_state != MS_INVALID) {
                        GC_mark_state = MS_ROOTS_PUSHED;
                    }
                }
            }
            break;
        case MS_PUSH_UNCOLLECTABLE:
            if ((word)GC_mark_stack_top
                >= (word)(GC_mark_stack + GC_mark_stack_size/4)) {
#ifdef PARALLEL_MARK
                  if (GC_parallel) GC_mark_stack_too_small = TRUE;
#endif
                MARK_FROM_MARK_STACK();
                break;
            } else {
                GC_scan_ptr = GC_push_next_marked_uncollectable(GC_scan_ptr);
                if (NULL == GC_scan_ptr) {
                    GC_push_roots(TRUE, cold_gc_frame);
                    GC_objects_are_marked = TRUE;
                    if (GC_mark_state != MS_INVALID) {
                        GC_mark_state = MS_ROOTS_PUSHED;
                    }
                }
            }
            break;
        case MS_ROOTS_PUSHED:
#ifdef PARALLEL_MARK
                if (GC_parallel && !GC_parallel_mark_disabled) {
                  GC_do_parallel_mark();
                  GC_ASSERT((word)GC_mark_stack_top < (word)GC_first_nonempty);
                  GC_mark_stack_top = GC_mark_stack - 1;
                  if (GC_mark_stack_too_small) {
                    alloc_mark_stack(2*GC_mark_stack_size);
                  }
                  if (GC_mark_state == MS_ROOTS_PUSHED) {
                    GC_mark_state = MS_NONE;
                    return(TRUE);
                  }
                  break;
                }
#endif
            if ((word)GC_mark_stack_top >= (word)GC_mark_stack) {
                MARK_FROM_MARK_STACK();
                break;
            } else {
                GC_mark_state = MS_NONE;
                if (GC_mark_stack_too_small) {
                    alloc_mark_stack(2*GC_mark_stack_size);
                }
                return(TRUE);
            }
        case MS_INVALID:
        case MS_PARTIALLY_INVALID:
            if (!GC_objects_are_marked) {
                GC_mark_state = MS_PUSH_UNCOLLECTABLE;
                break;
            }
            if ((word)GC_mark_stack_top >= (word)GC_mark_stack) {
                MARK_FROM_MARK_STACK();
                break;
            }
            if (NULL == GC_scan_ptr && GC_mark_state == MS_INVALID) {
                if (GC_mark_stack_too_small) {
                    alloc_mark_stack(2*GC_mark_stack_size);
                }
                GC_mark_state = MS_PARTIALLY_INVALID;
            }
            GC_scan_ptr = GC_push_next_marked(GC_scan_ptr);
            if (NULL == GC_scan_ptr && GC_mark_state == MS_PARTIALLY_INVALID) {
                GC_push_roots(TRUE, cold_gc_frame);
                GC_objects_are_marked = TRUE;
                if (GC_mark_state != MS_INVALID) {
                    GC_mark_state = MS_ROOTS_PUSHED;
                }
            }
            break;
        default:
            ABORT("GC_mark_some: bad state");
    }
    return(FALSE);
}
#ifdef WRAP_MARK_SOME
#if (defined(MSWIN32) || defined(MSWINCE)) && defined(__GNUC__)
    typedef struct {
      EXCEPTION_REGISTRATION ex_reg;
      void *alt_path;
    } ext_ex_regn;
    static EXCEPTION_DISPOSITION mark_ex_handler(
        struct _EXCEPTION_RECORD *ex_rec,
        void *est_frame,
        struct _CONTEXT *context,
        void *disp_ctxt GC_ATTR_UNUSED)
    {
        if (ex_rec->ExceptionCode == STATUS_ACCESS_VIOLATION) {
          ext_ex_regn *xer = (ext_ex_regn *)est_frame;
          context->Esp = context->Ebp;
          context->Ebp = *((DWORD *)context->Esp);
          context->Esp = context->Esp - 8;
          context->Eip = (DWORD )(xer->alt_path);
          return ExceptionContinueExecution;
        } else {
            return ExceptionContinueSearch;
        }
    }
#endif
  GC_INNER GC_bool GC_mark_some(ptr_t cold_gc_frame)
  {
      GC_bool ret_val;
#if defined(MSWIN32) || defined(MSWINCE)
#ifndef __GNUC__
      __try {
          ret_val = GC_mark_some_inner(cold_gc_frame);
      } __except (GetExceptionCode() == EXCEPTION_ACCESS_VIOLATION ?
                EXCEPTION_EXECUTE_HANDLER : EXCEPTION_CONTINUE_SEARCH) {
          goto handle_ex;
      }
#if defined(GC_WIN32_THREADS) && !defined(GC_PTHREADS)
        if (GC_started_thread_while_stopped())
          goto handle_thr_start;
#endif
     rm_handler:
      return ret_val;
#else
      ext_ex_regn er;
#if GC_GNUC_PREREQ(4, 7) || GC_CLANG_PREREQ(3, 3)
#pragma GCC diagnostic push
#if defined(__clang__) || GC_GNUC_PREREQ(6, 4)
#pragma GCC diagnostic ignored "-Wpedantic"
#else
#pragma GCC diagnostic ignored "-pedantic"
#endif
        er.alt_path = &&handle_ex;
#pragma GCC diagnostic pop
#elif !defined(CPPCHECK)
        er.alt_path = &&handle_ex;
#endif
      er.ex_reg.handler = mark_ex_handler;
      __asm__ __volatile__ ("movl %%fs:0, %0" : "=r" (er.ex_reg.prev));
      __asm__ __volatile__ ("movl %0, %%fs:0" : : "r" (&er));
      ret_val = GC_mark_some_inner(cold_gc_frame);
        if (er.alt_path == 0)
          goto handle_ex;
#if defined(GC_WIN32_THREADS) && !defined(GC_PTHREADS)
        if (GC_started_thread_while_stopped())
          goto handle_thr_start;
#endif
    rm_handler:
      __asm__ __volatile__ ("mov %0, %%fs:0" : : "r" (er.ex_reg.prev));
      return ret_val;
#endif
#else
#ifndef DEFAULT_VDB
        if (GC_auto_incremental) {
          static GC_bool is_warned = FALSE;
          if (!is_warned) {
            is_warned = TRUE;
            WARN("Incremental GC incompatible with /proc roots\n", 0);
          }
        }
#endif
      GC_setup_temporary_fault_handler();
      if(SETJMP(GC_jmp_buf) != 0) goto handle_ex;
      ret_val = GC_mark_some_inner(cold_gc_frame);
    rm_handler:
      GC_reset_fault_handler();
      return ret_val;
#endif
handle_ex:
      {
        static word warned_gc_no;
        if (warned_gc_no != GC_gc_no) {
          WARN("Caught ACCESS_VIOLATION in marker;"
               " memory mapping disappeared\n", 0);
          warned_gc_no = GC_gc_no;
        }
      }
#if (defined(MSWIN32) || defined(MSWINCE)) && defined(GC_WIN32_THREADS) \
       && !defined(GC_PTHREADS)
      handle_thr_start:
#endif
#ifdef REGISTER_LIBRARIES_EARLY
        START_WORLD();
        GC_cond_register_dynamic_libraries();
        STOP_WORLD();
#endif
      GC_invalidate_mark_state();
      GC_scan_ptr = NULL;
      ret_val = FALSE;
      goto rm_handler;
  }
#endif
GC_INNER void GC_invalidate_mark_state(void)
{
    GC_mark_state = MS_INVALID;
    GC_mark_stack_top = GC_mark_stack-1;
}
GC_INNER mse * GC_signal_mark_stack_overflow(mse *msp)
{
    GC_mark_state = MS_INVALID;
#ifdef PARALLEL_MARK
      if (!GC_parallel)
        GC_mark_stack_too_small = TRUE;
#else
      GC_mark_stack_too_small = TRUE;
#endif
    GC_COND_LOG_PRINTF("Mark stack overflow; current size: %lu entries\n",
                       (unsigned long)GC_mark_stack_size);
    return(msp - GC_MARK_STACK_DISCARDS);
}
GC_ATTR_NO_SANITIZE_ADDR GC_ATTR_NO_SANITIZE_MEMORY GC_ATTR_NO_SANITIZE_THREAD
GC_INNER mse * GC_mark_from(mse *mark_stack_top, mse *mark_stack,
                            mse *mark_stack_limit)
{
  signed_word credit = HBLKSIZE;
  ptr_t current_p;
  word current;
  ptr_t limit = 0;
  word descr;
  ptr_t greatest_ha = (ptr_t)GC_greatest_plausible_heap_addr;
  ptr_t least_ha = (ptr_t)GC_least_plausible_heap_addr;
  DECLARE_HDR_CACHE;
#define SPLIT_RANGE_WORDS 128
  GC_objects_are_marked = TRUE;
  INIT_HDR_CACHE;
#ifdef OS2
    while ((word)mark_stack_top >= (word)mark_stack && credit >= 0)
#else
    while (((((word)mark_stack_top - (word)mark_stack) | (word)credit)
            & SIGNB) == 0)
#endif
  {
    current_p = mark_stack_top -> mse_start;
    descr = mark_stack_top -> mse_descr.w;
  retry:
    if (descr & ((~(WORDS_TO_BYTES(SPLIT_RANGE_WORDS) - 1)) | GC_DS_TAGS)) {
      word tag = descr & GC_DS_TAGS;
      GC_STATIC_ASSERT(GC_DS_TAGS == 0x3);
      switch(tag) {
        case GC_DS_LENGTH:
          GC_ASSERT(descr < (word)GC_greatest_plausible_heap_addr
                            - (word)GC_least_plausible_heap_addr
                || (word)(current_p + descr)
                            <= (word)GC_least_plausible_heap_addr
                || (word)current_p >= (word)GC_greatest_plausible_heap_addr);
#ifdef PARALLEL_MARK
#define SHARE_BYTES 2048
            if (descr > SHARE_BYTES && GC_parallel
                && (word)mark_stack_top < (word)(mark_stack_limit - 1)) {
              word new_size = (descr/2) & ~(word)(sizeof(word)-1);
              mark_stack_top -> mse_start = current_p;
              mark_stack_top -> mse_descr.w = new_size + sizeof(word);
              mark_stack_top++;
#ifdef ENABLE_TRACE
                if ((word)GC_trace_addr >= (word)current_p
                    && (word)GC_trace_addr < (word)(current_p + descr)) {
                  GC_log_printf("GC #%lu: large section; start %p, len %lu,"
                                " splitting (parallel) at %p\n",
                                (unsigned long)GC_gc_no, (void *)current_p,
                                (unsigned long)descr,
                                (void *)(current_p + new_size));
                }
#endif
              current_p += new_size;
              descr -= new_size;
              goto retry;
            }
#endif
          mark_stack_top -> mse_start =
                limit = current_p + WORDS_TO_BYTES(SPLIT_RANGE_WORDS-1);
          mark_stack_top -> mse_descr.w =
                                descr - WORDS_TO_BYTES(SPLIT_RANGE_WORDS-1);
#ifdef ENABLE_TRACE
            if ((word)GC_trace_addr >= (word)current_p
                && (word)GC_trace_addr < (word)(current_p + descr)) {
              GC_log_printf("GC #%lu: large section; start %p, len %lu,"
                            " splitting at %p\n",
                            (unsigned long)GC_gc_no, (void *)current_p,
                            (unsigned long)descr, (void *)limit);
            }
#endif
          limit += sizeof(word) - ALIGNMENT;
          break;
        case GC_DS_BITMAP:
          mark_stack_top--;
#ifdef ENABLE_TRACE
            if ((word)GC_trace_addr >= (word)current_p
                && (word)GC_trace_addr < (word)(current_p
                                                + WORDS_TO_BYTES(WORDSZ-2))) {
              GC_log_printf("GC #%lu: tracing from %p bitmap descr %lu\n",
                            (unsigned long)GC_gc_no, (void *)current_p,
                            (unsigned long)descr);
            }
#endif
          descr &= ~GC_DS_TAGS;
          credit -= WORDS_TO_BYTES(WORDSZ/2);
          while (descr != 0) {
            if ((descr & SIGNB) != 0) {
              current = *(word *)current_p;
              FIXUP_POINTER(current);
              if (current >= (word)least_ha && current < (word)greatest_ha) {
                PREFETCH((ptr_t)current);
#ifdef ENABLE_TRACE
                  if (GC_trace_addr == current_p) {
                    GC_log_printf("GC #%lu: considering(3) %p -> %p\n",
                                  (unsigned long)GC_gc_no, (void *)current_p,
                                  (void *)current);
                  }
#endif
                PUSH_CONTENTS((ptr_t)current, mark_stack_top,
                              mark_stack_limit, current_p);
              }
            }
            descr <<= 1;
            current_p += sizeof(word);
          }
          continue;
        case GC_DS_PROC:
          mark_stack_top--;
#ifdef ENABLE_TRACE
            if ((word)GC_trace_addr >= (word)current_p
                && GC_base(current_p) != 0
                && GC_base(current_p) == GC_base(GC_trace_addr)) {
              GC_log_printf("GC #%lu: tracing from %p, proc descr %lu\n",
                            (unsigned long)GC_gc_no, (void *)current_p,
                            (unsigned long)descr);
            }
#endif
          credit -= GC_PROC_BYTES;
          mark_stack_top = (*PROC(descr))((word *)current_p, mark_stack_top,
                                          mark_stack_limit, ENV(descr));
          continue;
        case GC_DS_PER_OBJECT:
          if ((signed_word)descr >= 0) {
            descr = *(word *)(current_p + descr - GC_DS_PER_OBJECT);
          } else {
            ptr_t type_descr = *(ptr_t *)current_p;
            if (EXPECT(0 == type_descr, FALSE)) {
                mark_stack_top--;
                continue;
            }
            descr = *(word *)(type_descr
                              - ((signed_word)descr + (GC_INDIR_PER_OBJ_BIAS
                                                       - GC_DS_PER_OBJECT)));
          }
          if (0 == descr) {
              mark_stack_top--;
              continue;
          }
          goto retry;
      }
    } else {
      mark_stack_top--;
#ifndef SMALL_CONFIG
        if (descr < sizeof(word))
          continue;
#endif
#ifdef ENABLE_TRACE
        if ((word)GC_trace_addr >= (word)current_p
            && (word)GC_trace_addr < (word)(current_p + descr)) {
          GC_log_printf("GC #%lu: small object; start %p, len %lu\n",
                        (unsigned long)GC_gc_no, (void *)current_p,
                        (unsigned long)descr);
        }
#endif
      limit = current_p + (word)descr;
    }
    GC_ASSERT(!((word)current_p & (ALIGNMENT-1)));
    credit -= limit - current_p;
    limit -= sizeof(word);
    {
#define PREF_DIST 4
#ifndef SMALL_CONFIG
        word deferred;
        for(;;) {
          PREFETCH(limit - PREF_DIST*CACHE_LINE_SIZE);
          GC_ASSERT((word)limit >= (word)current_p);
          deferred = *(word *)limit;
          FIXUP_POINTER(deferred);
          limit -= ALIGNMENT;
          if (deferred >= (word)least_ha && deferred < (word)greatest_ha) {
            PREFETCH((ptr_t)deferred);
            break;
          }
          if ((word)current_p > (word)limit) goto next_object;
          deferred = *(word *)limit;
          FIXUP_POINTER(deferred);
          limit -= ALIGNMENT;
          if (deferred >= (word)least_ha && deferred < (word)greatest_ha) {
            PREFETCH((ptr_t)deferred);
            break;
          }
          if ((word)current_p > (word)limit) goto next_object;
        }
#endif
      while ((word)current_p <= (word)limit) {
        current = *(word *)current_p;
        FIXUP_POINTER(current);
        PREFETCH(current_p + PREF_DIST*CACHE_LINE_SIZE);
        if (current >= (word)least_ha && current < (word)greatest_ha) {
          PREFETCH((ptr_t)current);
#ifdef ENABLE_TRACE
            if (GC_trace_addr == current_p) {
              GC_log_printf("GC #%lu: considering(1) %p -> %p\n",
                            (unsigned long)GC_gc_no, (void *)current_p,
                            (void *)current);
            }
#endif
          PUSH_CONTENTS((ptr_t)current, mark_stack_top,
                        mark_stack_limit, current_p);
        }
        current_p += ALIGNMENT;
      }
#ifndef SMALL_CONFIG
#ifdef ENABLE_TRACE
            if (GC_trace_addr == current_p) {
              GC_log_printf("GC #%lu: considering(2) %p -> %p\n",
                            (unsigned long)GC_gc_no, (void *)current_p,
                            (void *)deferred);
            }
#endif
        PUSH_CONTENTS((ptr_t)deferred, mark_stack_top,
                      mark_stack_limit, current_p);
        next_object:;
#endif
    }
  }
  return mark_stack_top;
}
#ifdef PARALLEL_MARK
STATIC GC_bool GC_help_wanted = FALSE;
STATIC unsigned GC_helper_count = 0;
STATIC unsigned GC_active_count = 0;
GC_INNER word GC_mark_no = 0;
#ifdef LINT2
#define LOCAL_MARK_STACK_SIZE (HBLKSIZE / 8)
#else
#define LOCAL_MARK_STACK_SIZE HBLKSIZE
#endif
GC_INNER void GC_wait_for_markers_init(void)
{
  signed_word count;
  if (GC_markers_m1 == 0)
    return;
#ifndef CAN_HANDLE_FORK
    GC_ASSERT(NULL == GC_main_local_mark_stack);
#else
    if (NULL == GC_main_local_mark_stack)
#endif
  {
    size_t bytes_to_get =
                ROUNDUP_PAGESIZE_IF_MMAP(LOCAL_MARK_STACK_SIZE * sizeof(mse));
    GC_ASSERT(GC_page_size != 0);
    GC_main_local_mark_stack = (mse *)GET_MEM(bytes_to_get);
    if (NULL == GC_main_local_mark_stack)
      ABORT("Insufficient memory for main local_mark_stack");
    GC_add_to_our_memory((ptr_t)GC_main_local_mark_stack, bytes_to_get);
  }
  GC_acquire_mark_lock();
  GC_fl_builder_count += GC_markers_m1;
  count = GC_fl_builder_count;
  GC_release_mark_lock();
  if (count != 0) {
    GC_ASSERT(count > 0);
    GC_wait_for_reclaim();
  }
}
STATIC mse * GC_steal_mark_stack(mse * low, mse * high, mse * local,
                                 unsigned max, mse **next)
{
    mse *p;
    mse *top = local - 1;
    unsigned i = 0;
    GC_ASSERT((word)high >= (word)(low - 1)
              && (word)(high - low + 1) <= GC_mark_stack_size);
    for (p = low; (word)p <= (word)high && i <= max; ++p) {
        word descr = (word)AO_load(&p->mse_descr.ao);
        if (descr != 0) {
            AO_store_release_write(&p->mse_descr.ao, 0);
            ++top;
            top -> mse_descr.w = descr;
            top -> mse_start = p -> mse_start;
            GC_ASSERT((descr & GC_DS_TAGS) != GC_DS_LENGTH
                      || descr < (word)GC_greatest_plausible_heap_addr
                                        - (word)GC_least_plausible_heap_addr
                      || (word)(p->mse_start + descr)
                            <= (word)GC_least_plausible_heap_addr
                      || (word)p->mse_start
                            >= (word)GC_greatest_plausible_heap_addr);
            ++i;
            if ((descr & GC_DS_TAGS) == GC_DS_LENGTH) i += (int)(descr >> 8);
        }
    }
    *next = p;
    return top;
}
STATIC void GC_return_mark_stack(mse * low, mse * high)
{
    mse * my_top;
    mse * my_start;
    size_t stack_size;
    if ((word)high < (word)low) return;
    stack_size = high - low + 1;
    GC_acquire_mark_lock();
    my_top = GC_mark_stack_top;
    my_start = my_top + 1;
    if ((word)(my_start - GC_mark_stack + stack_size)
                > (word)GC_mark_stack_size) {
      GC_COND_LOG_PRINTF("No room to copy back mark stack\n");
      GC_mark_state = MS_INVALID;
      GC_mark_stack_too_small = TRUE;
    } else {
      BCOPY(low, my_start, stack_size * sizeof(mse));
      GC_ASSERT((mse *)AO_load((volatile AO_t *)(&GC_mark_stack_top))
                == my_top);
      AO_store_release_write((volatile AO_t *)(&GC_mark_stack_top),
                             (AO_t)(my_top + stack_size));
    }
    GC_release_mark_lock();
    GC_notify_all_marker();
}
#ifndef N_LOCAL_ITERS
#define N_LOCAL_ITERS 1
#endif
static GC_bool has_inactive_helpers(void)
{
  GC_bool res;
  GC_acquire_mark_lock();
  res = GC_active_count < GC_helper_count;
  GC_release_mark_lock();
  return res;
}
STATIC void GC_do_local_mark(mse *local_mark_stack, mse *local_top)
{
    unsigned n;
    for (;;) {
        for (n = 0; n < N_LOCAL_ITERS; ++n) {
            local_top = GC_mark_from(local_top, local_mark_stack,
                                     local_mark_stack + LOCAL_MARK_STACK_SIZE);
            if ((word)local_top < (word)local_mark_stack) return;
            if ((word)(local_top - local_mark_stack)
                        >= LOCAL_MARK_STACK_SIZE / 2) {
                GC_return_mark_stack(local_mark_stack, local_top);
                return;
            }
        }
        if ((word)AO_load((volatile AO_t *)&GC_mark_stack_top)
            < (word)AO_load(&GC_first_nonempty)
            && (word)local_top > (word)(local_mark_stack + 1)
            && has_inactive_helpers()) {
            mse * new_bottom = local_mark_stack
                                + (local_top - local_mark_stack)/2;
            GC_ASSERT((word)new_bottom > (word)local_mark_stack
                      && (word)new_bottom < (word)local_top);
            GC_return_mark_stack(local_mark_stack, new_bottom - 1);
            memmove(local_mark_stack, new_bottom,
                    (local_top - new_bottom + 1) * sizeof(mse));
            local_top -= (new_bottom - local_mark_stack);
        }
    }
}
#ifndef ENTRIES_TO_GET
#define ENTRIES_TO_GET 5
#endif
STATIC void GC_mark_local(mse *local_mark_stack, int id)
{
    mse * my_first_nonempty;
    GC_active_count++;
    my_first_nonempty = (mse *)AO_load(&GC_first_nonempty);
    GC_ASSERT((word)GC_mark_stack <= (word)my_first_nonempty);
    GC_ASSERT((word)my_first_nonempty
        <= (word)AO_load((volatile AO_t *)&GC_mark_stack_top) + sizeof(mse));
    GC_VERBOSE_LOG_PRINTF("Starting mark helper %d\n", id);
    GC_release_mark_lock();
    for (;;) {
        size_t n_on_stack;
        unsigned n_to_get;
        mse * my_top;
        mse * local_top;
        mse * global_first_nonempty = (mse *)AO_load(&GC_first_nonempty);
        GC_ASSERT((word)my_first_nonempty >= (word)GC_mark_stack &&
                  (word)my_first_nonempty <=
                        (word)AO_load((volatile AO_t *)&GC_mark_stack_top)
                        + sizeof(mse));
        GC_ASSERT((word)global_first_nonempty >= (word)GC_mark_stack);
        if ((word)my_first_nonempty < (word)global_first_nonempty) {
            my_first_nonempty = global_first_nonempty;
        } else if ((word)global_first_nonempty < (word)my_first_nonempty) {
            (void)AO_compare_and_swap(&GC_first_nonempty,
                                      (AO_t)global_first_nonempty,
                                      (AO_t)my_first_nonempty);
        }
        my_top = (mse *)AO_load_acquire((volatile AO_t *)(&GC_mark_stack_top));
        if ((word)my_top < (word)my_first_nonempty) {
            GC_acquire_mark_lock();
            my_top = GC_mark_stack_top;
            n_on_stack = my_top - my_first_nonempty + 1;
            if (0 == n_on_stack) {
                GC_active_count--;
                GC_ASSERT(GC_active_count <= GC_helper_count);
                if (0 == GC_active_count) GC_notify_all_marker();
                while (GC_active_count > 0
                       && (word)AO_load(&GC_first_nonempty)
                                > (word)GC_mark_stack_top) {
                    GC_wait_marker();
                }
                if (GC_active_count == 0
                    && (word)AO_load(&GC_first_nonempty)
                        > (word)GC_mark_stack_top) {
                    GC_bool need_to_notify = FALSE;
                    GC_helper_count--;
                    if (0 == GC_helper_count) need_to_notify = TRUE;
                    GC_VERBOSE_LOG_PRINTF("Finished mark helper %d\n", id);
                    if (need_to_notify) GC_notify_all_marker();
                    return;
                }
                GC_active_count++;
                GC_ASSERT(GC_active_count > 0);
                GC_release_mark_lock();
                continue;
            } else {
                GC_release_mark_lock();
            }
        } else {
            n_on_stack = my_top - my_first_nonempty + 1;
        }
        n_to_get = ENTRIES_TO_GET;
        if (n_on_stack < 2 * ENTRIES_TO_GET) n_to_get = 1;
        local_top = GC_steal_mark_stack(my_first_nonempty, my_top,
                                        local_mark_stack, n_to_get,
                                        &my_first_nonempty);
        GC_ASSERT((word)my_first_nonempty >= (word)GC_mark_stack &&
                  (word)my_first_nonempty <=
                        (word)AO_load((volatile AO_t *)&GC_mark_stack_top)
                        + sizeof(mse));
        GC_do_local_mark(local_mark_stack, local_top);
    }
}
STATIC void GC_do_parallel_mark(void)
{
    GC_acquire_mark_lock();
    GC_ASSERT(I_HOLD_LOCK());
    if (GC_help_wanted || GC_active_count != 0 || GC_helper_count != 0)
        ABORT("Tried to start parallel mark in bad state");
    GC_VERBOSE_LOG_PRINTF("Starting marking for mark phase number %lu\n",
                          (unsigned long)GC_mark_no);
    GC_first_nonempty = (AO_t)GC_mark_stack;
    GC_active_count = 0;
    GC_helper_count = 1;
    GC_help_wanted = TRUE;
    GC_notify_all_marker();
    GC_mark_local(GC_main_local_mark_stack, 0);
    GC_help_wanted = FALSE;
    while (GC_helper_count > 0) {
      GC_wait_marker();
    }
    GC_VERBOSE_LOG_PRINTF("Finished marking for mark phase number %lu\n",
                          (unsigned long)GC_mark_no);
    GC_mark_no++;
    GC_release_mark_lock();
    GC_notify_all_marker();
}
GC_INNER void GC_help_marker(word my_mark_no)
{
#define my_id my_id_mse.mse_descr.w
    mse my_id_mse;
    mse local_mark_stack[LOCAL_MARK_STACK_SIZE];
    GC_ASSERT(GC_parallel);
    while (GC_mark_no < my_mark_no
           || (!GC_help_wanted && GC_mark_no == my_mark_no)) {
      GC_wait_marker();
    }
    my_id = GC_helper_count;
    if (GC_mark_no != my_mark_no || my_id > (unsigned)GC_markers_m1) {
      return;
    }
    GC_helper_count = (unsigned)my_id + 1;
    GC_mark_local(local_mark_stack, (int)my_id);
#undef my_id
}
#endif
static void alloc_mark_stack(size_t n)
{
    mse * new_stack = (mse *)GC_scratch_alloc(n * sizeof(struct GC_ms_entry));
#ifdef GWW_VDB
      static GC_bool GC_incremental_at_stack_alloc = FALSE;
      GC_bool recycle_old = !GC_auto_incremental
                            || GC_incremental_at_stack_alloc;
      GC_incremental_at_stack_alloc = GC_auto_incremental;
#else
#define recycle_old TRUE
#endif
    GC_mark_stack_too_small = FALSE;
    if (GC_mark_stack != NULL) {
        if (new_stack != 0) {
          if (recycle_old) {
            GC_scratch_recycle_inner(GC_mark_stack,
                        GC_mark_stack_size * sizeof(struct GC_ms_entry));
          }
          GC_mark_stack = new_stack;
          GC_mark_stack_size = n;
          GC_mark_stack_limit = new_stack + n;
          GC_COND_LOG_PRINTF("Grew mark stack to %lu frames\n",
                             (unsigned long)GC_mark_stack_size);
        } else {
          WARN("Failed to grow mark stack to %" WARN_PRIdPTR " frames\n", n);
        }
    } else if (NULL == new_stack) {
        GC_err_printf("No space for mark stack\n");
        EXIT();
    } else {
        GC_mark_stack = new_stack;
        GC_mark_stack_size = n;
        GC_mark_stack_limit = new_stack + n;
    }
    GC_mark_stack_top = GC_mark_stack-1;
}
GC_INNER void GC_mark_init(void)
{
    alloc_mark_stack(INITIAL_MARK_STACK_SIZE);
}
GC_API void GC_CALL GC_push_all(void *bottom, void *top)
{
    word length;
    bottom = (void *)(((word)bottom + ALIGNMENT-1) & ~(ALIGNMENT-1));
    top = (void *)((word)top & ~(ALIGNMENT-1));
    if ((word)bottom >= (word)top) return;
    GC_mark_stack_top++;
    if ((word)GC_mark_stack_top >= (word)GC_mark_stack_limit) {
        ABORT("Unexpected mark stack overflow");
    }
    length = (word)top - (word)bottom;
#if GC_DS_TAGS > ALIGNMENT - 1
        length += GC_DS_TAGS;
        length &= ~GC_DS_TAGS;
#endif
    GC_mark_stack_top -> mse_start = (ptr_t)bottom;
    GC_mark_stack_top -> mse_descr.w = length;
}
#ifndef GC_DISABLE_INCREMENTAL
  STATIC void GC_push_selected(ptr_t bottom, ptr_t top,
                               GC_bool (*dirty_fn)(struct hblk *))
  {
    struct hblk * h;
    bottom = (ptr_t)(((word) bottom + ALIGNMENT-1) & ~(ALIGNMENT-1));
    top = (ptr_t)(((word) top) & ~(ALIGNMENT-1));
    if ((word)bottom >= (word)top) return;
    h = HBLKPTR(bottom + HBLKSIZE);
    if ((word)top <= (word)h) {
        if ((*dirty_fn)(h-1)) {
            GC_push_all(bottom, top);
        }
        return;
    }
    if ((*dirty_fn)(h-1)) {
        if ((word)(GC_mark_stack_top - GC_mark_stack)
            > 3 * GC_mark_stack_size / 4) {
            GC_push_all(bottom, top);
            return;
        }
        GC_push_all(bottom, h);
    }
    while ((word)(h+1) <= (word)top) {
        if ((*dirty_fn)(h)) {
            if ((word)(GC_mark_stack_top - GC_mark_stack)
                > 3 * GC_mark_stack_size / 4) {
                GC_push_all(h, top);
                return;
            } else {
                GC_push_all(h, h + 1);
            }
        }
        h++;
    }
    if ((ptr_t)h != top && (*dirty_fn)(h)) {
       GC_push_all(h, top);
    }
  }
  GC_API void GC_CALL GC_push_conditional(void *bottom, void *top, int all)
  {
    if (!all) {
      GC_push_selected((ptr_t)bottom, (ptr_t)top, GC_page_was_dirty);
    } else {
#ifdef PROC_VDB
        if (GC_auto_incremental) {
          GC_push_selected((ptr_t)bottom, (ptr_t)top, GC_page_was_ever_dirty);
        } else
#endif
       {
        GC_push_all(bottom, top);
      }
    }
  }
#ifndef NO_VDB_FOR_STATIC_ROOTS
#ifndef PROC_VDB
      STATIC GC_bool GC_static_page_was_dirty(struct hblk *h)
      {
        return get_pht_entry_from_index(GC_grungy_pages, PHT_HASH(h));
      }
#endif
    GC_INNER void GC_push_conditional_static(void *bottom, void *top,
                                             GC_bool all)
    {
#ifdef PROC_VDB
        GC_push_conditional(bottom, top, all);
#else
        if (all || !GC_is_vdb_for_static_roots()) {
          GC_push_all(bottom, top);
        } else {
          GC_push_selected((ptr_t)bottom, (ptr_t)top,
                           GC_static_page_was_dirty);
        }
#endif
    }
#endif
#else
  GC_API void GC_CALL GC_push_conditional(void *bottom, void *top,
                                          int all GC_ATTR_UNUSED)
  {
    GC_push_all(bottom, top);
  }
#endif
#if defined(AMIGA) || defined(MACOS) || defined(GC_DARWIN_THREADS)
  void GC_push_one(word p)
  {
    GC_PUSH_ONE_STACK(p, MARKED_FROM_REGISTER);
  }
#endif
#ifdef GC_WIN32_THREADS
  GC_INNER void GC_push_many_regs(const word *regs, unsigned count)
  {
    unsigned i;
    for (i = 0; i < count; i++)
      GC_PUSH_ONE_STACK(regs[i], MARKED_FROM_REGISTER);
  }
#endif
GC_API struct GC_ms_entry * GC_CALL GC_mark_and_push(void *obj,
                                                mse *mark_stack_ptr,
                                                mse *mark_stack_limit,
                                                void ** src GC_ATTR_UNUSED)
{
    hdr * hhdr;
    PREFETCH(obj);
    GET_HDR(obj, hhdr);
    if ((EXPECT(IS_FORWARDING_ADDR_OR_NIL(hhdr), FALSE)
         && (!GC_all_interior_pointers
             || NULL == (hhdr = GC_find_header((ptr_t)GC_base(obj)))))
        || EXPECT(HBLK_IS_FREE(hhdr), FALSE)) {
      GC_ADD_TO_BLACK_LIST_NORMAL(obj, (ptr_t)src);
      return mark_stack_ptr;
    }
    return GC_push_contents_hdr((ptr_t)obj, mark_stack_ptr, mark_stack_limit,
                                (ptr_t)src, hhdr, TRUE);
}
#if defined(PRINT_BLACK_LIST) || defined(KEEP_BACK_PTRS)
    GC_INNER void GC_mark_and_push_stack(ptr_t p, ptr_t source)
#else
    GC_INNER void GC_mark_and_push_stack(ptr_t p)
#define source ((ptr_t)0)
#endif
{
    hdr * hhdr;
    ptr_t r = p;
    PREFETCH(p);
    GET_HDR(p, hhdr);
    if (EXPECT(IS_FORWARDING_ADDR_OR_NIL(hhdr), FALSE)) {
      if (NULL == hhdr
            || (r = (ptr_t)GC_base(p)) == NULL
            || (hhdr = HDR(r)) == NULL) {
        GC_ADD_TO_BLACK_LIST_STACK(p, source);
        return;
      }
    }
    if (EXPECT(HBLK_IS_FREE(hhdr), FALSE)) {
        GC_ADD_TO_BLACK_LIST_NORMAL(p, source);
        return;
    }
#ifdef THREADS
      GC_dirty(p);
#endif
    GC_mark_stack_top = GC_push_contents_hdr(r, GC_mark_stack_top,
                                             GC_mark_stack_limit,
                                             source, hhdr, FALSE);
}
#undef source
#ifdef TRACE_BUF
#ifndef TRACE_ENTRIES
#define TRACE_ENTRIES 1000
#endif
struct trace_entry {
    char * kind;
    word gc_no;
    word bytes_allocd;
    word arg1;
    word arg2;
} GC_trace_buf[TRACE_ENTRIES] = { { NULL, 0, 0, 0, 0 } };
void GC_add_trace_entry(char *kind, word arg1, word arg2)
{
    GC_trace_buf[GC_trace_buf_ptr].kind = kind;
    GC_trace_buf[GC_trace_buf_ptr].gc_no = GC_gc_no;
    GC_trace_buf[GC_trace_buf_ptr].bytes_allocd = GC_bytes_allocd;
    GC_trace_buf[GC_trace_buf_ptr].arg1 = arg1 ^ 0x80000000;
    GC_trace_buf[GC_trace_buf_ptr].arg2 = arg2 ^ 0x80000000;
    GC_trace_buf_ptr++;
    if (GC_trace_buf_ptr >= TRACE_ENTRIES) GC_trace_buf_ptr = 0;
}
GC_API void GC_CALL GC_print_trace_inner(word gc_no)
{
    int i;
    for (i = GC_trace_buf_ptr-1; i != GC_trace_buf_ptr; i--) {
        struct trace_entry *p;
        if (i < 0) i = TRACE_ENTRIES-1;
        p = GC_trace_buf + i;
        if (p -> gc_no < gc_no || p -> kind == 0) {
            return;
        }
        GC_printf("Trace:%s (gc:%u, bytes:%lu) 0x%lX, 0x%lX\n",
                  p -> kind, (unsigned)p -> gc_no,
                  (unsigned long)p -> bytes_allocd,
                  (long)p->arg1 ^ 0x80000000L, (long)p->arg2 ^ 0x80000000L);
    }
    GC_printf("Trace incomplete\n");
}
GC_API void GC_CALL GC_print_trace(word gc_no)
{
    DCL_LOCK_STATE;
    LOCK();
    GC_print_trace_inner(gc_no);
    UNLOCK();
}
#endif
GC_ATTR_NO_SANITIZE_ADDR GC_ATTR_NO_SANITIZE_MEMORY GC_ATTR_NO_SANITIZE_THREAD
GC_API void GC_CALL GC_push_all_eager(void *bottom, void *top)
{
    word * b = (word *)(((word) bottom + ALIGNMENT-1) & ~(ALIGNMENT-1));
    word * t = (word *)(((word) top) & ~(ALIGNMENT-1));
    REGISTER word *p;
    REGISTER word *lim;
    REGISTER ptr_t greatest_ha = (ptr_t)GC_greatest_plausible_heap_addr;
    REGISTER ptr_t least_ha = (ptr_t)GC_least_plausible_heap_addr;
#define GC_greatest_plausible_heap_addr greatest_ha
#define GC_least_plausible_heap_addr least_ha
    if (top == 0) return;
      lim = t - 1 ;
      for (p = b; (word)p <= (word)lim;
           p = (word *)(((ptr_t)p) + ALIGNMENT)) {
        REGISTER word q = *p;
        GC_PUSH_ONE_STACK(q, p);
      }
#undef GC_greatest_plausible_heap_addr
#undef GC_least_plausible_heap_addr
}
GC_INNER void GC_push_all_stack(ptr_t bottom, ptr_t top)
{
#ifndef NEED_FIXUP_POINTER
      if (GC_all_interior_pointers
#if defined(THREADS) && defined(MPROTECT_VDB)
            && !GC_auto_incremental
#endif
          && (word)GC_mark_stack_top
             < (word)(GC_mark_stack_limit - INITIAL_MARK_STACK_SIZE/8)) {
        GC_push_all(bottom, top);
      } else
#endif
     {
      GC_push_all_eager(bottom, top);
    }
}
#if defined(WRAP_MARK_SOME) && defined(PARALLEL_MARK)
  GC_ATTR_NO_SANITIZE_ADDR GC_ATTR_NO_SANITIZE_MEMORY
  GC_ATTR_NO_SANITIZE_THREAD
  GC_INNER void GC_push_conditional_eager(void *bottom, void *top,
                                          GC_bool all)
  {
    word * b = (word *)(((word) bottom + ALIGNMENT-1) & ~(ALIGNMENT-1));
    word * t = (word *)(((word) top) & ~(ALIGNMENT-1));
    REGISTER word *p;
    REGISTER word *lim;
    REGISTER ptr_t greatest_ha = (ptr_t)GC_greatest_plausible_heap_addr;
    REGISTER ptr_t least_ha = (ptr_t)GC_least_plausible_heap_addr;
#define GC_greatest_plausible_heap_addr greatest_ha
#define GC_least_plausible_heap_addr least_ha
    if (top == NULL)
      return;
    (void)all;
    lim = t - 1;
    for (p = b; (word)p <= (word)lim; p = (word *)((ptr_t)p + ALIGNMENT)) {
      REGISTER word q = *p;
      GC_PUSH_ONE_HEAP(q, p, GC_mark_stack_top);
    }
#undef GC_greatest_plausible_heap_addr
#undef GC_least_plausible_heap_addr
  }
#endif
#if !defined(SMALL_CONFIG) && !defined(USE_MARK_BYTES) && \
    defined(MARK_BIT_PER_GRANULE)
#if GC_GRANULE_WORDS == 1
#define USE_PUSH_MARKED_ACCELERATORS
#define PUSH_GRANULE(q) \
                do { \
                  word qcontents = (q)[0]; \
                  GC_PUSH_ONE_HEAP(qcontents, q, GC_mark_stack_top); \
                } while (0)
#elif GC_GRANULE_WORDS == 2
#define USE_PUSH_MARKED_ACCELERATORS
#define PUSH_GRANULE(q) \
                do { \
                  word qcontents = (q)[0]; \
                  GC_PUSH_ONE_HEAP(qcontents, q, GC_mark_stack_top); \
                  qcontents = (q)[1]; \
                  GC_PUSH_ONE_HEAP(qcontents, (q)+1, GC_mark_stack_top); \
                } while (0)
#elif GC_GRANULE_WORDS == 4
#define USE_PUSH_MARKED_ACCELERATORS
#define PUSH_GRANULE(q) \
                do { \
                  word qcontents = (q)[0]; \
                  GC_PUSH_ONE_HEAP(qcontents, q, GC_mark_stack_top); \
                  qcontents = (q)[1]; \
                  GC_PUSH_ONE_HEAP(qcontents, (q)+1, GC_mark_stack_top); \
                  qcontents = (q)[2]; \
                  GC_PUSH_ONE_HEAP(qcontents, (q)+2, GC_mark_stack_top); \
                  qcontents = (q)[3]; \
                  GC_PUSH_ONE_HEAP(qcontents, (q)+3, GC_mark_stack_top); \
                } while (0)
#endif
#endif
#ifdef USE_PUSH_MARKED_ACCELERATORS
STATIC void GC_push_marked1(struct hblk *h, hdr *hhdr)
{
    word * mark_word_addr = &(hhdr->hb_marks[0]);
    word *p;
    word *plim;
    ptr_t greatest_ha = (ptr_t)GC_greatest_plausible_heap_addr;
    ptr_t least_ha = (ptr_t)GC_least_plausible_heap_addr;
    mse * mark_stack_top = GC_mark_stack_top;
    mse * mark_stack_limit = GC_mark_stack_limit;
#undef GC_mark_stack_top
#undef GC_mark_stack_limit
#define GC_mark_stack_top mark_stack_top
#define GC_mark_stack_limit mark_stack_limit
#define GC_greatest_plausible_heap_addr greatest_ha
#define GC_least_plausible_heap_addr least_ha
    p = (word *)(h->hb_body);
    plim = (word *)(((word)h) + HBLKSIZE);
        while ((word)p < (word)plim) {
            word mark_word = *mark_word_addr++;
            word *q = p;
            while(mark_word != 0) {
              if (mark_word & 1) {
                  PUSH_GRANULE(q);
              }
              q += GC_GRANULE_WORDS;
              mark_word >>= 1;
            }
            p += WORDSZ*GC_GRANULE_WORDS;
        }
#undef GC_greatest_plausible_heap_addr
#undef GC_least_plausible_heap_addr
#undef GC_mark_stack_top
#undef GC_mark_stack_limit
#define GC_mark_stack_limit GC_arrays._mark_stack_limit
#define GC_mark_stack_top GC_arrays._mark_stack_top
    GC_mark_stack_top = mark_stack_top;
}
#ifndef UNALIGNED_PTRS
STATIC void GC_push_marked2(struct hblk *h, hdr *hhdr)
{
    word * mark_word_addr = &(hhdr->hb_marks[0]);
    word *p;
    word *plim;
    ptr_t greatest_ha = (ptr_t)GC_greatest_plausible_heap_addr;
    ptr_t least_ha = (ptr_t)GC_least_plausible_heap_addr;
    mse * mark_stack_top = GC_mark_stack_top;
    mse * mark_stack_limit = GC_mark_stack_limit;
#undef GC_mark_stack_top
#undef GC_mark_stack_limit
#define GC_mark_stack_top mark_stack_top
#define GC_mark_stack_limit mark_stack_limit
#define GC_greatest_plausible_heap_addr greatest_ha
#define GC_least_plausible_heap_addr least_ha
    p = (word *)(h->hb_body);
    plim = (word *)(((word)h) + HBLKSIZE);
        while ((word)p < (word)plim) {
            word mark_word = *mark_word_addr++;
            word *q = p;
            while(mark_word != 0) {
              if (mark_word & 1) {
                  PUSH_GRANULE(q);
                  PUSH_GRANULE(q + GC_GRANULE_WORDS);
              }
              q += 2 * GC_GRANULE_WORDS;
              mark_word >>= 2;
            }
            p += WORDSZ*GC_GRANULE_WORDS;
        }
#undef GC_greatest_plausible_heap_addr
#undef GC_least_plausible_heap_addr
#undef GC_mark_stack_top
#undef GC_mark_stack_limit
#define GC_mark_stack_limit GC_arrays._mark_stack_limit
#define GC_mark_stack_top GC_arrays._mark_stack_top
    GC_mark_stack_top = mark_stack_top;
}
#if GC_GRANULE_WORDS < 4
STATIC void GC_push_marked4(struct hblk *h, hdr *hhdr)
{
    word * mark_word_addr = &(hhdr->hb_marks[0]);
    word *p;
    word *plim;
    ptr_t greatest_ha = (ptr_t)GC_greatest_plausible_heap_addr;
    ptr_t least_ha = (ptr_t)GC_least_plausible_heap_addr;
    mse * mark_stack_top = GC_mark_stack_top;
    mse * mark_stack_limit = GC_mark_stack_limit;
#undef GC_mark_stack_top
#undef GC_mark_stack_limit
#define GC_mark_stack_top mark_stack_top
#define GC_mark_stack_limit mark_stack_limit
#define GC_greatest_plausible_heap_addr greatest_ha
#define GC_least_plausible_heap_addr least_ha
    p = (word *)(h->hb_body);
    plim = (word *)(((word)h) + HBLKSIZE);
        while ((word)p < (word)plim) {
            word mark_word = *mark_word_addr++;
            word *q = p;
            while(mark_word != 0) {
              if (mark_word & 1) {
                  PUSH_GRANULE(q);
                  PUSH_GRANULE(q + GC_GRANULE_WORDS);
                  PUSH_GRANULE(q + 2*GC_GRANULE_WORDS);
                  PUSH_GRANULE(q + 3*GC_GRANULE_WORDS);
              }
              q += 4 * GC_GRANULE_WORDS;
              mark_word >>= 4;
            }
            p += WORDSZ*GC_GRANULE_WORDS;
        }
#undef GC_greatest_plausible_heap_addr
#undef GC_least_plausible_heap_addr
#undef GC_mark_stack_top
#undef GC_mark_stack_limit
#define GC_mark_stack_limit GC_arrays._mark_stack_limit
#define GC_mark_stack_top GC_arrays._mark_stack_top
    GC_mark_stack_top = mark_stack_top;
}
#endif
#endif
#endif
STATIC void GC_push_marked(struct hblk *h, hdr *hhdr)
{
    word sz = hhdr -> hb_sz;
    word descr = hhdr -> hb_descr;
    ptr_t p;
    word bit_no;
    ptr_t lim;
    mse * GC_mark_stack_top_reg;
    mse * mark_stack_limit = GC_mark_stack_limit;
        if (( GC_DS_LENGTH) == descr) return;
        if (GC_block_empty(hhdr)) return;
#if !defined(GC_DISABLE_INCREMENTAL)
      GC_n_rescuing_pages++;
#endif
    GC_objects_are_marked = TRUE;
    if (sz > MAXOBJBYTES) {
        lim = h -> hb_body;
    } else {
        lim = (ptr_t)((word)(h + 1)->hb_body - sz);
    }
    switch(BYTES_TO_GRANULES(sz)) {
#if defined(USE_PUSH_MARKED_ACCELERATORS)
     case 1:
       GC_push_marked1(h, hhdr);
       break;
#if !defined(UNALIGNED_PTRS)
       case 2:
         GC_push_marked2(h, hhdr);
         break;
#if GC_GRANULE_WORDS < 4
       case 4:
         GC_push_marked4(h, hhdr);
         break;
#endif
#endif
#else
     case 1:
#endif
     default:
      GC_mark_stack_top_reg = GC_mark_stack_top;
      for (p = h -> hb_body, bit_no = 0; (word)p <= (word)lim;
           p += sz, bit_no += MARK_BIT_OFFSET(sz)) {
        if (mark_bit_from_hdr(hhdr, bit_no)) {
          GC_mark_stack_top_reg = GC_push_obj(p, hhdr, GC_mark_stack_top_reg,
                                              mark_stack_limit);
        }
      }
      GC_mark_stack_top = GC_mark_stack_top_reg;
    }
}
#ifdef ENABLE_DISCLAIM
 STATIC void GC_push_unconditionally(struct hblk *h, hdr *hhdr)
 {
    word sz = hhdr -> hb_sz;
    word descr = hhdr -> hb_descr;
    ptr_t p;
    ptr_t lim;
    mse * GC_mark_stack_top_reg;
    mse * mark_stack_limit = GC_mark_stack_limit;
    if (( GC_DS_LENGTH) == descr)
        return;
#if !defined(GC_DISABLE_INCREMENTAL)
      GC_n_rescuing_pages++;
#endif
    GC_objects_are_marked = TRUE;
    if (sz > MAXOBJBYTES)
        lim = h -> hb_body;
    else
        lim = (ptr_t)((word)(h + 1)->hb_body - sz);
    GC_mark_stack_top_reg = GC_mark_stack_top;
    for (p = h -> hb_body; (word)p <= (word)lim; p += sz)
      if ((*(word *)p & 0x3) != 0)
        GC_mark_stack_top_reg = GC_push_obj(p, hhdr, GC_mark_stack_top_reg,
                                            mark_stack_limit);
    GC_mark_stack_top = GC_mark_stack_top_reg;
  }
#endif
#ifndef GC_DISABLE_INCREMENTAL
  STATIC GC_bool GC_block_was_dirty(struct hblk *h, hdr *hhdr)
  {
    word sz = hhdr -> hb_sz;
    if (sz <= MAXOBJBYTES) {
         return(GC_page_was_dirty(h));
    } else {
         ptr_t p = (ptr_t)h;
         while ((word)p < (word)h + sz) {
             if (GC_page_was_dirty((struct hblk *)p)) return(TRUE);
             p += HBLKSIZE;
         }
         return(FALSE);
    }
  }
#endif
STATIC struct hblk * GC_push_next_marked(struct hblk *h)
{
    hdr * hhdr = HDR(h);
    if (EXPECT(IS_FORWARDING_ADDR_OR_NIL(hhdr) || HBLK_IS_FREE(hhdr), FALSE)) {
      h = GC_next_block(h, FALSE);
      if (NULL == h) return NULL;
      hhdr = GC_find_header((ptr_t)h);
    } else {
#ifdef LINT2
        if (NULL == h) ABORT("Bad HDR() definition");
#endif
    }
    GC_push_marked(h, hhdr);
    return(h + OBJ_SZ_TO_BLOCKS(hhdr -> hb_sz));
}
#ifndef GC_DISABLE_INCREMENTAL
  STATIC struct hblk * GC_push_next_marked_dirty(struct hblk *h)
  {
    hdr * hhdr = HDR(h);
    if (!GC_incremental) ABORT("Dirty bits not set up");
    for (;;) {
        if (EXPECT(IS_FORWARDING_ADDR_OR_NIL(hhdr)
                   || HBLK_IS_FREE(hhdr), FALSE)) {
          h = GC_next_block(h, FALSE);
          if (NULL == h) return NULL;
          hhdr = GC_find_header((ptr_t)h);
        } else {
#ifdef LINT2
            if (NULL == h) ABORT("Bad HDR() definition");
#endif
        }
        if (GC_block_was_dirty(h, hhdr))
          break;
        h += OBJ_SZ_TO_BLOCKS(hhdr -> hb_sz);
        hhdr = HDR(h);
    }
#ifdef ENABLE_DISCLAIM
      if ((hhdr -> hb_flags & MARK_UNCONDITIONALLY) != 0) {
        GC_push_unconditionally(h, hhdr);
      } else
#endif
     {
      GC_push_marked(h, hhdr);
    }
    return(h + OBJ_SZ_TO_BLOCKS(hhdr -> hb_sz));
  }
#endif
STATIC struct hblk * GC_push_next_marked_uncollectable(struct hblk *h)
{
    hdr * hhdr = HDR(h);
    for (;;) {
        if (EXPECT(IS_FORWARDING_ADDR_OR_NIL(hhdr)
                   || HBLK_IS_FREE(hhdr), FALSE)) {
          h = GC_next_block(h, FALSE);
          if (NULL == h) return NULL;
          hhdr = GC_find_header((ptr_t)h);
        } else {
#ifdef LINT2
            if (NULL == h) ABORT("Bad HDR() definition");
#endif
        }
        if (hhdr -> hb_obj_kind == UNCOLLECTABLE) {
            GC_push_marked(h, hhdr);
            break;
        }
#ifdef ENABLE_DISCLAIM
            if ((hhdr -> hb_flags & MARK_UNCONDITIONALLY) != 0) {
                GC_push_unconditionally(h, hhdr);
                break;
            }
#endif
        h += OBJ_SZ_TO_BLOCKS(hhdr -> hb_sz);
        hhdr = HDR(h);
    }
    return(h + OBJ_SZ_TO_BLOCKS(hhdr -> hb_sz));
}
#include <stdio.h>
int GC_no_dls = 0;
#if !defined(NO_DEBUGGING) || defined(GC_ASSERTIONS)
  GC_INNER word GC_compute_root_size(void)
  {
    int i;
    word size = 0;
    for (i = 0; i < n_root_sets; i++) {
      size += GC_static_roots[i].r_end - GC_static_roots[i].r_start;
    }
    return size;
  }
#endif
#if !defined(NO_DEBUGGING)
  void GC_print_static_roots(void)
  {
    int i;
    word size;
    for (i = 0; i < n_root_sets; i++) {
        GC_printf("From %p to %p%s\n",
                  (void *)GC_static_roots[i].r_start,
                  (void *)GC_static_roots[i].r_end,
                  GC_static_roots[i].r_tmp ? " (temporary)" : "");
    }
    GC_printf("GC_root_size= %lu\n", (unsigned long)GC_root_size);
    if ((size = GC_compute_root_size()) != GC_root_size)
      GC_err_printf("GC_root_size incorrect!! Should be: %lu\n",
                    (unsigned long)size);
  }
#endif
#ifndef THREADS
  GC_INNER GC_bool GC_is_static_root(void *p)
  {
    static int last_root_set = MAX_ROOT_SETS;
    int i;
    if (last_root_set < n_root_sets
        && (word)p >= (word)GC_static_roots[last_root_set].r_start
        && (word)p < (word)GC_static_roots[last_root_set].r_end)
      return(TRUE);
    for (i = 0; i < n_root_sets; i++) {
        if ((word)p >= (word)GC_static_roots[i].r_start
            && (word)p < (word)GC_static_roots[i].r_end) {
          last_root_set = i;
          return(TRUE);
        }
    }
    return(FALSE);
  }
#endif
#if !defined(MSWIN32) && !defined(MSWINCE) && !defined(CYGWIN32)
  GC_INLINE int rt_hash(ptr_t addr)
  {
    word result = (word) addr;
#if CPP_WORDSZ > 8*LOG_RT_SIZE
        result ^= result >> 8*LOG_RT_SIZE;
#endif
#if CPP_WORDSZ > 4*LOG_RT_SIZE
        result ^= result >> 4*LOG_RT_SIZE;
#endif
    result ^= result >> 2*LOG_RT_SIZE;
    result ^= result >> LOG_RT_SIZE;
    result &= (RT_SIZE-1);
    return(result);
  }
  GC_INNER void * GC_roots_present(ptr_t b)
  {
    int h = rt_hash(b);
    struct roots *p = GC_root_index[h];
    while (p != 0) {
        if (p -> r_start == (ptr_t)b) return(p);
        p = p -> r_next;
    }
    return NULL;
  }
  GC_INLINE void add_roots_to_index(struct roots *p)
  {
    int h = rt_hash(p -> r_start);
    p -> r_next = GC_root_index[h];
    GC_root_index[h] = p;
  }
#endif
GC_INNER word GC_root_size = 0;
GC_API void GC_CALL GC_add_roots(void *b, void *e)
{
    DCL_LOCK_STATE;
    if (!EXPECT(GC_is_initialized, TRUE)) GC_init();
    LOCK();
    GC_add_roots_inner((ptr_t)b, (ptr_t)e, FALSE);
    UNLOCK();
}
void GC_add_roots_inner(ptr_t b, ptr_t e, GC_bool tmp)
{
    GC_ASSERT((word)b <= (word)e);
    b = (ptr_t)(((word)b + (sizeof(word) - 1)) & ~(word)(sizeof(word) - 1));
    e = (ptr_t)((word)e & ~(word)(sizeof(word) - 1));
    if ((word)b >= (word)e) return;
#if defined(MSWIN32) || defined(MSWINCE) || defined(CYGWIN32)
      {
        int i;
        struct roots * old = NULL;
        for (i = 0; i < n_root_sets; i++) {
            old = GC_static_roots + i;
            if ((word)b <= (word)old->r_end
                 && (word)e >= (word)old->r_start) {
                if ((word)b < (word)old->r_start) {
                    GC_root_size += old->r_start - b;
                    old -> r_start = b;
                }
                if ((word)e > (word)old->r_end) {
                    GC_root_size += e - old->r_end;
                    old -> r_end = e;
                }
                old -> r_tmp &= tmp;
                break;
            }
        }
        if (i < n_root_sets) {
            struct roots *other;
            for (i++; i < n_root_sets; i++) {
              other = GC_static_roots + i;
              b = other -> r_start;
              e = other -> r_end;
              if ((word)b <= (word)old->r_end
                  && (word)e >= (word)old->r_start) {
                if ((word)b < (word)old->r_start) {
                    GC_root_size += old->r_start - b;
                    old -> r_start = b;
                }
                if ((word)e > (word)old->r_end) {
                    GC_root_size += e - old->r_end;
                    old -> r_end = e;
                }
                old -> r_tmp &= other -> r_tmp;
                  GC_root_size -= (other -> r_end - other -> r_start);
                  other -> r_start = GC_static_roots[n_root_sets-1].r_start;
                  other -> r_end = GC_static_roots[n_root_sets-1].r_end;
                  n_root_sets--;
              }
            }
          return;
        }
      }
#else
      {
        struct roots * old = (struct roots *)GC_roots_present(b);
        if (old != 0) {
          if ((word)e <= (word)old->r_end) {
            old -> r_tmp &= tmp;
            return;
          }
          if (old -> r_tmp == tmp || !tmp) {
            GC_root_size += e - old -> r_end;
            old -> r_end = e;
            old -> r_tmp = tmp;
            return;
          }
          b = old -> r_end;
        }
      }
#endif
    if (n_root_sets == MAX_ROOT_SETS) {
        ABORT("Too many root sets");
    }
#ifdef DEBUG_ADD_DEL_ROOTS
      GC_log_printf("Adding data root section %d: %p .. %p%s\n",
                    n_root_sets, (void *)b, (void *)e,
                    tmp ? " (temporary)" : "");
#endif
    GC_static_roots[n_root_sets].r_start = (ptr_t)b;
    GC_static_roots[n_root_sets].r_end = (ptr_t)e;
    GC_static_roots[n_root_sets].r_tmp = tmp;
#if !defined(MSWIN32) && !defined(MSWINCE) && !defined(CYGWIN32)
      GC_static_roots[n_root_sets].r_next = 0;
      add_roots_to_index(GC_static_roots + n_root_sets);
#endif
    GC_root_size += e - b;
    n_root_sets++;
}
GC_API void GC_CALL GC_clear_roots(void)
{
    DCL_LOCK_STATE;
    if (!EXPECT(GC_is_initialized, TRUE)) GC_init();
    LOCK();
#ifdef THREADS
      GC_roots_were_cleared = TRUE;
#endif
    n_root_sets = 0;
    GC_root_size = 0;
#if !defined(MSWIN32) && !defined(MSWINCE) && !defined(CYGWIN32)
      BZERO(GC_root_index, RT_SIZE * sizeof(void *));
#endif
#ifdef DEBUG_ADD_DEL_ROOTS
      GC_log_printf("Clear all data root sections\n");
#endif
    UNLOCK();
}
STATIC void GC_remove_root_at_pos(int i)
{
#ifdef DEBUG_ADD_DEL_ROOTS
      GC_log_printf("Remove data root section at %d: %p .. %p%s\n",
                    i, (void *)GC_static_roots[i].r_start,
                    (void *)GC_static_roots[i].r_end,
                    GC_static_roots[i].r_tmp ? " (temporary)" : "");
#endif
    GC_root_size -= (GC_static_roots[i].r_end - GC_static_roots[i].r_start);
    GC_static_roots[i].r_start = GC_static_roots[n_root_sets-1].r_start;
    GC_static_roots[i].r_end = GC_static_roots[n_root_sets-1].r_end;
    GC_static_roots[i].r_tmp = GC_static_roots[n_root_sets-1].r_tmp;
    n_root_sets--;
}
#if !defined(MSWIN32) && !defined(MSWINCE) && !defined(CYGWIN32)
  STATIC void GC_rebuild_root_index(void)
  {
    int i;
    BZERO(GC_root_index, RT_SIZE * sizeof(void *));
    for (i = 0; i < n_root_sets; i++)
        add_roots_to_index(GC_static_roots + i);
  }
#endif
#if defined(DYNAMIC_LOADING) || defined(MSWIN32) || defined(MSWINCE) \
     || defined(PCR) || defined(CYGWIN32)
STATIC void GC_remove_tmp_roots(void)
{
    int i;
#if !defined(MSWIN32) && !defined(MSWINCE) && !defined(CYGWIN32)
      int old_n_roots = n_root_sets;
#endif
    for (i = 0; i < n_root_sets; ) {
        if (GC_static_roots[i].r_tmp) {
            GC_remove_root_at_pos(i);
        } else {
            i++;
        }
    }
#if !defined(MSWIN32) && !defined(MSWINCE) && !defined(CYGWIN32)
      if (n_root_sets < old_n_roots)
        GC_rebuild_root_index();
#endif
}
#endif
#if !defined(MSWIN32) && !defined(MSWINCE) && !defined(CYGWIN32)
  STATIC void GC_remove_roots_inner(ptr_t b, ptr_t e);
  GC_API void GC_CALL GC_remove_roots(void *b, void *e)
  {
    DCL_LOCK_STATE;
    if ((((word)b + (sizeof(word) - 1)) & ~(word)(sizeof(word) - 1)) >=
        ((word)e & ~(word)(sizeof(word) - 1)))
      return;
    LOCK();
    GC_remove_roots_inner((ptr_t)b, (ptr_t)e);
    UNLOCK();
  }
  STATIC void GC_remove_roots_inner(ptr_t b, ptr_t e)
  {
    int i;
    GC_bool rebuild = FALSE;
    for (i = 0; i < n_root_sets; ) {
        if ((word)GC_static_roots[i].r_start >= (word)b
            && (word)GC_static_roots[i].r_end <= (word)e) {
            GC_remove_root_at_pos(i);
            rebuild = TRUE;
        } else {
            i++;
        }
    }
    if (rebuild)
        GC_rebuild_root_index();
  }
#endif
#ifdef USE_PROC_FOR_LIBRARIES
  GC_INLINE void swap_static_roots(int i, int j)
  {
    ptr_t r_start = GC_static_roots[i].r_start;
    ptr_t r_end = GC_static_roots[i].r_end;
    GC_bool r_tmp = GC_static_roots[i].r_tmp;
    GC_static_roots[i].r_start = GC_static_roots[j].r_start;
    GC_static_roots[i].r_end = GC_static_roots[j].r_end;
    GC_static_roots[i].r_tmp = GC_static_roots[j].r_tmp;
    GC_static_roots[j].r_start = r_start;
    GC_static_roots[j].r_end = r_end;
    GC_static_roots[j].r_tmp = r_tmp;
  }
  GC_INNER void GC_remove_roots_subregion(ptr_t b, ptr_t e)
  {
    int i;
    GC_bool rebuild = FALSE;
    GC_ASSERT(I_HOLD_LOCK());
    GC_ASSERT((word)b % sizeof(word) == 0 && (word)e % sizeof(word) == 0);
    for (i = 0; i < n_root_sets; i++) {
      ptr_t r_start, r_end;
      if (GC_static_roots[i].r_tmp) {
#ifdef GC_ASSERTIONS
          int j;
          for (j = i + 1; j < n_root_sets; j++) {
            GC_ASSERT(GC_static_roots[j].r_tmp);
          }
#endif
        break;
      }
      r_start = GC_static_roots[i].r_start;
      r_end = GC_static_roots[i].r_end;
      if (!EXPECT((word)e <= (word)r_start || (word)r_end <= (word)b, TRUE)) {
#ifdef DEBUG_ADD_DEL_ROOTS
          GC_log_printf("Removing %p .. %p from root section %d (%p .. %p)\n",
                        (void *)b, (void *)e,
                        i, (void *)r_start, (void *)r_end);
#endif
        if ((word)r_start < (word)b) {
          GC_root_size -= r_end - b;
          GC_static_roots[i].r_end = b;
          if ((word)e < (word)r_end) {
            int j;
            if (rebuild) {
              GC_rebuild_root_index();
              rebuild = FALSE;
            }
            GC_add_roots_inner(e, r_end, FALSE);
            for (j = i + 1; j < n_root_sets; j++)
              if (GC_static_roots[j].r_tmp)
                break;
            if (j < n_root_sets-1 && !GC_static_roots[n_root_sets-1].r_tmp) {
              swap_static_roots(j, n_root_sets - 1);
              rebuild = TRUE;
            }
          }
        } else {
          if ((word)e < (word)r_end) {
            GC_root_size -= e - r_start;
            GC_static_roots[i].r_start = e;
          } else {
            GC_remove_root_at_pos(i);
            if (i < n_root_sets - 1 && GC_static_roots[i].r_tmp
                && !GC_static_roots[i + 1].r_tmp) {
              int j;
              for (j = i + 2; j < n_root_sets; j++)
                if (GC_static_roots[j].r_tmp)
                  break;
              swap_static_roots(i, j - 1);
            }
            i--;
          }
          rebuild = TRUE;
        }
      }
    }
    if (rebuild)
      GC_rebuild_root_index();
  }
#endif
#if !defined(NO_DEBUGGING)
  GC_API int GC_CALL GC_is_tmp_root(void *p)
  {
    static int last_root_set = MAX_ROOT_SETS;
    int i;
    if (last_root_set < n_root_sets
        && (word)p >= (word)GC_static_roots[last_root_set].r_start
        && (word)p < (word)GC_static_roots[last_root_set].r_end)
        return GC_static_roots[last_root_set].r_tmp;
    for (i = 0; i < n_root_sets; i++) {
        if ((word)p >= (word)GC_static_roots[i].r_start
            && (word)p < (word)GC_static_roots[i].r_end) {
            last_root_set = i;
            return GC_static_roots[i].r_tmp;
        }
    }
    return(FALSE);
  }
#endif
GC_INNER ptr_t GC_approx_sp(void)
{
    volatile word sp;
#if defined(S390) && !defined(CPPCHECK) && (__clang_major__ < 8)
        sp = (word)&sp;
#elif defined(CPPCHECK) || (__GNUC__ >= 4  \
                               && !defined(STACK_NOT_SCANNED))
        sp = (word)__builtin_frame_address(0);
#else
        sp = (word)&sp;
#endif
    return((ptr_t)sp);
}
GC_API void GC_CALL GC_clear_exclusion_table(void)
{
    GC_excl_table_entries = 0;
}
STATIC struct exclusion * GC_next_exclusion(ptr_t start_addr)
{
    size_t low = 0;
    size_t high;
    GC_ASSERT(GC_excl_table_entries > 0);
    high = GC_excl_table_entries - 1;
    while (high > low) {
        size_t mid = (low + high) >> 1;
        if ((word) GC_excl_table[mid].e_end <= (word) start_addr) {
            low = mid + 1;
        } else {
            high = mid;
        }
    }
    if ((word) GC_excl_table[low].e_end <= (word) start_addr) return 0;
    return GC_excl_table + low;
}
GC_INNER void GC_exclude_static_roots_inner(void *start, void *finish)
{
    struct exclusion * next;
    size_t next_index;
    GC_ASSERT((word)start % sizeof(word) == 0);
    GC_ASSERT((word)start < (word)finish);
    if (0 == GC_excl_table_entries) {
        next = 0;
    } else {
        next = GC_next_exclusion((ptr_t)start);
    }
    if (0 != next) {
      size_t i;
      if ((word)(next -> e_start) < (word) finish) {
        ABORT("Exclusion ranges overlap");
      }
      if ((word)(next -> e_start) == (word) finish) {
          next -> e_start = (ptr_t)start;
          return;
      }
      next_index = next - GC_excl_table;
      for (i = GC_excl_table_entries; i > next_index; --i) {
        GC_excl_table[i] = GC_excl_table[i-1];
      }
    } else {
      next_index = GC_excl_table_entries;
    }
    if (GC_excl_table_entries == MAX_EXCLUSIONS) ABORT("Too many exclusions");
    GC_excl_table[next_index].e_start = (ptr_t)start;
    GC_excl_table[next_index].e_end = (ptr_t)finish;
    ++GC_excl_table_entries;
}
GC_API void GC_CALL GC_exclude_static_roots(void *b, void *e)
{
    DCL_LOCK_STATE;
    if (b == e) return;
    b = (void *)((word)b & ~(word)(sizeof(word) - 1));
    e = (void *)(((word)e + (sizeof(word) - 1)) & ~(word)(sizeof(word) - 1));
    if (NULL == e)
      e = (void *)(~(word)(sizeof(word) - 1));
    LOCK();
    GC_exclude_static_roots_inner(b, e);
    UNLOCK();
}
#if defined(WRAP_MARK_SOME) && defined(PARALLEL_MARK)
#define GC_PUSH_CONDITIONAL(b, t, all) \
                (GC_parallel \
                    ? GC_push_conditional_eager(b, t, all) \
                    : GC_push_conditional_static(b, t, all))
#else
#define GC_PUSH_CONDITIONAL(b, t, all) GC_push_conditional_static(b, t, all)
#endif
STATIC void GC_push_conditional_with_exclusions(ptr_t bottom, ptr_t top,
                                                GC_bool all)
{
    while ((word)bottom < (word)top) {
        struct exclusion *next = GC_next_exclusion(bottom);
        ptr_t excl_start;
        if (0 == next
            || (word)(excl_start = next -> e_start) >= (word)top) {
          GC_PUSH_CONDITIONAL(bottom, top, all);
          break;
        }
        if ((word)excl_start > (word)bottom)
          GC_PUSH_CONDITIONAL(bottom, excl_start, all);
        bottom = next -> e_end;
    }
}
#ifdef IA64
  GC_INNER void GC_push_all_register_sections(ptr_t bs_lo, ptr_t bs_hi,
                  int eager, struct GC_traced_stack_sect_s *traced_stack_sect)
  {
    while (traced_stack_sect != NULL) {
        ptr_t frame_bs_lo = traced_stack_sect -> backing_store_end;
        GC_ASSERT((word)frame_bs_lo <= (word)bs_hi);
        if (eager) {
            GC_push_all_eager(frame_bs_lo, bs_hi);
        } else {
            GC_push_all_stack(frame_bs_lo, bs_hi);
        }
        bs_hi = traced_stack_sect -> saved_backing_store_ptr;
        traced_stack_sect = traced_stack_sect -> prev;
    }
    GC_ASSERT((word)bs_lo <= (word)bs_hi);
    if (eager) {
        GC_push_all_eager(bs_lo, bs_hi);
    } else {
        GC_push_all_stack(bs_lo, bs_hi);
    }
  }
#endif
#ifdef THREADS
GC_INNER void GC_push_all_stack_sections(ptr_t lo, ptr_t hi,
                        struct GC_traced_stack_sect_s *traced_stack_sect)
{
    while (traced_stack_sect != NULL) {
        GC_ASSERT((word)lo HOTTER_THAN (word)traced_stack_sect);
#ifdef STACK_GROWS_UP
            GC_push_all_stack((ptr_t)traced_stack_sect, lo);
#else
            GC_push_all_stack(lo, (ptr_t)traced_stack_sect);
#endif
        lo = traced_stack_sect -> saved_stack_ptr;
        GC_ASSERT(lo != NULL);
        traced_stack_sect = traced_stack_sect -> prev;
    }
    GC_ASSERT(!((word)hi HOTTER_THAN (word)lo));
#ifdef STACK_GROWS_UP
        GC_push_all_stack(hi, lo);
#else
        GC_push_all_stack(lo, hi);
#endif
}
#else
STATIC void GC_push_all_stack_partially_eager(ptr_t bottom, ptr_t top,
                                              ptr_t cold_gc_frame)
{
#ifndef NEED_FIXUP_POINTER
  if (GC_all_interior_pointers) {
    if (0 == cold_gc_frame) {
        GC_push_all_stack(bottom, top);
        return;
    }
    GC_ASSERT((word)bottom <= (word)cold_gc_frame
              && (word)cold_gc_frame <= (word)top);
#ifdef STACK_GROWS_DOWN
        GC_push_all(cold_gc_frame - sizeof(ptr_t), top);
        GC_push_all_eager(bottom, cold_gc_frame);
#else
        GC_push_all(bottom, cold_gc_frame + sizeof(ptr_t));
        GC_push_all_eager(cold_gc_frame, top);
#endif
  } else
#endif
   {
    GC_push_all_eager(bottom, top);
  }
#ifdef TRACE_BUF
    GC_add_trace_entry("GC_push_all_stack", (word)bottom, (word)top);
#endif
}
STATIC void GC_push_all_stack_part_eager_sections(ptr_t lo, ptr_t hi,
        ptr_t cold_gc_frame, struct GC_traced_stack_sect_s *traced_stack_sect)
{
    GC_ASSERT(traced_stack_sect == NULL || cold_gc_frame == NULL ||
              (word)cold_gc_frame HOTTER_THAN (word)traced_stack_sect);
    while (traced_stack_sect != NULL) {
        GC_ASSERT((word)lo HOTTER_THAN (word)traced_stack_sect);
#ifdef STACK_GROWS_UP
            GC_push_all_stack_partially_eager((ptr_t)traced_stack_sect, lo,
                                              cold_gc_frame);
#else
            GC_push_all_stack_partially_eager(lo, (ptr_t)traced_stack_sect,
                                              cold_gc_frame);
#endif
        lo = traced_stack_sect -> saved_stack_ptr;
        GC_ASSERT(lo != NULL);
        traced_stack_sect = traced_stack_sect -> prev;
        cold_gc_frame = NULL;
    }
    GC_ASSERT(!((word)hi HOTTER_THAN (word)lo));
#ifdef STACK_GROWS_UP
        GC_push_all_stack_partially_eager(hi, lo, cold_gc_frame);
#else
        GC_push_all_stack_partially_eager(lo, hi, cold_gc_frame);
#endif
}
#endif
STATIC void GC_push_current_stack(ptr_t cold_gc_frame,
                                  void * context GC_ATTR_UNUSED)
{
#if defined(THREADS)
#ifdef STACK_GROWS_DOWN
          GC_push_all_eager(GC_approx_sp(), cold_gc_frame);
#else
          GC_push_all_eager(cold_gc_frame, GC_approx_sp());
#endif
#else
        GC_push_all_stack_part_eager_sections(GC_approx_sp(), GC_stackbottom,
                                        cold_gc_frame, GC_traced_stack_sect);
#ifdef IA64
              {
                ptr_t bsp = GC_save_regs_ret_val;
                ptr_t cold_gc_bs_pointer = bsp - 2048;
                if (GC_all_interior_pointers
                    && (word)cold_gc_bs_pointer > (word)BACKING_STORE_BASE) {
                  if (GC_traced_stack_sect != NULL
                      && (word)cold_gc_bs_pointer
                          < (word)GC_traced_stack_sect->backing_store_end)
                    cold_gc_bs_pointer =
                                GC_traced_stack_sect->backing_store_end;
                  GC_push_all_register_sections(BACKING_STORE_BASE,
                        cold_gc_bs_pointer, FALSE, GC_traced_stack_sect);
                  GC_push_all_eager(cold_gc_bs_pointer, bsp);
                } else {
                  GC_push_all_register_sections(BACKING_STORE_BASE, bsp,
                                TRUE , GC_traced_stack_sect);
                }
              }
#endif
#endif
}
GC_INNER void (*GC_push_typed_structures)(void) = 0;
GC_INNER void GC_cond_register_dynamic_libraries(void)
{
#if (defined(DYNAMIC_LOADING) && !defined(MSWIN_XBOX1)) \
     || defined(CYGWIN32) || defined(MSWIN32) || defined(MSWINCE) \
     || defined(PCR)
    GC_remove_tmp_roots();
    if (!GC_no_dls) GC_register_dynamic_libraries();
#else
    GC_no_dls = TRUE;
#endif
}
STATIC void GC_push_regs_and_stack(ptr_t cold_gc_frame)
{
#ifdef THREADS
      if (NULL == cold_gc_frame)
        return;
#endif
    GC_with_callee_saves_pushed(GC_push_current_stack, cold_gc_frame);
}
GC_INNER void GC_push_roots(GC_bool all, ptr_t cold_gc_frame GC_ATTR_UNUSED)
{
    int i;
    unsigned kind;
#if !defined(REGISTER_LIBRARIES_EARLY)
        GC_cond_register_dynamic_libraries();
#endif
    for (i = 0; i < n_root_sets; i++) {
        GC_push_conditional_with_exclusions(
                             GC_static_roots[i].r_start,
                             GC_static_roots[i].r_end, all);
    }
    for (kind = 0; kind < GC_n_kinds; kind++) {
        void *base = GC_base(GC_obj_kinds[kind].ok_freelist);
        if (base != NULL) {
            GC_set_mark_bit(base);
        }
    }
#ifndef GC_NO_FINALIZATION
        GC_push_finalizer_structures();
#endif
#ifdef THREADS
        if (GC_no_dls || GC_roots_were_cleared)
            GC_push_thread_structures();
#endif
    if (GC_push_typed_structures)
        GC_push_typed_structures();
#if defined(THREAD_LOCAL_ALLOC)
        if (GC_world_stopped)
            GC_mark_thread_local_free_lists();
#endif
#ifndef STACK_NOT_SCANNED
        GC_push_regs_and_stack(cold_gc_frame);
#endif
    if (GC_push_other_roots != 0) {
        (*GC_push_other_roots)();
    }
}
#ifdef ENABLE_DISCLAIM
#endif
#include <stdio.h>
GC_INNER signed_word GC_bytes_found = 0;
#if defined(PARALLEL_MARK)
  GC_INNER signed_word GC_fl_builder_count = 0;
#endif
#ifndef MAX_LEAKED
#define MAX_LEAKED 40
#endif
STATIC ptr_t GC_leaked[MAX_LEAKED] = { NULL };
STATIC unsigned GC_n_leaked = 0;
GC_INNER GC_bool GC_have_errors = FALSE;
#if !defined(EAGER_SWEEP) && defined(ENABLE_DISCLAIM)
  STATIC void GC_reclaim_unconditionally_marked(void);
#endif
GC_INLINE void GC_add_leaked(ptr_t leaked)
{
#ifndef SHORT_DBG_HDRS
     if (GC_findleak_delay_free && !GC_check_leaked(leaked))
       return;
#endif
    GC_have_errors = TRUE;
    if (GC_n_leaked < MAX_LEAKED) {
      GC_leaked[GC_n_leaked++] = leaked;
      GC_set_mark_bit(leaked);
    }
}
GC_INNER void GC_print_all_errors(void)
{
    static GC_bool printing_errors = FALSE;
    GC_bool have_errors;
    unsigned i, n_leaked;
    ptr_t leaked[MAX_LEAKED];
    DCL_LOCK_STATE;
    LOCK();
    if (printing_errors) {
        UNLOCK();
        return;
    }
    have_errors = GC_have_errors;
    printing_errors = TRUE;
    n_leaked = GC_n_leaked;
    if (n_leaked > 0) {
      GC_ASSERT(n_leaked <= MAX_LEAKED);
      BCOPY(GC_leaked, leaked, n_leaked * sizeof(ptr_t));
      GC_n_leaked = 0;
      BZERO(GC_leaked, n_leaked * sizeof(ptr_t));
    }
    UNLOCK();
    if (GC_debugging_started) {
      GC_print_all_smashed();
    } else {
      have_errors = FALSE;
    }
    if (n_leaked > 0) {
        GC_err_printf("Found %u leaked objects:\n", n_leaked);
        have_errors = TRUE;
    }
    for (i = 0; i < n_leaked; i++) {
        ptr_t p = leaked[i];
#ifndef SKIP_LEAKED_OBJECTS_PRINTING
          GC_print_heap_obj(p);
#endif
        GC_free(p);
    }
    if (have_errors
#ifndef GC_ABORT_ON_LEAK
          && GETENV("GC_ABORT_ON_LEAK") != NULL
#endif
        ) {
      ABORT("Leaked or smashed objects encountered");
    }
    LOCK();
    printing_errors = FALSE;
    UNLOCK();
}
GC_INNER GC_bool GC_block_empty(hdr *hhdr)
{
    return (hhdr -> hb_n_marks == 0);
}
STATIC GC_bool GC_block_nearly_full(hdr *hhdr, word sz)
{
    return hhdr -> hb_n_marks > HBLK_OBJS(sz) * 7 / 8;
}
GC_INLINE word *GC_clear_block(word *p, word sz, signed_word *count)
{
  word *q = (word *)((ptr_t)p + sz);
#ifdef USE_MARK_BYTES
    GC_ASSERT((sz & 1) == 0);
    GC_ASSERT(((word)p & (2 * sizeof(word) - 1)) == 0);
    p[1] = 0;
    p += 2;
    while ((word)p < (word)q) {
      CLEAR_DOUBLE(p);
      p += 2;
    }
#else
    p++;
    while ((word)p < (word)q) {
      *p++ = 0;
    }
#endif
  *count += sz;
  return p;
}
STATIC ptr_t GC_reclaim_clear(struct hblk *hbp, hdr *hhdr, word sz,
                              ptr_t list, signed_word *count)
{
    word bit_no = 0;
    ptr_t p, plim;
    GC_ASSERT(hhdr == GC_find_header((ptr_t)hbp));
#ifndef THREADS
      GC_ASSERT(sz == hhdr -> hb_sz);
#else
#endif
    GC_ASSERT((sz & (BYTES_PER_WORD-1)) == 0);
    p = hbp->hb_body;
    plim = p + HBLKSIZE - sz;
        while ((word)p <= (word)plim) {
            if (mark_bit_from_hdr(hhdr, bit_no)) {
                p += sz;
            } else {
                obj_link(p) = list;
                list = p;
                p = (ptr_t)GC_clear_block((word *)p, sz, count);
            }
            bit_no += MARK_BIT_OFFSET(sz);
        }
    return list;
}
STATIC ptr_t GC_reclaim_uninit(struct hblk *hbp, hdr *hhdr, word sz,
                               ptr_t list, signed_word *count)
{
    word bit_no = 0;
    word *p, *plim;
    signed_word n_bytes_found = 0;
#ifndef THREADS
      GC_ASSERT(sz == hhdr -> hb_sz);
#endif
    p = (word *)(hbp->hb_body);
    plim = (word *)((ptr_t)hbp + HBLKSIZE - sz);
        while ((word)p <= (word)plim) {
            if (!mark_bit_from_hdr(hhdr, bit_no)) {
                n_bytes_found += sz;
                    obj_link(p) = list;
                    list = ((ptr_t)p);
            }
            p = (word *)((ptr_t)p + sz);
            bit_no += MARK_BIT_OFFSET(sz);
        }
    *count += n_bytes_found;
    return(list);
}
#ifdef ENABLE_DISCLAIM
  STATIC ptr_t GC_disclaim_and_reclaim(struct hblk *hbp, hdr *hhdr, word sz,
                                       ptr_t list, signed_word *count)
  {
    word bit_no = 0;
    ptr_t p, plim;
    struct obj_kind *ok = &GC_obj_kinds[hhdr->hb_obj_kind];
    int (GC_CALLBACK *disclaim)(void *) = ok->ok_disclaim_proc;
#ifndef THREADS
      GC_ASSERT(sz == hhdr -> hb_sz);
#endif
    p = hbp->hb_body;
    plim = p + HBLKSIZE - sz;
    for (; (word)p <= (word)plim; bit_no += MARK_BIT_OFFSET(sz)) {
        if (mark_bit_from_hdr(hhdr, bit_no)) {
            p += sz;
        } else if ((*disclaim)(p)) {
            set_mark_bit_from_hdr(hhdr, bit_no);
            hhdr -> hb_n_marks++;
            p += sz;
        } else {
            obj_link(p) = list;
            list = p;
            p = (ptr_t)GC_clear_block((word *)p, sz, count);
        }
    }
    return list;
  }
#endif
STATIC void GC_reclaim_check(struct hblk *hbp, hdr *hhdr, word sz)
{
    word bit_no;
    ptr_t p, plim;
#ifndef THREADS
      GC_ASSERT(sz == hhdr -> hb_sz);
#endif
    p = hbp->hb_body;
    plim = p + HBLKSIZE - sz;
    for (bit_no = 0; (word)p <= (word)plim;
         p += sz, bit_no += MARK_BIT_OFFSET(sz)) {
      if (!mark_bit_from_hdr(hhdr, bit_no)) {
        GC_add_leaked(p);
      }
    }
}
#ifdef AO_HAVE_load
#define IS_PTRFREE_SAFE(hhdr) \
                (AO_load((volatile AO_t *)&(hhdr)->hb_descr) == 0)
#else
#define IS_PTRFREE_SAFE(hhdr) ((hhdr)->hb_descr == 0)
#endif
GC_INNER ptr_t GC_reclaim_generic(struct hblk * hbp, hdr *hhdr, size_t sz,
                                  GC_bool init, ptr_t list,
                                  signed_word *count)
{
    ptr_t result;
    GC_ASSERT(GC_find_header((ptr_t)hbp) == hhdr);
#ifndef GC_DISABLE_INCREMENTAL
      GC_remove_protection(hbp, 1, IS_PTRFREE_SAFE(hhdr));
#endif
#ifdef ENABLE_DISCLAIM
      if ((hhdr -> hb_flags & HAS_DISCLAIM) != 0) {
        result = GC_disclaim_and_reclaim(hbp, hhdr, sz, list, count);
      } else
#endif
     if (init || GC_debugging_started) {
      result = GC_reclaim_clear(hbp, hhdr, sz, list, count);
    } else {
      GC_ASSERT(IS_PTRFREE_SAFE(hhdr));
      result = GC_reclaim_uninit(hbp, hhdr, sz, list, count);
    }
    if (IS_UNCOLLECTABLE(hhdr -> hb_obj_kind)) GC_set_hdr_marks(hhdr);
    return result;
}
STATIC void GC_reclaim_small_nonempty_block(struct hblk *hbp, word sz,
                                            GC_bool report_if_found)
{
    hdr *hhdr = HDR(hbp);
    struct obj_kind * ok = &GC_obj_kinds[hhdr -> hb_obj_kind];
    void **flh = &(ok -> ok_freelist[BYTES_TO_GRANULES(sz)]);
    hhdr -> hb_last_reclaimed = (unsigned short) GC_gc_no;
    if (report_if_found) {
        GC_reclaim_check(hbp, hhdr, sz);
    } else {
        *flh = GC_reclaim_generic(hbp, hhdr, sz, ok -> ok_init,
                                  (ptr_t)(*flh), &GC_bytes_found);
    }
}
#ifdef ENABLE_DISCLAIM
  STATIC void GC_disclaim_and_reclaim_or_free_small_block(struct hblk *hbp)
  {
    hdr *hhdr = HDR(hbp);
    word sz = hhdr -> hb_sz;
    struct obj_kind * ok = &GC_obj_kinds[hhdr -> hb_obj_kind];
    void **flh = &(ok -> ok_freelist[BYTES_TO_GRANULES(sz)]);
    void *flh_next;
    hhdr -> hb_last_reclaimed = (unsigned short) GC_gc_no;
    flh_next = GC_reclaim_generic(hbp, hhdr, sz, ok -> ok_init,
                                  (ptr_t)(*flh), &GC_bytes_found);
    if (hhdr -> hb_n_marks)
        *flh = flh_next;
    else {
        GC_bytes_found += HBLKSIZE;
        GC_freehblk(hbp);
    }
  }
#endif
STATIC void GC_reclaim_block(struct hblk *hbp, word report_if_found)
{
    hdr * hhdr = HDR(hbp);
    word sz;
    struct obj_kind * ok = &GC_obj_kinds[hhdr -> hb_obj_kind];
#ifdef AO_HAVE_load
        sz = (word)AO_load((volatile AO_t *)&hhdr->hb_sz);
#else
        sz = hhdr -> hb_sz;
#endif
    if( sz > MAXOBJBYTES ) {
        if( !mark_bit_from_hdr(hhdr, 0) ) {
            if (report_if_found) {
              GC_add_leaked((ptr_t)hbp);
            } else {
              word blocks;
#ifdef ENABLE_DISCLAIM
                if (EXPECT(hhdr->hb_flags & HAS_DISCLAIM, 0)) {
                  if ((*ok->ok_disclaim_proc)(hbp)) {
                    set_mark_bit_from_hdr(hhdr, 0);
                    goto in_use;
                  }
                }
#endif
              blocks = OBJ_SZ_TO_BLOCKS(sz);
#if defined(CPPCHECK)
                GC_noop1((word)&blocks);
#endif
              if (blocks > 1) {
                GC_large_allocd_bytes -= blocks * HBLKSIZE;
              }
              GC_bytes_found += sz;
              GC_freehblk(hbp);
            }
        } else {
#ifdef ENABLE_DISCLAIM
           in_use:
#endif
            if (IS_PTRFREE_SAFE(hhdr)) {
              GC_atomic_in_use += sz;
            } else {
              GC_composite_in_use += sz;
            }
        }
    } else {
        GC_bool empty = GC_block_empty(hhdr);
#ifdef PARALLEL_MARK
          GC_ASSERT(hhdr -> hb_n_marks <= 2 * (HBLKSIZE/sz + 1) + 16);
#else
          GC_ASSERT(sz * hhdr -> hb_n_marks <= HBLKSIZE);
#endif
        if (report_if_found) {
          GC_reclaim_small_nonempty_block(hbp, sz,
                                          TRUE );
        } else if (empty) {
#ifdef ENABLE_DISCLAIM
          if ((hhdr -> hb_flags & HAS_DISCLAIM) != 0) {
            GC_disclaim_and_reclaim_or_free_small_block(hbp);
          } else
#endif
           {
            GC_bytes_found += HBLKSIZE;
            GC_freehblk(hbp);
          }
        } else if (GC_find_leak || !GC_block_nearly_full(hhdr, sz)) {
          struct hblk **rlh = ok -> ok_reclaim_list;
          if (rlh != NULL) {
            rlh += BYTES_TO_GRANULES(sz);
            hhdr -> hb_next = *rlh;
            *rlh = hbp;
          }
        }
        if (IS_PTRFREE_SAFE(hhdr)) {
          GC_atomic_in_use += sz * hhdr -> hb_n_marks;
        } else {
          GC_composite_in_use += sz * hhdr -> hb_n_marks;
        }
    }
}
#if !defined(NO_DEBUGGING)
struct Print_stats
{
        size_t number_of_blocks;
        size_t total_bytes;
};
#ifdef USE_MARK_BYTES
unsigned GC_n_set_marks(hdr *hhdr)
{
    unsigned result = 0;
    word i;
    word sz = hhdr -> hb_sz;
    word offset = MARK_BIT_OFFSET(sz);
    word limit = FINAL_MARK_BIT(sz);
    for (i = 0; i < limit; i += offset) {
        result += hhdr -> hb_marks[i];
    }
    GC_ASSERT(hhdr -> hb_marks[limit]);
    return(result);
}
#else
static unsigned set_bits(word n)
{
    word m = n;
    unsigned result = 0;
    while (m > 0) {
        if (m & 1) result++;
        m >>= 1;
    }
    return(result);
}
unsigned GC_n_set_marks(hdr *hhdr)
{
    unsigned result = 0;
    word i;
    word n_mark_words;
#ifdef MARK_BIT_PER_OBJ
      word n_objs = HBLK_OBJS(hhdr -> hb_sz);
      if (0 == n_objs) n_objs = 1;
      n_mark_words = divWORDSZ(n_objs + WORDSZ - 1);
#else
      n_mark_words = MARK_BITS_SZ;
#endif
    for (i = 0; i < n_mark_words - 1; i++) {
        result += set_bits(hhdr -> hb_marks[i]);
    }
#ifdef MARK_BIT_PER_OBJ
      result += set_bits((hhdr -> hb_marks[n_mark_words - 1])
                         << (n_mark_words * WORDSZ - n_objs));
#else
      result += set_bits(hhdr -> hb_marks[n_mark_words - 1]);
#endif
    return result;
}
#endif
STATIC void GC_print_block_descr(struct hblk *h,
                                 word  raw_ps)
{
    hdr * hhdr = HDR(h);
    size_t bytes = hhdr -> hb_sz;
    struct Print_stats *ps;
    unsigned n_marks = GC_n_set_marks(hhdr);
    unsigned n_objs = (unsigned)HBLK_OBJS(bytes);
    if (0 == n_objs) n_objs = 1;
    if (hhdr -> hb_n_marks != n_marks) {
      GC_printf("%u,%u,%u!=%u,%u\n", hhdr->hb_obj_kind, (unsigned)bytes,
                (unsigned)hhdr->hb_n_marks, n_marks, n_objs);
    } else {
      GC_printf("%u,%u,%u,%u\n", hhdr->hb_obj_kind, (unsigned)bytes,
                n_marks, n_objs);
    }
    ps = (struct Print_stats *)raw_ps;
    ps->total_bytes += (bytes + (HBLKSIZE-1)) & ~(HBLKSIZE-1);
    ps->number_of_blocks++;
}
void GC_print_block_list(void)
{
    struct Print_stats pstats;
    GC_printf("kind(0=ptrfree,1=normal,2=unc.),"
              "size_in_bytes,#_marks_set,#objs\n");
    pstats.number_of_blocks = 0;
    pstats.total_bytes = 0;
    GC_apply_to_all_blocks(GC_print_block_descr, (word)&pstats);
    GC_printf("blocks= %lu, bytes= %lu\n",
              (unsigned long)pstats.number_of_blocks,
              (unsigned long)pstats.total_bytes);
}
GC_API void GC_CALL GC_print_free_list(int kind, size_t sz_in_granules)
{
    void *flh_next;
    int n;
    GC_ASSERT(kind < MAXOBJKINDS);
    GC_ASSERT(sz_in_granules <= MAXOBJGRANULES);
    flh_next = GC_obj_kinds[kind].ok_freelist[sz_in_granules];
    for (n = 0; flh_next; n++) {
        GC_printf("Free object in heap block %p [%d]: %p\n",
                  (void *)HBLKPTR(flh_next), n, flh_next);
        flh_next = obj_link(flh_next);
    }
}
#endif
STATIC void GC_clear_fl_links(void **flp)
{
    void *next = *flp;
    while (0 != next) {
       *flp = 0;
       flp = &(obj_link(next));
       next = *flp;
    }
}
GC_INNER void GC_start_reclaim(GC_bool report_if_found)
{
    unsigned kind;
#if defined(PARALLEL_MARK)
      GC_ASSERT(0 == GC_fl_builder_count);
#endif
      GC_composite_in_use = 0;
      GC_atomic_in_use = 0;
      for (kind = 0; kind < GC_n_kinds; kind++) {
        struct hblk ** rlist = GC_obj_kinds[kind].ok_reclaim_list;
        GC_bool should_clobber = (GC_obj_kinds[kind].ok_descriptor != 0);
        if (rlist == 0) continue;
        if (!report_if_found) {
            void **fop;
            void **lim = &(GC_obj_kinds[kind].ok_freelist[MAXOBJGRANULES+1]);
            for (fop = GC_obj_kinds[kind].ok_freelist;
                 (word)fop < (word)lim; (*(word **)&fop)++) {
              if (*fop != 0) {
                if (should_clobber) {
                  GC_clear_fl_links(fop);
                } else {
                  *fop = 0;
                }
              }
            }
        }
        BZERO(rlist, (MAXOBJGRANULES + 1) * sizeof(void *));
      }
    GC_apply_to_all_blocks(GC_reclaim_block, (word)report_if_found);
#ifdef EAGER_SWEEP
    GC_reclaim_all((GC_stop_func)0, FALSE);
#elif defined(ENABLE_DISCLAIM)
    GC_reclaim_unconditionally_marked();
#endif
#if defined(PARALLEL_MARK)
    GC_ASSERT(0 == GC_fl_builder_count);
#endif
}
GC_INNER void GC_continue_reclaim(word sz , int kind)
{
    hdr * hhdr;
    struct hblk * hbp;
    struct obj_kind * ok = &(GC_obj_kinds[kind]);
    struct hblk ** rlh = ok -> ok_reclaim_list;
    void **flh = &(ok -> ok_freelist[sz]);
    if (NULL == rlh)
        return;
    for (rlh += sz; (hbp = *rlh) != NULL; ) {
        hhdr = HDR(hbp);
        *rlh = hhdr -> hb_next;
        GC_reclaim_small_nonempty_block(hbp, hhdr -> hb_sz, FALSE);
        if (*flh != 0)
            break;
    }
}
GC_INNER GC_bool GC_reclaim_all(GC_stop_func stop_func, GC_bool ignore_old)
{
    word sz;
    unsigned kind;
    hdr * hhdr;
    struct hblk * hbp;
    struct obj_kind * ok;
    struct hblk ** rlp;
    struct hblk ** rlh;
#ifndef NO_CLOCK
      CLOCK_TYPE start_time = CLOCK_TYPE_INITIALIZER;
      if (GC_print_stats == VERBOSE)
        GET_TIME(start_time);
#endif
    for (kind = 0; kind < GC_n_kinds; kind++) {
        ok = &(GC_obj_kinds[kind]);
        rlp = ok -> ok_reclaim_list;
        if (rlp == 0) continue;
        for (sz = 1; sz <= MAXOBJGRANULES; sz++) {
            for (rlh = rlp + sz; (hbp = *rlh) != NULL; ) {
                if (stop_func != (GC_stop_func)0 && (*stop_func)()) {
                    return(FALSE);
                }
                hhdr = HDR(hbp);
                *rlh = hhdr -> hb_next;
                if (!ignore_old
                    || (word)hhdr->hb_last_reclaimed == GC_gc_no - 1) {
                    GC_reclaim_small_nonempty_block(hbp, hhdr->hb_sz, FALSE);
                }
            }
        }
    }
#ifndef NO_CLOCK
      if (GC_print_stats == VERBOSE) {
        CLOCK_TYPE done_time;
        GET_TIME(done_time);
        GC_verbose_log_printf(
                        "Disposing of reclaim lists took %lu ms %lu ns\n",
                        MS_TIME_DIFF(done_time, start_time),
                        NS_FRAC_TIME_DIFF(done_time, start_time));
      }
#endif
    return(TRUE);
}
#if !defined(EAGER_SWEEP) && defined(ENABLE_DISCLAIM)
  STATIC void GC_reclaim_unconditionally_marked(void)
  {
    word sz;
    unsigned kind;
    hdr * hhdr;
    struct hblk * hbp;
    struct obj_kind * ok;
    struct hblk ** rlp;
    struct hblk ** rlh;
    for (kind = 0; kind < GC_n_kinds; kind++) {
        ok = &(GC_obj_kinds[kind]);
        if (!ok->ok_mark_unconditionally)
          continue;
        rlp = ok->ok_reclaim_list;
        if (rlp == 0)
          continue;
        for (sz = 1; sz <= MAXOBJGRANULES; sz++) {
            rlh = rlp + sz;
            while ((hbp = *rlh) != 0) {
                hhdr = HDR(hbp);
                *rlh = hhdr->hb_next;
                GC_reclaim_small_nonempty_block(hbp, hhdr->hb_sz, FALSE);
            }
        }
    }
  }
#endif
struct enumerate_reachable_s {
  GC_reachable_object_proc proc;
  void *client_data;
};
STATIC void GC_do_enumerate_reachable_objects(struct hblk *hbp, word ped)
{
  struct hblkhdr *hhdr = HDR(hbp);
  size_t sz = (size_t)hhdr->hb_sz;
  size_t bit_no;
  char *p, *plim;
  if (GC_block_empty(hhdr)) {
    return;
  }
  p = hbp->hb_body;
  if (sz > MAXOBJBYTES) {
    plim = p;
  } else {
    plim = hbp->hb_body + HBLKSIZE - sz;
  }
  for (bit_no = 0; p <= plim; bit_no += MARK_BIT_OFFSET(sz), p += sz) {
    if (mark_bit_from_hdr(hhdr, bit_no)) {
      ((struct enumerate_reachable_s *)ped)->proc(p, sz,
                        ((struct enumerate_reachable_s *)ped)->client_data);
    }
  }
}
GC_API void GC_CALL GC_enumerate_reachable_objects_inner(
                                                GC_reachable_object_proc proc,
                                                void *client_data)
{
  struct enumerate_reachable_s ed;
  GC_ASSERT(I_HOLD_LOCK());
  ed.proc = proc;
  ed.client_data = client_data;
  GC_apply_to_all_blocks(GC_do_enumerate_reachable_objects, (word)&ed);
}
#ifndef GC_TYPED_H
#define GC_TYPED_H
#ifndef GC_H
#endif
#ifdef __cplusplus
  extern "C" {
#endif
typedef GC_word * GC_bitmap;
#define GC_WORDSZ (8 * sizeof(GC_word))
#define GC_get_bit(bm, index) \
            (((bm)[(index) / GC_WORDSZ] >> ((index) % GC_WORDSZ)) & 1)
#define GC_set_bit(bm, index) \
            ((bm)[(index) / GC_WORDSZ] |= (GC_word)1 << ((index) % GC_WORDSZ))
#define GC_WORD_OFFSET(t, f) (offsetof(t,f) / sizeof(GC_word))
#define GC_WORD_LEN(t) (sizeof(t) / sizeof(GC_word))
#define GC_BITMAP_SIZE(t) ((GC_WORD_LEN(t) + GC_WORDSZ - 1) / GC_WORDSZ)
typedef GC_word GC_descr;
GC_API GC_descr GC_CALL GC_make_descriptor(const GC_word * ,
                                size_t );
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
        GC_malloc_explicitly_typed(size_t ,
                                   GC_descr );
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
        GC_malloc_explicitly_typed_ignore_off_page(size_t ,
                                                   GC_descr );
GC_API GC_ATTR_MALLOC GC_ATTR_CALLOC_SIZE(1, 2) void * GC_CALL
        GC_calloc_explicitly_typed(size_t ,
                                   size_t ,
                                   GC_descr );
#ifdef GC_DEBUG
#define GC_MALLOC_EXPLICITLY_TYPED(bytes, d) ((void)(d), GC_MALLOC(bytes))
#define GC_CALLOC_EXPLICITLY_TYPED(n, bytes, d) \
                        ((void)(d), GC_MALLOC((n) * (bytes)))
#else
#define GC_MALLOC_EXPLICITLY_TYPED(bytes, d) \
                        GC_malloc_explicitly_typed(bytes, d)
#define GC_CALLOC_EXPLICITLY_TYPED(n, bytes, d) \
                        GC_calloc_explicitly_typed(n, bytes, d)
#endif
#ifdef __cplusplus
  }
#endif
#endif
#define TYPD_EXTRA_BYTES (sizeof(word) - EXTRA_BYTES)
STATIC int GC_explicit_kind = 0;
STATIC int GC_array_kind = 0;
struct LeafDescriptor {
        word ld_tag;
#define LEAF_TAG 1
        size_t ld_size;
        size_t ld_nelements;
        GC_descr ld_descriptor;
};
struct ComplexArrayDescriptor {
        word ad_tag;
#define ARRAY_TAG 2
        size_t ad_nelements;
        union ComplexDescriptor * ad_element_descr;
};
struct SequenceDescriptor {
        word sd_tag;
#define SEQUENCE_TAG 3
        union ComplexDescriptor * sd_first;
        union ComplexDescriptor * sd_second;
};
typedef union ComplexDescriptor {
    struct LeafDescriptor ld;
    struct ComplexArrayDescriptor ad;
    struct SequenceDescriptor sd;
} complex_descriptor;
#define TAG ad.ad_tag
#define ED_INITIAL_SIZE 100
STATIC int GC_typed_mark_proc_index = 0;
STATIC int GC_array_mark_proc_index = 0;
STATIC void GC_push_typed_structures_proc(void)
{
  GC_PUSH_ALL_SYM(GC_ext_descriptors);
}
STATIC signed_word GC_add_ext_descriptor(const word * bm, word nbits)
{
    size_t nwords = divWORDSZ(nbits + WORDSZ-1);
    signed_word result;
    size_t i;
    word last_part;
    size_t extra_bits;
    DCL_LOCK_STATE;
    LOCK();
    while (GC_avail_descr + nwords >= GC_ed_size) {
        typed_ext_descr_t *newExtD;
        size_t new_size;
        word ed_size = GC_ed_size;
        if (ed_size == 0) {
            GC_ASSERT((word)(&GC_ext_descriptors) % sizeof(word) == 0);
            GC_push_typed_structures = GC_push_typed_structures_proc;
            UNLOCK();
            new_size = ED_INITIAL_SIZE;
        } else {
            UNLOCK();
            new_size = 2 * ed_size;
            if (new_size > MAX_ENV) return(-1);
        }
        newExtD = (typed_ext_descr_t*)GC_malloc_atomic(new_size
                                                * sizeof(typed_ext_descr_t));
        if (NULL == newExtD)
            return -1;
        LOCK();
        if (ed_size == GC_ed_size) {
            if (GC_avail_descr != 0) {
                BCOPY(GC_ext_descriptors, newExtD,
                      GC_avail_descr * sizeof(typed_ext_descr_t));
            }
            GC_ed_size = new_size;
            GC_ext_descriptors = newExtD;
        }
    }
    result = GC_avail_descr;
    for (i = 0; i < nwords-1; i++) {
        GC_ext_descriptors[result + i].ed_bitmap = bm[i];
        GC_ext_descriptors[result + i].ed_continued = TRUE;
    }
    last_part = bm[i];
    extra_bits = nwords * WORDSZ - nbits;
    last_part <<= extra_bits;
    last_part >>= extra_bits;
    GC_ext_descriptors[result + i].ed_bitmap = last_part;
    GC_ext_descriptors[result + i].ed_continued = FALSE;
    GC_avail_descr += nwords;
    UNLOCK();
    return(result);
}
STATIC GC_descr GC_bm_table[WORDSZ/2];
STATIC GC_descr GC_double_descr(GC_descr descriptor, word nwords)
{
    if ((descriptor & GC_DS_TAGS) == GC_DS_LENGTH) {
        descriptor = GC_bm_table[BYTES_TO_WORDS((word)descriptor)];
    }
    descriptor |= (descriptor & ~GC_DS_TAGS) >> nwords;
    return(descriptor);
}
STATIC complex_descriptor *
GC_make_sequence_descriptor(complex_descriptor *first,
                            complex_descriptor *second);
#define COMPLEX 2
#define LEAF    1
#define SIMPLE  0
#define NO_MEM  (-1)
STATIC int GC_make_array_descriptor(size_t nelements, size_t size,
                                    GC_descr descriptor, GC_descr *simple_d,
                                    complex_descriptor **complex_d,
                                    struct LeafDescriptor * leaf)
{
#define OPT_THRESHOLD 50
    if ((descriptor & GC_DS_TAGS) == GC_DS_LENGTH) {
      if (descriptor == (GC_descr)size) {
        *simple_d = nelements * descriptor;
        return(SIMPLE);
      } else if ((word)descriptor == 0) {
        *simple_d = (GC_descr)0;
        return(SIMPLE);
      }
    }
    if (nelements <= OPT_THRESHOLD) {
      if (nelements <= 1) {
        if (nelements == 1) {
            *simple_d = descriptor;
            return(SIMPLE);
        } else {
            *simple_d = (GC_descr)0;
            return(SIMPLE);
        }
      }
    } else if (size <= BITMAP_BITS/2
               && (descriptor & GC_DS_TAGS) != GC_DS_PROC
               && (size & (sizeof(word)-1)) == 0) {
      int result =
          GC_make_array_descriptor(nelements/2, 2*size,
                                   GC_double_descr(descriptor,
                                                   BYTES_TO_WORDS(size)),
                                   simple_d, complex_d, leaf);
      if ((nelements & 1) == 0) {
          return(result);
      } else {
          struct LeafDescriptor * one_element =
              (struct LeafDescriptor *)
                GC_malloc_atomic(sizeof(struct LeafDescriptor));
          if (result == NO_MEM || one_element == 0) return(NO_MEM);
          one_element -> ld_tag = LEAF_TAG;
          one_element -> ld_size = size;
          one_element -> ld_nelements = 1;
          one_element -> ld_descriptor = descriptor;
          switch(result) {
            case SIMPLE:
            {
              struct LeafDescriptor * beginning =
                (struct LeafDescriptor *)
                  GC_malloc_atomic(sizeof(struct LeafDescriptor));
              if (beginning == 0) return(NO_MEM);
              beginning -> ld_tag = LEAF_TAG;
              beginning -> ld_size = size;
              beginning -> ld_nelements = 1;
              beginning -> ld_descriptor = *simple_d;
              *complex_d = GC_make_sequence_descriptor(
                                (complex_descriptor *)beginning,
                                (complex_descriptor *)one_element);
              break;
            }
            case LEAF:
            {
              struct LeafDescriptor * beginning =
                (struct LeafDescriptor *)
                  GC_malloc_atomic(sizeof(struct LeafDescriptor));
              if (beginning == 0) return(NO_MEM);
              beginning -> ld_tag = LEAF_TAG;
              beginning -> ld_size = leaf -> ld_size;
              beginning -> ld_nelements = leaf -> ld_nelements;
              beginning -> ld_descriptor = leaf -> ld_descriptor;
              *complex_d = GC_make_sequence_descriptor(
                                (complex_descriptor *)beginning,
                                (complex_descriptor *)one_element);
              break;
            }
            case COMPLEX:
              *complex_d = GC_make_sequence_descriptor(
                                *complex_d,
                                (complex_descriptor *)one_element);
              break;
          }
          return(COMPLEX);
      }
    }
    leaf -> ld_size = size;
    leaf -> ld_nelements = nelements;
    leaf -> ld_descriptor = descriptor;
    return(LEAF);
}
STATIC complex_descriptor *
GC_make_sequence_descriptor(complex_descriptor *first,
                            complex_descriptor *second)
{
    struct SequenceDescriptor * result =
        (struct SequenceDescriptor *)
                GC_malloc(sizeof(struct SequenceDescriptor));
    if (result != 0) {
        result -> sd_tag = SEQUENCE_TAG;
        result -> sd_first = first;
        result -> sd_second = second;
        GC_dirty(result);
        REACHABLE_AFTER_DIRTY(first);
        REACHABLE_AFTER_DIRTY(second);
    }
    return((complex_descriptor *)result);
}
STATIC mse * GC_typed_mark_proc(word * addr, mse * mark_stack_ptr,
                                mse * mark_stack_limit, word env);
STATIC mse * GC_array_mark_proc(word * addr, mse * mark_stack_ptr,
                                mse * mark_stack_limit, word env);
STATIC void GC_init_explicit_typing(void)
{
    unsigned i;
    GC_STATIC_ASSERT(sizeof(struct LeafDescriptor) % sizeof(word) == 0);
      GC_explicit_kind = GC_new_kind_inner(GC_new_free_list_inner(),
                            (WORDS_TO_BYTES((word)-1) | GC_DS_PER_OBJECT),
                            TRUE, TRUE);
      GC_typed_mark_proc_index = GC_new_proc_inner(GC_typed_mark_proc);
      GC_array_mark_proc_index = GC_new_proc_inner(GC_array_mark_proc);
      GC_array_kind = GC_new_kind_inner(GC_new_free_list_inner(),
                            GC_MAKE_PROC(GC_array_mark_proc_index, 0),
                            FALSE, TRUE);
      GC_bm_table[0] = GC_DS_BITMAP;
      for (i = 1; i < WORDSZ/2; i++) {
          GC_bm_table[i] = (((word)-1) << (WORDSZ - i)) | GC_DS_BITMAP;
      }
}
STATIC mse * GC_typed_mark_proc(word * addr, mse * mark_stack_ptr,
                                mse * mark_stack_limit, word env)
{
    word bm = GC_ext_descriptors[env].ed_bitmap;
    word * current_p = addr;
    word current;
    ptr_t greatest_ha = (ptr_t)GC_greatest_plausible_heap_addr;
    ptr_t least_ha = (ptr_t)GC_least_plausible_heap_addr;
    DECLARE_HDR_CACHE;
    INIT_HDR_CACHE;
    for (; bm != 0; bm >>= 1, current_p++) {
        if (bm & 1) {
            current = *current_p;
            FIXUP_POINTER(current);
            if (current >= (word)least_ha && current <= (word)greatest_ha) {
                PUSH_CONTENTS((ptr_t)current, mark_stack_ptr,
                              mark_stack_limit, (ptr_t)current_p);
            }
        }
    }
    if (GC_ext_descriptors[env].ed_continued) {
        mark_stack_ptr++;
        if ((word)mark_stack_ptr >= (word)mark_stack_limit) {
            mark_stack_ptr = GC_signal_mark_stack_overflow(mark_stack_ptr);
        }
        mark_stack_ptr -> mse_start = (ptr_t)(addr + WORDSZ);
        mark_stack_ptr -> mse_descr.w =
                        GC_MAKE_PROC(GC_typed_mark_proc_index, env + 1);
    }
    return(mark_stack_ptr);
}
STATIC word GC_descr_obj_size(complex_descriptor *d)
{
    switch(d -> TAG) {
      case LEAF_TAG:
        return(d -> ld.ld_nelements * d -> ld.ld_size);
      case ARRAY_TAG:
        return(d -> ad.ad_nelements
               * GC_descr_obj_size(d -> ad.ad_element_descr));
      case SEQUENCE_TAG:
        return(GC_descr_obj_size(d -> sd.sd_first)
               + GC_descr_obj_size(d -> sd.sd_second));
      default:
        ABORT_RET("Bad complex descriptor");
        return 0;
    }
}
STATIC mse * GC_push_complex_descriptor(word *addr, complex_descriptor *d,
                                        mse *msp, mse *msl)
{
    ptr_t current = (ptr_t)addr;
    word nelements;
    word sz;
    word i;
    switch(d -> TAG) {
      case LEAF_TAG:
        {
          GC_descr descr = d -> ld.ld_descriptor;
          nelements = d -> ld.ld_nelements;
          if (msl - msp <= (ptrdiff_t)nelements) return(0);
          sz = d -> ld.ld_size;
          for (i = 0; i < nelements; i++) {
              msp++;
              msp -> mse_start = current;
              msp -> mse_descr.w = descr;
              current += sz;
          }
          return(msp);
        }
      case ARRAY_TAG:
        {
          complex_descriptor *descr = d -> ad.ad_element_descr;
          nelements = d -> ad.ad_nelements;
          sz = GC_descr_obj_size(descr);
          for (i = 0; i < nelements; i++) {
              msp = GC_push_complex_descriptor((word *)current, descr,
                                                msp, msl);
              if (msp == 0) return(0);
              current += sz;
          }
          return(msp);
        }
      case SEQUENCE_TAG:
        {
          sz = GC_descr_obj_size(d -> sd.sd_first);
          msp = GC_push_complex_descriptor((word *)current, d -> sd.sd_first,
                                           msp, msl);
          if (msp == 0) return(0);
          current += sz;
          msp = GC_push_complex_descriptor((word *)current, d -> sd.sd_second,
                                           msp, msl);
          return(msp);
        }
      default:
        ABORT_RET("Bad complex descriptor");
        return 0;
   }
}
STATIC mse * GC_array_mark_proc(word * addr, mse * mark_stack_ptr,
                                mse * mark_stack_limit,
                                word env GC_ATTR_UNUSED)
{
    hdr * hhdr = HDR(addr);
    word sz = hhdr -> hb_sz;
    word nwords = BYTES_TO_WORDS(sz);
    complex_descriptor * descr = (complex_descriptor *)(addr[nwords-1]);
    mse * orig_mark_stack_ptr = mark_stack_ptr;
    mse * new_mark_stack_ptr;
    if (descr == 0) {
        return(orig_mark_stack_ptr);
    }
    new_mark_stack_ptr = GC_push_complex_descriptor(addr, descr,
                                                    mark_stack_ptr,
                                                    mark_stack_limit-1);
    if (new_mark_stack_ptr == 0) {
        if (NULL == mark_stack_ptr) ABORT("Bad mark_stack_ptr");
#ifdef PARALLEL_MARK
            if (GC_mark_stack + GC_mark_stack_size == mark_stack_limit)
#endif
        {
            GC_mark_stack_too_small = TRUE;
        }
        new_mark_stack_ptr = orig_mark_stack_ptr + 1;
        new_mark_stack_ptr -> mse_start = (ptr_t)addr;
        new_mark_stack_ptr -> mse_descr.w = sz | GC_DS_LENGTH;
    } else {
        new_mark_stack_ptr++;
        new_mark_stack_ptr -> mse_start = (ptr_t)(addr + nwords - 1);
        new_mark_stack_ptr -> mse_descr.w = sizeof(word) | GC_DS_LENGTH;
    }
    return new_mark_stack_ptr;
}
GC_API GC_descr GC_CALL GC_make_descriptor(const GC_word * bm, size_t len)
{
    signed_word last_set_bit = len - 1;
    GC_descr result;
    DCL_LOCK_STATE;
#if defined(AO_HAVE_load_acquire) && defined(AO_HAVE_store_release)
      if (!EXPECT(AO_load_acquire(&GC_explicit_typing_initialized), TRUE)) {
        LOCK();
        if (!GC_explicit_typing_initialized) {
          GC_init_explicit_typing();
          AO_store_release(&GC_explicit_typing_initialized, TRUE);
        }
        UNLOCK();
      }
#else
      LOCK();
      if (!EXPECT(GC_explicit_typing_initialized, TRUE)) {
        GC_init_explicit_typing();
        GC_explicit_typing_initialized = TRUE;
      }
      UNLOCK();
#endif
    while (last_set_bit >= 0 && !GC_get_bit(bm, last_set_bit))
      last_set_bit--;
    if (last_set_bit < 0) return(0 );
#if ALIGNMENT == CPP_WORDSZ/8
    {
      signed_word i;
      for (i = 0; i < last_set_bit; i++) {
        if (!GC_get_bit(bm, i)) {
          break;
        }
      }
      if (i == last_set_bit) {
        return (WORDS_TO_BYTES(last_set_bit+1) | GC_DS_LENGTH);
      }
    }
#endif
    if ((word)last_set_bit < BITMAP_BITS) {
        signed_word i;
        result = SIGNB;
        for (i = last_set_bit - 1; i >= 0; i--) {
            result >>= 1;
            if (GC_get_bit(bm, i)) result |= SIGNB;
        }
        result |= GC_DS_BITMAP;
    } else {
        signed_word index = GC_add_ext_descriptor(bm, (word)last_set_bit + 1);
        if (index == -1) return(WORDS_TO_BYTES(last_set_bit+1) | GC_DS_LENGTH);
        result = GC_MAKE_PROC(GC_typed_mark_proc_index, (word)index);
    }
    return result;
}
GC_API GC_ATTR_MALLOC void * GC_CALL GC_malloc_explicitly_typed(size_t lb,
                                                                GC_descr d)
{
    word *op;
    size_t lg;
    GC_ASSERT(GC_explicit_typing_initialized);
    lb = SIZET_SAT_ADD(lb, TYPD_EXTRA_BYTES);
    op = (word *)GC_malloc_kind(lb, GC_explicit_kind);
    if (EXPECT(NULL == op, FALSE))
        return NULL;
    lg = BYTES_TO_GRANULES(GC_size(op));
    op[GRANULES_TO_WORDS(lg) - 1] = d;
    GC_dirty(op + GRANULES_TO_WORDS(lg) - 1);
    REACHABLE_AFTER_DIRTY(d);
    return op;
}
#define GENERAL_MALLOC_IOP(lb, k) \
                GC_clear_stack(GC_generic_malloc_ignore_off_page(lb, k))
GC_API GC_ATTR_MALLOC void * GC_CALL
    GC_malloc_explicitly_typed_ignore_off_page(size_t lb, GC_descr d)
{
    ptr_t op;
    size_t lg;
    DCL_LOCK_STATE;
    GC_ASSERT(GC_explicit_typing_initialized);
    lb = SIZET_SAT_ADD(lb, TYPD_EXTRA_BYTES);
    if (SMALL_OBJ(lb)) {
        void **opp;
        GC_DBG_COLLECT_AT_MALLOC(lb);
        LOCK();
        lg = GC_size_map[lb];
        opp = &GC_obj_kinds[GC_explicit_kind].ok_freelist[lg];
        op = (ptr_t)(*opp);
        if (EXPECT(0 == op, FALSE)) {
            UNLOCK();
            op = (ptr_t)GENERAL_MALLOC_IOP(lb, GC_explicit_kind);
            if (0 == op) return 0;
            lg = BYTES_TO_GRANULES(GC_size(op));
        } else {
            *opp = obj_link(op);
            obj_link(op) = 0;
            GC_bytes_allocd += GRANULES_TO_BYTES((word)lg);
            UNLOCK();
        }
    } else {
        op = (ptr_t)GENERAL_MALLOC_IOP(lb, GC_explicit_kind);
        if (NULL == op) return NULL;
        lg = BYTES_TO_GRANULES(GC_size(op));
    }
    ((word *)op)[GRANULES_TO_WORDS(lg) - 1] = d;
    GC_dirty(op + GRANULES_TO_WORDS(lg) - 1);
    REACHABLE_AFTER_DIRTY(d);
    return op;
}
GC_API GC_ATTR_MALLOC void * GC_CALL GC_calloc_explicitly_typed(size_t n,
                                                        size_t lb, GC_descr d)
{
    word *op;
    size_t lg;
    GC_descr simple_descr;
    complex_descriptor *complex_descr;
    int descr_type;
    struct LeafDescriptor leaf;
    GC_ASSERT(GC_explicit_typing_initialized);
    descr_type = GC_make_array_descriptor((word)n, (word)lb, d, &simple_descr,
                                          &complex_descr, &leaf);
    if ((lb | n) > GC_SQRT_SIZE_MAX
        && lb > 0 && n > GC_SIZE_MAX / lb)
      return (*GC_get_oom_fn())(GC_SIZE_MAX);
    lb *= n;
    switch(descr_type) {
        case NO_MEM: return(0);
        case SIMPLE:
            return GC_malloc_explicitly_typed(lb, simple_descr);
        case LEAF:
            lb = SIZET_SAT_ADD(lb,
                        sizeof(struct LeafDescriptor) + TYPD_EXTRA_BYTES);
            break;
        case COMPLEX:
            lb = SIZET_SAT_ADD(lb, TYPD_EXTRA_BYTES);
            break;
    }
    op = (word *)GC_malloc_kind(lb, GC_array_kind);
    if (EXPECT(NULL == op, FALSE))
        return NULL;
    lg = BYTES_TO_GRANULES(GC_size(op));
    if (descr_type == LEAF) {
        volatile struct LeafDescriptor * lp =
            (struct LeafDescriptor *)
                (op + GRANULES_TO_WORDS(lg)
                    - (BYTES_TO_WORDS(sizeof(struct LeafDescriptor)) + 1));
        lp -> ld_tag = LEAF_TAG;
        lp -> ld_size = leaf.ld_size;
        lp -> ld_nelements = leaf.ld_nelements;
        lp -> ld_descriptor = leaf.ld_descriptor;
        ((volatile word *)op)[GRANULES_TO_WORDS(lg) - 1] = (word)lp;
    } else {
#ifndef GC_NO_FINALIZATION
        size_t lw = GRANULES_TO_WORDS(lg);
        op[lw - 1] = (word)complex_descr;
        GC_dirty(op + lw - 1);
        REACHABLE_AFTER_DIRTY(complex_descr);
        if (EXPECT(GC_general_register_disappearing_link(
                                                (void **)(op + lw - 1), op)
                  == GC_NO_MEMORY, FALSE))
#endif
        {
            return (*GC_get_oom_fn())(lb);
        }
    }
    return op;
}
#include <stdio.h>
#include <limits.h>
#include <stdarg.h>
#ifndef MSWINCE
#include <signal.h>
#endif
#ifdef GC_SOLARIS_THREADS
#include <sys/syscall.h>
#endif
#if defined(UNIX_LIKE) || defined(CYGWIN32) || defined(SYMBIAN) \
    || (defined(CONSOLE_LOG) && defined(MSWIN32))
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#endif
#if defined(CONSOLE_LOG) && defined(MSWIN32) && defined(_MSC_VER)
#include <io.h>
#endif
#ifdef NONSTOP
#include <floss.h>
#endif
#ifdef THREADS
#ifdef PCR
#include "il/PCR_IL.h"
    GC_INNER PCR_Th_ML GC_allocate_ml;
#elif defined(SN_TARGET_PSP2)
    GC_INNER WapiMutex GC_allocate_ml_PSP2 = { 0, NULL };
#elif defined(GC_DEFN_ALLOCATE_ML) || defined(SN_TARGET_PS3)
#include <pthread.h>
    GC_INNER pthread_mutex_t GC_allocate_ml;
#endif
#endif
#ifdef DYNAMIC_LOADING
#define GC_REGISTER_MAIN_STATIC_DATA() GC_register_main_static_data()
#elif defined(GC_DONT_REGISTER_MAIN_STATIC_DATA)
#define GC_REGISTER_MAIN_STATIC_DATA() FALSE
#else
#define GC_REGISTER_MAIN_STATIC_DATA() TRUE
#endif
#ifdef NEED_CANCEL_DISABLE_COUNT
  __thread unsigned char GC_cancel_disable_count = 0;
#endif
GC_FAR struct _GC_arrays GC_arrays ;
GC_INNER unsigned GC_n_mark_procs = GC_RESERVED_MARK_PROCS;
GC_INNER unsigned GC_n_kinds = GC_N_KINDS_INITIAL_VALUE;
GC_INNER GC_bool GC_debugging_started = FALSE;
ptr_t GC_stackbottom = 0;
#ifdef IA64
  ptr_t GC_register_stackbottom = 0;
#endif
int GC_dont_gc = FALSE;
int GC_dont_precollect = FALSE;
GC_bool GC_quiet = 0;
#if !defined(NO_CLOCK) || !defined(SMALL_CONFIG)
  int GC_print_stats = 0;
#endif
#ifdef GC_PRINT_BACK_HEIGHT
  GC_INNER GC_bool GC_print_back_height = TRUE;
#else
  GC_INNER GC_bool GC_print_back_height = FALSE;
#endif
#ifndef NO_DEBUGGING
#ifdef GC_DUMP_REGULARLY
    GC_INNER GC_bool GC_dump_regularly = TRUE;
#else
    GC_INNER GC_bool GC_dump_regularly = FALSE;
#endif
#ifndef NO_CLOCK
    STATIC CLOCK_TYPE GC_init_time;
#endif
#endif
#ifdef KEEP_BACK_PTRS
  GC_INNER long GC_backtraces = 0;
#endif
#ifdef FIND_LEAK
  int GC_find_leak = 1;
#else
  int GC_find_leak = 0;
#endif
#ifndef SHORT_DBG_HDRS
#ifdef GC_FINDLEAK_DELAY_FREE
    GC_INNER GC_bool GC_findleak_delay_free = TRUE;
#else
    GC_INNER GC_bool GC_findleak_delay_free = FALSE;
#endif
#endif
#ifdef ALL_INTERIOR_POINTERS
  int GC_all_interior_pointers = 1;
#else
  int GC_all_interior_pointers = 0;
#endif
#ifdef FINALIZE_ON_DEMAND
  int GC_finalize_on_demand = 1;
#else
  int GC_finalize_on_demand = 0;
#endif
#ifdef JAVA_FINALIZATION
  int GC_java_finalization = 1;
#else
  int GC_java_finalization = 0;
#endif
GC_finalizer_notifier_proc GC_finalizer_notifier =
                                        (GC_finalizer_notifier_proc)0;
#ifdef GC_FORCE_UNMAP_ON_GCOLLECT
  GC_INNER GC_bool GC_force_unmap_on_gcollect = TRUE;
#else
  GC_INNER GC_bool GC_force_unmap_on_gcollect = FALSE;
#endif
#ifndef GC_LARGE_ALLOC_WARN_INTERVAL
#define GC_LARGE_ALLOC_WARN_INTERVAL 5
#endif
GC_INNER long GC_large_alloc_warn_interval = GC_LARGE_ALLOC_WARN_INTERVAL;
STATIC void * GC_CALLBACK GC_default_oom_fn(
                                        size_t bytes_requested GC_ATTR_UNUSED)
{
    return(0);
}
GC_oom_func GC_oom_fn = GC_default_oom_fn;
#ifdef CAN_HANDLE_FORK
#ifdef HANDLE_FORK
    GC_INNER int GC_handle_fork = 1;
#else
    GC_INNER int GC_handle_fork = FALSE;
#endif
#elif !defined(HAVE_NO_FORK)
  GC_API void GC_CALL GC_atfork_prepare(void)
  {
#ifdef THREADS
      ABORT("fork() handling unsupported");
#endif
  }
  GC_API void GC_CALL GC_atfork_parent(void)
  {
  }
  GC_API void GC_CALL GC_atfork_child(void)
  {
  }
#endif
GC_API void GC_CALL GC_set_handle_fork(int value GC_ATTR_UNUSED)
{
#ifdef CAN_HANDLE_FORK
    if (!GC_is_initialized)
      GC_handle_fork = value >= -1 ? value : 1;
#elif defined(THREADS) || (defined(DARWIN) && defined(MPROTECT_VDB))
    if (!GC_is_initialized && value) {
#ifndef SMALL_CONFIG
        GC_init();
#ifndef THREADS
          if (GC_manual_vdb)
            return;
#endif
#endif
      ABORT("fork() handling unsupported");
    }
#else
#endif
}
STATIC void GC_init_size_map(void)
{
    size_t i;
      GC_size_map[0] = 1;
    for (i = 1; i <= GRANULES_TO_BYTES(TINY_FREELISTS-1) - EXTRA_BYTES; i++) {
        GC_size_map[i] = ROUNDED_UP_GRANULES(i);
#ifndef _MSC_VER
          GC_ASSERT(GC_size_map[i] < TINY_FREELISTS);
#endif
    }
}
#ifndef SMALL_CLEAR_SIZE
#define SMALL_CLEAR_SIZE 256
#endif
#if defined(ALWAYS_SMALL_CLEAR_STACK) || defined(STACK_NOT_SCANNED)
  GC_API void * GC_CALL GC_clear_stack(void *arg)
  {
#ifndef STACK_NOT_SCANNED
      word volatile dummy[SMALL_CLEAR_SIZE];
      BZERO(( void *)dummy, sizeof(dummy));
#endif
    return arg;
  }
#else
#ifdef THREADS
#define BIG_CLEAR_SIZE 2048
#else
    STATIC word GC_stack_last_cleared = 0;
    STATIC ptr_t GC_min_sp = NULL;
    STATIC ptr_t GC_high_water = NULL;
    STATIC word GC_bytes_allocd_at_reset = 0;
#define DEGRADE_RATE 50
#endif
#if defined(ASM_CLEAR_CODE)
    void *GC_clear_stack_inner(void *, ptr_t);
#else
    void *GC_clear_stack_inner(void *arg,
#if defined(__APPLE_CC__) && !GC_CLANG_PREREQ(6, 0)
                               volatile
#endif
                               ptr_t limit)
    {
#define CLEAR_SIZE 213
      volatile word dummy[CLEAR_SIZE];
      BZERO(( void *)dummy, sizeof(dummy));
      if ((word)GC_approx_sp() COOLER_THAN (word)limit) {
        (void)GC_clear_stack_inner(arg, limit);
      }
#if defined(CPPCHECK)
        GC_noop1(dummy[0]);
#else
        GC_noop1(COVERT_DATAFLOW(dummy));
#endif
      return(arg);
    }
#endif
#ifdef THREADS
    GC_ATTR_NO_SANITIZE_THREAD
    static unsigned next_random_no(void)
    {
      static unsigned random_no = 0;
      return ++random_no % 13;
    }
#endif
  GC_API void * GC_CALL GC_clear_stack(void *arg)
  {
    ptr_t sp = GC_approx_sp();
#ifdef THREADS
        word volatile dummy[SMALL_CLEAR_SIZE];
#endif
#define SLOP 400
#define GC_SLOP 4000
#define CLEAR_THRESHOLD 100000
#ifdef THREADS
      if (next_random_no() == 0) {
        ptr_t limit = sp;
        MAKE_HOTTER(limit, BIG_CLEAR_SIZE*sizeof(word));
        limit = (ptr_t)((word)limit & ~0xf);
        return GC_clear_stack_inner(arg, limit);
      }
      BZERO((void *)dummy, SMALL_CLEAR_SIZE*sizeof(word));
#else
      if (GC_gc_no > GC_stack_last_cleared) {
        if (GC_stack_last_cleared == 0)
          GC_high_water = (ptr_t)GC_stackbottom;
        GC_min_sp = GC_high_water;
        GC_stack_last_cleared = GC_gc_no;
        GC_bytes_allocd_at_reset = GC_bytes_allocd;
      }
      MAKE_COOLER(GC_high_water, WORDS_TO_BYTES(DEGRADE_RATE) + GC_SLOP);
      if ((word)sp HOTTER_THAN (word)GC_high_water) {
          GC_high_water = sp;
      }
      MAKE_HOTTER(GC_high_water, GC_SLOP);
      {
        ptr_t limit = GC_min_sp;
        MAKE_HOTTER(limit, SLOP);
        if ((word)sp COOLER_THAN (word)limit) {
          limit = (ptr_t)((word)limit & ~0xf);
          GC_min_sp = sp;
          return GC_clear_stack_inner(arg, limit);
        }
      }
      if (GC_bytes_allocd - GC_bytes_allocd_at_reset > CLEAR_THRESHOLD) {
        GC_min_sp = sp;
        MAKE_HOTTER(GC_min_sp, CLEAR_THRESHOLD/4);
        if ((word)GC_min_sp HOTTER_THAN (word)GC_high_water)
          GC_min_sp = GC_high_water;
        GC_bytes_allocd_at_reset = GC_bytes_allocd;
      }
#endif
    return arg;
  }
#endif
GC_API void * GC_CALL GC_base(void * p)
{
    ptr_t r;
    struct hblk *h;
    bottom_index *bi;
    hdr *candidate_hdr;
    r = (ptr_t)p;
    if (!EXPECT(GC_is_initialized, TRUE)) return 0;
    h = HBLKPTR(r);
    GET_BI(r, bi);
    candidate_hdr = HDR_FROM_BI(bi, r);
    if (candidate_hdr == 0) return(0);
        while (IS_FORWARDING_ADDR_OR_NIL(candidate_hdr)) {
           h = FORWARDED_ADDR(h,candidate_hdr);
           r = (ptr_t)h;
           candidate_hdr = HDR(h);
        }
    if (HBLK_IS_FREE(candidate_hdr)) return(0);
        r = (ptr_t)((word)r & ~(WORDS_TO_BYTES(1) - 1));
        {
            size_t offset = HBLKDISPL(r);
            word sz = candidate_hdr -> hb_sz;
            size_t obj_displ = offset % sz;
            ptr_t limit;
            r -= obj_displ;
            limit = r + sz;
            if ((word)limit > (word)(h + 1) && sz <= HBLKSIZE) {
                return(0);
            }
            if ((word)p >= (word)limit) return(0);
        }
    return((void *)r);
}
GC_API int GC_CALL GC_is_heap_ptr(const void *p)
{
    bottom_index *bi;
    GC_ASSERT(GC_is_initialized);
    GET_BI(p, bi);
    return HDR_FROM_BI(bi, p) != 0;
}
GC_API size_t GC_CALL GC_size(const void * p)
{
    hdr * hhdr = HDR(p);
    return (size_t)hhdr->hb_sz;
}
GC_API size_t GC_CALL GC_get_heap_size(void)
{
    return (size_t)(GC_heapsize - GC_unmapped_bytes);
}
GC_API size_t GC_CALL GC_get_obtained_from_os_bytes(void)
{
    return (size_t)GC_our_mem_bytes;
}
GC_API size_t GC_CALL GC_get_free_bytes(void)
{
    return (size_t)(GC_large_free_bytes - GC_unmapped_bytes);
}
GC_API size_t GC_CALL GC_get_unmapped_bytes(void)
{
    return (size_t)GC_unmapped_bytes;
}
GC_API size_t GC_CALL GC_get_bytes_since_gc(void)
{
    return (size_t)GC_bytes_allocd;
}
GC_API size_t GC_CALL GC_get_total_bytes(void)
{
    return (size_t)(GC_bytes_allocd + GC_bytes_allocd_before_gc);
}
#ifndef GC_GET_HEAP_USAGE_NOT_NEEDED
GC_API size_t GC_CALL GC_get_size_map_at(int i)
{
  if ((unsigned)i > MAXOBJBYTES)
    return GC_SIZE_MAX;
  return GRANULES_TO_BYTES(GC_size_map[i]);
}
GC_API void GC_CALL GC_get_heap_usage_safe(GC_word *pheap_size,
                        GC_word *pfree_bytes, GC_word *punmapped_bytes,
                        GC_word *pbytes_since_gc, GC_word *ptotal_bytes)
{
  DCL_LOCK_STATE;
  LOCK();
  if (pheap_size != NULL)
    *pheap_size = GC_heapsize - GC_unmapped_bytes;
  if (pfree_bytes != NULL)
    *pfree_bytes = GC_large_free_bytes - GC_unmapped_bytes;
  if (punmapped_bytes != NULL)
    *punmapped_bytes = GC_unmapped_bytes;
  if (pbytes_since_gc != NULL)
    *pbytes_since_gc = GC_bytes_allocd;
  if (ptotal_bytes != NULL)
    *ptotal_bytes = GC_bytes_allocd + GC_bytes_allocd_before_gc;
  UNLOCK();
}
  GC_INNER word GC_reclaimed_bytes_before_gc = 0;
  static void fill_prof_stats(struct GC_prof_stats_s *pstats)
  {
    pstats->heapsize_full = GC_heapsize;
    pstats->free_bytes_full = GC_large_free_bytes;
    pstats->unmapped_bytes = GC_unmapped_bytes;
    pstats->bytes_allocd_since_gc = GC_bytes_allocd;
    pstats->allocd_bytes_before_gc = GC_bytes_allocd_before_gc;
    pstats->non_gc_bytes = GC_non_gc_bytes;
    pstats->gc_no = GC_gc_no;
#ifdef PARALLEL_MARK
      pstats->markers_m1 = (word)((signed_word)GC_markers_m1);
#else
      pstats->markers_m1 = 0;
#endif
    pstats->bytes_reclaimed_since_gc = GC_bytes_found > 0 ?
                                        (word)GC_bytes_found : 0;
    pstats->reclaimed_bytes_before_gc = GC_reclaimed_bytes_before_gc;
    pstats->expl_freed_bytes_since_gc = GC_bytes_freed;
    pstats->obtained_from_os_bytes = GC_our_mem_bytes;
  }
#include <string.h>
  GC_API size_t GC_CALL GC_get_prof_stats(struct GC_prof_stats_s *pstats,
                                          size_t stats_sz)
  {
    struct GC_prof_stats_s stats;
    DCL_LOCK_STATE;
    LOCK();
    fill_prof_stats(stats_sz >= sizeof(stats) ? pstats : &stats);
    UNLOCK();
    if (stats_sz == sizeof(stats)) {
      return sizeof(stats);
    } else if (stats_sz > sizeof(stats)) {
      memset((char *)pstats + sizeof(stats), 0xff, stats_sz - sizeof(stats));
      return sizeof(stats);
    } else {
      if (EXPECT(stats_sz > 0, TRUE))
        BCOPY(&stats, pstats, stats_sz);
      return stats_sz;
    }
  }
#ifdef THREADS
    GC_API size_t GC_CALL GC_get_prof_stats_unsafe(
                                            struct GC_prof_stats_s *pstats,
                                            size_t stats_sz)
    {
      struct GC_prof_stats_s stats;
      if (stats_sz >= sizeof(stats)) {
        fill_prof_stats(pstats);
        if (stats_sz > sizeof(stats))
          memset((char *)pstats + sizeof(stats), 0xff,
                 stats_sz - sizeof(stats));
        return sizeof(stats);
      } else {
        if (EXPECT(stats_sz > 0, TRUE)) {
          fill_prof_stats(&stats);
          BCOPY(&stats, pstats, stats_sz);
        }
        return stats_sz;
      }
    }
#endif
#endif
#if defined(GC_DARWIN_THREADS) || defined(GC_OPENBSD_UTHREADS) \
    || defined(GC_WIN32_THREADS) || (defined(NACL) && defined(THREADS))
  GC_API void GC_CALL GC_set_suspend_signal(int sig GC_ATTR_UNUSED)
  {
  }
  GC_API void GC_CALL GC_set_thr_restart_signal(int sig GC_ATTR_UNUSED)
  {
  }
  GC_API int GC_CALL GC_get_suspend_signal(void)
  {
    return -1;
  }
  GC_API int GC_CALL GC_get_thr_restart_signal(void)
  {
    return -1;
  }
#endif
#if !defined(_MAX_PATH) && (defined(MSWIN32) || defined(MSWINCE) \
                            || defined(CYGWIN32))
#define _MAX_PATH MAX_PATH
#endif
#ifdef GC_READ_ENV_FILE
  STATIC char *GC_envfile_content = NULL;
  STATIC unsigned GC_envfile_length = 0;
#ifndef GC_ENVFILE_MAXLEN
#define GC_ENVFILE_MAXLEN 0x4000
#endif
#define GC_ENV_FILE_EXT ".gc.env"
  STATIC void GC_envfile_init(void)
  {
#if defined(MSWIN32) || defined(MSWINCE) || defined(CYGWIN32)
      HANDLE hFile;
      char *content;
      unsigned ofs;
      unsigned len;
      DWORD nBytesRead;
      TCHAR path[_MAX_PATH + 0x10];
      size_t bytes_to_get;
      len = (unsigned)GetModuleFileName(NULL , path,
                                        _MAX_PATH + 1);
      if (len > 4 && path[len - 4] == (TCHAR)'.') {
        len -= 4;
      }
      BCOPY(TEXT(GC_ENV_FILE_EXT), &path[len], sizeof(TEXT(GC_ENV_FILE_EXT)));
      hFile = CreateFile(path, GENERIC_READ,
                         FILE_SHARE_READ | FILE_SHARE_WRITE,
                         NULL , OPEN_EXISTING,
                         FILE_ATTRIBUTE_NORMAL, NULL );
      if (hFile == INVALID_HANDLE_VALUE)
        return;
      len = (unsigned)GetFileSize(hFile, NULL);
      if (len <= 1 || len >= GC_ENVFILE_MAXLEN) {
        CloseHandle(hFile);
        return;
      }
      GC_ASSERT(GC_page_size != 0);
      bytes_to_get = ROUNDUP_PAGESIZE_IF_MMAP((size_t)len + 1);
      content = (char *)GET_MEM(bytes_to_get);
      if (content == NULL) {
        CloseHandle(hFile);
        return;
      }
      GC_add_to_our_memory(content, bytes_to_get);
      ofs = 0;
      nBytesRead = (DWORD)-1L;
      while (ReadFile(hFile, content + ofs, len - ofs + 1, &nBytesRead,
                      NULL ) && nBytesRead != 0) {
        if ((ofs += nBytesRead) > len)
          break;
      }
      CloseHandle(hFile);
      if (ofs != len || nBytesRead != 0) {
        return;
      }
      content[ofs] = '\0';
      while (ofs-- > 0) {
       if (content[ofs] == '\r' || content[ofs] == '\n')
         content[ofs] = '\0';
      }
      GC_ASSERT(NULL == GC_envfile_content);
      GC_envfile_length = len + 1;
      GC_envfile_content = content;
#endif
  }
  GC_INNER char * GC_envfile_getenv(const char *name)
  {
    char *p;
    char *end_of_content;
    unsigned namelen;
#ifndef NO_GETENV
      p = getenv(name);
      if (p != NULL)
        return *p != '\0' ? p : NULL;
#endif
    p = GC_envfile_content;
    if (p == NULL)
      return NULL;
    namelen = strlen(name);
    if (namelen == 0)
      return NULL;
    for (end_of_content = p + GC_envfile_length;
         p != end_of_content; p += strlen(p) + 1) {
      if (strncmp(p, name, namelen) == 0 && *(p += namelen) == '=') {
        p++;
        return *p != '\0' ? p : NULL;
      }
    }
    return NULL;
  }
#endif
GC_INNER GC_bool GC_is_initialized = FALSE;
GC_API int GC_CALL GC_is_init_called(void)
{
  return GC_is_initialized;
}
#if defined(GC_WIN32_THREADS) \
    && ((defined(MSWIN32) && !defined(CONSOLE_LOG)) || defined(MSWINCE))
  GC_INNER CRITICAL_SECTION GC_write_cs;
#endif
#ifndef DONT_USE_ATEXIT
#if !defined(PCR) && !defined(SMALL_CONFIG)
    static GC_bool skip_gc_atexit = FALSE;
#else
#define skip_gc_atexit FALSE
#endif
  STATIC void GC_exit_check(void)
  {
    if (GC_find_leak && !skip_gc_atexit) {
#ifdef THREADS
        GC_in_thread_creation = TRUE;
        GC_gcollect();
        GC_in_thread_creation = FALSE;
#else
        GC_gcollect();
#endif
    }
  }
#endif
#if defined(UNIX_LIKE) && !defined(NO_DEBUGGING)
  static void looping_handler(int sig)
  {
    GC_err_printf("Caught signal %d: looping in handler\n", sig);
    for (;;) {
    }
  }
  static GC_bool installed_looping_handler = FALSE;
  static void maybe_install_looping_handler(void)
  {
    if (!installed_looping_handler && 0 != GETENV("GC_LOOP_ON_ABORT")) {
      GC_set_and_save_fault_handler(looping_handler);
      installed_looping_handler = TRUE;
    }
  }
#else
#define maybe_install_looping_handler()
#endif
#define GC_DEFAULT_STDOUT_FD 1
#define GC_DEFAULT_STDERR_FD 2
#if !defined(OS2) && !defined(MACOS) && !defined(GC_ANDROID_LOG) \
    && !defined(NN_PLATFORM_CTR) && !defined(NINTENDO_SWITCH) \
    && (!defined(MSWIN32) || defined(CONSOLE_LOG)) && !defined(MSWINCE)
  STATIC int GC_stdout = GC_DEFAULT_STDOUT_FD;
  STATIC int GC_stderr = GC_DEFAULT_STDERR_FD;
  STATIC int GC_log = GC_DEFAULT_STDERR_FD;
#ifndef MSWIN32
    GC_API void GC_CALL GC_set_log_fd(int fd)
    {
      GC_log = fd;
    }
#endif
#endif
#ifdef MSGBOX_ON_ERROR
  STATIC void GC_win32_MessageBoxA(const char *msg, const char *caption,
                                   unsigned flags)
  {
#ifndef DONT_USE_USER32_DLL
      (void)MessageBoxA(NULL, msg, caption, flags);
#else
      HINSTANCE hU32 = LoadLibrary(TEXT("user32.dll"));
      if (hU32) {
        FARPROC pfn = GetProcAddress(hU32, "MessageBoxA");
        if (pfn)
          (void)(*(int (WINAPI *)(HWND, LPCSTR, LPCSTR, UINT))(word)pfn)(
                              NULL , msg, caption, flags);
        (void)FreeLibrary(hU32);
      }
#endif
  }
#endif
#if defined(THREADS) && defined(UNIX_LIKE) && !defined(NO_GETCONTEXT)
  static void callee_saves_pushed_dummy_fn(ptr_t data GC_ATTR_UNUSED,
                                           void * context GC_ATTR_UNUSED) {}
#endif
#ifndef SMALL_CONFIG
#ifdef MANUAL_VDB
    static GC_bool manual_vdb_allowed = TRUE;
#else
    static GC_bool manual_vdb_allowed = FALSE;
#endif
  GC_API void GC_CALL GC_set_manual_vdb_allowed(int value)
  {
    manual_vdb_allowed = (GC_bool)value;
  }
  GC_API int GC_CALL GC_get_manual_vdb_allowed(void)
  {
    return (int)manual_vdb_allowed;
  }
#endif
STATIC word GC_parse_mem_size_arg(const char *str)
{
  word result = 0;
  if (*str != '\0') {
    char *endptr;
    char ch;
    result = (word)STRTOULL(str, &endptr, 10);
    ch = *endptr;
    if (ch != '\0') {
      if (*(endptr + 1) != '\0')
        return 0;
      switch (ch) {
      case 'K':
      case 'k':
        result <<= 10;
        break;
      case 'M':
      case 'm':
        result <<= 20;
        break;
      case 'G':
      case 'g':
        result <<= 30;
        break;
      default:
        result = 0;
      }
    }
  }
  return result;
}
#define GC_LOG_STD_NAME "gc.log"
GC_API void GC_CALL GC_init(void)
{
    word initial_heap_sz;
    IF_CANCEL(int cancel_state;)
#if defined(GC_ASSERTIONS) && defined(GC_ALWAYS_MULTITHREADED)
      DCL_LOCK_STATE;
#endif
    if (EXPECT(GC_is_initialized, TRUE)) return;
#ifdef REDIRECT_MALLOC
      {
        static GC_bool init_started = FALSE;
        if (init_started)
          ABORT("Redirected malloc() called during GC init");
        init_started = TRUE;
      }
#endif
#if defined(GC_INITIAL_HEAP_SIZE) && !defined(CPPCHECK)
      initial_heap_sz = GC_INITIAL_HEAP_SIZE;
#else
      initial_heap_sz = MINHINCR * HBLKSIZE;
#endif
    DISABLE_CANCEL(cancel_state);
#ifdef THREADS
#ifndef GC_ALWAYS_MULTITHREADED
        GC_ASSERT(!GC_need_to_lock);
#endif
#ifdef SN_TARGET_PS3
        {
          pthread_mutexattr_t mattr;
          if (0 != pthread_mutexattr_init(&mattr)) {
            ABORT("pthread_mutexattr_init failed");
          }
          if (0 != pthread_mutex_init(&GC_allocate_ml, &mattr)) {
            ABORT("pthread_mutex_init failed");
          }
          (void)pthread_mutexattr_destroy(&mattr);
        }
#endif
#endif
#if defined(GC_WIN32_THREADS) && !defined(GC_PTHREADS)
#ifndef SPIN_COUNT
#define SPIN_COUNT 4000
#endif
#ifdef MSWINRT_FLAVOR
        InitializeCriticalSectionAndSpinCount(&GC_allocate_ml, SPIN_COUNT);
#else
        {
#ifndef MSWINCE
            FARPROC pfn = 0;
            HMODULE hK32 = GetModuleHandle(TEXT("kernel32.dll"));
            if (hK32)
              pfn = GetProcAddress(hK32,
                                   "InitializeCriticalSectionAndSpinCount");
            if (pfn) {
              (*(BOOL (WINAPI *)(LPCRITICAL_SECTION, DWORD))(word)pfn)(
                                &GC_allocate_ml, SPIN_COUNT);
            } else
#endif
           InitializeCriticalSection(&GC_allocate_ml);
        }
#endif
#endif
#if defined(GC_WIN32_THREADS) \
       && ((defined(MSWIN32) && !defined(CONSOLE_LOG)) || defined(MSWINCE))
      InitializeCriticalSection(&GC_write_cs);
#endif
    GC_setpagesize();
#ifdef MSWIN32
      GC_init_win32();
#endif
#ifdef GC_READ_ENV_FILE
      GC_envfile_init();
#endif
#if !defined(NO_CLOCK) || !defined(SMALL_CONFIG)
#ifdef GC_PRINT_VERBOSE_STATS
        GC_print_stats = VERBOSE;
#else
        if (0 != GETENV("GC_PRINT_VERBOSE_STATS")) {
          GC_print_stats = VERBOSE;
        } else if (0 != GETENV("GC_PRINT_STATS")) {
          GC_print_stats = 1;
        }
#endif
#endif
#if ((defined(UNIX_LIKE) && !defined(GC_ANDROID_LOG)) \
        || (defined(CONSOLE_LOG) && defined(MSWIN32)) \
        || defined(CYGWIN32) || defined(SYMBIAN)) && !defined(SMALL_CONFIG)
        {
          char * file_name = TRUSTED_STRING(GETENV("GC_LOG_FILE"));
#ifdef GC_LOG_TO_FILE_ALWAYS
            if (NULL == file_name)
              file_name = GC_LOG_STD_NAME;
#else
            if (0 != file_name)
#endif
          {
#if defined(_MSC_VER)
              int log_d = _open(file_name, O_CREAT | O_WRONLY | O_APPEND);
#else
              int log_d = open(file_name, O_CREAT | O_WRONLY | O_APPEND, 0644);
#endif
            if (log_d < 0) {
              GC_err_printf("Failed to open %s as log file\n", file_name);
            } else {
              char *str;
              GC_log = log_d;
              str = GETENV("GC_ONLY_LOG_TO_FILE");
#ifdef GC_ONLY_LOG_TO_FILE
                if (str != NULL && *str == '0' && *(str + 1) == '\0')
#else
                if (str == NULL || (*str == '0' && *(str + 1) == '\0'))
#endif
              {
                GC_stdout = log_d;
                GC_stderr = log_d;
              }
            }
          }
        }
#endif
#if !defined(NO_DEBUGGING) && !defined(GC_DUMP_REGULARLY)
      if (0 != GETENV("GC_DUMP_REGULARLY")) {
        GC_dump_regularly = TRUE;
      }
#endif
#ifdef KEEP_BACK_PTRS
      {
        char * backtraces_string = GETENV("GC_BACKTRACES");
        if (0 != backtraces_string) {
          GC_backtraces = atol(backtraces_string);
          if (backtraces_string[0] == '\0') GC_backtraces = 1;
        }
      }
#endif
    if (0 != GETENV("GC_FIND_LEAK")) {
      GC_find_leak = 1;
    }
#ifndef SHORT_DBG_HDRS
      if (0 != GETENV("GC_FINDLEAK_DELAY_FREE")) {
        GC_findleak_delay_free = TRUE;
      }
#endif
    if (0 != GETENV("GC_ALL_INTERIOR_POINTERS")) {
      GC_all_interior_pointers = 1;
    }
    if (0 != GETENV("GC_DONT_GC")) {
#ifdef LINT2
        GC_disable();
#else
        GC_dont_gc = 1;
#endif
    }
    if (0 != GETENV("GC_PRINT_BACK_HEIGHT")) {
      GC_print_back_height = TRUE;
    }
    if (0 != GETENV("GC_NO_BLACKLIST_WARNING")) {
      GC_large_alloc_warn_interval = LONG_MAX;
    }
    {
      char * addr_string = GETENV("GC_TRACE");
      if (0 != addr_string) {
#ifndef ENABLE_TRACE
          WARN("Tracing not enabled: Ignoring GC_TRACE value\n", 0);
#else
          word addr = (word)STRTOULL(addr_string, NULL, 16);
          if (addr < 0x1000)
              WARN("Unlikely trace address: %p\n", (void *)addr);
          GC_trace_addr = (ptr_t)addr;
#endif
      }
    }
#ifdef GC_COLLECT_AT_MALLOC
      {
        char * string = GETENV("GC_COLLECT_AT_MALLOC");
        if (0 != string) {
          size_t min_lb = (size_t)STRTOULL(string, NULL, 10);
          if (min_lb > 0)
            GC_dbg_collect_at_malloc_min_lb = min_lb;
        }
      }
#endif
#if !defined(GC_DISABLE_INCREMENTAL) && !defined(NO_CLOCK)
      {
        char * time_limit_string = GETENV("GC_PAUSE_TIME_TARGET");
        if (0 != time_limit_string) {
          long time_limit = atol(time_limit_string);
          if (time_limit > 0) {
            GC_time_limit = time_limit;
          }
        }
      }
#endif
#ifndef SMALL_CONFIG
      {
        char * full_freq_string = GETENV("GC_FULL_FREQUENCY");
        if (full_freq_string != NULL) {
          int full_freq = atoi(full_freq_string);
          if (full_freq > 0)
            GC_full_freq = full_freq;
        }
      }
#endif
    {
      char * interval_string = GETENV("GC_LARGE_ALLOC_WARN_INTERVAL");
      if (0 != interval_string) {
        long interval = atol(interval_string);
        if (interval <= 0) {
          WARN("GC_LARGE_ALLOC_WARN_INTERVAL environment variable has "
               "bad value: Ignoring\n", 0);
        } else {
          GC_large_alloc_warn_interval = interval;
        }
      }
    }
    {
        char * space_divisor_string = GETENV("GC_FREE_SPACE_DIVISOR");
        if (space_divisor_string != NULL) {
          int space_divisor = atoi(space_divisor_string);
          if (space_divisor > 0)
            GC_free_space_divisor = (unsigned)space_divisor;
        }
    }
#ifdef USE_MUNMAP
      {
        char * string = GETENV("GC_UNMAP_THRESHOLD");
        if (string != NULL) {
          if (*string == '0' && *(string + 1) == '\0') {
            GC_unmap_threshold = 0;
          } else {
            int unmap_threshold = atoi(string);
            if (unmap_threshold > 0)
              GC_unmap_threshold = unmap_threshold;
          }
        }
      }
      {
        char * string = GETENV("GC_FORCE_UNMAP_ON_GCOLLECT");
        if (string != NULL) {
          if (*string == '0' && *(string + 1) == '\0') {
            GC_force_unmap_on_gcollect = FALSE;
          } else {
            GC_force_unmap_on_gcollect = TRUE;
          }
        }
      }
      {
        char * string = GETENV("GC_USE_ENTIRE_HEAP");
        if (string != NULL) {
          if (*string == '0' && *(string + 1) == '\0') {
            GC_use_entire_heap = FALSE;
          } else {
            GC_use_entire_heap = TRUE;
          }
        }
      }
#endif
#if !defined(NO_DEBUGGING) && !defined(NO_CLOCK)
      GET_TIME(GC_init_time);
#endif
    maybe_install_looping_handler();
#if ALIGNMENT > GC_DS_TAGS
      if (EXTRA_BYTES != 0)
        GC_obj_kinds[NORMAL].ok_descriptor = (word)(-ALIGNMENT) | GC_DS_LENGTH;
#endif
    GC_exclude_static_roots_inner(beginGC_arrays, endGC_arrays);
    GC_exclude_static_roots_inner(beginGC_obj_kinds, endGC_obj_kinds);
#ifdef SEPARATE_GLOBALS
      GC_exclude_static_roots_inner(beginGC_objfreelist, endGC_objfreelist);
      GC_exclude_static_roots_inner(beginGC_aobjfreelist, endGC_aobjfreelist);
#endif
#if defined(USE_PROC_FOR_LIBRARIES) && defined(GC_LINUX_THREADS)
        WARN("USE_PROC_FOR_LIBRARIES + GC_LINUX_THREADS performs poorly.\n", 0);
#endif
#if defined(SEARCH_FOR_DATA_START)
        GC_init_linux_data_start();
#endif
#if defined(NETBSD) && defined(__ELF__)
        GC_init_netbsd_elf();
#endif
#if !defined(THREADS) || defined(GC_PTHREADS) \
        || defined(NN_PLATFORM_CTR) || defined(NINTENDO_SWITCH) \
        || defined(GC_WIN32_THREADS) || defined(GC_SOLARIS_THREADS)
      if (GC_stackbottom == 0) {
        GC_stackbottom = GC_get_main_stack_base();
#if (defined(LINUX) || defined(HPUX)) && defined(IA64)
          GC_register_stackbottom = GC_get_register_stack_base();
#endif
      } else {
#if (defined(LINUX) || defined(HPUX)) && defined(IA64)
          if (GC_register_stackbottom == 0) {
            WARN("GC_register_stackbottom should be set with GC_stackbottom\n", 0);
            GC_register_stackbottom = GC_get_register_stack_base();
          }
#endif
      }
#endif
#if !defined(CPPCHECK)
      GC_STATIC_ASSERT(sizeof(ptr_t) == sizeof(word));
      GC_STATIC_ASSERT(sizeof(signed_word) == sizeof(word));
#if !defined(_AUX_SOURCE) || defined(__GNUC__)
        GC_STATIC_ASSERT((word)(-1) > (word)0);
#endif
      GC_STATIC_ASSERT((signed_word)(-1) < (signed_word)0);
#endif
    GC_STATIC_ASSERT(sizeof (struct hblk) == HBLKSIZE);
#ifndef THREADS
      GC_ASSERT(!((word)GC_stackbottom HOTTER_THAN (word)GC_approx_sp()));
#endif
    GC_init_headers();
#ifndef GC_DISABLE_INCREMENTAL
      if (GC_incremental || 0 != GETENV("GC_ENABLE_INCREMENTAL")) {
#if defined(BASE_ATOMIC_OPS_EMULATED) || defined(CHECKSUMS) \
           || defined(REDIRECT_MALLOC) || defined(REDIRECT_MALLOC_IN_HEADER) \
           || defined(SMALL_CONFIG)
#else
          if (manual_vdb_allowed) {
              GC_manual_vdb = TRUE;
              GC_incremental = TRUE;
          } else
#endif
         {
          GC_incremental = GC_dirty_init();
          GC_ASSERT(GC_bytes_allocd == 0);
        }
      }
#endif
      if (GC_REGISTER_MAIN_STATIC_DATA()) GC_register_data_segments();
    GC_bl_init();
    GC_mark_init();
    {
        char * sz_str = GETENV("GC_INITIAL_HEAP_SIZE");
        if (sz_str != NULL) {
          initial_heap_sz = GC_parse_mem_size_arg(sz_str);
          if (initial_heap_sz <= MINHINCR * HBLKSIZE) {
            WARN("Bad initial heap size %s - ignoring it.\n", sz_str);
          }
        }
    }
    {
        char * sz_str = GETENV("GC_MAXIMUM_HEAP_SIZE");
        if (sz_str != NULL) {
          word max_heap_sz = GC_parse_mem_size_arg(sz_str);
          if (max_heap_sz < initial_heap_sz) {
            WARN("Bad maximum heap size %s - ignoring it.\n", sz_str);
          }
          if (0 == GC_max_retries) GC_max_retries = 2;
          GC_set_max_heap_size(max_heap_sz);
        }
    }
#if defined(GC_ASSERTIONS) && defined(GC_ALWAYS_MULTITHREADED)
        LOCK();
#endif
    if (!GC_expand_hp_inner(divHBLKSZ(initial_heap_sz))) {
        GC_err_printf("Can't start up: not enough memory\n");
        EXIT();
    } else {
        GC_requested_heapsize += initial_heap_sz;
    }
    if (GC_all_interior_pointers)
      GC_initialize_offsets();
    GC_register_displacement_inner(0L);
#if defined(GC_LINUX_THREADS) && defined(REDIRECT_MALLOC)
      if (!GC_all_interior_pointers) {
        GC_register_displacement_inner(sizeof(void *));
      }
#endif
    GC_init_size_map();
#ifdef PCR
      if (PCR_IL_Lock(PCR_Bool_false, PCR_allSigsBlocked, PCR_waitForever)
          != PCR_ERes_okay) {
          ABORT("Can't lock load state");
      } else if (PCR_IL_Unlock() != PCR_ERes_okay) {
          ABORT("Can't unlock load state");
      }
      PCR_IL_Unlock();
      GC_pcr_install();
#endif
    GC_is_initialized = TRUE;
#if defined(GC_PTHREADS) || defined(GC_WIN32_THREADS)
#if defined(LINT2) \
           && !(defined(GC_ASSERTIONS) && defined(GC_ALWAYS_MULTITHREADED))
          LOCK();
          GC_thr_init();
          UNLOCK();
#else
          GC_thr_init();
#endif
#ifdef PARALLEL_MARK
#if defined(GC_ASSERTIONS) && defined(GC_ALWAYS_MULTITHREADED)
            UNLOCK();
#endif
          GC_start_mark_threads_inner();
#if defined(GC_ASSERTIONS) && defined(GC_ALWAYS_MULTITHREADED)
            LOCK();
#endif
#endif
#endif
    COND_DUMP;
    if (!GC_dont_precollect || GC_incremental) {
        GC_gcollect_inner();
    }
#if defined(GC_ASSERTIONS) && defined(GC_ALWAYS_MULTITHREADED)
        UNLOCK();
#endif
#if defined(THREADS) && defined(UNIX_LIKE) && !defined(NO_GETCONTEXT)
      if (GC_dont_gc || GC_dont_precollect)
        GC_with_callee_saves_pushed(callee_saves_pushed_dummy_fn, NULL);
#endif
#ifndef DONT_USE_ATEXIT
      if (GC_find_leak) {
        atexit(GC_exit_check);
      }
#endif
#if defined(PARALLEL_MARK) || defined(THREAD_LOCAL_ALLOC) \
       || (defined(GC_ALWAYS_MULTITHREADED) && defined(GC_WIN32_THREADS) \
           && !defined(GC_NO_THREADS_DISCOVERY))
        GC_init_parallel();
#endif
#if defined(DYNAMIC_LOADING) && defined(DARWIN)
        GC_init_dyld();
#endif
    RESTORE_CANCEL(cancel_state);
}
GC_API void GC_CALL GC_enable_incremental(void)
{
#if !defined(GC_DISABLE_INCREMENTAL) && !defined(KEEP_BACK_PTRS)
    DCL_LOCK_STATE;
    if (!GC_find_leak && 0 == GETENV("GC_DISABLE_INCREMENTAL")) {
      LOCK();
      if (!GC_incremental) {
        GC_setpagesize();
        maybe_install_looping_handler();
        if (!GC_is_initialized) {
          UNLOCK();
          GC_incremental = TRUE;
          GC_init();
          LOCK();
        } else {
#if !defined(BASE_ATOMIC_OPS_EMULATED) && !defined(CHECKSUMS) \
             && !defined(REDIRECT_MALLOC) \
             && !defined(REDIRECT_MALLOC_IN_HEADER) && !defined(SMALL_CONFIG)
            if (manual_vdb_allowed) {
              GC_manual_vdb = TRUE;
              GC_incremental = TRUE;
            } else
#endif
           {
            GC_incremental = GC_dirty_init();
          }
        }
        if (GC_incremental && !GC_dont_gc) {
          IF_CANCEL(int cancel_state;)
          DISABLE_CANCEL(cancel_state);
          if (GC_bytes_allocd > 0) {
            GC_gcollect_inner();
          }
          GC_read_dirty(FALSE);
          RESTORE_CANCEL(cancel_state);
        }
      }
      UNLOCK();
      return;
    }
#endif
  GC_init();
}
#if defined(THREADS)
  GC_API void GC_CALL GC_start_mark_threads(void)
  {
#if defined(PARALLEL_MARK) && defined(CAN_HANDLE_FORK) \
       && !defined(THREAD_SANITIZER)
      IF_CANCEL(int cancel_state;)
      DISABLE_CANCEL(cancel_state);
      GC_start_mark_threads_inner();
      RESTORE_CANCEL(cancel_state);
#else
      GC_ASSERT(I_DONT_HOLD_LOCK());
#endif
  }
#endif
  GC_API void GC_CALL GC_deinit(void)
  {
    if (GC_is_initialized) {
      GC_is_initialized = FALSE;
#if defined(GC_WIN32_THREADS) && (defined(MSWIN32) || defined(MSWINCE))
#if !defined(CONSOLE_LOG) || defined(MSWINCE)
          DeleteCriticalSection(&GC_write_cs);
#endif
        DeleteCriticalSection(&GC_allocate_ml);
#endif
    }
  }
#if (defined(MSWIN32) && !defined(CONSOLE_LOG)) || defined(MSWINCE)
#if defined(_MSC_VER) && defined(_DEBUG) && !defined(MSWINCE)
#include <crtdbg.h>
#endif
  STATIC HANDLE GC_log = 0;
#ifdef THREADS
#if defined(PARALLEL_MARK) && !defined(GC_ALWAYS_MULTITHREADED)
#define IF_NEED_TO_LOCK(x) if (GC_parallel || GC_need_to_lock) x
#else
#define IF_NEED_TO_LOCK(x) if (GC_need_to_lock) x
#endif
#else
#define IF_NEED_TO_LOCK(x)
#endif
#ifdef MSWINRT_FLAVOR
#include <windows.storage.h>
    DECLSPEC_IMPORT HRESULT WINAPI RoGetActivationFactory(
                                        HSTRING activatableClassId,
                                        REFIID iid, void** factory);
    static GC_bool getWinRTLogPath(wchar_t* buf, size_t bufLen)
    {
      static const GUID kIID_IApplicationDataStatics = {
        0x5612147B, 0xE843, 0x45E3,
        0x94, 0xD8, 0x06, 0x16, 0x9E, 0x3C, 0x8E, 0x17
      };
      static const GUID kIID_IStorageItem = {
        0x4207A996, 0xCA2F, 0x42F7,
        0xBD, 0xE8, 0x8B, 0x10, 0x45, 0x7A, 0x7F, 0x30
      };
      GC_bool result = FALSE;
      HSTRING_HEADER appDataClassNameHeader;
      HSTRING appDataClassName;
      __x_ABI_CWindows_CStorage_CIApplicationDataStatics* appDataStatics = 0;
      GC_ASSERT(bufLen > 0);
      if (SUCCEEDED(WindowsCreateStringReference(
                      RuntimeClass_Windows_Storage_ApplicationData,
                      (sizeof(RuntimeClass_Windows_Storage_ApplicationData)-1)
                        / sizeof(wchar_t),
                      &appDataClassNameHeader, &appDataClassName))
          && SUCCEEDED(RoGetActivationFactory(appDataClassName,
                                              &kIID_IApplicationDataStatics,
                                              &appDataStatics))) {
        __x_ABI_CWindows_CStorage_CIApplicationData* appData = NULL;
        __x_ABI_CWindows_CStorage_CIStorageFolder* tempFolder = NULL;
        __x_ABI_CWindows_CStorage_CIStorageItem* tempFolderItem = NULL;
        HSTRING tempPath = NULL;
        if (SUCCEEDED(appDataStatics->lpVtbl->get_Current(appDataStatics,
                                                          &appData))
            && SUCCEEDED(appData->lpVtbl->get_TemporaryFolder(appData,
                                                              &tempFolder))
            && SUCCEEDED(tempFolder->lpVtbl->QueryInterface(tempFolder,
                                                        &kIID_IStorageItem,
                                                        &tempFolderItem))
            && SUCCEEDED(tempFolderItem->lpVtbl->get_Path(tempFolderItem,
                                                          &tempPath))) {
          UINT32 tempPathLen;
          const wchar_t* tempPathBuf =
                          WindowsGetStringRawBuffer(tempPath, &tempPathLen);
          buf[0] = '\0';
          if (wcsncat_s(buf, bufLen, tempPathBuf, tempPathLen) == 0
              && wcscat_s(buf, bufLen, L"\\") == 0
              && wcscat_s(buf, bufLen, TEXT(GC_LOG_STD_NAME)) == 0)
            result = TRUE;
          WindowsDeleteString(tempPath);
        }
        if (tempFolderItem != NULL)
          tempFolderItem->lpVtbl->Release(tempFolderItem);
        if (tempFolder != NULL)
          tempFolder->lpVtbl->Release(tempFolder);
        if (appData != NULL)
          appData->lpVtbl->Release(appData);
        appDataStatics->lpVtbl->Release(appDataStatics);
      }
      return result;
    }
#endif
  STATIC HANDLE GC_CreateLogFile(void)
  {
    HANDLE hFile;
#ifdef MSWINRT_FLAVOR
      TCHAR pathBuf[_MAX_PATH + 0x10];
      hFile = INVALID_HANDLE_VALUE;
      if (getWinRTLogPath(pathBuf, _MAX_PATH + 1)) {
        CREATEFILE2_EXTENDED_PARAMETERS extParams;
        BZERO(&extParams, sizeof(extParams));
        extParams.dwSize = sizeof(extParams);
        extParams.dwFileAttributes = FILE_ATTRIBUTE_NORMAL;
        extParams.dwFileFlags = GC_print_stats == VERBOSE ? 0
                                    : FILE_FLAG_WRITE_THROUGH;
        hFile = CreateFile2(pathBuf, GENERIC_WRITE, FILE_SHARE_READ,
                            CREATE_ALWAYS, &extParams);
      }
#else
    TCHAR *logPath;
#if defined(NO_GETENV_WIN32) && defined(CPPCHECK)
#define appendToFile FALSE
#else
      BOOL appendToFile = FALSE;
#endif
#if !defined(NO_GETENV_WIN32) || !defined(OLD_WIN32_LOG_FILE)
      TCHAR pathBuf[_MAX_PATH + 0x10];
      logPath = pathBuf;
#endif
#ifndef NO_GETENV_WIN32
      if (GetEnvironmentVariable(TEXT("GC_LOG_FILE"), pathBuf,
                                 _MAX_PATH + 1) - 1U < (DWORD)_MAX_PATH) {
        appendToFile = TRUE;
      } else
#endif
     {
#ifdef OLD_WIN32_LOG_FILE
        logPath = TEXT(GC_LOG_STD_NAME);
#else
        int len = (int)GetModuleFileName(NULL , pathBuf,
                                         _MAX_PATH + 1);
        if (len > 4 && pathBuf[len - 4] == (TCHAR)'.') {
          len -= 4;
        }
        BCOPY(TEXT(".") TEXT(GC_LOG_STD_NAME), &pathBuf[len],
              sizeof(TEXT(".") TEXT(GC_LOG_STD_NAME)));
#endif
    }
    hFile = CreateFile(logPath, GENERIC_WRITE, FILE_SHARE_READ,
                       NULL ,
                       appendToFile ? OPEN_ALWAYS : CREATE_ALWAYS,
                       GC_print_stats == VERBOSE ? FILE_ATTRIBUTE_NORMAL :
                            FILE_ATTRIBUTE_NORMAL | FILE_FLAG_WRITE_THROUGH,
                       NULL );
#ifndef NO_GETENV_WIN32
      if (appendToFile && hFile != INVALID_HANDLE_VALUE) {
        LONG posHigh = 0;
        (void)SetFilePointer(hFile, 0, &posHigh, FILE_END);
      }
#endif
#undef appendToFile
#endif
    return hFile;
  }
  STATIC int GC_write(const char *buf, size_t len)
  {
      BOOL res;
      DWORD written;
#if defined(THREADS) && defined(GC_ASSERTIONS)
        static GC_bool inside_write = FALSE;
        if (inside_write)
          return -1;
#endif
      if (len == 0)
          return 0;
      IF_NEED_TO_LOCK(EnterCriticalSection(&GC_write_cs));
#if defined(THREADS) && defined(GC_ASSERTIONS)
        if (GC_write_disabled) {
          inside_write = TRUE;
          ABORT("Assertion failure: GC_write called with write_disabled");
        }
#endif
      if (GC_log == 0) {
        GC_log = GC_CreateLogFile();
      }
      if (GC_log == INVALID_HANDLE_VALUE) {
        IF_NEED_TO_LOCK(LeaveCriticalSection(&GC_write_cs));
#ifdef NO_DEBUGGING
          return 0;
#else
          return -1;
#endif
      }
      res = WriteFile(GC_log, buf, (DWORD)len, &written, NULL);
#if defined(_MSC_VER) && defined(_DEBUG) && !defined(NO_CRT)
#ifdef MSWINCE
              {
                  WCHAR wbuf[1024];
                  wbuf[MultiByteToWideChar(CP_ACP, 0 ,
                                buf, len, wbuf,
                                sizeof(wbuf) / sizeof(wbuf[0]) - 1)] = 0;
                  OutputDebugStringW(wbuf);
              }
#else
              _CrtDbgReport(_CRT_WARN, NULL, 0, NULL, "%.*s", len, buf);
#endif
#endif
      IF_NEED_TO_LOCK(LeaveCriticalSection(&GC_write_cs));
      return res ? (int)written : -1;
  }
#define WRITE(f, buf, len) GC_write(buf, len)
#elif defined(OS2) || defined(MACOS)
  STATIC FILE * GC_stdout = NULL;
  STATIC FILE * GC_stderr = NULL;
  STATIC FILE * GC_log = NULL;
  STATIC void GC_set_files(void)
  {
    if (GC_stdout == NULL) {
      GC_stdout = stdout;
    }
    if (GC_stderr == NULL) {
      GC_stderr = stderr;
    }
    if (GC_log == NULL) {
      GC_log = stderr;
    }
  }
  GC_INLINE int GC_write(FILE *f, const char *buf, size_t len)
  {
    int res = fwrite(buf, 1, len, f);
    fflush(f);
    return res;
  }
#define WRITE(f, buf, len) (GC_set_files(), GC_write(f, buf, len))
#elif defined(GC_ANDROID_LOG)
#include <android/log.h>
#ifndef GC_ANDROID_LOG_TAG
#define GC_ANDROID_LOG_TAG "BDWGC"
#endif
#define GC_stdout ANDROID_LOG_DEBUG
#define GC_stderr ANDROID_LOG_ERROR
#define GC_log GC_stdout
#define WRITE(level, buf, unused_len) \
                __android_log_write(level, GC_ANDROID_LOG_TAG, buf)
#elif defined(NN_PLATFORM_CTR)
  int n3ds_log_write(const char* text, int length);
#define WRITE(level, buf, len) n3ds_log_write(buf, len)
#elif defined(NINTENDO_SWITCH)
  int switch_log_write(const char* text, int length);
#define WRITE(level, buf, len) switch_log_write(buf, len)
#else
#if !defined(GC_NO_TYPES) && !defined(SN_TARGET_PSP2)
#if !defined(AMIGA) && !defined(MSWIN32) && !defined(MSWIN_XBOX1) \
       && !defined(__CC_ARM)
#include <unistd.h>
#endif
#if !defined(ECOS) && !defined(NOSYS)
#include <errno.h>
#endif
#endif
  STATIC int GC_write(int fd, const char *buf, size_t len)
  {
#if defined(ECOS) || defined(PLATFORM_WRITE) || defined(SN_TARGET_PSP2) \
       || defined(NOSYS)
#ifdef ECOS
#else
#endif
      return len;
#else
      int bytes_written = 0;
      IF_CANCEL(int cancel_state;)
      DISABLE_CANCEL(cancel_state);
      while ((unsigned)bytes_written < len) {
#ifdef GC_SOLARIS_THREADS
             int result = syscall(SYS_write, fd, buf + bytes_written,
                                             len - bytes_written);
#elif defined(_MSC_VER)
             int result = _write(fd, buf + bytes_written,
                                 (unsigned)(len - bytes_written));
#else
             int result = write(fd, buf + bytes_written, len - bytes_written);
#endif
         if (-1 == result) {
             if (EAGAIN == errno)
               continue;
             RESTORE_CANCEL(cancel_state);
             return(result);
         }
         bytes_written += result;
      }
      RESTORE_CANCEL(cancel_state);
      return(bytes_written);
#endif
  }
#define WRITE(f, buf, len) GC_write(f, buf, len)
#endif
#define BUFSZ 1024
#if defined(DJGPP) || defined(__STRICT_ANSI__)
#define GC_VSNPRINTF(buf, bufsz, format, args) vsprintf(buf, format, args)
#elif defined(_MSC_VER)
#ifdef MSWINCE
#define GC_VSNPRINTF StringCchVPrintfA
#else
#define GC_VSNPRINTF _vsnprintf
#endif
#else
#define GC_VSNPRINTF vsnprintf
#endif
#define GC_PRINTF_FILLBUF(buf, format) \
        do { \
          va_list args; \
          va_start(args, format); \
          (buf)[sizeof(buf) - 1] = 0x15;  \
          (void)GC_VSNPRINTF(buf, sizeof(buf) - 1, format, args); \
          va_end(args); \
          if ((buf)[sizeof(buf) - 1] != 0x15) \
            ABORT("GC_printf clobbered stack"); \
        } while (0)
void GC_printf(const char *format, ...)
{
    if (!GC_quiet) {
      char buf[BUFSZ + 1];
      GC_PRINTF_FILLBUF(buf, format);
#ifdef NACL
        (void)WRITE(GC_stdout, buf, strlen(buf));
#else
        if (WRITE(GC_stdout, buf, strlen(buf)) < 0
#if defined(CYGWIN32) || (defined(CONSOLE_LOG) && defined(MSWIN32))
              && GC_stdout != GC_DEFAULT_STDOUT_FD
#endif
           ) {
          ABORT("write to stdout failed");
        }
#endif
    }
}
void GC_err_printf(const char *format, ...)
{
    char buf[BUFSZ + 1];
    GC_PRINTF_FILLBUF(buf, format);
    GC_err_puts(buf);
}
void GC_log_printf(const char *format, ...)
{
    char buf[BUFSZ + 1];
    GC_PRINTF_FILLBUF(buf, format);
#ifdef NACL
      (void)WRITE(GC_log, buf, strlen(buf));
#else
      if (WRITE(GC_log, buf, strlen(buf)) < 0
#if defined(CYGWIN32) || (defined(CONSOLE_LOG) && defined(MSWIN32))
            && GC_log != GC_DEFAULT_STDERR_FD
#endif
         ) {
        ABORT("write to GC log failed");
      }
#endif
}
#ifndef GC_ANDROID_LOG
#define GC_warn_printf GC_err_printf
#else
  GC_INNER void GC_info_log_printf(const char *format, ...)
  {
    char buf[BUFSZ + 1];
    GC_PRINTF_FILLBUF(buf, format);
    (void)WRITE(ANDROID_LOG_INFO, buf, 0 );
  }
  GC_INNER void GC_verbose_log_printf(const char *format, ...)
  {
    char buf[BUFSZ + 1];
    GC_PRINTF_FILLBUF(buf, format);
    (void)WRITE(ANDROID_LOG_VERBOSE, buf, 0);
  }
  STATIC void GC_warn_printf(const char *format, ...)
  {
    char buf[BUFSZ + 1];
    GC_PRINTF_FILLBUF(buf, format);
    (void)WRITE(ANDROID_LOG_WARN, buf, 0);
  }
#endif
void GC_err_puts(const char *s)
{
    (void)WRITE(GC_stderr, s, strlen(s));
}
STATIC void GC_CALLBACK GC_default_warn_proc(char *msg, GC_word arg)
{
    GC_warn_printf(msg, arg);
}
GC_INNER GC_warn_proc GC_current_warn_proc = GC_default_warn_proc;
GC_API void GC_CALLBACK GC_ignore_warn_proc(char *msg, GC_word arg)
{
    if (GC_print_stats) {
      GC_default_warn_proc(msg, arg);
    }
}
GC_API void GC_CALL GC_set_warn_proc(GC_warn_proc p)
{
    DCL_LOCK_STATE;
    GC_ASSERT(NONNULL_ARG_NOT_NULL(p));
#ifdef GC_WIN32_THREADS
#ifdef CYGWIN32
        GC_ASSERT(GC_is_initialized);
#else
        if (!GC_is_initialized) GC_init();
#endif
#endif
    LOCK();
    GC_current_warn_proc = p;
    UNLOCK();
}
GC_API GC_warn_proc GC_CALL GC_get_warn_proc(void)
{
    GC_warn_proc result;
    DCL_LOCK_STATE;
    LOCK();
    result = GC_current_warn_proc;
    UNLOCK();
    return(result);
}
#if !defined(PCR) && !defined(SMALL_CONFIG)
  STATIC void GC_CALLBACK GC_default_on_abort(const char *msg)
  {
#ifndef DONT_USE_ATEXIT
      skip_gc_atexit = TRUE;
#endif
    if (msg != NULL) {
#ifdef MSGBOX_ON_ERROR
        GC_win32_MessageBoxA(msg, "Fatal error in GC", MB_ICONERROR | MB_OK);
#endif
#ifndef GC_ANDROID_LOG
#if defined(GC_WIN32_THREADS) && defined(GC_ASSERTIONS) \
         && ((defined(MSWIN32) && !defined(CONSOLE_LOG)) || defined(MSWINCE))
        if (!GC_write_disabled)
#endif
      {
        if (WRITE(GC_stderr, msg, strlen(msg)) >= 0)
          (void)WRITE(GC_stderr, "\n", 1);
      }
#else
      __android_log_assert("*" , GC_ANDROID_LOG_TAG, "%s\n", msg);
#endif
    }
#if !defined(NO_DEBUGGING) && !defined(GC_ANDROID_LOG)
      if (GETENV("GC_LOOP_ON_ABORT") != NULL) {
            for(;;) {
            }
      }
#endif
  }
  GC_abort_func GC_on_abort = GC_default_on_abort;
  GC_API void GC_CALL GC_set_abort_func(GC_abort_func fn)
  {
      DCL_LOCK_STATE;
      GC_ASSERT(NONNULL_ARG_NOT_NULL(fn));
      LOCK();
      GC_on_abort = fn;
      UNLOCK();
  }
  GC_API GC_abort_func GC_CALL GC_get_abort_func(void)
  {
      GC_abort_func fn;
      DCL_LOCK_STATE;
      LOCK();
      fn = GC_on_abort;
      UNLOCK();
      return fn;
  }
#endif
GC_API void GC_CALL GC_enable(void)
{
    DCL_LOCK_STATE;
    LOCK();
    GC_ASSERT(GC_dont_gc != 0);
    GC_dont_gc--;
    UNLOCK();
}
GC_API void GC_CALL GC_disable(void)
{
    DCL_LOCK_STATE;
    LOCK();
    GC_dont_gc++;
    UNLOCK();
}
GC_API int GC_CALL GC_is_disabled(void)
{
    return GC_dont_gc != 0;
}
GC_API void ** GC_CALL GC_new_free_list_inner(void)
{
    void *result;
    GC_ASSERT(I_HOLD_LOCK());
    result = GC_INTERNAL_MALLOC((MAXOBJGRANULES+1) * sizeof(ptr_t), PTRFREE);
    if (NULL == result) ABORT("Failed to allocate freelist for new kind");
    BZERO(result, (MAXOBJGRANULES+1)*sizeof(ptr_t));
    return (void **)result;
}
GC_API void ** GC_CALL GC_new_free_list(void)
{
    void ** result;
    DCL_LOCK_STATE;
    LOCK();
    result = GC_new_free_list_inner();
    UNLOCK();
    return result;
}
GC_API unsigned GC_CALL GC_new_kind_inner(void **fl, GC_word descr,
                                          int adjust, int clear)
{
    unsigned result = GC_n_kinds;
    GC_ASSERT(NONNULL_ARG_NOT_NULL(fl));
    GC_ASSERT(adjust == FALSE || adjust == TRUE);
    GC_ASSERT(clear == TRUE
              || (descr == 0 && adjust == FALSE && clear == FALSE));
    if (result < MAXOBJKINDS) {
      GC_ASSERT(result > 0);
      GC_n_kinds++;
      GC_obj_kinds[result].ok_freelist = fl;
      GC_obj_kinds[result].ok_reclaim_list = 0;
      GC_obj_kinds[result].ok_descriptor = descr;
      GC_obj_kinds[result].ok_relocate_descr = adjust;
      GC_obj_kinds[result].ok_init = (GC_bool)clear;
#ifdef ENABLE_DISCLAIM
        GC_obj_kinds[result].ok_mark_unconditionally = FALSE;
        GC_obj_kinds[result].ok_disclaim_proc = 0;
#endif
    } else {
      ABORT("Too many kinds");
    }
    return result;
}
GC_API unsigned GC_CALL GC_new_kind(void **fl, GC_word descr, int adjust,
                                    int clear)
{
    unsigned result;
    DCL_LOCK_STATE;
    LOCK();
    result = GC_new_kind_inner(fl, descr, adjust, clear);
    UNLOCK();
    return result;
}
GC_API unsigned GC_CALL GC_new_proc_inner(GC_mark_proc proc)
{
    unsigned result = GC_n_mark_procs;
    if (result < MAX_MARK_PROCS) {
      GC_n_mark_procs++;
      GC_mark_procs[result] = proc;
    } else {
      ABORT("Too many mark procedures");
    }
    return result;
}
GC_API unsigned GC_CALL GC_new_proc(GC_mark_proc proc)
{
    unsigned result;
    DCL_LOCK_STATE;
    LOCK();
    result = GC_new_proc_inner(proc);
    UNLOCK();
    return result;
}
GC_API void * GC_CALL GC_call_with_alloc_lock(GC_fn_type fn, void *client_data)
{
    void * result;
    DCL_LOCK_STATE;
#ifdef THREADS
      LOCK();
#endif
    result = (*fn)(client_data);
#ifdef THREADS
      UNLOCK();
#endif
    return(result);
}
GC_API void * GC_CALL GC_call_with_stack_base(GC_stack_base_func fn, void *arg)
{
    struct GC_stack_base base;
    void *result;
    base.mem_base = (void *)&base;
#ifdef IA64
      base.reg_base = (void *)GC_save_regs_in_stack();
#endif
    result = fn(&base, arg);
    GC_noop1(COVERT_DATAFLOW(&base));
    return result;
}
#ifndef THREADS
GC_INNER ptr_t GC_blocked_sp = NULL;
#ifdef IA64
    STATIC ptr_t GC_blocked_register_sp = NULL;
#endif
GC_INNER struct GC_traced_stack_sect_s *GC_traced_stack_sect = NULL;
GC_API void * GC_CALL GC_call_with_gc_active(GC_fn_type fn,
                                             void * client_data)
{
    struct GC_traced_stack_sect_s stacksect;
    GC_ASSERT(GC_is_initialized);
    if ((word)GC_stackbottom HOTTER_THAN (word)(&stacksect))
      GC_stackbottom = (ptr_t)COVERT_DATAFLOW(&stacksect);
    if (GC_blocked_sp == NULL) {
      client_data = fn(client_data);
      GC_noop1(COVERT_DATAFLOW(&stacksect));
      return client_data;
    }
    stacksect.saved_stack_ptr = GC_blocked_sp;
#ifdef IA64
      stacksect.backing_store_end = GC_save_regs_in_stack();
      stacksect.saved_backing_store_ptr = GC_blocked_register_sp;
#endif
    stacksect.prev = GC_traced_stack_sect;
    GC_blocked_sp = NULL;
    GC_traced_stack_sect = &stacksect;
    client_data = fn(client_data);
    GC_ASSERT(GC_blocked_sp == NULL);
    GC_ASSERT(GC_traced_stack_sect == &stacksect);
#if defined(CPPCHECK)
      GC_noop1((word)GC_traced_stack_sect - (word)GC_blocked_sp);
#endif
    GC_traced_stack_sect = stacksect.prev;
#ifdef IA64
      GC_blocked_register_sp = stacksect.saved_backing_store_ptr;
#endif
    GC_blocked_sp = stacksect.saved_stack_ptr;
    return client_data;
}
STATIC void GC_do_blocking_inner(ptr_t data, void * context GC_ATTR_UNUSED)
{
    struct blocking_data * d = (struct blocking_data *) data;
    GC_ASSERT(GC_is_initialized);
    GC_ASSERT(GC_blocked_sp == NULL);
#ifdef SPARC
        GC_blocked_sp = GC_save_regs_in_stack();
#else
        GC_blocked_sp = (ptr_t) &d;
#endif
#ifdef IA64
        GC_blocked_register_sp = GC_save_regs_in_stack();
#endif
    d -> client_data = (d -> fn)(d -> client_data);
#ifdef SPARC
        GC_ASSERT(GC_blocked_sp != NULL);
#else
        GC_ASSERT(GC_blocked_sp == (ptr_t)(&d));
#endif
#if defined(CPPCHECK)
      GC_noop1((word)GC_blocked_sp);
#endif
    GC_blocked_sp = NULL;
}
  GC_API void GC_CALL GC_set_stackbottom(void *gc_thread_handle,
                                         const struct GC_stack_base *sb)
  {
    GC_ASSERT(sb -> mem_base != NULL);
    GC_ASSERT(NULL == gc_thread_handle || &GC_stackbottom == gc_thread_handle);
    GC_ASSERT(NULL == GC_blocked_sp
              && NULL == GC_traced_stack_sect);
    (void)gc_thread_handle;
    GC_stackbottom = (char *)sb->mem_base;
#ifdef IA64
      GC_register_stackbottom = (ptr_t)sb->reg_base;
#endif
  }
  GC_API void * GC_CALL GC_get_my_stackbottom(struct GC_stack_base *sb)
  {
    GC_ASSERT(GC_is_initialized);
    sb -> mem_base = GC_stackbottom;
#ifdef IA64
      sb -> reg_base = GC_register_stackbottom;
#endif
    return &GC_stackbottom;
  }
#endif
GC_API void * GC_CALL GC_do_blocking(GC_fn_type fn, void * client_data)
{
    struct blocking_data my_data;
    my_data.fn = fn;
    my_data.client_data = client_data;
    GC_with_callee_saves_pushed(GC_do_blocking_inner, (ptr_t)(&my_data));
    return my_data.client_data;
}
#if !defined(NO_DEBUGGING)
  GC_API void GC_CALL GC_dump(void)
  {
    DCL_LOCK_STATE;
    LOCK();
    GC_dump_named(NULL);
    UNLOCK();
  }
  GC_API void GC_CALL GC_dump_named(const char *name)
  {
#ifndef NO_CLOCK
      CLOCK_TYPE current_time;
      GET_TIME(current_time);
#endif
    if (name != NULL) {
      GC_printf("***GC Dump %s\n", name);
    } else {
      GC_printf("***GC Dump collection #%lu\n", (unsigned long)GC_gc_no);
    }
#ifndef NO_CLOCK
      GC_printf("Time since GC init: %lu ms\n",
                MS_TIME_DIFF(current_time, GC_init_time));
#endif
    GC_printf("\n***Static roots:\n");
    GC_print_static_roots();
    GC_printf("\n***Heap sections:\n");
    GC_print_heap_sects();
    GC_printf("\n***Free blocks:\n");
    GC_print_hblkfreelist();
    GC_printf("\n***Blocks in use:\n");
    GC_print_block_list();
  }
#endif
static void block_add_size(struct hblk *h, word pbytes)
{
  hdr *hhdr = HDR(h);
  *(word *)pbytes += (WORDS_TO_BYTES(hhdr->hb_sz) + (HBLKSIZE - 1))
                        & ~(word)(HBLKSIZE - 1);
}
GC_API size_t GC_CALL GC_get_memory_use(void)
{
  word bytes = 0;
  DCL_LOCK_STATE;
  LOCK();
  GC_apply_to_all_blocks(block_add_size, (word)(&bytes));
  UNLOCK();
  return (size_t)bytes;
}
GC_API GC_word GC_CALL GC_get_gc_no(void)
{
    return GC_gc_no;
}
#ifdef THREADS
  GC_API int GC_CALL GC_get_parallel(void)
  {
    return GC_parallel;
  }
  GC_API void GC_CALL GC_alloc_lock(void)
  {
    DCL_LOCK_STATE;
    LOCK();
  }
  GC_API void GC_CALL GC_alloc_unlock(void)
  {
    UNLOCK();
  }
  GC_INNER GC_on_thread_event_proc GC_on_thread_event = 0;
  GC_API void GC_CALL GC_set_on_thread_event(GC_on_thread_event_proc fn)
  {
    DCL_LOCK_STATE;
    LOCK();
    GC_on_thread_event = fn;
    UNLOCK();
  }
  GC_API GC_on_thread_event_proc GC_CALL GC_get_on_thread_event(void)
  {
    GC_on_thread_event_proc fn;
    DCL_LOCK_STATE;
    LOCK();
    fn = GC_on_thread_event;
    UNLOCK();
    return fn;
  }
#endif
GC_API void GC_CALL GC_set_oom_fn(GC_oom_func fn)
{
    GC_ASSERT(NONNULL_ARG_NOT_NULL(fn));
    DCL_LOCK_STATE;
    LOCK();
    GC_oom_fn = fn;
    UNLOCK();
}
GC_API GC_oom_func GC_CALL GC_get_oom_fn(void)
{
    GC_oom_func fn;
    DCL_LOCK_STATE;
    LOCK();
    fn = GC_oom_fn;
    UNLOCK();
    return fn;
}
GC_API void GC_CALL GC_set_on_heap_resize(GC_on_heap_resize_proc fn)
{
    DCL_LOCK_STATE;
    LOCK();
    GC_on_heap_resize = fn;
    UNLOCK();
}
GC_API GC_on_heap_resize_proc GC_CALL GC_get_on_heap_resize(void)
{
    GC_on_heap_resize_proc fn;
    DCL_LOCK_STATE;
    LOCK();
    fn = GC_on_heap_resize;
    UNLOCK();
    return fn;
}
GC_API void GC_CALL GC_set_finalizer_notifier(GC_finalizer_notifier_proc fn)
{
    DCL_LOCK_STATE;
    LOCK();
    GC_finalizer_notifier = fn;
    UNLOCK();
}
GC_API GC_finalizer_notifier_proc GC_CALL GC_get_finalizer_notifier(void)
{
    GC_finalizer_notifier_proc fn;
    DCL_LOCK_STATE;
    LOCK();
    fn = GC_finalizer_notifier;
    UNLOCK();
    return fn;
}
GC_API void GC_CALL GC_set_find_leak(int value)
{
    GC_find_leak = value;
}
GC_API int GC_CALL GC_get_find_leak(void)
{
    return GC_find_leak;
}
GC_API void GC_CALL GC_set_all_interior_pointers(int value)
{
    DCL_LOCK_STATE;
    GC_all_interior_pointers = value ? 1 : 0;
    if (GC_is_initialized) {
      LOCK();
      GC_initialize_offsets();
      if (!GC_all_interior_pointers)
        GC_bl_init_no_interiors();
      UNLOCK();
    }
}
GC_API int GC_CALL GC_get_all_interior_pointers(void)
{
    return GC_all_interior_pointers;
}
GC_API void GC_CALL GC_set_finalize_on_demand(int value)
{
    GC_ASSERT(value != -1);
    GC_finalize_on_demand = value;
}
GC_API int GC_CALL GC_get_finalize_on_demand(void)
{
    return GC_finalize_on_demand;
}
GC_API void GC_CALL GC_set_java_finalization(int value)
{
    GC_ASSERT(value != -1);
    GC_java_finalization = value;
}
GC_API int GC_CALL GC_get_java_finalization(void)
{
    return GC_java_finalization;
}
GC_API void GC_CALL GC_set_dont_expand(int value)
{
    GC_ASSERT(value != -1);
    GC_dont_expand = value;
}
GC_API int GC_CALL GC_get_dont_expand(void)
{
    return GC_dont_expand;
}
GC_API void GC_CALL GC_set_no_dls(int value)
{
    GC_ASSERT(value != -1);
    GC_no_dls = value;
}
GC_API int GC_CALL GC_get_no_dls(void)
{
    return GC_no_dls;
}
GC_API void GC_CALL GC_set_non_gc_bytes(GC_word value)
{
    GC_non_gc_bytes = value;
}
GC_API GC_word GC_CALL GC_get_non_gc_bytes(void)
{
    return GC_non_gc_bytes;
}
GC_API void GC_CALL GC_set_free_space_divisor(GC_word value)
{
    GC_ASSERT(value > 0);
    GC_free_space_divisor = value;
}
GC_API GC_word GC_CALL GC_get_free_space_divisor(void)
{
    return GC_free_space_divisor;
}
GC_API void GC_CALL GC_set_max_retries(GC_word value)
{
    GC_ASSERT((GC_signed_word)value != -1);
    GC_max_retries = value;
}
GC_API GC_word GC_CALL GC_get_max_retries(void)
{
    return GC_max_retries;
}
GC_API void GC_CALL GC_set_dont_precollect(int value)
{
    GC_ASSERT(value != -1);
    GC_dont_precollect = value;
}
GC_API int GC_CALL GC_get_dont_precollect(void)
{
    return GC_dont_precollect;
}
GC_API void GC_CALL GC_set_full_freq(int value)
{
    GC_ASSERT(value >= 0);
    GC_full_freq = value;
}
GC_API int GC_CALL GC_get_full_freq(void)
{
    return GC_full_freq;
}
GC_API void GC_CALL GC_set_time_limit(unsigned long value)
{
    GC_ASSERT((long)value != -1L);
    GC_time_limit = value;
}
GC_API unsigned long GC_CALL GC_get_time_limit(void)
{
    return GC_time_limit;
}
GC_API void GC_CALL GC_set_force_unmap_on_gcollect(int value)
{
    GC_force_unmap_on_gcollect = (GC_bool)value;
}
GC_API int GC_CALL GC_get_force_unmap_on_gcollect(void)
{
    return (int)GC_force_unmap_on_gcollect;
}
GC_API void GC_CALL GC_abort_on_oom(void)
{
    GC_err_printf("Insufficient memory for the allocation\n");
    EXIT();
}
#ifdef THREADS
  GC_API void GC_CALL GC_stop_world_external(void)
  {
    GC_ASSERT(GC_is_initialized);
    LOCK();
#ifdef THREAD_LOCAL_ALLOC
      GC_ASSERT(!GC_world_stopped);
#endif
    STOP_WORLD();
#ifdef THREAD_LOCAL_ALLOC
      GC_world_stopped = TRUE;
#endif
  }
  GC_API void GC_CALL GC_start_world_external(void)
  {
#ifdef THREAD_LOCAL_ALLOC
      GC_ASSERT(GC_world_stopped);
      GC_world_stopped = FALSE;
#else
      GC_ASSERT(GC_is_initialized);
#endif
    START_WORLD();
    UNLOCK();
  }
#endif
#if !defined(OS2) && !defined(PCR) && !defined(AMIGA) && !defined(MACOS) \
    && !defined(MSWINCE) && !defined(SN_TARGET_ORBIS) \
    && !defined(SN_TARGET_PSP2) && !defined(__CC_ARM)
#include <sys/types.h>
#if !defined(MSWIN32) && !defined(MSWIN_XBOX1)
#include <unistd.h>
#endif
#endif
#include <stdio.h>
#if defined(MSWINCE) || defined(SN_TARGET_PS3)
#define SIGSEGV 0
#else
#include <signal.h>
#endif
#if defined(UNIX_LIKE) || defined(CYGWIN32) || defined(NACL) \
    || defined(SYMBIAN)
#include <fcntl.h>
#endif
#if defined(LINUX) || defined(LINUX_STACKBOTTOM)
#include <ctype.h>
#endif
#ifdef AMIGA
#define GC_AMIGA_DEF
#if !defined(GC_AMIGA_DEF) && !defined(GC_AMIGA_SB) && !defined(GC_AMIGA_DS) && !defined(GC_AMIGA_AM)
#include <stdio.h>
#include <signal.h>
#define GC_AMIGA_DEF
#define GC_AMIGA_SB
#define GC_AMIGA_DS
#define GC_AMIGA_AM
#endif
#ifdef GC_AMIGA_DEF
#ifndef __GNUC__
#include <exec/exec.h>
#endif
#include <proto/exec.h>
#include <proto/dos.h>
#include <dos/dosextens.h>
#include <workbench/startup.h>
#endif
#ifdef GC_AMIGA_SB
ptr_t GC_get_main_stack_base(void)
{
    struct Process *proc = (struct Process*)SysBase->ThisTask;
    if (proc->pr_Task.tc_Node.ln_Type==NT_PROCESS
        && proc->pr_CLI != NULL) {
        return (char *)proc->pr_ReturnAddr + sizeof(ULONG);
    } else {
        return (char *)proc->pr_Task.tc_SPUpper;
    }
}
#endif
#ifdef GC_AMIGA_DS
   void GC_register_data_segments(void)
   {
     struct Process     *proc;
     struct CommandLineInterface *cli;
     BPTR myseglist;
     ULONG *data;
#ifdef __GNUC__
        ULONG dataSegSize;
        GC_bool found_segment = FALSE;
        extern char __data_size[];
        dataSegSize=__data_size+8;
#endif
        proc= (struct Process*)SysBase->ThisTask;
        myseglist = proc->pr_SegList;
        if (proc->pr_Task.tc_Node.ln_Type==NT_PROCESS) {
          if (proc->pr_CLI != NULL) {
            cli = BADDR(proc->pr_CLI);
            myseglist = cli->cli_Module;
          }
        } else {
          ABORT("Not a Process.");
        }
        if (myseglist == NULL) {
            ABORT("Arrrgh.. can't find segments, aborting");
        }
        for (data = (ULONG *)BADDR(myseglist); data != NULL;
             data = (ULONG *)BADDR(data[0])) {
          if ((ULONG)GC_register_data_segments < (ULONG)(&data[1])
              || (ULONG)GC_register_data_segments > (ULONG)(&data[1])
                                                    + data[-1]) {
#ifdef __GNUC__
                if (dataSegSize == data[-1]) {
                  found_segment = TRUE;
                }
#endif
              GC_add_roots_inner((char *)&data[1],
                                 ((char *)&data[1]) + data[-1], FALSE);
          }
        }
#ifdef __GNUC__
           if (!found_segment) {
             ABORT("Can`t find correct Segments.\nSolution: Use an newer version of ixemul.library");
           }
#endif
   }
#endif
#ifdef GC_AMIGA_AM
#ifndef GC_AMIGA_FASTALLOC
void *GC_amiga_allocwrapper(size_t size,void *(*AllocFunction)(size_t size2)){
        return (*AllocFunction)(size);
}
void *(*GC_amiga_allocwrapper_do)(size_t size,void *(*AllocFunction)(size_t size2))
        =GC_amiga_allocwrapper;
#else
void *GC_amiga_allocwrapper_firsttime(size_t size,void *(*AllocFunction)(size_t size2));
void *(*GC_amiga_allocwrapper_do)(size_t size,void *(*AllocFunction)(size_t size2))
        =GC_amiga_allocwrapper_firsttime;
struct GC_Amiga_AllocedMemoryHeader{
        ULONG size;
        struct GC_Amiga_AllocedMemoryHeader *next;
};
struct GC_Amiga_AllocedMemoryHeader *GC_AMIGAMEM=(struct GC_Amiga_AllocedMemoryHeader *)(int)~(NULL);
ULONG GC_AMIGA_MEMF = MEMF_FAST | MEMF_CLEAR;
#ifndef GC_AMIGA_ONLYFAST
BOOL GC_amiga_dontalloc=FALSE;
#endif
#ifdef GC_AMIGA_PRINTSTATS
int succ=0,succ2=0;
int nsucc=0,nsucc2=0;
int nullretries=0;
int numcollects=0;
int chipa=0;
int allochip=0;
int allocfast=0;
int cur0=0;
int cur1=0;
int cur10=0;
int cur50=0;
int cur150=0;
int cur151=0;
int ncur0=0;
int ncur1=0;
int ncur10=0;
int ncur50=0;
int ncur150=0;
int ncur151=0;
#endif
void GC_amiga_free_all_mem(void){
        struct GC_Amiga_AllocedMemoryHeader *gc_am=(struct GC_Amiga_AllocedMemoryHeader *)(~(int)(GC_AMIGAMEM));
#ifdef GC_AMIGA_PRINTSTATS
        printf("\n\n"
                "%d bytes of chip-mem, and %d bytes of fast-mem where allocated from the OS.\n",
                allochip,allocfast
        );
        printf(
                "%d bytes of chip-mem were returned from the GC_AMIGA_FASTALLOC supported allocating functions.\n",
                chipa
        );
        printf("\n");
        printf("GC_gcollect was called %d times to avoid returning NULL or start allocating with the MEMF_ANY flag.\n",numcollects);
        printf("%d of them was a success. (the others had to use allocation from the OS.)\n",nullretries);
        printf("\n");
        printf("Succeeded forcing %d gc-allocations (%d bytes) of chip-mem to be fast-mem.\n",succ,succ2);
        printf("Failed forcing %d gc-allocations (%d bytes) of chip-mem to be fast-mem.\n",nsucc,nsucc2);
        printf("\n");
        printf(
                "Number of retries before succeeding a chip->fast force:\n"
                "0: %d, 1: %d, 2-9: %d, 10-49: %d, 50-149: %d, >150: %d\n",
                cur0,cur1,cur10,cur50,cur150,cur151
        );
        printf(
                "Number of retries before giving up a chip->fast force:\n"
                "0: %d, 1: %d, 2-9: %d, 10-49: %d, 50-149: %d, >150: %d\n",
                ncur0,ncur1,ncur10,ncur50,ncur150,ncur151
        );
#endif
        while(gc_am!=NULL){
                struct GC_Amiga_AllocedMemoryHeader *temp = gc_am->next;
                FreeMem(gc_am,gc_am->size);
                gc_am=(struct GC_Amiga_AllocedMemoryHeader *)(~(int)(temp));
        }
}
#ifndef GC_AMIGA_ONLYFAST
char *chipmax;
size_t latestsize;
#endif
#ifdef GC_AMIGA_FASTALLOC
void *GC_amiga_get_mem(size_t size){
        struct GC_Amiga_AllocedMemoryHeader *gc_am;
#ifndef GC_AMIGA_ONLYFAST
        if(GC_amiga_dontalloc==TRUE){
                return NULL;
        }
        if(GC_AMIGA_MEMF==(MEMF_ANY|MEMF_CLEAR) && size>100000 && latestsize<50000) return NULL;
#endif
        gc_am=AllocMem((ULONG)(size + sizeof(struct GC_Amiga_AllocedMemoryHeader)),GC_AMIGA_MEMF);
        if(gc_am==NULL) return NULL;
        gc_am->next=GC_AMIGAMEM;
        gc_am->size=size + sizeof(struct GC_Amiga_AllocedMemoryHeader);
        GC_AMIGAMEM=(struct GC_Amiga_AllocedMemoryHeader *)(~(int)(gc_am));
#ifdef GC_AMIGA_PRINTSTATS
        if((char *)gc_am<chipmax){
                allochip+=size;
        }else{
                allocfast+=size;
        }
#endif
        return gc_am+1;
}
#endif
#ifndef GC_AMIGA_ONLYFAST
#ifdef GC_AMIGA_RETRY
void *GC_amiga_rec_alloc(size_t size,void *(*AllocFunction)(size_t size2),const int rec){
        void *ret;
        ret=(*AllocFunction)(size);
#ifdef GC_AMIGA_PRINTSTATS
        if((char *)ret>chipmax || ret==NULL){
                if(ret==NULL){
                        nsucc++;
                        nsucc2+=size;
                        if(rec==0) ncur0++;
                        if(rec==1) ncur1++;
                        if(rec>1 && rec<10) ncur10++;
                        if(rec>=10 && rec<50) ncur50++;
                        if(rec>=50 && rec<150) ncur150++;
                        if(rec>=150) ncur151++;
                }else{
                        succ++;
                        succ2+=size;
                        if(rec==0) cur0++;
                        if(rec==1) cur1++;
                        if(rec>1 && rec<10) cur10++;
                        if(rec>=10 && rec<50) cur50++;
                        if(rec>=50 && rec<150) cur150++;
                        if(rec>=150) cur151++;
                }
        }
#endif
        if (((char *)ret)<=chipmax && ret!=NULL && (rec<(size>500000?9:size/5000))){
                ret=GC_amiga_rec_alloc(size,AllocFunction,rec+1);
        }
        return ret;
}
#endif
void *GC_amiga_allocwrapper_any(size_t size,void *(*AllocFunction)(size_t size2)){
        void *ret;
        GC_amiga_dontalloc=TRUE;
        latestsize=size;
        ret=(*AllocFunction)(size);
        if(((char *)ret) <= chipmax){
                if(ret==NULL){
#ifdef GC_AMIGA_GC
                        if(!GC_dont_gc){
                                GC_gcollect();
#ifdef GC_AMIGA_PRINTSTATS
                                numcollects++;
#endif
                                ret=(*AllocFunction)(size);
                        }
                        if(ret==NULL)
#endif
                        {
                                GC_amiga_dontalloc=FALSE;
                                ret=(*AllocFunction)(size);
                                if(ret==NULL){
                                        WARN("Out of Memory!  Returning NIL!\n", 0);
                                }
                        }
#ifdef GC_AMIGA_PRINTSTATS
                        else{
                                nullretries++;
                        }
                        if(ret!=NULL && (char *)ret<=chipmax) chipa+=size;
#endif
                }
#ifdef GC_AMIGA_RETRY
                else{
                        void *ret2;
                        if(
                                AllocFunction!=GC_malloc_uncollectable
#ifdef GC_ATOMIC_UNCOLLECTABLE
                                && AllocFunction!=GC_malloc_atomic_uncollectable
#endif
                        ){
                                ret2=GC_amiga_rec_alloc(size,AllocFunction,0);
                        }else{
                                ret2=(*AllocFunction)(size);
#ifdef GC_AMIGA_PRINTSTATS
                                if((char *)ret2<chipmax || ret2==NULL){
                                        nsucc++;
                                        nsucc2+=size;
                                        ncur0++;
                                }else{
                                        succ++;
                                        succ2+=size;
                                        cur0++;
                                }
#endif
                        }
                        if(((char *)ret2)>chipmax){
                                GC_free(ret);
                                ret=ret2;
                        }else{
                                GC_free(ret2);
                        }
                }
#endif
        }
#if defined(CPPCHECK)
      if (GC_amiga_dontalloc)
#endif
        GC_amiga_dontalloc=FALSE;
        return ret;
}
void (*GC_amiga_toany)(void)=NULL;
void GC_amiga_set_toany(void (*func)(void)){
        GC_amiga_toany=func;
}
#endif
void *GC_amiga_allocwrapper_fast(size_t size,void *(*AllocFunction)(size_t size2)){
        void *ret;
        ret=(*AllocFunction)(size);
        if(ret==NULL){
#ifdef GC_AMIGA_GC
                if(!GC_dont_gc){
                        GC_gcollect();
#ifdef GC_AMIGA_PRINTSTATS
                        numcollects++;
#endif
                        ret=(*AllocFunction)(size);
                }
                if(ret==NULL)
#endif
                {
#ifndef GC_AMIGA_ONLYFAST
                        GC_AMIGA_MEMF=MEMF_ANY | MEMF_CLEAR;
                        if(GC_amiga_toany!=NULL) (*GC_amiga_toany)();
                        GC_amiga_allocwrapper_do=GC_amiga_allocwrapper_any;
                        return GC_amiga_allocwrapper_any(size,AllocFunction);
#endif
                }
#ifdef GC_AMIGA_PRINTSTATS
                else{
                        nullretries++;
                }
#endif
        }
        return ret;
}
void *GC_amiga_allocwrapper_firsttime(size_t size,void *(*AllocFunction)(size_t size2)){
        atexit(&GC_amiga_free_all_mem);
        chipmax=(char *)SysBase->MaxLocMem;
        GC_amiga_allocwrapper_do=GC_amiga_allocwrapper_fast;
        return GC_amiga_allocwrapper_fast(size,AllocFunction);
}
#endif
void *GC_amiga_realloc(void *old_object,size_t new_size_in_bytes){
#ifndef GC_AMIGA_FASTALLOC
        return GC_realloc(old_object,new_size_in_bytes);
#else
        void *ret;
        latestsize=new_size_in_bytes;
        ret=GC_realloc(old_object,new_size_in_bytes);
        if(ret==NULL && new_size_in_bytes != 0
           && GC_AMIGA_MEMF==(MEMF_FAST | MEMF_CLEAR)){
#ifdef GC_AMIGA_GC
                if(!GC_dont_gc){
                        GC_gcollect();
#ifdef GC_AMIGA_PRINTSTATS
                        numcollects++;
#endif
                        ret=GC_realloc(old_object,new_size_in_bytes);
                }
                if(ret==NULL)
#endif
                {
#ifndef GC_AMIGA_ONLYFAST
                        GC_AMIGA_MEMF=MEMF_ANY | MEMF_CLEAR;
                        if(GC_amiga_toany!=NULL) (*GC_amiga_toany)();
                        GC_amiga_allocwrapper_do=GC_amiga_allocwrapper_any;
                        ret=GC_realloc(old_object,new_size_in_bytes);
#endif
                }
#ifdef GC_AMIGA_PRINTSTATS
                else{
                        nullretries++;
                }
#endif
        }
        if(ret==NULL && new_size_in_bytes != 0){
                WARN("Out of Memory!  Returning NIL!\n", 0);
        }
#ifdef GC_AMIGA_PRINTSTATS
        if(((char *)ret)<chipmax && ret!=NULL){
                chipa+=new_size_in_bytes;
        }
#endif
        return ret;
#endif
}
#endif
#undef GC_AMIGA_DEF
#endif
#ifdef MACOS
#include <Processes.h>
#endif
#ifdef IRIX5
#include <sys/uio.h>
#include <malloc.h>
#endif
#if defined(MMAP_SUPPORTED) || defined(ADD_HEAP_GUARD_PAGES)
#if defined(USE_MUNMAP) && !defined(USE_MMAP) && !defined(CPPCHECK)
#error Invalid config: USE_MUNMAP requires USE_MMAP
#endif
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#endif
#if defined(ADD_HEAP_GUARD_PAGES) || defined(LINUX_STACKBOTTOM) \
    || defined(MMAP_SUPPORTED) || defined(NEED_PROC_MAPS)
#include <errno.h>
#endif
#ifdef DARWIN
#include <mach-o/getsect.h>
#endif
#ifdef DJGPP
  typedef long unsigned int caddr_t;
#endif
#ifdef PCR
#include "mm/PCR_MM.h"
#endif
#if defined(GC_DARWIN_THREADS) && defined(MPROTECT_VDB)
#ifndef GC_DARWIN_STOP_WORLD_H
#define GC_DARWIN_STOP_WORLD_H
#if !defined(GC_DARWIN_THREADS)
#error darwin_stop_world.h included without GC_DARWIN_THREADS defined
#endif
#include <mach/mach.h>
#include <mach/thread_act.h>
EXTERN_C_BEGIN
struct thread_stop_info {
  mach_port_t mach_thread;
  ptr_t stack_ptr;
};
#ifndef DARWIN_DONT_PARSE_STACK
  GC_INNER ptr_t GC_FindTopOfStack(unsigned long);
#endif
#ifdef MPROTECT_VDB
  GC_INNER void GC_mprotect_stop(void);
  GC_INNER void GC_mprotect_resume(void);
#ifndef GC_NO_THREADS_DISCOVERY
    GC_INNER void GC_darwin_register_mach_handler_thread(mach_port_t thread);
#endif
#endif
#if defined(PARALLEL_MARK) && !defined(GC_NO_THREADS_DISCOVERY)
  GC_INNER GC_bool GC_is_mach_marker(thread_act_t);
#endif
EXTERN_C_END
#endif
#endif
#if !defined(NO_EXECUTE_PERMISSION)
  STATIC GC_bool GC_pages_executable = TRUE;
#else
  STATIC GC_bool GC_pages_executable = FALSE;
#endif
#define IGNORE_PAGES_EXECUTABLE 1
#if ((defined(LINUX_STACKBOTTOM) || defined(NEED_PROC_MAPS) \
      || defined(PROC_VDB) || defined(SOFT_VDB)) && !defined(PROC_READ)) \
    || defined(CPPCHECK)
#define PROC_READ read
#endif
#if defined(LINUX_STACKBOTTOM) || defined(NEED_PROC_MAPS)
  STATIC ssize_t GC_repeat_read(int fd, char *buf, size_t count)
  {
    size_t num_read = 0;
    ASSERT_CANCEL_DISABLED();
    while (num_read < count) {
        ssize_t result = PROC_READ(fd, buf + num_read, count - num_read);
        if (result < 0) return result;
        if (result == 0) break;
        num_read += result;
    }
    return num_read;
  }
#endif
#ifdef NEED_PROC_MAPS
#ifdef THREADS
  STATIC size_t GC_get_file_len(int f)
  {
    size_t total = 0;
    ssize_t result;
#define GET_FILE_LEN_BUF_SZ 500
    char buf[GET_FILE_LEN_BUF_SZ];
    do {
        result = PROC_READ(f, buf, sizeof(buf));
        if (result == -1) return 0;
        total += result;
    } while (result > 0);
    return total;
  }
  STATIC size_t GC_get_maps_len(void)
  {
    int f = open("/proc/self/maps", O_RDONLY);
    size_t result;
    if (f < 0) return 0;
    result = GC_get_file_len(f);
    close(f);
    return result;
  }
#endif
GC_INNER const char * GC_get_maps(void)
{
    ssize_t result;
    static char *maps_buf = NULL;
    static size_t maps_buf_sz = 1;
    size_t maps_size;
#ifdef THREADS
      size_t old_maps_size = 0;
#endif
    GC_ASSERT(I_HOLD_LOCK());
#ifdef THREADS
        maps_size = GC_get_maps_len();
        if (0 == maps_size)
          ABORT("Cannot determine length of /proc/self/maps");
#else
        maps_size = 4000;
#endif
        do {
            int f;
            while (maps_size >= maps_buf_sz) {
#ifdef LINT2
                GC_noop1((word)maps_buf);
#else
                GC_scratch_recycle_no_gww(maps_buf, maps_buf_sz);
#endif
              while (maps_size >= maps_buf_sz) maps_buf_sz *= 2;
              maps_buf = GC_scratch_alloc(maps_buf_sz);
              if (NULL == maps_buf)
                ABORT_ARG1("Insufficient space for /proc/self/maps buffer",
                        ", %lu bytes requested", (unsigned long)maps_buf_sz);
#ifdef THREADS
                maps_size = GC_get_maps_len();
                if (0 == maps_size)
                  ABORT("Cannot determine length of /proc/self/maps");
#endif
            }
            GC_ASSERT(maps_buf_sz >= maps_size + 1);
            f = open("/proc/self/maps", O_RDONLY);
            if (-1 == f)
              ABORT_ARG1("Cannot open /proc/self/maps",
                         ": errno= %d", errno);
#ifdef THREADS
              old_maps_size = maps_size;
#endif
            maps_size = 0;
            do {
                result = GC_repeat_read(f, maps_buf, maps_buf_sz-1);
                if (result <= 0) {
                  ABORT_ARG1("Failed to read /proc/self/maps",
                             ": errno= %d", result < 0 ? errno : 0);
                }
                maps_size += result;
            } while ((size_t)result == maps_buf_sz-1);
            close(f);
#ifdef THREADS
              if (maps_size > old_maps_size) {
                WARN("Unexpected asynchronous /proc/self/maps growth"
                     " (to %" WARN_PRIdPTR " bytes)\n", maps_size);
              }
#endif
        } while (maps_size >= maps_buf_sz
#ifdef THREADS
                   || maps_size < old_maps_size
#endif
                );
        maps_buf[maps_size] = '\0';
        return maps_buf;
}
#if (defined(DYNAMIC_LOADING) && defined(USE_PROC_FOR_LIBRARIES)) \
    || defined(IA64) || defined(INCLUDE_LINUX_THREAD_DESCR) \
    || defined(REDIRECT_MALLOC)
  GC_INNER const char *GC_parse_map_entry(const char *maps_ptr,
                                          ptr_t *start, ptr_t *end,
                                          const char **prot, unsigned *maj_dev,
                                          const char **mapping_name)
  {
    const unsigned char *start_start, *end_start, *maj_dev_start;
    const unsigned char *p;
    if (maps_ptr == NULL || *maps_ptr == '\0') {
        return NULL;
    }
    p = (const unsigned char *)maps_ptr;
    while (isspace(*p)) ++p;
    start_start = p;
    GC_ASSERT(isxdigit(*start_start));
    *start = (ptr_t)strtoul((const char *)start_start, (char **)&p, 16);
    GC_ASSERT(*p=='-');
    ++p;
    end_start = p;
    GC_ASSERT(isxdigit(*end_start));
    *end = (ptr_t)strtoul((const char *)end_start, (char **)&p, 16);
    GC_ASSERT(isspace(*p));
    while (isspace(*p)) ++p;
    GC_ASSERT(*p == 'r' || *p == '-');
    *prot = (const char *)p;
    while (!isspace(*p)) ++p;
    while (isspace(*p)) p++;
    GC_ASSERT(isxdigit(*p));
    while (!isspace(*p)) ++p;
    while (isspace(*p)) p++;
    maj_dev_start = p;
    GC_ASSERT(isxdigit(*maj_dev_start));
    *maj_dev = strtoul((const char *)maj_dev_start, NULL, 16);
    if (mapping_name != NULL) {
      while (*p && *p != '\n' && *p != '/' && *p != '[') p++;
      *mapping_name = (const char *)p;
    }
    while (*p && *p++ != '\n');
    return (const char *)p;
  }
#endif
#if defined(IA64) || defined(INCLUDE_LINUX_THREAD_DESCR)
  GC_INNER GC_bool GC_enclosing_mapping(ptr_t addr, ptr_t *startp,
                                        ptr_t *endp)
  {
    const char *prot;
    ptr_t my_start, my_end;
    unsigned int maj_dev;
    const char *maps_ptr = GC_get_maps();
    for (;;) {
      maps_ptr = GC_parse_map_entry(maps_ptr, &my_start, &my_end,
                                    &prot, &maj_dev, 0);
      if (NULL == maps_ptr) break;
      if (prot[1] == 'w' && maj_dev == 0
          && (word)my_end > (word)addr && (word)my_start <= (word)addr) {
            *startp = my_start;
            *endp = my_end;
            return TRUE;
      }
    }
    return FALSE;
  }
#endif
#if defined(REDIRECT_MALLOC)
  GC_INNER GC_bool GC_text_mapping(char *nm, ptr_t *startp, ptr_t *endp)
  {
    size_t nm_len = strlen(nm);
    const char *prot, *map_path;
    ptr_t my_start, my_end;
    unsigned int maj_dev;
    const char *maps_ptr = GC_get_maps();
    for (;;) {
      maps_ptr = GC_parse_map_entry(maps_ptr, &my_start, &my_end,
                                    &prot, &maj_dev, &map_path);
      if (NULL == maps_ptr) break;
      if (prot[0] == 'r' && prot[1] == '-' && prot[2] == 'x') {
          const char *p = map_path;
            while (*p != '\0' && *p != '\n' && *p != ' ' && *p != '\t') ++p;
            while (*p != '/' && (word)p >= (word)map_path) --p;
            ++p;
          if (strncmp(nm, p, nm_len) == 0) {
            *startp = my_start;
            *endp = my_end;
            return TRUE;
          }
      }
    }
    return FALSE;
  }
#endif
#ifdef IA64
  static ptr_t backing_store_base_from_proc(void)
  {
    ptr_t my_start, my_end;
    if (!GC_enclosing_mapping(GC_save_regs_in_stack(), &my_start, &my_end)) {
        GC_COND_LOG_PRINTF("Failed to find backing store base from /proc\n");
        return 0;
    }
    return my_start;
  }
#endif
#endif
#if defined(SEARCH_FOR_DATA_START)
#if defined(LINUX) || defined(HURD)
    EXTERN_C_BEGIN
#pragma weak __data_start
#pragma weak data_start
    extern int __data_start[], data_start[];
    EXTERN_C_END
#endif
  ptr_t GC_data_start = NULL;
  GC_INNER void GC_init_linux_data_start(void)
  {
    ptr_t data_end = DATAEND;
#if (defined(LINUX) || defined(HURD)) && defined(USE_PROG_DATA_START)
      if (COVERT_DATAFLOW(__data_start) != 0) {
        GC_data_start = (ptr_t)(__data_start);
      } else {
        GC_data_start = (ptr_t)(data_start);
      }
      if (COVERT_DATAFLOW(GC_data_start) != 0) {
        if ((word)GC_data_start > (word)data_end)
          ABORT_ARG2("Wrong __data_start/_end pair",
                     ": %p .. %p", (void *)GC_data_start, (void *)data_end);
        return;
      }
#ifdef DEBUG_ADD_DEL_ROOTS
        GC_log_printf("__data_start not provided\n");
#endif
#endif
    if (GC_no_dls) {
      GC_data_start = data_end;
      return;
    }
    GC_data_start = (ptr_t)GC_find_limit(data_end, FALSE);
  }
#endif
#ifdef ECOS
#ifndef ECOS_GC_MEMORY_SIZE
#define ECOS_GC_MEMORY_SIZE (448 * 1024)
#endif
  static char ecos_gc_memory[ECOS_GC_MEMORY_SIZE];
  static char *ecos_gc_brk = ecos_gc_memory;
  static void *tiny_sbrk(ptrdiff_t increment)
  {
    void *p = ecos_gc_brk;
    ecos_gc_brk += increment;
    if ((word)ecos_gc_brk > (word)(ecos_gc_memory + sizeof(ecos_gc_memory))) {
      ecos_gc_brk -= increment;
      return NULL;
    }
    return p;
  }
#define sbrk tiny_sbrk
#endif
#if defined(NETBSD) && defined(__ELF__)
  ptr_t GC_data_start = NULL;
  EXTERN_C_BEGIN
  extern char **environ;
  EXTERN_C_END
  GC_INNER void GC_init_netbsd_elf(void)
  {
    GC_data_start = (ptr_t)GC_find_limit(&environ, FALSE);
  }
#endif
#if defined(ADDRESS_SANITIZER) && (defined(UNIX_LIKE) \
                    || defined(NEED_FIND_LIMIT) || defined(MPROTECT_VDB)) \
    && !defined(CUSTOM_ASAN_DEF_OPTIONS)
  GC_API const char *__asan_default_options(void)
  {
    return "allow_user_segv_handler=1";
  }
#endif
#ifdef OPENBSD
  static struct sigaction old_segv_act;
  STATIC JMP_BUF GC_jmp_buf_openbsd;
  STATIC void GC_fault_handler_openbsd(int sig GC_ATTR_UNUSED)
  {
     LONGJMP(GC_jmp_buf_openbsd, 1);
  }
#ifdef GC_OPENBSD_UTHREADS
#include <sys/syscall.h>
    EXTERN_C_BEGIN
    extern sigset_t __syscall(quad_t, ...);
    EXTERN_C_END
  STATIC ptr_t GC_find_limit_openbsd(ptr_t p, ptr_t bound)
  {
    static volatile ptr_t result;
    struct sigaction act;
    word pgsz = (word)sysconf(_SC_PAGESIZE);
    GC_ASSERT((word)bound >= pgsz);
    GC_ASSERT(I_HOLD_LOCK());
    act.sa_handler = GC_fault_handler_openbsd;
    sigemptyset(&act.sa_mask);
    act.sa_flags = SA_NODEFER | SA_RESTART;
    sigaction(SIGSEGV, &act, &old_segv_act);
    if (SETJMP(GC_jmp_buf_openbsd) == 0) {
      result = (ptr_t)((word)p & ~(pgsz-1));
      for (;;) {
        if ((word)result >= (word)bound - pgsz) {
          result = bound;
          break;
        }
        result += pgsz;
        GC_noop1((word)(*result));
      }
    }
#ifdef THREADS
      __syscall(SYS_sigprocmask, SIG_UNBLOCK, sigmask(SIGPROF));
#endif
    sigaction(SIGSEGV, &old_segv_act, 0);
    return(result);
  }
#endif
  static volatile int firstpass;
  STATIC ptr_t GC_skip_hole_openbsd(ptr_t p, ptr_t bound)
  {
    static volatile ptr_t result;
    struct sigaction act;
    word pgsz = (word)sysconf(_SC_PAGESIZE);
    GC_ASSERT((word)bound >= pgsz);
    GC_ASSERT(I_HOLD_LOCK());
    act.sa_handler = GC_fault_handler_openbsd;
    sigemptyset(&act.sa_mask);
    act.sa_flags = SA_NODEFER | SA_RESTART;
    sigaction(SIGSEGV, &act, &old_segv_act);
    firstpass = 1;
    result = (ptr_t)((word)p & ~(pgsz-1));
    if (SETJMP(GC_jmp_buf_openbsd) != 0 || firstpass) {
      firstpass = 0;
      if ((word)result >= (word)bound - pgsz) {
        result = bound;
      } else {
        result += pgsz;
        GC_noop1((word)(*result));
      }
    }
    sigaction(SIGSEGV, &old_segv_act, 0);
    return(result);
  }
#endif
#ifdef OS2
#include <stddef.h>
#if !defined(__IBMC__) && !defined(__WATCOMC__)
struct exe_hdr {
    unsigned short      magic_number;
    unsigned short      padding[29];
    long                new_exe_offset;
};
#define E_MAGIC(x)      (x).magic_number
#define EMAGIC          0x5A4D
#define E_LFANEW(x)     (x).new_exe_offset
struct e32_exe {
    unsigned char       magic_number[2];
    unsigned char       byte_order;
    unsigned char       word_order;
    unsigned long       exe_format_level;
    unsigned short      cpu;
    unsigned short      os;
    unsigned long       padding1[13];
    unsigned long       object_table_offset;
    unsigned long       object_count;
    unsigned long       padding2[31];
};
#define E32_MAGIC1(x)   (x).magic_number[0]
#define E32MAGIC1       'L'
#define E32_MAGIC2(x)   (x).magic_number[1]
#define E32MAGIC2       'X'
#define E32_BORDER(x)   (x).byte_order
#define E32LEBO         0
#define E32_WORDER(x)   (x).word_order
#define E32LEWO         0
#define E32_CPU(x)      (x).cpu
#define E32CPU286       1
#define E32_OBJTAB(x)   (x).object_table_offset
#define E32_OBJCNT(x)   (x).object_count
struct o32_obj {
    unsigned long       size;
    unsigned long       base;
    unsigned long       flags;
    unsigned long       pagemap;
    unsigned long       mapsize;
    unsigned long       reserved;
};
#define O32_FLAGS(x)    (x).flags
#define OBJREAD         0x0001L
#define OBJWRITE        0x0002L
#define OBJINVALID      0x0080L
#define O32_SIZE(x)     (x).size
#define O32_BASE(x)     (x).base
#else
#ifndef WORD
#define WORD unsigned short
#endif
#ifndef DWORD
#define DWORD unsigned long
#endif
#define EXE386 1
#include <newexe.h>
#include <exe386.h>
#endif
#define INCL_DOSEXCEPTIONS
#define INCL_DOSPROCESS
#define INCL_DOSERRORS
#define INCL_DOSMODULEMGR
#define INCL_DOSMEMMGR
#include <os2.h>
#endif
GC_INNER size_t GC_page_size = 0;
#if defined(MSWIN32) || defined(MSWINCE) || defined(CYGWIN32)
#ifndef VER_PLATFORM_WIN32_CE
#define VER_PLATFORM_WIN32_CE 3
#endif
#if defined(MSWINCE) && defined(THREADS)
    GC_INNER GC_bool GC_dont_query_stack_min = FALSE;
#endif
  GC_INNER SYSTEM_INFO GC_sysinfo;
  GC_INNER void GC_setpagesize(void)
  {
    GetSystemInfo(&GC_sysinfo);
#if defined(CYGWIN32) && (defined(MPROTECT_VDB) || defined(USE_MUNMAP))
      GC_page_size = (size_t)GC_sysinfo.dwAllocationGranularity;
      GC_ASSERT(GC_page_size >= (size_t)GC_sysinfo.dwPageSize);
#else
      GC_page_size = (size_t)GC_sysinfo.dwPageSize;
#endif
#if defined(MSWINCE) && !defined(_WIN32_WCE_EMULATION)
      {
        OSVERSIONINFO verInfo;
        verInfo.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
        if (!GetVersionEx(&verInfo))
          ABORT("GetVersionEx failed");
        if (verInfo.dwPlatformId == VER_PLATFORM_WIN32_CE &&
            verInfo.dwMajorVersion < 6) {
          GC_sysinfo.lpMaximumApplicationAddress = (LPVOID)((word)32 << 20);
#ifdef THREADS
            if (verInfo.dwMajorVersion < 5)
              GC_dont_query_stack_min = TRUE;
#endif
        }
      }
#endif
  }
#ifndef CYGWIN32
#define is_writable(prot) ((prot) == PAGE_READWRITE \
                            || (prot) == PAGE_WRITECOPY \
                            || (prot) == PAGE_EXECUTE_READWRITE \
                            || (prot) == PAGE_EXECUTE_WRITECOPY)
    STATIC word GC_get_writable_length(ptr_t p, ptr_t *base)
    {
      MEMORY_BASIC_INFORMATION buf;
      word result;
      word protect;
      result = VirtualQuery(p, &buf, sizeof(buf));
      if (result != sizeof(buf)) ABORT("Weird VirtualQuery result");
      if (base != 0) *base = (ptr_t)(buf.AllocationBase);
      protect = (buf.Protect & ~(PAGE_GUARD | PAGE_NOCACHE));
      if (!is_writable(protect)) {
        return(0);
      }
      if (buf.State != MEM_COMMIT) return(0);
      return(buf.RegionSize);
    }
    GC_API int GC_CALL GC_get_stack_base(struct GC_stack_base *sb)
    {
      ptr_t trunc_sp;
      word size;
      if (!GC_page_size) GC_setpagesize();
      trunc_sp = (ptr_t)((word)GC_approx_sp() & ~(GC_page_size - 1));
      size = GC_get_writable_length(trunc_sp, 0);
      GC_ASSERT(size != 0);
      sb -> mem_base = trunc_sp + size;
      return GC_SUCCESS;
    }
#else
    GC_API int GC_CALL GC_get_stack_base(struct GC_stack_base *sb)
    {
#ifdef X86_64
        sb -> mem_base = ((NT_TIB*)NtCurrentTeb())->StackBase;
#else
        void * _tlsbase;
        __asm__ ("movl %%fs:4, %0"
                 : "=r" (_tlsbase));
        sb -> mem_base = _tlsbase;
#endif
      return GC_SUCCESS;
    }
#endif
#define HAVE_GET_STACK_BASE
#else
  GC_INNER void GC_setpagesize(void)
  {
#if defined(MPROTECT_VDB) || defined(PROC_VDB) || defined(SOFT_VDB) \
       || defined(USE_MMAP)
      GC_page_size = (size_t)GETPAGESIZE();
#if !defined(CPPCHECK)
        if (0 == GC_page_size)
          ABORT("getpagesize failed");
#endif
#else
      GC_page_size = HBLKSIZE;
#endif
  }
#endif
#ifdef HAIKU
#include <kernel/OS.h>
  GC_API int GC_CALL GC_get_stack_base(struct GC_stack_base *sb)
  {
    thread_info th;
    get_thread_info(find_thread(NULL),&th);
    sb->mem_base = th.stack_end;
    return GC_SUCCESS;
  }
#define HAVE_GET_STACK_BASE
#endif
#ifdef OS2
  GC_API int GC_CALL GC_get_stack_base(struct GC_stack_base *sb)
  {
    PTIB ptib;
    PPIB ppib;
    if (DosGetInfoBlocks(&ptib, &ppib) != NO_ERROR) {
      WARN("DosGetInfoBlocks failed\n", 0);
      return GC_UNIMPLEMENTED;
    }
    sb->mem_base = ptib->tib_pstacklimit;
    return GC_SUCCESS;
  }
#define HAVE_GET_STACK_BASE
#endif
#ifdef AMIGA
#define GC_AMIGA_SB
#undef GC_AMIGA_SB
#define GET_MAIN_STACKBASE_SPECIAL
#endif
#if defined(NEED_FIND_LIMIT) || defined(UNIX_LIKE)
    typedef void (*GC_fault_handler_t)(int);
#if defined(SUNOS5SIGS) || defined(IRIX5) || defined(OSF1) \
       || defined(HAIKU) || defined(HURD) || defined(FREEBSD) \
       || defined(NETBSD)
        static struct sigaction old_segv_act;
#if defined(_sigargs)  \
           || defined(HURD) || defined(NETBSD) || defined(FREEBSD)
            static struct sigaction old_bus_act;
#endif
#else
      static GC_fault_handler_t old_segv_handler;
#ifdef HAVE_SIGBUS
        static GC_fault_handler_t old_bus_handler;
#endif
#endif
    GC_INNER void GC_set_and_save_fault_handler(GC_fault_handler_t h)
    {
#if defined(SUNOS5SIGS) || defined(IRIX5) || defined(OSF1) \
           || defined(HAIKU) || defined(HURD) || defined(FREEBSD) \
           || defined(NETBSD) || defined(OPENBSD)
          struct sigaction act;
          act.sa_handler = h;
#ifdef SIGACTION_FLAGS_NODEFER_HACK
            act.sa_flags = SA_RESTART | SA_NODEFER;
#else
            act.sa_flags = SA_RESTART;
#endif
          (void) sigemptyset(&act.sa_mask);
#ifdef GC_IRIX_THREADS
            (void) sigaction(SIGSEGV, 0, &old_segv_act);
            (void) sigaction(SIGSEGV, &act, 0);
#else
            (void) sigaction(SIGSEGV, &act, &old_segv_act);
#if defined(IRIX5) && defined(_sigargs)  \
               || defined(HURD) || defined(NETBSD) || defined(FREEBSD)
              (void) sigaction(SIGBUS, &act, &old_bus_act);
#endif
#endif
#else
          old_segv_handler = signal(SIGSEGV, h);
#ifdef HAVE_SIGBUS
            old_bus_handler = signal(SIGBUS, h);
#endif
#endif
#if defined(CPPCHECK) && defined(ADDRESS_SANITIZER)
          GC_noop1((word)&__asan_default_options);
#endif
    }
#endif
#if defined(NEED_FIND_LIMIT) \
     || (defined(USE_PROC_FOR_LIBRARIES) && defined(THREADS))
#define MIN_PAGE_SIZE 256
    GC_INNER JMP_BUF GC_jmp_buf;
    STATIC void GC_fault_handler(int sig GC_ATTR_UNUSED)
    {
        LONGJMP(GC_jmp_buf, 1);
    }
    GC_INNER void GC_setup_temporary_fault_handler(void)
    {
        GC_ASSERT(I_HOLD_LOCK());
        GC_set_and_save_fault_handler(GC_fault_handler);
    }
    GC_INNER void GC_reset_fault_handler(void)
    {
#if defined(SUNOS5SIGS) || defined(IRIX5) || defined(OSF1) \
           || defined(HAIKU) || defined(HURD) || defined(FREEBSD) \
           || defined(NETBSD) || defined(OPENBSD)
          (void) sigaction(SIGSEGV, &old_segv_act, 0);
#if defined(IRIX5) && defined(_sigargs)  \
             || defined(HURD) || defined(NETBSD)
              (void) sigaction(SIGBUS, &old_bus_act, 0);
#endif
#else
          (void) signal(SIGSEGV, old_segv_handler);
#ifdef HAVE_SIGBUS
            (void) signal(SIGBUS, old_bus_handler);
#endif
#endif
    }
    GC_ATTR_NO_SANITIZE_ADDR
    STATIC ptr_t GC_find_limit_with_bound(ptr_t p, GC_bool up, ptr_t bound)
    {
        static volatile ptr_t result;
        GC_ASSERT(up ? (word)bound >= MIN_PAGE_SIZE
                     : (word)bound <= ~(word)MIN_PAGE_SIZE);
        GC_ASSERT(I_HOLD_LOCK());
        GC_setup_temporary_fault_handler();
        if (SETJMP(GC_jmp_buf) == 0) {
            result = (ptr_t)(((word)(p))
                              & ~(MIN_PAGE_SIZE-1));
            for (;;) {
                if (up) {
                    if ((word)result >= (word)bound - MIN_PAGE_SIZE) {
                      result = bound;
                      break;
                    }
                    result += MIN_PAGE_SIZE;
                } else {
                    if ((word)result <= (word)bound + MIN_PAGE_SIZE) {
                      result = bound - MIN_PAGE_SIZE;
                      break;
                    }
                    result -= MIN_PAGE_SIZE;
                }
                GC_noop1((word)(*result));
            }
        }
        GC_reset_fault_handler();
        if (!up) {
            result += MIN_PAGE_SIZE;
        }
        return(result);
    }
    void * GC_find_limit(void * p, int up)
    {
        return GC_find_limit_with_bound((ptr_t)p, (GC_bool)up,
                                        up ? (ptr_t)GC_WORD_MAX : 0);
    }
#endif
#ifdef HPUX_MAIN_STACKBOTTOM
#include <sys/param.h>
#include <sys/pstat.h>
  STATIC ptr_t GC_hpux_main_stack_base(void)
  {
    struct pst_vm_status vm_status;
    int i = 0;
    while (pstat_getprocvm(&vm_status, sizeof(vm_status), 0, i++) == 1) {
      if (vm_status.pst_type == PS_STACK)
        return (ptr_t)vm_status.pst_vaddr;
    }
#ifdef STACK_GROWS_UP
      return (ptr_t)GC_find_limit(GC_approx_sp(),  FALSE);
#else
      return (ptr_t)GC_find_limit(GC_approx_sp(), TRUE);
#endif
  }
#endif
#ifdef HPUX_STACKBOTTOM
#include <sys/param.h>
#include <sys/pstat.h>
  GC_INNER ptr_t GC_get_register_stack_base(void)
  {
    struct pst_vm_status vm_status;
    int i = 0;
    while (pstat_getprocvm(&vm_status, sizeof(vm_status), 0, i++) == 1) {
      if (vm_status.pst_type == PS_RSESTACK) {
        return (ptr_t) vm_status.pst_vaddr;
      }
    }
    return (ptr_t)(((word)GC_stackbottom - BACKING_STORE_DISPLACEMENT - 1)
                   & ~(BACKING_STORE_ALIGNMENT - 1));
  }
#endif
#ifdef LINUX_STACKBOTTOM
#include <sys/types.h>
#include <sys/stat.h>
#define STAT_SKIP 27
#ifdef USE_LIBC_PRIVATES
    EXTERN_C_BEGIN
#pragma weak __libc_stack_end
    extern ptr_t __libc_stack_end;
#ifdef IA64
#pragma weak __libc_ia64_register_backing_store_base
      extern ptr_t __libc_ia64_register_backing_store_base;
#endif
    EXTERN_C_END
#endif
#ifdef IA64
    GC_INNER ptr_t GC_get_register_stack_base(void)
    {
      ptr_t result;
#ifdef USE_LIBC_PRIVATES
        if (0 != &__libc_ia64_register_backing_store_base
            && 0 != __libc_ia64_register_backing_store_base) {
          return __libc_ia64_register_backing_store_base;
        }
#endif
      result = backing_store_base_from_proc();
      if (0 == result) {
          result = (ptr_t)GC_find_limit(GC_save_regs_in_stack(), FALSE);
      }
      return result;
    }
#endif
  STATIC ptr_t GC_linux_main_stack_base(void)
  {
#define STAT_BUF_SIZE 4096
    char stat_buf[STAT_BUF_SIZE];
    int f;
    word result;
    ssize_t i, buf_offset = 0, len;
#ifdef USE_LIBC_PRIVATES
      if (0 != &__libc_stack_end && 0 != __libc_stack_end ) {
#if defined(IA64)
          if (((word)__libc_stack_end & 0xfff) + 0x10 < 0x1000) {
            return __libc_stack_end + 0x10;
          }
#elif defined(SPARC)
          if (__libc_stack_end != (ptr_t) (unsigned long)0x1)
            return __libc_stack_end;
#else
          return __libc_stack_end;
#endif
      }
#endif
    f = open("/proc/self/stat", O_RDONLY);
    if (-1 == f)
      ABORT_ARG1("Could not open /proc/self/stat", ": errno= %d", errno);
    len = GC_repeat_read(f, stat_buf, sizeof(stat_buf));
    if (len < 0)
      ABORT_ARG1("Failed to read /proc/self/stat",
                 ": errno= %d", errno);
    close(f);
    for (i = 0; i < STAT_SKIP; ++i) {
      while (buf_offset < len && isspace(stat_buf[buf_offset++])) {
      }
      while (buf_offset < len && !isspace(stat_buf[buf_offset++])) {
      }
    }
    while (buf_offset < len && isspace(stat_buf[buf_offset])) {
      buf_offset++;
    }
    for (i = 0; buf_offset + i < len; i++) {
      if (!isdigit(stat_buf[buf_offset + i])) break;
    }
    if (buf_offset + i >= len) ABORT("Could not parse /proc/self/stat");
    stat_buf[buf_offset + i] = '\0';
    result = (word)STRTOULL(&stat_buf[buf_offset], NULL, 10);
    if (result < 0x100000 || (result & (sizeof(word) - 1)) != 0)
      ABORT_ARG1("Absurd stack bottom value",
                 ": 0x%lx", (unsigned long)result);
    return (ptr_t)result;
  }
#endif
#ifdef FREEBSD_STACKBOTTOM
#include <unistd.h>
#include <sys/types.h>
#include <sys/sysctl.h>
  STATIC ptr_t GC_freebsd_main_stack_base(void)
  {
    int nm[2] = {CTL_KERN, KERN_USRSTACK};
    ptr_t base;
    size_t len = sizeof(ptr_t);
    int r = sysctl(nm, 2, &base, &len, NULL, 0);
    if (r) ABORT("Error getting main stack base");
    return base;
  }
#endif
#if defined(ECOS) || defined(NOSYS)
  ptr_t GC_get_main_stack_base(void)
  {
    return STACKBOTTOM;
  }
#define GET_MAIN_STACKBASE_SPECIAL
#elif defined(SYMBIAN)
  EXTERN_C_BEGIN
  extern int GC_get_main_symbian_stack_base(void);
  EXTERN_C_END
  ptr_t GC_get_main_stack_base(void)
  {
    return (ptr_t)GC_get_main_symbian_stack_base();
  }
#define GET_MAIN_STACKBASE_SPECIAL
#elif defined(EMSCRIPTEN)
#include <emscripten.h>
  static void* emscripten_stack_base;
  static void scan_stack_cb(void *begin, void *end)
  {
    (void)begin;
    emscripten_stack_base = end;
  }
  ptr_t GC_get_main_stack_base(void)
  {
    emscripten_scan_stack(scan_stack_cb);
    return (ptr_t)emscripten_stack_base;
  }
#define GET_MAIN_STACKBASE_SPECIAL
#elif !defined(AMIGA) && !defined(HAIKU) && !defined(OS2) \
      && !defined(MSWIN32) && !defined(MSWINCE) && !defined(CYGWIN32) \
      && !defined(GC_OPENBSD_THREADS) \
      && (!defined(GC_SOLARIS_THREADS) || defined(_STRICT_STDC))
#if (defined(HAVE_PTHREAD_ATTR_GET_NP) || defined(HAVE_PTHREAD_GETATTR_NP)) \
     && (defined(THREADS) || defined(USE_GET_STACKBASE_FOR_MAIN))
#include <pthread.h>
#ifdef HAVE_PTHREAD_NP_H
#include <pthread_np.h>
#endif
#elif defined(DARWIN) && !defined(NO_PTHREAD_GET_STACKADDR_NP)
#include <pthread.h>
#undef STACKBOTTOM
#define STACKBOTTOM (ptr_t)pthread_get_stackaddr_np(pthread_self())
#endif
  ptr_t GC_get_main_stack_base(void)
  {
    ptr_t result;
#if (defined(HAVE_PTHREAD_ATTR_GET_NP) \
        || defined(HAVE_PTHREAD_GETATTR_NP)) \
       && (defined(USE_GET_STACKBASE_FOR_MAIN) \
           || (defined(THREADS) && !defined(REDIRECT_MALLOC)))
      pthread_attr_t attr;
      void *stackaddr;
      size_t size;
#ifdef HAVE_PTHREAD_ATTR_GET_NP
        if (pthread_attr_init(&attr) == 0
            && (pthread_attr_get_np(pthread_self(), &attr) == 0
                ? TRUE : (pthread_attr_destroy(&attr), FALSE)))
#else
        if (pthread_getattr_np(pthread_self(), &attr) == 0)
#endif
      {
        if (pthread_attr_getstack(&attr, &stackaddr, &size) == 0
            && stackaddr != NULL) {
          (void)pthread_attr_destroy(&attr);
#ifdef STACK_GROWS_DOWN
            stackaddr = (char *)stackaddr + size;
#endif
          return (ptr_t)stackaddr;
        }
        (void)pthread_attr_destroy(&attr);
      }
      WARN("pthread_getattr_np or pthread_attr_getstack failed"
           " for main thread\n", 0);
#endif
#ifdef STACKBOTTOM
      result = STACKBOTTOM;
#else
#ifdef HEURISTIC1
#define STACKBOTTOM_ALIGNMENT_M1 ((word)STACK_GRAN - 1)
#ifdef STACK_GROWS_DOWN
          result = (ptr_t)(((word)GC_approx_sp() + STACKBOTTOM_ALIGNMENT_M1)
                           & ~STACKBOTTOM_ALIGNMENT_M1);
#else
          result = (ptr_t)((word)GC_approx_sp() & ~STACKBOTTOM_ALIGNMENT_M1);
#endif
#elif defined(HPUX_MAIN_STACKBOTTOM)
        result = GC_hpux_main_stack_base();
#elif defined(LINUX_STACKBOTTOM)
        result = GC_linux_main_stack_base();
#elif defined(FREEBSD_STACKBOTTOM)
        result = GC_freebsd_main_stack_base();
#elif defined(HEURISTIC2)
        {
          ptr_t sp = GC_approx_sp();
#ifdef STACK_GROWS_DOWN
            result = (ptr_t)GC_find_limit(sp, TRUE);
#if defined(HEURISTIC2_LIMIT) && !defined(CPPCHECK)
              if ((word)result > (word)HEURISTIC2_LIMIT
                  && (word)sp < (word)HEURISTIC2_LIMIT) {
                result = HEURISTIC2_LIMIT;
              }
#endif
#else
            result = (ptr_t)GC_find_limit(sp, FALSE);
#if defined(HEURISTIC2_LIMIT) && !defined(CPPCHECK)
              if ((word)result < (word)HEURISTIC2_LIMIT
                  && (word)sp > (word)HEURISTIC2_LIMIT) {
                result = HEURISTIC2_LIMIT;
              }
#endif
#endif
        }
#elif defined(STACK_NOT_SCANNED) || defined(CPPCHECK)
        result = NULL;
#else
#error None of HEURISTIC* and *STACKBOTTOM defined!
#endif
#if defined(STACK_GROWS_DOWN) && !defined(CPPCHECK)
        if (result == 0)
          result = (ptr_t)(signed_word)(-sizeof(ptr_t));
#endif
#endif
#if !defined(CPPCHECK)
      GC_ASSERT((word)GC_approx_sp() HOTTER_THAN (word)result);
#endif
    return(result);
  }
#define GET_MAIN_STACKBASE_SPECIAL
#endif
#if (defined(HAVE_PTHREAD_ATTR_GET_NP) || defined(HAVE_PTHREAD_GETATTR_NP)) \
    && defined(THREADS) && !defined(HAVE_GET_STACK_BASE)
#include <pthread.h>
#ifdef HAVE_PTHREAD_NP_H
#include <pthread_np.h>
#endif
  GC_API int GC_CALL GC_get_stack_base(struct GC_stack_base *b)
  {
    pthread_attr_t attr;
    size_t size;
#ifdef IA64
      DCL_LOCK_STATE;
#endif
#ifdef HAVE_PTHREAD_ATTR_GET_NP
      if (pthread_attr_init(&attr) != 0)
        ABORT("pthread_attr_init failed");
      if (pthread_attr_get_np(pthread_self(), &attr) != 0) {
        WARN("pthread_attr_get_np failed\n", 0);
        (void)pthread_attr_destroy(&attr);
        return GC_UNIMPLEMENTED;
      }
#else
      if (pthread_getattr_np(pthread_self(), &attr) != 0) {
        WARN("pthread_getattr_np failed\n", 0);
        return GC_UNIMPLEMENTED;
      }
#endif
    if (pthread_attr_getstack(&attr, &(b -> mem_base), &size) != 0) {
        ABORT("pthread_attr_getstack failed");
    }
    (void)pthread_attr_destroy(&attr);
#ifdef STACK_GROWS_DOWN
        b -> mem_base = (char *)(b -> mem_base) + size;
#endif
#ifdef IA64
      LOCK();
      {
        IF_CANCEL(int cancel_state;)
        ptr_t bsp;
        ptr_t next_stack;
        DISABLE_CANCEL(cancel_state);
        bsp = GC_save_regs_in_stack();
        next_stack = GC_greatest_stack_base_below(bsp);
        if (0 == next_stack) {
          b -> reg_base = GC_find_limit(bsp, FALSE);
        } else {
          b -> reg_base = GC_find_limit_with_bound(bsp, FALSE, next_stack);
        }
        RESTORE_CANCEL(cancel_state);
      }
      UNLOCK();
#endif
    return GC_SUCCESS;
  }
#define HAVE_GET_STACK_BASE
#endif
#if defined(GC_DARWIN_THREADS) && !defined(NO_PTHREAD_GET_STACKADDR_NP)
#include <pthread.h>
  GC_API int GC_CALL GC_get_stack_base(struct GC_stack_base *b)
  {
    b->mem_base = pthread_get_stackaddr_np(pthread_self());
    GC_ASSERT((word)GC_approx_sp() HOTTER_THAN (word)b->mem_base);
    return GC_SUCCESS;
  }
#define HAVE_GET_STACK_BASE
#endif
#ifdef GC_OPENBSD_THREADS
#include <sys/signal.h>
#include <pthread.h>
#include <pthread_np.h>
  GC_API int GC_CALL GC_get_stack_base(struct GC_stack_base *sb)
  {
    stack_t stack;
    if (pthread_stackseg_np(pthread_self(), &stack))
      ABORT("pthread_stackseg_np(self) failed");
    sb->mem_base = stack.ss_sp;
    return GC_SUCCESS;
  }
#define HAVE_GET_STACK_BASE
#endif
#if defined(GC_SOLARIS_THREADS) && !defined(_STRICT_STDC)
#include <thread.h>
#include <signal.h>
#include <pthread.h>
  static pthread_t stackbase_main_self = 0;
  static void *stackbase_main_ss_sp = NULL;
  GC_API int GC_CALL GC_get_stack_base(struct GC_stack_base *b)
  {
    stack_t s;
    pthread_t self = pthread_self();
    if (self == stackbase_main_self)
      {
        b -> mem_base = stackbase_main_ss_sp;
        GC_ASSERT(b -> mem_base != NULL);
        return GC_SUCCESS;
      }
    if (thr_stksegment(&s)) {
      ABORT("thr_stksegment failed");
    }
    GC_ASSERT((word)GC_approx_sp() HOTTER_THAN (word)s.ss_sp);
    if (!stackbase_main_self && thr_main() != 0)
      {
        stackbase_main_ss_sp = s.ss_sp;
        stackbase_main_self = self;
      }
    b -> mem_base = s.ss_sp;
    return GC_SUCCESS;
  }
#define HAVE_GET_STACK_BASE
#endif
#ifdef GC_RTEMS_PTHREADS
  GC_API int GC_CALL GC_get_stack_base(struct GC_stack_base *sb)
  {
    sb->mem_base = rtems_get_stack_bottom();
    return GC_SUCCESS;
  }
#define HAVE_GET_STACK_BASE
#endif
#ifndef HAVE_GET_STACK_BASE
#ifdef NEED_FIND_LIMIT
    GC_API int GC_CALL GC_get_stack_base(struct GC_stack_base *b)
    {
      IF_CANCEL(int cancel_state;)
      DCL_LOCK_STATE;
      LOCK();
      DISABLE_CANCEL(cancel_state);
#ifdef STACK_GROWS_DOWN
        b -> mem_base = GC_find_limit(GC_approx_sp(), TRUE);
#ifdef IA64
          b -> reg_base = GC_find_limit(GC_save_regs_in_stack(), FALSE);
#endif
#else
        b -> mem_base = GC_find_limit(GC_approx_sp(), FALSE);
#endif
      RESTORE_CANCEL(cancel_state);
      UNLOCK();
      return GC_SUCCESS;
    }
#else
    GC_API int GC_CALL GC_get_stack_base(
                                struct GC_stack_base *b GC_ATTR_UNUSED)
    {
#if defined(GET_MAIN_STACKBASE_SPECIAL) && !defined(THREADS) \
         && !defined(IA64)
        b->mem_base = GC_get_main_stack_base();
        return GC_SUCCESS;
#else
        return GC_UNIMPLEMENTED;
#endif
    }
#endif
#endif
#ifndef GET_MAIN_STACKBASE_SPECIAL
  ptr_t GC_get_main_stack_base(void)
  {
    struct GC_stack_base sb;
    if (GC_get_stack_base(&sb) != GC_SUCCESS)
      ABORT("GC_get_stack_base failed");
    GC_ASSERT((word)GC_approx_sp() HOTTER_THAN (word)sb.mem_base);
    return (ptr_t)sb.mem_base;
  }
#endif
#ifdef OS2
void GC_register_data_segments(void)
{
    PTIB ptib;
    PPIB ppib;
    HMODULE module_handle;
#define PBUFSIZ 512
    UCHAR path[PBUFSIZ];
    FILE * myexefile;
    struct exe_hdr hdrdos;
    struct e32_exe hdr386;
    struct o32_obj seg;
    int nsegs;
#if defined(CPPCHECK)
        hdrdos.padding[0] = 0;
        hdr386.exe_format_level = 0;
        hdr386.os = 0;
        hdr386.padding1[0] = 0;
        hdr386.padding2[0] = 0;
        seg.pagemap = 0;
        seg.mapsize = 0;
        seg.reserved = 0;
#endif
    if (DosGetInfoBlocks(&ptib, &ppib) != NO_ERROR) {
        ABORT("DosGetInfoBlocks failed");
    }
    module_handle = ppib -> pib_hmte;
    if (DosQueryModuleName(module_handle, PBUFSIZ, path) != NO_ERROR) {
        ABORT("DosQueryModuleName failed");
    }
    myexefile = fopen(path, "rb");
    if (myexefile == 0) {
        ABORT_ARG1("Failed to open executable", ": %s", path);
    }
    if (fread((char *)(&hdrdos), 1, sizeof(hdrdos), myexefile)
          < sizeof(hdrdos)) {
        ABORT_ARG1("Could not read MSDOS header", " from: %s", path);
    }
    if (E_MAGIC(hdrdos) != EMAGIC) {
        ABORT_ARG1("Bad DOS magic number", " in file: %s", path);
    }
    if (fseek(myexefile, E_LFANEW(hdrdos), SEEK_SET) != 0) {
        ABORT_ARG1("Bad DOS magic number", " in file: %s", path);
    }
    if (fread((char *)(&hdr386), 1, sizeof(hdr386), myexefile)
          < sizeof(hdr386)) {
        ABORT_ARG1("Could not read OS/2 header", " from: %s", path);
    }
    if (E32_MAGIC1(hdr386) != E32MAGIC1 || E32_MAGIC2(hdr386) != E32MAGIC2) {
        ABORT_ARG1("Bad OS/2 magic number", " in file: %s", path);
    }
    if (E32_BORDER(hdr386) != E32LEBO || E32_WORDER(hdr386) != E32LEWO) {
        ABORT_ARG1("Bad byte order in executable", " file: %s", path);
    }
    if (E32_CPU(hdr386) == E32CPU286) {
        ABORT_ARG1("GC cannot handle 80286 executables", ": %s", path);
    }
    if (fseek(myexefile, E_LFANEW(hdrdos) + E32_OBJTAB(hdr386),
              SEEK_SET) != 0) {
        ABORT_ARG1("Seek to object table failed", " in file: %s", path);
    }
    for (nsegs = E32_OBJCNT(hdr386); nsegs > 0; nsegs--) {
      int flags;
      if (fread((char *)(&seg), 1, sizeof(seg), myexefile) < sizeof(seg)) {
        ABORT_ARG1("Could not read obj table entry", " from file: %s", path);
      }
      flags = O32_FLAGS(seg);
      if (!(flags & OBJWRITE)) continue;
      if (!(flags & OBJREAD)) continue;
      if (flags & OBJINVALID) {
          GC_err_printf("Object with invalid pages?\n");
          continue;
      }
      GC_add_roots_inner((ptr_t)O32_BASE(seg),
                         (ptr_t)(O32_BASE(seg)+O32_SIZE(seg)), FALSE);
    }
    (void)fclose(myexefile);
}
#else
#if defined(GWW_VDB)
#ifndef MEM_WRITE_WATCH
#define MEM_WRITE_WATCH 0x200000
#endif
#ifndef WRITE_WATCH_FLAG_RESET
#define WRITE_WATCH_FLAG_RESET 1
#endif
#define GC_ULONG_PTR word
    typedef UINT (WINAPI * GetWriteWatch_type)(
                                DWORD, PVOID, GC_ULONG_PTR ,
                                PVOID *, GC_ULONG_PTR *, PULONG);
    static FARPROC GetWriteWatch_func;
    static DWORD GetWriteWatch_alloc_flag;
#define GC_GWW_AVAILABLE() (GetWriteWatch_func != 0)
    static void detect_GetWriteWatch(void)
    {
      static GC_bool done;
      HMODULE hK32;
      if (done)
        return;
#if defined(MPROTECT_VDB)
        {
          char * str = GETENV("GC_USE_GETWRITEWATCH");
#if defined(GC_PREFER_MPROTECT_VDB)
            if (str == NULL || (*str == '0' && *(str + 1) == '\0')) {
              done = TRUE;
              return;
            }
#else
            if (str != NULL && *str == '0' && *(str + 1) == '\0') {
              done = TRUE;
              return;
            }
#endif
        }
#endif
#ifdef MSWINRT_FLAVOR
        {
          MEMORY_BASIC_INFORMATION memInfo;
          SIZE_T result = VirtualQuery(GetProcAddress,
                                       &memInfo, sizeof(memInfo));
          if (result != sizeof(memInfo))
            ABORT("Weird VirtualQuery result");
          hK32 = (HMODULE)memInfo.AllocationBase;
        }
#else
        hK32 = GetModuleHandle(TEXT("kernel32.dll"));
#endif
      if (hK32 != (HMODULE)0 &&
          (GetWriteWatch_func = GetProcAddress(hK32, "GetWriteWatch")) != 0) {
        void * page;
        GC_ASSERT(GC_page_size != 0);
        page = VirtualAlloc(NULL, GC_page_size, MEM_WRITE_WATCH | MEM_RESERVE,
                            PAGE_READWRITE);
        if (page != NULL) {
          PVOID pages[16];
          GC_ULONG_PTR count = 16;
          DWORD page_size;
          if ((*(GetWriteWatch_type)(word)GetWriteWatch_func)(
                                        WRITE_WATCH_FLAG_RESET, page,
                                        GC_page_size, pages, &count,
                                        &page_size) != 0) {
            GetWriteWatch_func = 0;
          } else {
            GetWriteWatch_alloc_flag = MEM_WRITE_WATCH;
          }
          VirtualFree(page, 0 , MEM_RELEASE);
        } else {
          GetWriteWatch_func = 0;
        }
      }
      done = TRUE;
    }
#else
#define GetWriteWatch_alloc_flag 0
#endif
#if defined(MSWIN32) || defined(MSWINCE) || defined(CYGWIN32)
#ifdef MSWIN32
  GC_INNER GC_bool GC_no_win32_dlls = FALSE;
  GC_INNER GC_bool GC_wnt = FALSE;
  GC_INNER void GC_init_win32(void)
  {
#if defined(_WIN64) || (defined(_MSC_VER) && _MSC_VER >= 1800)
      GC_wnt = TRUE;
#else
      DWORD v = GetVersion();
      GC_wnt = !(v & 0x80000000);
      GC_no_win32_dlls |= ((!GC_wnt) && (v & 0xff) <= 3);
#endif
#ifdef USE_MUNMAP
      if (GC_no_win32_dlls) {
        GC_unmap_threshold = 0;
      }
#endif
  }
  STATIC ptr_t GC_least_described_address(ptr_t start)
  {
    MEMORY_BASIC_INFORMATION buf;
    LPVOID limit = GC_sysinfo.lpMinimumApplicationAddress;
    ptr_t p = (ptr_t)((word)start & ~(GC_page_size - 1));
    GC_ASSERT(GC_page_size != 0);
    for (;;) {
        size_t result;
        LPVOID q = (LPVOID)(p - GC_page_size);
        if ((word)q > (word)p  || (word)q < (word)limit) break;
        result = VirtualQuery(q, &buf, sizeof(buf));
        if (result != sizeof(buf) || buf.AllocationBase == 0) break;
        p = (ptr_t)(buf.AllocationBase);
    }
    return p;
  }
#endif
#if defined(USE_WINALLOC) && !defined(REDIRECT_MALLOC)
  STATIC size_t GC_max_root_size = 100000;
  STATIC struct GC_malloc_heap_list {
    void * allocation_base;
    struct GC_malloc_heap_list *next;
  } *GC_malloc_heap_l = 0;
  STATIC GC_bool GC_is_malloc_heap_base(void *p)
  {
    struct GC_malloc_heap_list *q = GC_malloc_heap_l;
    while (0 != q) {
      if (q -> allocation_base == p) return TRUE;
      q = q -> next;
    }
    return FALSE;
  }
  STATIC void *GC_get_allocation_base(void *p)
  {
    MEMORY_BASIC_INFORMATION buf;
    size_t result = VirtualQuery(p, &buf, sizeof(buf));
    if (result != sizeof(buf)) {
      ABORT("Weird VirtualQuery result");
    }
    return buf.AllocationBase;
  }
  GC_INNER void GC_add_current_malloc_heap(void)
  {
    struct GC_malloc_heap_list *new_l = (struct GC_malloc_heap_list *)
                 malloc(sizeof(struct GC_malloc_heap_list));
    void *candidate;
    if (NULL == new_l) return;
    candidate = GC_get_allocation_base(new_l);
    if (GC_is_malloc_heap_base(candidate)) {
        size_t req_size = 10000;
        do {
          void *p = malloc(req_size);
          if (0 == p) {
            free(new_l);
            return;
          }
          candidate = GC_get_allocation_base(p);
          free(p);
          req_size *= 2;
        } while (GC_is_malloc_heap_base(candidate)
                 && req_size < GC_max_root_size/10 && req_size < 500000);
        if (GC_is_malloc_heap_base(candidate)) {
          free(new_l);
          return;
        }
    }
    GC_COND_LOG_PRINTF("Found new system malloc AllocationBase at %p\n",
                       candidate);
    new_l -> allocation_base = candidate;
    new_l -> next = GC_malloc_heap_l;
    GC_malloc_heap_l = new_l;
  }
  STATIC void GC_free_malloc_heap_list(void)
  {
    struct GC_malloc_heap_list *q = GC_malloc_heap_l;
    GC_malloc_heap_l = NULL;
    while (q != NULL) {
      struct GC_malloc_heap_list *next = q -> next;
      free(q);
      q = next;
    }
  }
#endif
  GC_INNER GC_bool GC_is_heap_base(void *p)
  {
     int i;
#if defined(USE_WINALLOC) && !defined(REDIRECT_MALLOC)
       if (GC_root_size > GC_max_root_size)
         GC_max_root_size = GC_root_size;
       if (GC_is_malloc_heap_base(p))
         return TRUE;
#endif
     for (i = 0; i < (int)GC_n_heap_bases; i++) {
         if (GC_heap_bases[i] == p) return TRUE;
     }
     return FALSE;
  }
#ifdef MSWIN32
  STATIC void GC_register_root_section(ptr_t static_root)
  {
      MEMORY_BASIC_INFORMATION buf;
      LPVOID p;
      char * base;
      char * limit;
      if (!GC_no_win32_dlls) return;
      p = base = limit = GC_least_described_address(static_root);
      while ((word)p < (word)GC_sysinfo.lpMaximumApplicationAddress) {
        size_t result = VirtualQuery(p, &buf, sizeof(buf));
        char * new_limit;
        DWORD protect;
        if (result != sizeof(buf) || buf.AllocationBase == 0
            || GC_is_heap_base(buf.AllocationBase)) break;
        new_limit = (char *)p + buf.RegionSize;
        protect = buf.Protect;
        if (buf.State == MEM_COMMIT
            && is_writable(protect)) {
            if ((char *)p == limit) {
                limit = new_limit;
            } else {
                if (base != limit) GC_add_roots_inner(base, limit, FALSE);
                base = (char *)p;
                limit = new_limit;
            }
        }
        if ((word)p > (word)new_limit ) break;
        p = (LPVOID)new_limit;
      }
      if (base != limit) GC_add_roots_inner(base, limit, FALSE);
  }
#endif
  void GC_register_data_segments(void)
  {
#ifdef MSWIN32
      GC_register_root_section((ptr_t)&GC_pages_executable);
#endif
  }
#else
#if (defined(SVR4) || defined(AIX) || defined(DGUX) \
      || (defined(LINUX) && defined(SPARC))) && !defined(PCR)
  ptr_t GC_SysVGetDataStart(size_t max_page_size, ptr_t etext_addr)
  {
    word text_end = ((word)(etext_addr) + sizeof(word) - 1)
                    & ~(word)(sizeof(word) - 1);
    word next_page = ((text_end + (word)max_page_size - 1)
                      & ~((word)max_page_size - 1));
    word page_offset = (text_end & ((word)max_page_size - 1));
    volatile ptr_t result = (char *)(next_page + page_offset);
    GC_setup_temporary_fault_handler();
    if (SETJMP(GC_jmp_buf) == 0) {
#ifdef AO_HAVE_fetch_and_add
          volatile AO_t zero = 0;
          (void)AO_fetch_and_add((volatile AO_t *)result, zero);
#else
          char v = *result;
#if defined(CPPCHECK)
            GC_noop1((word)&v);
#endif
          *result = v;
#endif
        GC_reset_fault_handler();
    } else {
        GC_reset_fault_handler();
        result = (char *)GC_find_limit(DATAEND, FALSE);
    }
    return ( ptr_t)result;
  }
#endif
#ifdef DATASTART_USES_BSDGETDATASTART
  GC_INNER ptr_t GC_FreeBSDGetDataStart(size_t max_page_size,
                                        ptr_t etext_addr)
  {
    word text_end = ((word)(etext_addr) + sizeof(word) - 1)
                     & ~(word)(sizeof(word) - 1);
    volatile word next_page = (text_end + (word)max_page_size - 1)
                              & ~((word)max_page_size - 1);
    volatile ptr_t result = (ptr_t)text_end;
    GC_setup_temporary_fault_handler();
    if (SETJMP(GC_jmp_buf) == 0) {
        for (; next_page < (word)DATAEND; next_page += (word)max_page_size)
            *(volatile char *)next_page;
        GC_reset_fault_handler();
    } else {
        GC_reset_fault_handler();
        result = (ptr_t)GC_find_limit(DATAEND, FALSE);
    }
    return(result);
  }
#endif
#ifdef AMIGA
#define GC_AMIGA_DS
#undef GC_AMIGA_DS
#elif defined(OPENBSD)
void GC_register_data_segments(void)
{
  ptr_t region_start = DATASTART;
  if ((word)region_start - 1U >= (word)DATAEND)
    ABORT_ARG2("Wrong DATASTART/END pair",
               ": %p .. %p", (void *)region_start, (void *)DATAEND);
  for (;;) {
#ifdef GC_OPENBSD_UTHREADS
      ptr_t region_end = GC_find_limit_openbsd(region_start, DATAEND);
#else
      ptr_t region_end = GC_find_limit_with_bound(region_start, TRUE, DATAEND);
#endif
    GC_add_roots_inner(region_start, region_end, FALSE);
    if ((word)region_end >= (word)DATAEND)
      break;
    region_start = GC_skip_hole_openbsd(region_end, DATAEND);
  }
}
#else
#if !defined(PCR) && !defined(MACOS) && defined(REDIRECT_MALLOC) \
     && defined(GC_SOLARIS_THREADS)
    EXTERN_C_BEGIN
    extern caddr_t sbrk(int);
    EXTERN_C_END
#endif
  void GC_register_data_segments(void)
  {
#if !defined(PCR) && !defined(MACOS)
#if defined(REDIRECT_MALLOC) && defined(GC_SOLARIS_THREADS)
        GC_ASSERT(DATASTART);
        {
          ptr_t p = (ptr_t)sbrk(0);
          if ((word)DATASTART < (word)p)
            GC_add_roots_inner(DATASTART, p, FALSE);
        }
#else
        if ((word)DATASTART - 1U >= (word)DATAEND) {
          ABORT_ARG2("Wrong DATASTART/END pair",
                     ": %p .. %p", (void *)DATASTART, (void *)DATAEND);
        }
        GC_add_roots_inner(DATASTART, DATAEND, FALSE);
#ifdef GC_HAVE_DATAREGION2
          if ((word)DATASTART2 - 1U >= (word)DATAEND2)
            ABORT_ARG2("Wrong DATASTART/END2 pair",
                       ": %p .. %p", (void *)DATASTART2, (void *)DATAEND2);
          GC_add_roots_inner(DATASTART2, DATAEND2, FALSE);
#endif
#endif
#endif
#if defined(MACOS)
    {
#if defined(THINK_C)
        extern void* GC_MacGetDataStart(void);
        GC_add_roots_inner((ptr_t)GC_MacGetDataStart(),
                           (ptr_t)LMGetCurrentA5(), FALSE);
#else
#if defined(__MWERKS__)
#if !__POWERPC__
          extern void* GC_MacGetDataStart(void);
#if __option(far_data)
          extern void* GC_MacGetDataEnd(void);
#endif
          GC_add_roots_inner((ptr_t)GC_MacGetDataStart(),
                             (ptr_t)LMGetCurrentA5(), FALSE);
#if __option(far_data)
          GC_add_roots_inner((ptr_t)LMGetCurrentA5(),
                             (ptr_t)GC_MacGetDataEnd(), FALSE);
#endif
#else
          extern char __data_start__[], __data_end__[];
          GC_add_roots_inner((ptr_t)&__data_start__,
                             (ptr_t)&__data_end__, FALSE);
#endif
#endif
#endif
    }
#endif
  }
#endif
#endif
#endif
#if !defined(OS2) && !defined(PCR) && !defined(AMIGA) \
     && !defined(USE_WINALLOC) && !defined(MACOS) && !defined(DOS4GW) \
     && !defined(NINTENDO_SWITCH) && !defined(NONSTOP) \
     && !defined(SN_TARGET_ORBIS) && !defined(SN_TARGET_PS3) \
     && !defined(SN_TARGET_PSP2) && !defined(RTEMS) && !defined(__CC_ARM)
#define SBRK_ARG_T ptrdiff_t
#if defined(MMAP_SUPPORTED)
#ifdef USE_MMAP_FIXED
#define GC_MMAP_FLAGS MAP_FIXED | MAP_PRIVATE
#else
#define GC_MMAP_FLAGS MAP_PRIVATE
#endif
#ifdef USE_MMAP_ANON
#define zero_fd -1
#if defined(MAP_ANONYMOUS) && !defined(CPPCHECK)
#define OPT_MAP_ANON MAP_ANONYMOUS
#else
#define OPT_MAP_ANON MAP_ANON
#endif
#else
  static int zero_fd = -1;
#define OPT_MAP_ANON 0
#endif
#ifndef MSWIN_XBOX1
#if defined(SYMBIAN) && !defined(USE_MMAP_ANON)
      EXTERN_C_BEGIN
      extern char *GC_get_private_path_and_zero_file(void);
      EXTERN_C_END
#endif
  STATIC ptr_t GC_unix_mmap_get_mem(size_t bytes)
  {
    void *result;
    static ptr_t last_addr = HEAP_START;
#ifndef USE_MMAP_ANON
      static GC_bool initialized = FALSE;
      if (!EXPECT(initialized, TRUE)) {
#ifdef SYMBIAN
          char *path = GC_get_private_path_and_zero_file();
          if (path != NULL) {
            zero_fd = open(path, O_RDWR | O_CREAT, 0644);
            free(path);
          }
#else
          zero_fd = open("/dev/zero", O_RDONLY);
#endif
          if (zero_fd == -1)
            ABORT("Could not open /dev/zero");
          if (fcntl(zero_fd, F_SETFD, FD_CLOEXEC) == -1)
            WARN("Could not set FD_CLOEXEC for /dev/zero\n", 0);
          initialized = TRUE;
      }
#endif
    GC_ASSERT(GC_page_size != 0);
    if (bytes & (GC_page_size - 1)) ABORT("Bad GET_MEM arg");
    result = mmap(last_addr, bytes, (PROT_READ | PROT_WRITE)
                                    | (GC_pages_executable ? PROT_EXEC : 0),
                  GC_MMAP_FLAGS | OPT_MAP_ANON, zero_fd, 0);
#undef IGNORE_PAGES_EXECUTABLE
    if (EXPECT(MAP_FAILED == result, FALSE)) {
      if (HEAP_START == last_addr && GC_pages_executable && EACCES == errno)
        ABORT("Cannot allocate executable pages");
      return NULL;
    }
    last_addr = (ptr_t)(((word)result + bytes + GC_page_size - 1)
                        & ~(GC_page_size - 1));
#if !defined(LINUX)
      if (last_addr == 0) {
        munmap(result, ~GC_page_size - (size_t)result + 1);
        return GC_unix_mmap_get_mem(bytes);
      }
#else
      GC_ASSERT(last_addr != 0);
#endif
    if (((word)result % HBLKSIZE) != 0)
      ABORT(
       "GC_unix_get_mem: Memory returned by mmap is not aligned to HBLKSIZE.");
    return((ptr_t)result);
  }
#endif
#endif
#if defined(USE_MMAP)
  ptr_t GC_unix_get_mem(size_t bytes)
  {
    return GC_unix_mmap_get_mem(bytes);
  }
#else
STATIC ptr_t GC_unix_sbrk_get_mem(size_t bytes)
{
  ptr_t result;
#ifdef IRIX5
    __LOCK_MALLOC();
#endif
  {
    ptr_t cur_brk = (ptr_t)sbrk(0);
    SBRK_ARG_T lsbs = (word)cur_brk & (GC_page_size-1);
    GC_ASSERT(GC_page_size != 0);
    if ((SBRK_ARG_T)bytes < 0) {
        result = 0;
        goto out;
    }
    if (lsbs != 0) {
        if((ptr_t)sbrk((SBRK_ARG_T)GC_page_size - lsbs) == (ptr_t)(-1)) {
            result = 0;
            goto out;
        }
    }
#ifdef ADD_HEAP_GUARD_PAGES
      {
        ptr_t guard = (ptr_t)sbrk((SBRK_ARG_T)GC_page_size);
        if (mprotect(guard, GC_page_size, PROT_NONE) != 0)
            ABORT("ADD_HEAP_GUARD_PAGES: mprotect failed");
      }
#endif
    result = (ptr_t)sbrk((SBRK_ARG_T)bytes);
    if (result == (ptr_t)(-1)) result = 0;
  }
 out:
#ifdef IRIX5
    __UNLOCK_MALLOC();
#endif
  return(result);
}
ptr_t GC_unix_get_mem(size_t bytes)
{
#if defined(MMAP_SUPPORTED)
    static GC_bool sbrk_failed = FALSE;
    ptr_t result = 0;
    if (GC_pages_executable) {
        return GC_unix_mmap_get_mem(bytes);
    }
    if (!sbrk_failed) result = GC_unix_sbrk_get_mem(bytes);
    if (0 == result) {
        sbrk_failed = TRUE;
        result = GC_unix_mmap_get_mem(bytes);
    }
    if (0 == result) {
        result = GC_unix_sbrk_get_mem(bytes);
    }
    return result;
#else
    return GC_unix_sbrk_get_mem(bytes);
#endif
}
#endif
#endif
#ifdef OS2
void * os2_alloc(size_t bytes)
{
    void * result;
    if (DosAllocMem(&result, bytes, (PAG_READ | PAG_WRITE | PAG_COMMIT)
                                    | (GC_pages_executable ? PAG_EXECUTE : 0))
                    != NO_ERROR) {
        return(0);
    }
    if (result == 0) return(os2_alloc(bytes));
    return(result);
}
#endif
#ifdef MSWIN_XBOX1
    ptr_t GC_durango_get_mem(size_t bytes)
    {
      if (0 == bytes) return NULL;
      return (ptr_t)VirtualAlloc(NULL, bytes, MEM_COMMIT | MEM_TOP_DOWN,
                                 PAGE_READWRITE);
    }
#elif defined(MSWINCE)
  ptr_t GC_wince_get_mem(size_t bytes)
  {
    ptr_t result = 0;
    word i;
    GC_ASSERT(GC_page_size != 0);
    bytes = ROUNDUP_PAGESIZE(bytes);
    for (i = 0; i < GC_n_heap_bases; i++) {
        if (((word)(-(signed_word)GC_heap_lengths[i])
             & (GC_sysinfo.dwAllocationGranularity-1))
            >= bytes) {
            result = GC_heap_bases[i] + GC_heap_lengths[i];
            break;
        }
    }
    if (i == GC_n_heap_bases) {
        size_t res_bytes =
            SIZET_SAT_ADD(bytes, (size_t)GC_sysinfo.dwAllocationGranularity-1)
            & ~((size_t)GC_sysinfo.dwAllocationGranularity-1);
        result = (ptr_t) VirtualAlloc(NULL, res_bytes,
                                MEM_RESERVE | MEM_TOP_DOWN,
                                GC_pages_executable ? PAGE_EXECUTE_READWRITE :
                                                      PAGE_READWRITE);
        if (HBLKDISPL(result) != 0) ABORT("Bad VirtualAlloc result");
        if (GC_n_heap_bases >= MAX_HEAP_SECTS) ABORT("Too many heap sections");
        if (result == NULL) return NULL;
        GC_heap_bases[GC_n_heap_bases] = result;
        GC_heap_lengths[GC_n_heap_bases] = 0;
        GC_n_heap_bases++;
    }
    result = (ptr_t) VirtualAlloc(result, bytes, MEM_COMMIT,
                              GC_pages_executable ? PAGE_EXECUTE_READWRITE :
                                                    PAGE_READWRITE);
#undef IGNORE_PAGES_EXECUTABLE
    if (result != NULL) {
        if (HBLKDISPL(result) != 0) ABORT("Bad VirtualAlloc result");
        GC_heap_lengths[i] += bytes;
    }
    return(result);
  }
#elif (defined(USE_WINALLOC) && !defined(MSWIN_XBOX1)) || defined(CYGWIN32)
#ifdef USE_GLOBAL_ALLOC
#define GLOBAL_ALLOC_TEST 1
#else
#define GLOBAL_ALLOC_TEST GC_no_win32_dlls
#endif
#if (defined(GC_USE_MEM_TOP_DOWN) && defined(USE_WINALLOC)) \
     || defined(CPPCHECK)
    DWORD GC_mem_top_down = MEM_TOP_DOWN;
#else
#define GC_mem_top_down 0
#endif
  ptr_t GC_win32_get_mem(size_t bytes)
  {
    ptr_t result;
#ifndef USE_WINALLOC
    result = GC_unix_get_mem(bytes);
#else
#if defined(MSWIN32) && !defined(MSWINRT_FLAVOR)
      if (GLOBAL_ALLOC_TEST) {
        result = (ptr_t)GlobalAlloc(0, SIZET_SAT_ADD(bytes, HBLKSIZE));
        result = (ptr_t)(((word)result + HBLKSIZE - 1)
                         & ~(word)(HBLKSIZE - 1));
      } else
#endif
     {
#ifdef MPROTECT_VDB
#ifdef GWW_VDB
#define VIRTUAL_ALLOC_PAD (GC_GWW_AVAILABLE() ? 0 : 1)
#else
#define VIRTUAL_ALLOC_PAD 1
#endif
#else
#define VIRTUAL_ALLOC_PAD 0
#endif
        result = (ptr_t) VirtualAlloc(NULL,
                            SIZET_SAT_ADD(bytes, VIRTUAL_ALLOC_PAD),
                            GetWriteWatch_alloc_flag
                                | (MEM_COMMIT | MEM_RESERVE)
                                | GC_mem_top_down,
                            GC_pages_executable ? PAGE_EXECUTE_READWRITE :
                                                  PAGE_READWRITE);
#undef IGNORE_PAGES_EXECUTABLE
    }
#endif
    if (HBLKDISPL(result) != 0) ABORT("Bad VirtualAlloc result");
    if (GC_n_heap_bases >= MAX_HEAP_SECTS) ABORT("Too many heap sections");
    if (0 != result) GC_heap_bases[GC_n_heap_bases++] = result;
    return(result);
  }
#endif
#if defined(MSWIN32) || defined(MSWINCE) || defined(CYGWIN32) \
    || defined(MSWIN_XBOX1)
  GC_API void GC_CALL GC_win32_free_heap(void)
  {
#if defined(USE_WINALLOC) && !defined(REDIRECT_MALLOC) \
       && !defined(MSWIN_XBOX1)
      GC_free_malloc_heap_list();
#endif
#if (defined(USE_WINALLOC) && !defined(MSWIN_XBOX1) \
        && !defined(MSWINCE)) || defined(CYGWIN32)
#ifndef MSWINRT_FLAVOR
#ifndef CYGWIN32
          if (GLOBAL_ALLOC_TEST)
#endif
        {
          while (GC_n_heap_bases-- > 0) {
#ifdef CYGWIN32
#else
              GlobalFree(GC_heap_bases[GC_n_heap_bases]);
#endif
            GC_heap_bases[GC_n_heap_bases] = 0;
          }
          return;
        }
#endif
#ifndef CYGWIN32
        while (GC_n_heap_bases > 0) {
          VirtualFree(GC_heap_bases[--GC_n_heap_bases], 0, MEM_RELEASE);
          GC_heap_bases[GC_n_heap_bases] = 0;
        }
#endif
#endif
  }
#endif
#ifdef AMIGA
#define GC_AMIGA_AM
#undef GC_AMIGA_AM
#endif
#if defined(HAIKU)
#include <stdlib.h>
  ptr_t GC_haiku_get_mem(size_t bytes)
  {
    void* mem;
    GC_ASSERT(GC_page_size != 0);
    if (posix_memalign(&mem, GC_page_size, bytes) == 0)
      return mem;
    return NULL;
  }
#endif
#if (defined(USE_MUNMAP) || defined(MPROTECT_VDB)) && !defined(USE_WINALLOC)
#define ABORT_ON_REMAP_FAIL(C_msg_prefix, start_addr, len) \
        ABORT_ARG3(C_msg_prefix " failed", \
                   " at %p (length %lu), errno= %d", \
                   (void *)(start_addr), (unsigned long)(len), errno)
#endif
#ifdef USE_MUNMAP
#if !defined(NN_PLATFORM_CTR) && !defined(MSWIN32) && !defined(MSWINCE) \
    && !defined(MSWIN_XBOX1)
#include <unistd.h>
#ifdef SN_TARGET_PS3
#include <sys/memory.h>
#else
#include <sys/mman.h>
#endif
#include <sys/stat.h>
#include <sys/types.h>
#endif
STATIC ptr_t GC_unmap_start(ptr_t start, size_t bytes)
{
    ptr_t result = (ptr_t)(((word)start + GC_page_size - 1)
                            & ~(GC_page_size - 1));
    GC_ASSERT(GC_page_size != 0);
    if ((word)(result + GC_page_size) > (word)(start + bytes)) return 0;
    return result;
}
static void block_unmap_inner(ptr_t start_addr, size_t len)
{
    if (0 == start_addr) return;
#ifdef USE_WINALLOC
      while (len != 0) {
          MEMORY_BASIC_INFORMATION mem_info;
          word free_len;
          if (VirtualQuery(start_addr, &mem_info, sizeof(mem_info))
              != sizeof(mem_info))
              ABORT("Weird VirtualQuery result");
          free_len = (len < mem_info.RegionSize) ? len : mem_info.RegionSize;
          if (!VirtualFree(start_addr, free_len, MEM_DECOMMIT))
              ABORT("VirtualFree failed");
          GC_unmapped_bytes += free_len;
          start_addr += free_len;
          len -= free_len;
      }
#elif defined(SN_TARGET_PS3)
      ps3_free_mem(start_addr, len);
#else
      if (len != 0) {
#if defined(AIX) || defined(CYGWIN32) || defined(HAIKU) \
           || (defined(LINUX) && !defined(PREFER_MMAP_PROT_NONE)) \
           || defined(HPUX)
          if (mprotect(start_addr, len, PROT_NONE))
            ABORT_ON_REMAP_FAIL("unmap: mprotect", start_addr, len);
#elif defined(EMSCRIPTEN)
#else
          void * result = mmap(start_addr, len, PROT_NONE,
                               MAP_PRIVATE | MAP_FIXED | OPT_MAP_ANON,
                               zero_fd, 0);
          if (EXPECT(MAP_FAILED == result, FALSE))
            ABORT_ON_REMAP_FAIL("unmap: mmap", start_addr, len);
          if (result != (void *)start_addr)
            ABORT("unmap: mmap() result differs from start_addr");
#if defined(CPPCHECK) || defined(LINT2)
            GC_noop1((word)result);
#endif
#endif
        GC_unmapped_bytes += len;
      }
#endif
}
GC_INNER void GC_unmap(ptr_t start, size_t bytes)
{
    ptr_t start_addr = GC_unmap_start(start, bytes);
    ptr_t end_addr = GC_unmap_end(start, bytes);
    block_unmap_inner(start_addr, (size_t)(end_addr - start_addr));
}
GC_INNER void GC_remap(ptr_t start, size_t bytes)
{
    ptr_t start_addr = GC_unmap_start(start, bytes);
    ptr_t end_addr = GC_unmap_end(start, bytes);
    word len = end_addr - start_addr;
    if (0 == start_addr) return;
#ifdef USE_WINALLOC
      while (len != 0) {
          MEMORY_BASIC_INFORMATION mem_info;
          word alloc_len;
          ptr_t result;
          if (VirtualQuery(start_addr, &mem_info, sizeof(mem_info))
              != sizeof(mem_info))
              ABORT("Weird VirtualQuery result");
          alloc_len = (len < mem_info.RegionSize) ? len : mem_info.RegionSize;
          result = (ptr_t)VirtualAlloc(start_addr, alloc_len, MEM_COMMIT,
                                       GC_pages_executable
                                                ? PAGE_EXECUTE_READWRITE
                                                : PAGE_READWRITE);
          if (result != start_addr) {
              if (GetLastError() == ERROR_NOT_ENOUGH_MEMORY ||
                  GetLastError() == ERROR_OUTOFMEMORY) {
                  ABORT("Not enough memory to process remapping");
              } else {
                  ABORT("VirtualAlloc remapping failed");
              }
          }
#ifdef LINT2
            GC_noop1((word)result);
#endif
          GC_unmapped_bytes -= alloc_len;
          start_addr += alloc_len;
          len -= alloc_len;
      }
#else
      {
#if defined(NACL) || defined(NETBSD)
          void *result = mmap(start_addr, len, (PROT_READ | PROT_WRITE)
                                    | (GC_pages_executable ? PROT_EXEC : 0),
                                   MAP_PRIVATE | MAP_FIXED | OPT_MAP_ANON,
                                   zero_fd, 0 );
          if (EXPECT(MAP_FAILED == result, FALSE))
            ABORT_ON_REMAP_FAIL("remap: mmap", start_addr, len);
          if (result != (void *)start_addr)
            ABORT("remap: mmap() result differs from start_addr");
#if defined(CPPCHECK) || defined(LINT2)
            GC_noop1((word)result);
#endif
#else
          if (mprotect(start_addr, len, (PROT_READ | PROT_WRITE)
                            | (GC_pages_executable ? PROT_EXEC : 0)))
            ABORT_ON_REMAP_FAIL("remap: mprotect", start_addr, len);
#endif
      }
#undef IGNORE_PAGES_EXECUTABLE
      GC_unmapped_bytes -= len;
#endif
}
GC_INNER void GC_unmap_gap(ptr_t start1, size_t bytes1, ptr_t start2,
                           size_t bytes2)
{
    ptr_t start1_addr = GC_unmap_start(start1, bytes1);
    ptr_t end1_addr = GC_unmap_end(start1, bytes1);
    ptr_t start2_addr = GC_unmap_start(start2, bytes2);
    ptr_t start_addr = end1_addr;
    ptr_t end_addr = start2_addr;
    GC_ASSERT(start1 + bytes1 == start2);
    if (0 == start1_addr) start_addr = GC_unmap_start(start1, bytes1 + bytes2);
    if (0 == start2_addr) end_addr = GC_unmap_end(start1, bytes1 + bytes2);
    block_unmap_inner(start_addr, (size_t)(end_addr - start_addr));
}
#endif
#ifndef THREADS
#ifdef EMSCRIPTEN
    static void scan_regs_cb(void *begin, void *end)
    {
      GC_push_all_stack((ptr_t)begin, (ptr_t)end);
    }
    STATIC void GC_CALLBACK GC_default_push_other_roots(void)
    {
      emscripten_scan_registers(scan_regs_cb);
    }
#else
#define GC_default_push_other_roots 0
#endif
#else
#ifdef PCR
PCR_ERes GC_push_thread_stack(PCR_Th_T *t, PCR_Any dummy)
{
    struct PCR_ThCtl_TInfoRep info;
    PCR_ERes result;
    info.ti_stkLow = info.ti_stkHi = 0;
    result = PCR_ThCtl_GetInfo(t, &info);
    GC_push_all_stack((ptr_t)(info.ti_stkLow), (ptr_t)(info.ti_stkHi));
    return(result);
}
PCR_ERes GC_push_old_obj(void *p, size_t size, PCR_Any data)
{
    GC_push_all_stack((ptr_t)p, (ptr_t)p + size);
    return(PCR_ERes_okay);
}
extern struct PCR_MM_ProcsRep * GC_old_allocator;
STATIC void GC_CALLBACK GC_default_push_other_roots(void)
{
          if ((*(GC_old_allocator->mmp_enumerate))(PCR_Bool_false,
                                                   GC_push_old_obj, 0)
              != PCR_ERes_okay) {
              ABORT("Old object enumeration failed");
          }
        if (PCR_ERes_IsErr(
                PCR_ThCtl_ApplyToAllOtherThreads(GC_push_thread_stack,0))
            || PCR_ERes_IsErr(GC_push_thread_stack(PCR_Th_CurrThread(), 0))) {
          ABORT("Thread stack marking failed");
        }
}
#elif defined(SN_TARGET_PS3)
    STATIC void GC_CALLBACK GC_default_push_other_roots(void)
    {
      ABORT("GC_default_push_other_roots is not implemented");
    }
    void GC_push_thread_structures(void)
    {
      ABORT("GC_push_thread_structures is not implemented");
    }
#else
    STATIC void GC_CALLBACK GC_default_push_other_roots(void)
    {
      GC_push_all_stacks();
    }
#endif
#endif
GC_push_other_roots_proc GC_push_other_roots = GC_default_push_other_roots;
GC_API void GC_CALL GC_set_push_other_roots(GC_push_other_roots_proc fn)
{
    GC_push_other_roots = fn;
}
GC_API GC_push_other_roots_proc GC_CALL GC_get_push_other_roots(void)
{
    return GC_push_other_roots;
}
#if defined(SOFT_VDB) && !defined(NO_SOFT_VDB_LINUX_VER_RUNTIME_CHECK) \
    || (defined(GLIBC_2_19_TSX_BUG) && defined(THREADS))
  GC_INNER int GC_parse_version(int *pminor, const char *pverstr) {
    char *endp;
    unsigned long value = strtoul(pverstr, &endp, 10);
    int major = (int)value;
    if (major < 0 || (char *)pverstr == endp || (unsigned)major != value) {
      return -1;
    }
    if (*endp != '.') {
      *pminor = -1;
    } else {
      value = strtoul(endp + 1, &endp, 10);
      *pminor = (int)value;
      if (*pminor < 0 || (unsigned)(*pminor) != value) {
        return -1;
      }
    }
    return major;
  }
#endif
#if (defined(CHECKSUMS) && (defined(GWW_VDB) || defined(SOFT_VDB))) \
    || defined(PROC_VDB)
    STATIC void GC_or_pages(page_hash_table pht1, page_hash_table pht2)
    {
      unsigned i;
      for (i = 0; i < PHT_SIZE; i++) pht1[i] |= pht2[i];
    }
#endif
#ifdef GWW_VDB
#define GC_GWW_BUF_LEN (MAXHINCR * HBLKSIZE / 4096 )
  static PVOID gww_buf[GC_GWW_BUF_LEN];
#ifndef MPROTECT_VDB
#define GC_gww_dirty_init GC_dirty_init
#endif
    GC_INNER GC_bool GC_gww_dirty_init(void)
    {
      detect_GetWriteWatch();
      return GC_GWW_AVAILABLE();
    }
  GC_INLINE void GC_gww_read_dirty(GC_bool output_unneeded)
  {
    word i;
    if (!output_unneeded)
      BZERO(GC_grungy_pages, sizeof(GC_grungy_pages));
    for (i = 0; i != GC_n_heap_sects; ++i) {
      GC_ULONG_PTR count;
      do {
        PVOID * pages = gww_buf;
        DWORD page_size;
        count = GC_GWW_BUF_LEN;
        if ((*(GetWriteWatch_type)(word)GetWriteWatch_func)(
                                        WRITE_WATCH_FLAG_RESET,
                                        GC_heap_sects[i].hs_start,
                                        GC_heap_sects[i].hs_bytes,
                                        pages, &count, &page_size) != 0) {
          static int warn_count = 0;
          struct hblk * start = (struct hblk *)GC_heap_sects[i].hs_start;
          static struct hblk *last_warned = 0;
          size_t nblocks = divHBLKSZ(GC_heap_sects[i].hs_bytes);
          if (i != 0 && last_warned != start && warn_count++ < 5) {
            last_warned = start;
            WARN("GC_gww_read_dirty unexpectedly failed at %p: "
                 "Falling back to marking all pages dirty\n", start);
          }
          if (!output_unneeded) {
            unsigned j;
            for (j = 0; j < nblocks; ++j) {
              word hash = PHT_HASH(start + j);
              set_pht_entry_from_index(GC_grungy_pages, hash);
            }
          }
          count = 1;
        } else  if (!output_unneeded) {
          PVOID * pages_end = pages + count;
          while (pages != pages_end) {
            struct hblk * h = (struct hblk *) *pages++;
            struct hblk * h_end = (struct hblk *) ((char *) h + page_size);
            do {
              set_pht_entry_from_index(GC_grungy_pages, PHT_HASH(h));
            } while ((word)(++h) < (word)h_end);
          }
        }
      } while (count == GC_GWW_BUF_LEN);
    }
#ifdef CHECKSUMS
      GC_ASSERT(!output_unneeded);
      GC_or_pages(GC_written_pages, GC_grungy_pages);
#endif
  }
#elif defined(SOFT_VDB)
  static int clear_refs_fd = -1;
#define GC_GWW_AVAILABLE() (clear_refs_fd != -1)
#else
#define GC_GWW_AVAILABLE() FALSE
#endif
#ifdef DEFAULT_VDB
  GC_INNER GC_bool GC_dirty_init(void)
  {
    GC_VERBOSE_LOG_PRINTF("Initializing DEFAULT_VDB...\n");
    return TRUE;
  }
#endif
#ifndef GC_DISABLE_INCREMENTAL
#if !defined(THREADS) || defined(HAVE_LOCKFREE_AO_OR)
#define async_set_pht_entry_from_index(db, index) \
                        set_pht_entry_from_index_concurrent(db, index)
#elif defined(AO_HAVE_test_and_set_acquire)
    GC_INNER volatile AO_TS_t GC_fault_handler_lock = AO_TS_INITIALIZER;
    static void async_set_pht_entry_from_index(volatile page_hash_table db,
                                               size_t index)
    {
      GC_acquire_dirty_lock();
      set_pht_entry_from_index(db, index);
      GC_release_dirty_lock();
    }
#else
#error No test_and_set operation: Introduces a race.
#endif
#endif
#ifdef MPROTECT_VDB
#ifdef DARWIN
#include <mach/vm_map.h>
    STATIC mach_port_t GC_task_self = 0;
#define PROTECT_INNER(addr, len, allow_write, C_msg_prefix) \
        if (vm_protect(GC_task_self, (vm_address_t)(addr), (vm_size_t)(len), \
                       FALSE, VM_PROT_READ \
                              | ((allow_write) ? VM_PROT_WRITE : 0) \
                              | (GC_pages_executable ? VM_PROT_EXECUTE : 0)) \
                == KERN_SUCCESS) {} else ABORT(C_msg_prefix \
                                               "vm_protect() failed")
#elif !defined(USE_WINALLOC)
#include <sys/mman.h>
#include <signal.h>
#if !defined(CYGWIN32) && !defined(HAIKU)
#include <sys/syscall.h>
#endif
#define PROTECT_INNER(addr, len, allow_write, C_msg_prefix) \
        if (mprotect((caddr_t)(addr), (size_t)(len), \
                     PROT_READ | ((allow_write) ? PROT_WRITE : 0) \
                     | (GC_pages_executable ? PROT_EXEC : 0)) >= 0) { \
        } else if (GC_pages_executable) { \
            ABORT_ON_REMAP_FAIL(C_msg_prefix \
                                    "mprotect vdb executable pages", \
                                addr, len); \
        } else ABORT_ON_REMAP_FAIL(C_msg_prefix "mprotect vdb", addr, len)
#undef IGNORE_PAGES_EXECUTABLE
#else
#ifndef MSWINCE
#include <signal.h>
#endif
    static DWORD protect_junk;
#define PROTECT_INNER(addr, len, allow_write, C_msg_prefix) \
        if (VirtualProtect(addr, len, \
                           GC_pages_executable ? \
                                ((allow_write) ? PAGE_EXECUTE_READWRITE : \
                                                 PAGE_EXECUTE_READ) : \
                                 (allow_write) ? PAGE_READWRITE : \
                                                 PAGE_READONLY, \
                           &protect_junk)) { \
        } else ABORT_ARG1(C_msg_prefix "VirtualProtect failed", \
                          ": errcode= 0x%X", (unsigned)GetLastError())
#endif
#define PROTECT(addr, len) PROTECT_INNER(addr, len, FALSE, "")
#define UNPROTECT(addr, len) PROTECT_INNER(addr, len, TRUE, "un-")
#if defined(MSWIN32)
    typedef LPTOP_LEVEL_EXCEPTION_FILTER SIG_HNDLR_PTR;
#undef SIG_DFL
#define SIG_DFL (LPTOP_LEVEL_EXCEPTION_FILTER)((signed_word)-1)
#elif defined(MSWINCE)
    typedef LONG (WINAPI *SIG_HNDLR_PTR)(struct _EXCEPTION_POINTERS *);
#undef SIG_DFL
#define SIG_DFL (SIG_HNDLR_PTR) (-1)
#elif defined(DARWIN)
    typedef void (* SIG_HNDLR_PTR)();
#else
    typedef void (* SIG_HNDLR_PTR)(int, siginfo_t *, void *);
    typedef void (* PLAIN_HNDLR_PTR)(int);
#endif
#if defined(__GLIBC__)
#if __GLIBC__ < 2 || __GLIBC__ == 2 && __GLIBC_MINOR__ < 2
#error glibc too old?
#endif
#endif
#ifndef DARWIN
  STATIC SIG_HNDLR_PTR GC_old_segv_handler = 0;
#if defined(FREEBSD) || defined(HPUX) || defined(HURD) || defined(LINUX)
    STATIC SIG_HNDLR_PTR GC_old_bus_handler = 0;
#ifndef LINUX
      STATIC GC_bool GC_old_bus_handler_used_si = FALSE;
#endif
#endif
#if !defined(MSWIN32) && !defined(MSWINCE)
    STATIC GC_bool GC_old_segv_handler_used_si = FALSE;
#endif
#endif
#ifdef THREADS
  GC_ATTR_NO_SANITIZE_THREAD
  static GC_bool is_header_found_async(void *addr)
  {
#ifdef HASH_TL
      hdr *result;
      GET_HDR((ptr_t)addr, result);
      return result != NULL;
#else
      return HDR_INNER(addr) != NULL;
#endif
  }
#else
#define is_header_found_async(addr) (HDR(addr) != NULL)
#endif
#ifndef DARWIN
#if !defined(MSWIN32) && !defined(MSWINCE)
#include <errno.h>
#if defined(FREEBSD) || defined(HURD) || defined(HPUX)
#define SIG_OK (sig == SIGBUS || sig == SIGSEGV)
#else
#define SIG_OK (sig == SIGSEGV)
#endif
#if defined(FREEBSD)
#ifndef SEGV_ACCERR
#define SEGV_ACCERR 2
#endif
#if defined(AARCH64) || defined(ARM32) || defined(MIPS) \
         || __FreeBSD__ >= 7
#define CODE_OK (si -> si_code == SEGV_ACCERR)
#elif defined(POWERPC)
#define AIM
#include <machine/trap.h>
#define CODE_OK (si -> si_code == EXC_DSI \
                        || si -> si_code == SEGV_ACCERR)
#else
#define CODE_OK (si -> si_code == BUS_PAGE_FAULT \
                        || si -> si_code == SEGV_ACCERR)
#endif
#elif defined(OSF1)
#define CODE_OK (si -> si_code == 2 )
#elif defined(IRIX5)
#define CODE_OK (si -> si_code == EACCES)
#elif defined(CYGWIN32) || defined(HAIKU) || defined(HURD)
#define CODE_OK TRUE
#elif defined(LINUX)
#define CODE_OK TRUE
#elif defined(HPUX)
#define CODE_OK (si -> si_code == SEGV_ACCERR \
                      || si -> si_code == BUS_ADRERR \
                      || si -> si_code == BUS_UNKNOWN \
                      || si -> si_code == SEGV_UNKNOWN \
                      || si -> si_code == BUS_OBJERR)
#elif defined(SUNOS5SIGS)
#define CODE_OK (si -> si_code == SEGV_ACCERR)
#endif
#ifndef NO_GETCONTEXT
#include <ucontext.h>
#endif
    STATIC void GC_write_fault_handler(int sig, siginfo_t *si, void *raw_sc)
#else
#define SIG_OK (exc_info -> ExceptionRecord -> ExceptionCode \
                     == STATUS_ACCESS_VIOLATION)
#define CODE_OK (exc_info -> ExceptionRecord -> ExceptionInformation[0] \
                      == 1)
    STATIC LONG WINAPI GC_write_fault_handler(
                                struct _EXCEPTION_POINTERS *exc_info)
#endif
  {
#if !defined(MSWIN32) && !defined(MSWINCE)
        char *addr = (char *)si->si_addr;
#else
        char * addr = (char *) (exc_info -> ExceptionRecord
                                -> ExceptionInformation[1]);
#endif
    if (SIG_OK && CODE_OK) {
        struct hblk * h = (struct hblk *)((word)addr & ~(GC_page_size-1));
        GC_bool in_allocd_block;
        size_t i;
        GC_ASSERT(GC_page_size != 0);
#ifdef CHECKSUMS
          GC_record_fault(h);
#endif
#ifdef SUNOS5SIGS
            in_allocd_block = FALSE;
            for (i = 0; i < divHBLKSZ(GC_page_size); i++) {
              if (is_header_found_async(&h[i])) {
                in_allocd_block = TRUE;
                break;
              }
            }
#else
            in_allocd_block = is_header_found_async(addr);
#endif
        if (!in_allocd_block) {
            SIG_HNDLR_PTR old_handler;
#if defined(MSWIN32) || defined(MSWINCE)
                old_handler = GC_old_segv_handler;
#else
                GC_bool used_si;
#if defined(FREEBSD) || defined(HURD) || defined(HPUX)
                if (sig == SIGBUS) {
                   old_handler = GC_old_bus_handler;
                   used_si = GC_old_bus_handler_used_si;
                } else
#endif
                 {
                   old_handler = GC_old_segv_handler;
                   used_si = GC_old_segv_handler_used_si;
                }
#endif
            if (old_handler == (SIG_HNDLR_PTR)(signed_word)SIG_DFL) {
#if !defined(MSWIN32) && !defined(MSWINCE)
                    ABORT_ARG1("Unexpected bus error or segmentation fault",
                               " at %p", (void *)addr);
#else
                    return(EXCEPTION_CONTINUE_SEARCH);
#endif
            } else {
#if defined(MSWIN32) || defined(MSWINCE)
                    return((*old_handler)(exc_info));
#else
                    if (used_si)
                      ((SIG_HNDLR_PTR)old_handler) (sig, si, raw_sc);
                    else
                      ((PLAIN_HNDLR_PTR)(signed_word)old_handler)(sig);
                    return;
#endif
            }
        }
        UNPROTECT(h, GC_page_size);
        for (i = 0; i < divHBLKSZ(GC_page_size); i++) {
            word index = PHT_HASH(h+i);
            async_set_pht_entry_from_index(GC_dirty_pages, index);
        }
#if defined(MSWIN32) || defined(MSWINCE)
            return(EXCEPTION_CONTINUE_EXECUTION);
#else
            return;
#endif
    }
#if defined(MSWIN32) || defined(MSWINCE)
      return EXCEPTION_CONTINUE_SEARCH;
#else
      ABORT_ARG1("Unexpected bus error or segmentation fault",
                 " at %p", (void *)addr);
#endif
  }
#if defined(GC_WIN32_THREADS) && !defined(CYGWIN32)
    GC_INNER void GC_set_write_fault_handler(void)
    {
      SetUnhandledExceptionFilter(GC_write_fault_handler);
    }
#endif
#ifdef SOFT_VDB
    static GC_bool soft_dirty_init(void);
#endif
  GC_INNER GC_bool GC_dirty_init(void)
  {
#if !defined(MSWIN32) && !defined(MSWINCE)
      struct sigaction act, oldact;
      act.sa_flags = SA_RESTART | SA_SIGINFO;
      act.sa_sigaction = GC_write_fault_handler;
      (void)sigemptyset(&act.sa_mask);
#if defined(THREADS) && !defined(GC_OPENBSD_UTHREADS) \
         && !defined(GC_WIN32_THREADS) && !defined(NACL)
        (void)sigaddset(&act.sa_mask, GC_get_suspend_signal());
#endif
#endif
    GC_VERBOSE_LOG_PRINTF(
                "Initializing mprotect virtual dirty bit implementation\n");
    if (GC_page_size % HBLKSIZE != 0) {
        ABORT("Page size not multiple of HBLKSIZE");
    }
#ifdef GWW_VDB
      if (GC_gww_dirty_init()) {
        GC_COND_LOG_PRINTF("Using GetWriteWatch()\n");
        return TRUE;
      }
#elif defined(SOFT_VDB)
      if (soft_dirty_init()) {
        GC_COND_LOG_PRINTF("Using soft-dirty bit feature\n");
        return TRUE;
      }
#endif
#ifdef MSWIN32
      GC_old_segv_handler = SetUnhandledExceptionFilter(
                                        GC_write_fault_handler);
      if (GC_old_segv_handler != NULL) {
        GC_COND_LOG_PRINTF("Replaced other UnhandledExceptionFilter\n");
      } else {
          GC_old_segv_handler = SIG_DFL;
      }
#elif defined(MSWINCE)
#else
#if defined(GC_IRIX_THREADS)
        sigaction(SIGSEGV, 0, &oldact);
        sigaction(SIGSEGV, &act, 0);
#else
        {
          int res = sigaction(SIGSEGV, &act, &oldact);
          if (res != 0) ABORT("Sigaction failed");
        }
#endif
      if (oldact.sa_flags & SA_SIGINFO) {
        GC_old_segv_handler = oldact.sa_sigaction;
        GC_old_segv_handler_used_si = TRUE;
      } else {
        GC_old_segv_handler = (SIG_HNDLR_PTR)(signed_word)oldact.sa_handler;
        GC_old_segv_handler_used_si = FALSE;
      }
      if (GC_old_segv_handler == (SIG_HNDLR_PTR)(signed_word)SIG_IGN) {
        WARN("Previously ignored segmentation violation!?\n", 0);
        GC_old_segv_handler = (SIG_HNDLR_PTR)(signed_word)SIG_DFL;
      }
      if (GC_old_segv_handler != (SIG_HNDLR_PTR)(signed_word)SIG_DFL) {
        GC_VERBOSE_LOG_PRINTF("Replaced other SIGSEGV handler\n");
      }
#if defined(HPUX) || defined(LINUX) || defined(HURD) \
       || (defined(FREEBSD) && (defined(__GLIBC__) || defined(SUNOS5SIGS)))
      sigaction(SIGBUS, &act, &oldact);
      if ((oldact.sa_flags & SA_SIGINFO) != 0) {
        GC_old_bus_handler = oldact.sa_sigaction;
#if !defined(LINUX)
          GC_old_bus_handler_used_si = TRUE;
#endif
      } else {
        GC_old_bus_handler = (SIG_HNDLR_PTR)(signed_word)oldact.sa_handler;
      }
      if (GC_old_bus_handler == (SIG_HNDLR_PTR)(signed_word)SIG_IGN) {
        WARN("Previously ignored bus error!?\n", 0);
#if !defined(LINUX)
          GC_old_bus_handler = (SIG_HNDLR_PTR)(signed_word)SIG_DFL;
#else
#endif
      } else if (GC_old_bus_handler != (SIG_HNDLR_PTR)(signed_word)SIG_DFL) {
          GC_VERBOSE_LOG_PRINTF("Replaced other SIGBUS handler\n");
      }
#endif
#endif
#if defined(CPPCHECK) && defined(ADDRESS_SANITIZER)
      GC_noop1((word)&__asan_default_options);
#endif
    return TRUE;
  }
#endif
GC_API int GC_CALL GC_incremental_protection_needs(void)
{
    GC_ASSERT(GC_is_initialized);
#if defined(GWW_VDB) || defined(SOFT_VDB)
      if (GC_GWW_AVAILABLE())
        return GC_PROTECTS_NONE;
#endif
    if (GC_page_size == HBLKSIZE) {
        return GC_PROTECTS_POINTER_HEAP;
    } else {
        return GC_PROTECTS_POINTER_HEAP | GC_PROTECTS_PTRFREE_HEAP;
    }
}
#define HAVE_INCREMENTAL_PROTECTION_NEEDS
#define IS_PTRFREE(hhdr) ((hhdr)->hb_descr == 0)
#define PAGE_ALIGNED(x) !((word)(x) & (GC_page_size - 1))
STATIC void GC_protect_heap(void)
{
    unsigned i;
    GC_bool protect_all =
        (0 != (GC_incremental_protection_needs() & GC_PROTECTS_PTRFREE_HEAP));
    GC_ASSERT(GC_page_size != 0);
    for (i = 0; i < GC_n_heap_sects; i++) {
        ptr_t start = GC_heap_sects[i].hs_start;
        size_t len = GC_heap_sects[i].hs_bytes;
        if (protect_all) {
          PROTECT(start, len);
        } else {
          struct hblk * current;
          struct hblk * current_start;
          struct hblk * limit;
          GC_ASSERT(PAGE_ALIGNED(len));
          GC_ASSERT(PAGE_ALIGNED(start));
          current_start = current = (struct hblk *)start;
          limit = (struct hblk *)(start + len);
          while ((word)current < (word)limit) {
            hdr * hhdr;
            word nhblks;
            GC_bool is_ptrfree;
            GC_ASSERT(PAGE_ALIGNED(current));
            GET_HDR(current, hhdr);
            if (IS_FORWARDING_ADDR_OR_NIL(hhdr)) {
              GC_ASSERT(current_start == current);
              current_start = ++current;
              continue;
            }
            if (HBLK_IS_FREE(hhdr)) {
              GC_ASSERT(PAGE_ALIGNED(hhdr -> hb_sz));
              nhblks = divHBLKSZ(hhdr -> hb_sz);
              is_ptrfree = TRUE;
            } else {
              nhblks = OBJ_SZ_TO_BLOCKS(hhdr -> hb_sz);
              is_ptrfree = IS_PTRFREE(hhdr);
            }
            if (is_ptrfree) {
              if ((word)current_start < (word)current) {
                PROTECT(current_start, (ptr_t)current - (ptr_t)current_start);
              }
              current_start = (current += nhblks);
            } else {
              current += nhblks;
            }
          }
          if ((word)current_start < (word)current) {
            PROTECT(current_start, (ptr_t)current - (ptr_t)current_start);
          }
        }
    }
}
#endif
#if !defined(THREADS) && (defined(PROC_VDB) || defined(SOFT_VDB))
  static pid_t saved_proc_pid;
#endif
#ifdef PROC_VDB
#include <errno.h>
#include <sys/types.h>
#include <sys/signal.h>
#include <sys/syscall.h>
#include <sys/stat.h>
#ifdef GC_NO_SYS_FAULT_H
#define PG_MODIFIED 1
    struct prpageheader {
      int dummy[2];
      unsigned long pr_nmap;
      unsigned long pr_npage;
    };
    struct prasmap {
      char *pr_vaddr;
      size_t pr_npage;
      char dummy1[64+8];
      unsigned pr_mflags;
      unsigned pr_pagesize;
      int dummy2[2];
    };
#else
#include <sys/fault.h>
#include <sys/procfs.h>
#endif
#define INITIAL_BUF_SZ 16384
  STATIC size_t GC_proc_buf_size = INITIAL_BUF_SZ;
  STATIC char *GC_proc_buf = NULL;
  STATIC int GC_proc_fd = -1;
  static GC_bool proc_dirty_open_files(void)
  {
    char buf[40];
    pid_t pid = getpid();
    (void)snprintf(buf, sizeof(buf), "/proc/%ld/pagedata", (long)pid);
    buf[sizeof(buf) - 1] = '\0';
    GC_proc_fd = open(buf, O_RDONLY);
    if (-1 == GC_proc_fd) {
      WARN("/proc open failed; cannot enable GC incremental mode\n", 0);
      return FALSE;
    }
    if (syscall(SYS_fcntl, GC_proc_fd, F_SETFD, FD_CLOEXEC) == -1)
      WARN("Could not set FD_CLOEXEC for /proc\n", 0);
#ifndef THREADS
      saved_proc_pid = pid;
#endif
    return TRUE;
  }
#ifdef CAN_HANDLE_FORK
    GC_INNER void GC_dirty_update_child(void)
    {
      if (-1 == GC_proc_fd)
        return;
      close(GC_proc_fd);
      if (!proc_dirty_open_files())
        GC_incremental = FALSE;
    }
#endif
GC_INNER GC_bool GC_dirty_init(void)
{
    if (GC_bytes_allocd != 0 || GC_bytes_allocd_before_gc != 0) {
      memset(GC_written_pages, 0xff, sizeof(page_hash_table));
      GC_VERBOSE_LOG_PRINTF(
                "Allocated %lu bytes: all pages may have been written\n",
                (unsigned long)(GC_bytes_allocd + GC_bytes_allocd_before_gc));
    }
    if (!proc_dirty_open_files())
      return FALSE;
    GC_proc_buf = GC_scratch_alloc(GC_proc_buf_size);
    if (GC_proc_buf == NULL)
      ABORT("Insufficient space for /proc read");
    return TRUE;
}
GC_INLINE void GC_proc_read_dirty(GC_bool output_unneeded)
{
    int nmaps;
    char * bufp = GC_proc_buf;
    int i;
#ifndef THREADS
      if (getpid() != saved_proc_pid
          && (-1 == GC_proc_fd
              || (close(GC_proc_fd), !proc_dirty_open_files()))) {
        if (!output_unneeded)
          memset(GC_grungy_pages, 0xff, sizeof(page_hash_table));
        memset(GC_written_pages, 0xff, sizeof(page_hash_table));
        return;
      }
#endif
    BZERO(GC_grungy_pages, sizeof(GC_grungy_pages));
    if (PROC_READ(GC_proc_fd, bufp, GC_proc_buf_size) <= 0) {
        size_t new_size = 2 * GC_proc_buf_size;
        char *new_buf;
        WARN("/proc read failed: GC_proc_buf_size= %" WARN_PRIdPTR "\n",
             (signed_word)GC_proc_buf_size);
        new_buf = GC_scratch_alloc(new_size);
        if (new_buf != 0) {
            GC_scratch_recycle_no_gww(bufp, GC_proc_buf_size);
            GC_proc_buf = bufp = new_buf;
            GC_proc_buf_size = new_size;
        }
        if (PROC_READ(GC_proc_fd, bufp, GC_proc_buf_size) <= 0) {
            WARN("Insufficient space for /proc read\n", 0);
            if (!output_unneeded)
              memset(GC_grungy_pages, 0xff, sizeof (page_hash_table));
            memset(GC_written_pages, 0xff, sizeof(page_hash_table));
            return;
        }
    }
    nmaps = ((struct prpageheader *)bufp) -> pr_nmap;
#ifdef DEBUG_DIRTY_BITS
      GC_log_printf("Proc VDB read: pr_nmap= %u, pr_npage= %lu\n",
                    nmaps, ((struct prpageheader *)bufp)->pr_npage);
#endif
#if defined(GC_NO_SYS_FAULT_H) && defined(CPPCHECK)
      GC_noop1(((struct prpageheader *)bufp)->dummy[0]);
#endif
    bufp += sizeof(struct prpageheader);
    for (i = 0; i < nmaps; i++) {
        struct prasmap * map = (struct prasmap *)bufp;
        ptr_t vaddr = (ptr_t)(map -> pr_vaddr);
        unsigned long npages = map -> pr_npage;
        unsigned pagesize = map -> pr_pagesize;
        ptr_t limit;
#if defined(GC_NO_SYS_FAULT_H) && defined(CPPCHECK)
          GC_noop1(map->dummy1[0] + map->dummy2[0]);
#endif
#ifdef DEBUG_DIRTY_BITS
          GC_log_printf(
                "pr_vaddr= %p, npage= %lu, mflags= 0x%x, pagesize= 0x%x\n",
                (void *)vaddr, npages, map->pr_mflags, pagesize);
#endif
        bufp += sizeof(struct prasmap);
        limit = vaddr + pagesize * npages;
        for (; (word)vaddr < (word)limit; vaddr += pagesize) {
            if ((*bufp++) & PG_MODIFIED) {
                struct hblk * h;
                ptr_t next_vaddr = vaddr + pagesize;
#ifdef DEBUG_DIRTY_BITS
                  GC_log_printf("dirty page at: %p\n", (void *)vaddr);
#endif
                for (h = (struct hblk *)vaddr;
                     (word)h < (word)next_vaddr; h++) {
                    word index = PHT_HASH(h);
                    set_pht_entry_from_index(GC_grungy_pages, index);
                }
            }
        }
        bufp = (char *)(((word)bufp + (sizeof(long)-1))
                        & ~(word)(sizeof(long)-1));
    }
#ifdef DEBUG_DIRTY_BITS
      GC_log_printf("Proc VDB read done\n");
#endif
    GC_or_pages(GC_written_pages, GC_grungy_pages);
}
#endif
#ifdef SOFT_VDB
#ifndef VDB_BUF_SZ
#define VDB_BUF_SZ 16384
#endif
  static int open_proc_fd(pid_t pid, const char *proc_filename, int mode)
  {
    int f;
    char buf[40];
    (void)snprintf(buf, sizeof(buf), "/proc/%ld/%s", (long)pid,
                   proc_filename);
    buf[sizeof(buf) - 1] = '\0';
    f = open(buf, mode);
    if (-1 == f) {
      WARN("/proc/self/%s open failed; cannot enable GC incremental mode\n",
           proc_filename);
    } else if (fcntl(f, F_SETFD, FD_CLOEXEC) == -1) {
      WARN("Could not set FD_CLOEXEC for /proc\n", 0);
    }
    return f;
  }
#include <stdint.h>
  typedef uint64_t pagemap_elem_t;
  static pagemap_elem_t *soft_vdb_buf;
  static int pagemap_fd;
  static GC_bool soft_dirty_open_files(void)
  {
    pid_t pid = getpid();
    clear_refs_fd = open_proc_fd(pid, "clear_refs", O_WRONLY);
    if (-1 == clear_refs_fd)
      return FALSE;
    pagemap_fd = open_proc_fd(pid, "pagemap", O_RDONLY);
    if (-1 == pagemap_fd) {
      close(clear_refs_fd);
      clear_refs_fd = -1;
      return FALSE;
    }
#ifndef THREADS
      saved_proc_pid = pid;
#endif
    return TRUE;
  }
#ifdef CAN_HANDLE_FORK
    GC_INNER void GC_dirty_update_child(void)
    {
      if (-1 == clear_refs_fd)
        return;
      close(clear_refs_fd);
      close(pagemap_fd);
      if (!soft_dirty_open_files())
        GC_incremental = FALSE;
    }
#endif
#define PM_SOFTDIRTY_MASK ((pagemap_elem_t)1 << 55)
  static GC_bool detect_soft_dirty_supported(ptr_t vaddr)
  {
    off_t fpos;
    pagemap_elem_t buf[1];
    *vaddr = 1;
    GC_ASSERT(GC_page_size != 0);
    fpos = (off_t)((word)vaddr / GC_page_size * sizeof(pagemap_elem_t));
    if (lseek(pagemap_fd, fpos, SEEK_SET) == (off_t)(-1))
      return FALSE;
    if (PROC_READ(pagemap_fd, buf, sizeof(buf)) != (int)sizeof(buf))
      return FALSE;
    return (buf[0] & PM_SOFTDIRTY_MASK) != 0;
  }
#ifndef NO_SOFT_VDB_LINUX_VER_RUNTIME_CHECK
#include <sys/utsname.h>
#include <string.h>
    static GC_bool ensure_min_linux_ver(int major, int minor) {
      struct utsname info;
      int actual_major;
      int actual_minor = -1;
      if (uname(&info) == -1) {
        return FALSE;
      }
      if (strcmp(info.sysname, "Linux")) {
        WARN("Cannot ensure Linux version as running on other OS: %s\n",
             info.sysname);
        return FALSE;
      }
      actual_major = GC_parse_version(&actual_minor, info.release);
      return actual_major > major
             || (actual_major == major && actual_minor >= minor);
    }
#endif
#ifdef MPROTECT_VDB
    static GC_bool soft_dirty_init(void)
#else
    GC_INNER GC_bool GC_dirty_init(void)
#endif
  {
    GC_ASSERT(NULL == soft_vdb_buf);
#ifdef MPROTECT_VDB
      char * str = GETENV("GC_USE_GETWRITEWATCH");
#ifdef GC_PREFER_MPROTECT_VDB
        if (str == NULL || (*str == '0' && *(str + 1) == '\0'))
          return FALSE;
#else
        if (str != NULL && *str == '0' && *(str + 1) == '\0')
          return FALSE;
#endif
#endif
#ifndef NO_SOFT_VDB_LINUX_VER_RUNTIME_CHECK
      if (!ensure_min_linux_ver(3, 18)) {
        GC_COND_LOG_PRINTF(
            "Running on old kernel lacking correct soft-dirty bit support\n");
        return FALSE;
      }
#endif
    if (!soft_dirty_open_files())
      return FALSE;
    soft_vdb_buf = (pagemap_elem_t *)GC_scratch_alloc(VDB_BUF_SZ);
    if (NULL == soft_vdb_buf)
      ABORT("Insufficient space for /proc pagemap buffer");
    if (!detect_soft_dirty_supported((ptr_t)soft_vdb_buf)) {
      GC_COND_LOG_PRINTF("Soft-dirty bit is not supported by kernel\n");
      GC_scratch_recycle_no_gww(soft_vdb_buf, VDB_BUF_SZ);
      soft_vdb_buf = NULL;
      close(clear_refs_fd);
      clear_refs_fd = -1;
      close(pagemap_fd);
      return FALSE;
    }
    return TRUE;
  }
  static off_t pagemap_buf_fpos;
  static size_t pagemap_buf_len;
  static const pagemap_elem_t *pagemap_buffered_read(size_t *pres,
                                                     off_t fpos, size_t len,
                                                     off_t next_fpos_hint)
  {
    ssize_t res;
    size_t ofs;
    GC_ASSERT(len > 0);
    if (pagemap_buf_fpos <= fpos
        && fpos < pagemap_buf_fpos + (off_t)pagemap_buf_len) {
      ofs = (size_t)(fpos - pagemap_buf_fpos);
      res = (ssize_t)(pagemap_buf_fpos + pagemap_buf_len - fpos);
    } else {
      off_t aligned_pos = fpos & ~(GC_page_size < VDB_BUF_SZ
                                    ? GC_page_size-1 : VDB_BUF_SZ-1);
      for (;;) {
        size_t count;
        if ((0 == pagemap_buf_len
             || pagemap_buf_fpos + (off_t)pagemap_buf_len != aligned_pos)
            && lseek(pagemap_fd, aligned_pos, SEEK_SET) == (off_t)(-1))
          ABORT_ARG2("Failed to lseek /proc/self/pagemap",
                     ": offset= %lu, errno= %d", (unsigned long)fpos, errno);
        ofs = (size_t)(fpos - aligned_pos);
        GC_ASSERT(ofs < VDB_BUF_SZ);
        if (next_fpos_hint > aligned_pos
            && next_fpos_hint - aligned_pos < VDB_BUF_SZ) {
          count = VDB_BUF_SZ;
        } else {
          count = len + ofs;
          if (count > VDB_BUF_SZ)
            count = VDB_BUF_SZ;
        }
        GC_ASSERT(count % sizeof(pagemap_elem_t) == 0);
        res = PROC_READ(pagemap_fd, soft_vdb_buf, count);
        if (res > (ssize_t)ofs)
          break;
        if (res <= 0)
          ABORT_ARG1("Failed to read /proc/self/pagemap",
                     ": errno= %d", res < 0 ? errno : 0);
        aligned_pos = fpos;
      }
      pagemap_buf_fpos = aligned_pos;
      pagemap_buf_len = (size_t)res;
      res -= (ssize_t)ofs;
    }
    GC_ASSERT(ofs % sizeof(pagemap_elem_t) == 0);
    *pres = (size_t)res < len ? (size_t)res : len;
    return &soft_vdb_buf[ofs / sizeof(pagemap_elem_t)];
  }
  static void soft_set_grungy_pages(ptr_t vaddr , ptr_t limit,
                                    ptr_t next_start_hint)
  {
    GC_ASSERT(GC_page_size != 0);
    while ((word)vaddr < (word)limit) {
      size_t res;
      word limit_buf;
      const pagemap_elem_t *bufp = pagemap_buffered_read(&res,
                (off_t)((word)vaddr / GC_page_size * sizeof(pagemap_elem_t)),
                (size_t)((((word)limit - (word)vaddr + GC_page_size-1)
                         / GC_page_size) * sizeof(pagemap_elem_t)),
                (off_t)((word)next_start_hint / GC_page_size
                        * sizeof(pagemap_elem_t)));
      if (res % sizeof(pagemap_elem_t) != 0) {
        memset(GC_grungy_pages, 0xff, sizeof(page_hash_table));
        WARN("Incomplete read of pagemap, not multiple of entry size\n", 0);
        break;
      }
      limit_buf = ((word)vaddr & ~(GC_page_size-1))
                  + (res / sizeof(pagemap_elem_t)) * GC_page_size;
      for (; (word)vaddr < limit_buf; vaddr += GC_page_size, bufp++)
        if ((*bufp & PM_SOFTDIRTY_MASK) != 0) {
          struct hblk * h;
          ptr_t next_vaddr = vaddr + GC_page_size;
#ifdef DEBUG_DIRTY_BITS
            GC_log_printf("dirty page at: %p\n", (void *)vaddr);
#endif
          for (h = (struct hblk *)vaddr; (word)h < (word)next_vaddr; h++) {
            word index = PHT_HASH(h);
            set_pht_entry_from_index(GC_grungy_pages, index);
          }
        }
    }
  }
  GC_INLINE void GC_soft_read_dirty(GC_bool output_unneeded)
  {
    ssize_t res;
#ifndef THREADS
      if (getpid() != saved_proc_pid
          && (-1 == clear_refs_fd
              || (close(clear_refs_fd), close(pagemap_fd),
                  !soft_dirty_open_files()))) {
        if (!output_unneeded) {
          memset(GC_grungy_pages, 0xff, sizeof(page_hash_table));
#ifdef CHECKSUMS
            memset(GC_written_pages, 0xff, sizeof(page_hash_table));
#endif
        }
        return;
      }
#endif
    if (!output_unneeded) {
      word i;
      BZERO(GC_grungy_pages, sizeof(GC_grungy_pages));
      pagemap_buf_len = 0;
      for (i = 0; i != GC_n_heap_sects; ++i) {
        ptr_t vaddr = GC_heap_sects[i].hs_start;
        soft_set_grungy_pages(vaddr, vaddr + GC_heap_sects[i].hs_bytes,
                              i < GC_n_heap_sects-1 ?
                                    GC_heap_sects[i+1].hs_start : NULL);
      }
#ifdef CHECKSUMS
        GC_or_pages(GC_written_pages, GC_grungy_pages);
#endif
#ifndef NO_VDB_FOR_STATIC_ROOTS
        for (i = 0; (int)i < n_root_sets; ++i) {
          soft_set_grungy_pages(GC_static_roots[i].r_start,
                                GC_static_roots[i].r_end,
                                (int)i < n_root_sets-1 ?
                                    GC_static_roots[i+1].r_start : NULL);
        }
#endif
    }
    res = write(clear_refs_fd, "4\n", 2);
    if (res != 2)
      ABORT_ARG1("Failed to write to /proc/self/clear_refs",
                 ": errno= %d", res < 0 ? errno : 0);
  }
#endif
#ifdef PCR_VDB
#include "vd/PCR_VD.h"
#define NPAGES (32*1024)
PCR_VD_DB GC_grungy_bits[NPAGES];
STATIC ptr_t GC_vd_base = NULL;
GC_INNER GC_bool GC_dirty_init(void)
{
    GC_vd_base = GC_heap_sects[0].hs_start;
    if (GC_vd_base == 0) {
        ABORT("Bad initial heap segment");
    }
    if (PCR_VD_Start(HBLKSIZE, GC_vd_base, NPAGES*HBLKSIZE)
        != PCR_ERes_okay) {
        ABORT("Dirty bit initialization failed");
    }
    return TRUE;
}
#endif
#ifndef GC_DISABLE_INCREMENTAL
  GC_INNER GC_bool GC_manual_vdb = FALSE;
  GC_INNER void GC_dirty_inner(const void *p)
  {
    word index = PHT_HASH(p);
#if defined(MPROTECT_VDB)
      GC_ASSERT(GC_manual_vdb);
#endif
    async_set_pht_entry_from_index(GC_dirty_pages, index);
  }
  GC_INNER void GC_read_dirty(GC_bool output_unneeded)
  {
    if (GC_manual_vdb
#if defined(MPROTECT_VDB)
          || !GC_GWW_AVAILABLE()
#endif
        ) {
      if (!output_unneeded)
        BCOPY(( void *)GC_dirty_pages, GC_grungy_pages,
              sizeof(GC_dirty_pages));
      BZERO(( void *)GC_dirty_pages,
            sizeof(GC_dirty_pages));
#ifdef MPROTECT_VDB
        if (!GC_manual_vdb)
          GC_protect_heap();
#endif
      return;
    }
#ifdef GWW_VDB
      GC_gww_read_dirty(output_unneeded);
#elif defined(PROC_VDB)
      GC_proc_read_dirty(output_unneeded);
#elif defined(SOFT_VDB)
      GC_soft_read_dirty(output_unneeded);
#elif defined(PCR_VDB)
      {
        static int onhs = 0;
        int nhs = GC_n_heap_sects;
        for (; onhs < nhs; onhs++) {
            PCR_VD_WriteProtectEnable(
                    GC_heap_sects[onhs].hs_start,
                    GC_heap_sects[onhs].hs_bytes);
        }
      }
      if (PCR_VD_Clear(GC_vd_base, NPAGES*HBLKSIZE, GC_grungy_bits)
          != PCR_ERes_okay) {
        ABORT("Dirty bit read failed");
      }
#endif
  }
#if !defined(NO_VDB_FOR_STATIC_ROOTS) && !defined(PROC_VDB)
    GC_INNER GC_bool GC_is_vdb_for_static_roots(void)
    {
      if (GC_manual_vdb) return FALSE;
#if defined(MPROTECT_VDB)
        return GC_GWW_AVAILABLE();
#else
        GC_ASSERT(GC_incremental);
        return TRUE;
#endif
    }
#endif
  GC_INNER GC_bool GC_page_was_dirty(struct hblk *h)
  {
    word index;
#ifdef PCR_VDB
      if (!GC_manual_vdb) {
        if ((word)h < (word)GC_vd_base
            || (word)h >= (word)(GC_vd_base + NPAGES * HBLKSIZE)) {
          return TRUE;
        }
        return GC_grungy_bits[h-(struct hblk*)GC_vd_base] & PCR_VD_DB_dirtyBit;
      }
#elif defined(DEFAULT_VDB)
      if (!GC_manual_vdb)
        return TRUE;
#elif defined(PROC_VDB)
      if (GC_manual_vdb)
#endif
      {
        if (NULL == HDR(h))
          return TRUE;
      }
    index = PHT_HASH(h);
    return get_pht_entry_from_index(GC_grungy_pages, index);
  }
#if defined(CHECKSUMS) || defined(PROC_VDB)
    GC_INNER GC_bool GC_page_was_ever_dirty(struct hblk *h)
    {
#if defined(GWW_VDB) || defined(PROC_VDB) || defined(SOFT_VDB)
        word index;
#ifdef MPROTECT_VDB
          if (!GC_GWW_AVAILABLE())
            return TRUE;
#endif
#if defined(PROC_VDB)
          if (GC_manual_vdb)
#endif
        {
          if (NULL == HDR(h))
            return TRUE;
        }
        index = PHT_HASH(h);
        return get_pht_entry_from_index(GC_written_pages, index);
#else
        (void)h;
        return TRUE;
#endif
    }
#endif
  GC_INNER void GC_remove_protection(struct hblk *h, word nblocks,
                                     GC_bool is_ptrfree)
  {
#ifdef PCR_VDB
      (void)is_ptrfree;
      if (!GC_auto_incremental)
        return;
      PCR_VD_WriteProtectDisable(h, nblocks*HBLKSIZE);
      PCR_VD_WriteProtectEnable(h, nblocks*HBLKSIZE);
#elif defined(MPROTECT_VDB)
      struct hblk * h_trunc;
      struct hblk * h_end;
      struct hblk * current;
      if (!GC_auto_incremental || GC_GWW_AVAILABLE())
        return;
      GC_ASSERT(GC_page_size != 0);
      h_trunc = (struct hblk *)((word)h & ~(GC_page_size-1));
      h_end = (struct hblk *)(((word)(h + nblocks) + GC_page_size - 1)
                              & ~(GC_page_size - 1));
      if (h_end == h_trunc + 1 &&
        get_pht_entry_from_index(GC_dirty_pages, PHT_HASH(h_trunc))) {
        return;
      }
      for (current = h_trunc; (word)current < (word)h_end; ++current) {
        word index = PHT_HASH(current);
        if (!is_ptrfree || (word)current < (word)h
            || (word)current >= (word)(h + nblocks)) {
          async_set_pht_entry_from_index(GC_dirty_pages, index);
        }
      }
      UNPROTECT(h_trunc, (ptr_t)h_end - (ptr_t)h_trunc);
#else
      (void)h; (void)nblocks; (void)is_ptrfree;
#endif
  }
#endif
#if defined(MPROTECT_VDB) && defined(DARWIN)
#include <mach/mach.h>
#include <mach/mach_error.h>
#include <mach/exception.h>
#include <mach/task.h>
#include <pthread.h>
EXTERN_C_BEGIN
extern boolean_t
exc_server(mach_msg_header_t *, mach_msg_header_t *);
extern kern_return_t
exception_raise(mach_port_t, mach_port_t, mach_port_t, exception_type_t,
                exception_data_t, mach_msg_type_number_t);
extern kern_return_t
exception_raise_state(mach_port_t, mach_port_t, mach_port_t, exception_type_t,
                      exception_data_t, mach_msg_type_number_t,
                      thread_state_flavor_t*, thread_state_t,
                      mach_msg_type_number_t, thread_state_t,
                      mach_msg_type_number_t*);
extern kern_return_t
exception_raise_state_identity(mach_port_t, mach_port_t, mach_port_t,
                               exception_type_t, exception_data_t,
                               mach_msg_type_number_t, thread_state_flavor_t*,
                               thread_state_t, mach_msg_type_number_t,
                               thread_state_t, mach_msg_type_number_t*);
GC_API_OSCALL kern_return_t
catch_exception_raise(mach_port_t exception_port, mach_port_t thread,
                      mach_port_t task, exception_type_t exception,
                      exception_data_t code,
                      mach_msg_type_number_t code_count);
GC_API_OSCALL kern_return_t
catch_exception_raise_state(mach_port_name_t exception_port,
                int exception, exception_data_t code,
                mach_msg_type_number_t codeCnt, int flavor,
                thread_state_t old_state, int old_stateCnt,
                thread_state_t new_state, int new_stateCnt);
GC_API_OSCALL kern_return_t
catch_exception_raise_state_identity(mach_port_name_t exception_port,
                mach_port_t thread, mach_port_t task, int exception,
                exception_data_t code, mach_msg_type_number_t codeCnt,
                int flavor, thread_state_t old_state, int old_stateCnt,
                thread_state_t new_state, int new_stateCnt);
EXTERN_C_END
GC_API_OSCALL kern_return_t
catch_exception_raise_state(mach_port_name_t exception_port GC_ATTR_UNUSED,
    int exception GC_ATTR_UNUSED, exception_data_t code GC_ATTR_UNUSED,
    mach_msg_type_number_t codeCnt GC_ATTR_UNUSED, int flavor GC_ATTR_UNUSED,
    thread_state_t old_state GC_ATTR_UNUSED, int old_stateCnt GC_ATTR_UNUSED,
    thread_state_t new_state GC_ATTR_UNUSED, int new_stateCnt GC_ATTR_UNUSED)
{
  ABORT_RET("Unexpected catch_exception_raise_state invocation");
  return(KERN_INVALID_ARGUMENT);
}
GC_API_OSCALL kern_return_t
catch_exception_raise_state_identity(
    mach_port_name_t exception_port GC_ATTR_UNUSED,
    mach_port_t thread GC_ATTR_UNUSED, mach_port_t task GC_ATTR_UNUSED,
    int exception GC_ATTR_UNUSED, exception_data_t code GC_ATTR_UNUSED,
    mach_msg_type_number_t codeCnt GC_ATTR_UNUSED, int flavor GC_ATTR_UNUSED,
    thread_state_t old_state GC_ATTR_UNUSED, int old_stateCnt GC_ATTR_UNUSED,
    thread_state_t new_state GC_ATTR_UNUSED, int new_stateCnt GC_ATTR_UNUSED)
{
  ABORT_RET("Unexpected catch_exception_raise_state_identity invocation");
  return(KERN_INVALID_ARGUMENT);
}
#define MAX_EXCEPTION_PORTS 16
static struct {
  mach_msg_type_number_t count;
  exception_mask_t      masks[MAX_EXCEPTION_PORTS];
  exception_handler_t   ports[MAX_EXCEPTION_PORTS];
  exception_behavior_t  behaviors[MAX_EXCEPTION_PORTS];
  thread_state_flavor_t flavors[MAX_EXCEPTION_PORTS];
} GC_old_exc_ports;
STATIC struct ports_s {
  void (*volatile os_callback[3])(void);
  mach_port_t exception;
#if defined(THREADS)
    mach_port_t reply;
#endif
} GC_ports = {
  {
    (void (*)(void))catch_exception_raise,
    (void (*)(void))catch_exception_raise_state,
    (void (*)(void))catch_exception_raise_state_identity
  },
#ifdef THREADS
    0,
#endif
  0
};
typedef struct {
    mach_msg_header_t head;
} GC_msg_t;
typedef enum {
    GC_MP_NORMAL,
    GC_MP_DISCARDING,
    GC_MP_STOPPED
} GC_mprotect_state_t;
#ifdef THREADS
#define ID_STOP 1
#define ID_RESUME 2
#define ID_ACK 3
  STATIC GC_mprotect_state_t GC_mprotect_state = GC_MP_NORMAL;
  STATIC void GC_mprotect_thread_notify(mach_msg_id_t id)
  {
    struct buf_s {
      GC_msg_t msg;
      mach_msg_trailer_t trailer;
    } buf;
    mach_msg_return_t r;
    buf.msg.head.msgh_bits = MACH_MSGH_BITS(MACH_MSG_TYPE_MAKE_SEND, 0);
    buf.msg.head.msgh_size = sizeof(buf.msg);
    buf.msg.head.msgh_remote_port = GC_ports.exception;
    buf.msg.head.msgh_local_port = MACH_PORT_NULL;
    buf.msg.head.msgh_id = id;
    r = mach_msg(&buf.msg.head, MACH_SEND_MSG | MACH_RCV_MSG | MACH_RCV_LARGE,
                 sizeof(buf.msg), sizeof(buf), GC_ports.reply,
                 MACH_MSG_TIMEOUT_NONE, MACH_PORT_NULL);
    if (r != MACH_MSG_SUCCESS)
      ABORT("mach_msg failed in GC_mprotect_thread_notify");
    if (buf.msg.head.msgh_id != ID_ACK)
      ABORT("Invalid ack in GC_mprotect_thread_notify");
  }
  STATIC void GC_mprotect_thread_reply(void)
  {
    GC_msg_t msg;
    mach_msg_return_t r;
    msg.head.msgh_bits = MACH_MSGH_BITS(MACH_MSG_TYPE_MAKE_SEND, 0);
    msg.head.msgh_size = sizeof(msg);
    msg.head.msgh_remote_port = GC_ports.reply;
    msg.head.msgh_local_port = MACH_PORT_NULL;
    msg.head.msgh_id = ID_ACK;
    r = mach_msg(&msg.head, MACH_SEND_MSG, sizeof(msg), 0, MACH_PORT_NULL,
                 MACH_MSG_TIMEOUT_NONE, MACH_PORT_NULL);
    if (r != MACH_MSG_SUCCESS)
      ABORT("mach_msg failed in GC_mprotect_thread_reply");
  }
  GC_INNER void GC_mprotect_stop(void)
  {
    GC_mprotect_thread_notify(ID_STOP);
  }
  GC_INNER void GC_mprotect_resume(void)
  {
    GC_mprotect_thread_notify(ID_RESUME);
  }
#else
#define GC_mprotect_state GC_MP_NORMAL
#endif
struct mp_reply_s {
  mach_msg_header_t head;
  char data[256];
};
struct mp_msg_s {
  mach_msg_header_t head;
  mach_msg_body_t msgh_body;
  char data[1024];
};
STATIC void *GC_mprotect_thread(void *arg)
{
  mach_msg_return_t r;
  struct mp_reply_s reply;
  struct mp_msg_s msg;
  mach_msg_id_t id;
  if ((word)arg == GC_WORD_MAX) return 0;
#if defined(CPPCHECK)
    reply.data[0] = 0;
    msg.data[0] = 0;
#endif
#if defined(HAVE_PTHREAD_SETNAME_NP_WITHOUT_TID)
    (void)pthread_setname_np("GC-mprotect");
#endif
#if defined(THREADS) && !defined(GC_NO_THREADS_DISCOVERY)
    GC_darwin_register_mach_handler_thread(mach_thread_self());
#endif
  for(;;) {
    r = mach_msg(&msg.head, MACH_RCV_MSG | MACH_RCV_LARGE |
                 (GC_mprotect_state == GC_MP_DISCARDING ? MACH_RCV_TIMEOUT : 0),
                 0, sizeof(msg), GC_ports.exception,
                 GC_mprotect_state == GC_MP_DISCARDING ? 0
                 : MACH_MSG_TIMEOUT_NONE, MACH_PORT_NULL);
    id = r == MACH_MSG_SUCCESS ? msg.head.msgh_id : -1;
#if defined(THREADS)
      if(GC_mprotect_state == GC_MP_DISCARDING) {
        if(r == MACH_RCV_TIMED_OUT) {
          GC_mprotect_state = GC_MP_STOPPED;
          GC_mprotect_thread_reply();
          continue;
        }
        if(r == MACH_MSG_SUCCESS && (id == ID_STOP || id == ID_RESUME))
          ABORT("Out of order mprotect thread request");
      }
#endif
    if (r != MACH_MSG_SUCCESS) {
      ABORT_ARG2("mach_msg failed",
                 ": errcode= %d (%s)", (int)r, mach_error_string(r));
    }
    switch(id) {
#if defined(THREADS)
        case ID_STOP:
          if(GC_mprotect_state != GC_MP_NORMAL)
            ABORT("Called mprotect_stop when state wasn't normal");
          GC_mprotect_state = GC_MP_DISCARDING;
          break;
        case ID_RESUME:
          if(GC_mprotect_state != GC_MP_STOPPED)
            ABORT("Called mprotect_resume when state wasn't stopped");
          GC_mprotect_state = GC_MP_NORMAL;
          GC_mprotect_thread_reply();
          break;
#endif
        default:
          if(!exc_server(&msg.head, &reply.head))
            ABORT("exc_server failed");
          r = mach_msg(&reply.head, MACH_SEND_MSG, reply.head.msgh_size, 0,
                       MACH_PORT_NULL, MACH_MSG_TIMEOUT_NONE,
                       MACH_PORT_NULL);
          if(r != MACH_MSG_SUCCESS) {
#ifdef BROKEN_EXCEPTION_HANDLING
              GC_err_printf("mach_msg failed with %d %s while sending "
                            "exc reply\n", (int)r, mach_error_string(r));
#else
              ABORT("mach_msg failed while sending exception reply");
#endif
          }
    }
  }
}
#ifdef BROKEN_EXCEPTION_HANDLING
  STATIC int GC_sigbus_count = 0;
  STATIC void GC_darwin_sigbus(int num, siginfo_t *sip, void *context)
  {
    if (num != SIGBUS)
      ABORT("Got a non-sigbus signal in the sigbus handler");
    if (GC_sigbus_count >= 8) {
      ABORT("Got more than 8 SIGBUSs in a row!");
    } else {
      GC_sigbus_count++;
      WARN("Ignoring SIGBUS\n", 0);
    }
  }
#endif
GC_INNER GC_bool GC_dirty_init(void)
{
  kern_return_t r;
  mach_port_t me;
  pthread_t thread;
  pthread_attr_t attr;
  exception_mask_t mask;
#ifdef CAN_HANDLE_FORK
    if (GC_handle_fork) {
      WARN("Can't turn on GC incremental mode as fork()"
           " handling requested\n", 0);
      return FALSE;
    }
#endif
  GC_VERBOSE_LOG_PRINTF("Initializing mach/darwin mprotect"
                        " virtual dirty bit implementation\n");
#ifdef BROKEN_EXCEPTION_HANDLING
    WARN("Enabling workarounds for various darwin "
         "exception handling bugs\n", 0);
#endif
  if (GC_page_size % HBLKSIZE != 0) {
    ABORT("Page size not multiple of HBLKSIZE");
  }
  GC_task_self = me = mach_task_self();
  r = mach_port_allocate(me, MACH_PORT_RIGHT_RECEIVE, &GC_ports.exception);
  if (r != KERN_SUCCESS)
    ABORT("mach_port_allocate failed (exception port)");
  r = mach_port_insert_right(me, GC_ports.exception, GC_ports.exception,
                             MACH_MSG_TYPE_MAKE_SEND);
  if (r != KERN_SUCCESS)
    ABORT("mach_port_insert_right failed (exception port)");
#if defined(THREADS)
     r = mach_port_allocate(me, MACH_PORT_RIGHT_RECEIVE, &GC_ports.reply);
     if(r != KERN_SUCCESS)
       ABORT("mach_port_allocate failed (reply port)");
#endif
  mask = EXC_MASK_BAD_ACCESS;
  r = task_get_exception_ports(me, mask, GC_old_exc_ports.masks,
                               &GC_old_exc_ports.count, GC_old_exc_ports.ports,
                               GC_old_exc_ports.behaviors,
                               GC_old_exc_ports.flavors);
  if (r != KERN_SUCCESS)
    ABORT("task_get_exception_ports failed");
  r = task_set_exception_ports(me, mask, GC_ports.exception, EXCEPTION_DEFAULT,
                               GC_MACH_THREAD_STATE);
  if (r != KERN_SUCCESS)
    ABORT("task_set_exception_ports failed");
  if (pthread_attr_init(&attr) != 0)
    ABORT("pthread_attr_init failed");
  if (pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED) != 0)
    ABORT("pthread_attr_setdetachedstate failed");
#undef pthread_create
  if (pthread_create(&thread, &attr, GC_mprotect_thread, NULL) != 0)
    ABORT("pthread_create failed");
  (void)pthread_attr_destroy(&attr);
#ifdef BROKEN_EXCEPTION_HANDLING
    {
      struct sigaction sa, oldsa;
      sa.sa_handler = (SIG_HNDLR_PTR)GC_darwin_sigbus;
      sigemptyset(&sa.sa_mask);
      sa.sa_flags = SA_RESTART|SA_SIGINFO;
      if (sigaction(SIGBUS, &sa, &oldsa) < 0)
        ABORT("sigaction failed");
      if (oldsa.sa_handler != (SIG_HNDLR_PTR)(signed_word)SIG_DFL) {
        GC_VERBOSE_LOG_PRINTF("Replaced other SIGBUS handler\n");
      }
    }
#endif
#if defined(CPPCHECK)
    GC_noop1((word)GC_ports.os_callback[0]);
#endif
  return TRUE;
}
STATIC kern_return_t GC_forward_exception(mach_port_t thread, mach_port_t task,
                                          exception_type_t exception,
                                          exception_data_t data,
                                          mach_msg_type_number_t data_count)
{
  unsigned int i;
  kern_return_t r;
  mach_port_t port;
  exception_behavior_t behavior;
  thread_state_flavor_t flavor;
  thread_state_data_t thread_state;
  mach_msg_type_number_t thread_state_count = THREAD_STATE_MAX;
  for (i=0; i < GC_old_exc_ports.count; i++)
    if (GC_old_exc_ports.masks[i] & (1 << exception))
      break;
  if (i == GC_old_exc_ports.count)
    ABORT("No handler for exception!");
  port = GC_old_exc_ports.ports[i];
  behavior = GC_old_exc_ports.behaviors[i];
  flavor = GC_old_exc_ports.flavors[i];
  if (behavior == EXCEPTION_STATE || behavior == EXCEPTION_STATE_IDENTITY) {
    r = thread_get_state(thread, flavor, thread_state, &thread_state_count);
    if(r != KERN_SUCCESS)
      ABORT("thread_get_state failed in forward_exception");
    }
  switch(behavior) {
    case EXCEPTION_STATE:
      r = exception_raise_state(port, thread, task, exception, data, data_count,
                                &flavor, thread_state, thread_state_count,
                                thread_state, &thread_state_count);
      break;
    case EXCEPTION_STATE_IDENTITY:
      r = exception_raise_state_identity(port, thread, task, exception, data,
                                         data_count, &flavor, thread_state,
                                         thread_state_count, thread_state,
                                         &thread_state_count);
      break;
    default:
      r = exception_raise(port, thread, task, exception, data, data_count);
  }
  if (behavior == EXCEPTION_STATE || behavior == EXCEPTION_STATE_IDENTITY) {
    r = thread_set_state(thread, flavor, thread_state, thread_state_count);
    if (r != KERN_SUCCESS)
      ABORT("thread_set_state failed in forward_exception");
  }
  return r;
}
#define FWD() GC_forward_exception(thread, task, exception, code, code_count)
#ifdef ARM32
#define DARWIN_EXC_STATE         ARM_EXCEPTION_STATE
#define DARWIN_EXC_STATE_COUNT   ARM_EXCEPTION_STATE_COUNT
#define DARWIN_EXC_STATE_T       arm_exception_state_t
#define DARWIN_EXC_STATE_DAR     THREAD_FLD_NAME(far)
#elif defined(AARCH64)
#define DARWIN_EXC_STATE         ARM_EXCEPTION_STATE64
#define DARWIN_EXC_STATE_COUNT   ARM_EXCEPTION_STATE64_COUNT
#define DARWIN_EXC_STATE_T       arm_exception_state64_t
#define DARWIN_EXC_STATE_DAR     THREAD_FLD_NAME(far)
#elif defined(POWERPC)
#if CPP_WORDSZ == 32
#define DARWIN_EXC_STATE       PPC_EXCEPTION_STATE
#define DARWIN_EXC_STATE_COUNT PPC_EXCEPTION_STATE_COUNT
#define DARWIN_EXC_STATE_T     ppc_exception_state_t
#else
#define DARWIN_EXC_STATE       PPC_EXCEPTION_STATE64
#define DARWIN_EXC_STATE_COUNT PPC_EXCEPTION_STATE64_COUNT
#define DARWIN_EXC_STATE_T     ppc_exception_state64_t
#endif
#define DARWIN_EXC_STATE_DAR     THREAD_FLD_NAME(dar)
#elif defined(I386) || defined(X86_64)
#if CPP_WORDSZ == 32
#if defined(i386_EXCEPTION_STATE_COUNT) \
       && !defined(x86_EXCEPTION_STATE32_COUNT)
#define DARWIN_EXC_STATE           i386_EXCEPTION_STATE
#define DARWIN_EXC_STATE_COUNT     i386_EXCEPTION_STATE_COUNT
#define DARWIN_EXC_STATE_T         i386_exception_state_t
#else
#define DARWIN_EXC_STATE           x86_EXCEPTION_STATE32
#define DARWIN_EXC_STATE_COUNT     x86_EXCEPTION_STATE32_COUNT
#define DARWIN_EXC_STATE_T         x86_exception_state32_t
#endif
#else
#define DARWIN_EXC_STATE       x86_EXCEPTION_STATE64
#define DARWIN_EXC_STATE_COUNT x86_EXCEPTION_STATE64_COUNT
#define DARWIN_EXC_STATE_T     x86_exception_state64_t
#endif
#define DARWIN_EXC_STATE_DAR     THREAD_FLD_NAME(faultvaddr)
#elif !defined(CPPCHECK)
#error FIXME for non-arm/ppc/x86 darwin
#endif
GC_API_OSCALL kern_return_t
catch_exception_raise(mach_port_t exception_port GC_ATTR_UNUSED,
                      mach_port_t thread, mach_port_t task GC_ATTR_UNUSED,
                      exception_type_t exception, exception_data_t code,
                      mach_msg_type_number_t code_count GC_ATTR_UNUSED)
{
  kern_return_t r;
  char *addr;
  thread_state_flavor_t flavor = DARWIN_EXC_STATE;
  mach_msg_type_number_t exc_state_count = DARWIN_EXC_STATE_COUNT;
  DARWIN_EXC_STATE_T exc_state;
  if (exception != EXC_BAD_ACCESS || code[0] != KERN_PROTECTION_FAILURE) {
#ifdef DEBUG_EXCEPTION_HANDLING
      GC_log_printf("Exception: 0x%x Code: 0x%x 0x%x in catch...\n",
                    exception, code_count > 0 ? code[0] : -1,
                    code_count > 1 ? code[1] : -1);
#endif
    return FWD();
  }
  r = thread_get_state(thread, flavor, (natural_t*)&exc_state,
                       &exc_state_count);
  if(r != KERN_SUCCESS) {
#ifdef BROKEN_EXCEPTION_HANDLING
      GC_err_printf("thread_get_state failed in catch_exception_raise\n");
      return KERN_SUCCESS;
#else
      ABORT("thread_get_state failed in catch_exception_raise");
#endif
  }
  addr = (char*) exc_state.DARWIN_EXC_STATE_DAR;
  if (!is_header_found_async(addr)) {
#ifdef BROKEN_EXCEPTION_HANDLING
      static char *last_fault;
      static int last_fault_count;
      if(addr != last_fault) {
        last_fault = addr;
        last_fault_count = 0;
      }
      if(++last_fault_count < 32) {
        if(last_fault_count == 1)
          WARN("Ignoring KERN_PROTECTION_FAILURE at %p\n", addr);
        return KERN_SUCCESS;
      }
      GC_err_printf("Unexpected KERN_PROTECTION_FAILURE at %p; aborting...\n",
                    (void *)addr);
      EXIT();
#else
      return FWD();
#endif
  }
#ifdef BROKEN_EXCEPTION_HANDLING
    GC_sigbus_count = 0;
#endif
  GC_ASSERT(GC_page_size != 0);
  if (GC_mprotect_state == GC_MP_NORMAL) {
    struct hblk * h = (struct hblk*)((word)addr & ~(GC_page_size-1));
    size_t i;
    UNPROTECT(h, GC_page_size);
    for (i = 0; i < divHBLKSZ(GC_page_size); i++) {
      word index = PHT_HASH(h+i);
      async_set_pht_entry_from_index(GC_dirty_pages, index);
    }
  } else if (GC_mprotect_state == GC_MP_DISCARDING) {
  } else {
    GC_err_printf("KERN_PROTECTION_FAILURE while world is stopped\n");
    return FWD();
  }
  return KERN_SUCCESS;
}
#undef FWD
#ifndef NO_DESC_CATCH_EXCEPTION_RAISE
  __asm__(".desc _catch_exception_raise, 0x10");
  __asm__(".desc _catch_exception_raise_state, 0x10");
  __asm__(".desc _catch_exception_raise_state_identity, 0x10");
#endif
#endif
#ifndef HAVE_INCREMENTAL_PROTECTION_NEEDS
  GC_API int GC_CALL GC_incremental_protection_needs(void)
  {
    GC_ASSERT(GC_is_initialized);
    return GC_PROTECTS_NONE;
  }
#endif
#ifdef ECOS
#undef sbrk
#endif
GC_API void GC_CALL GC_set_pages_executable(int value)
{
  GC_ASSERT(!GC_is_initialized);
  GC_pages_executable = (GC_bool)(value != 0);
}
GC_API int GC_CALL GC_get_pages_executable(void)
{
#ifdef IGNORE_PAGES_EXECUTABLE
    return 1;
#else
    return (int)GC_pages_executable;
#endif
}
#if defined(I386) && defined(LINUX) && defined(SAVE_CALL_CHAIN)
#include <features.h>
    struct frame {
        struct frame *fr_savfp;
        long    fr_savpc;
#if NARGS > 0
          long  fr_arg[NARGS];
#endif
    };
#endif
#if defined(SPARC)
#if defined(LINUX)
#include <features.h>
#if defined(SAVE_CALL_CHAIN)
     struct frame {
        long    fr_local[8];
        long    fr_arg[6];
        struct frame *fr_savfp;
        long    fr_savpc;
#ifndef __arch64__
          char  *fr_stret;
#endif
        long    fr_argd[6];
        long    fr_argx[0];
     };
#endif
#elif defined (DRSNX)
#include <sys/sparc/frame.h>
#elif defined(OPENBSD)
#include <frame.h>
#elif defined(FREEBSD) || defined(NETBSD)
#include <machine/frame.h>
#else
#include <sys/frame.h>
#endif
#if NARGS > 6
#error We only know how to get the first 6 arguments
#endif
#endif
#ifdef NEED_CALLINFO
#ifdef LINUX
#include <unistd.h>
#endif
#endif
#if defined(GC_HAVE_BUILTIN_BACKTRACE)
#ifdef _MSC_VER
#ifndef GC_MSVC_DBG_H
#define GC_MSVC_DBG_H
#include <stdlib.h>
#ifdef __cplusplus
  extern "C" {
#endif
#if !MSVC_DBG_DLL
#define MSVC_DBG_EXPORT
#elif MSVC_DBG_BUILD
#define MSVC_DBG_EXPORT __declspec(dllexport)
#else
#define MSVC_DBG_EXPORT __declspec(dllimport)
#endif
#ifndef MAX_SYM_NAME
#define MAX_SYM_NAME 2000
#endif
typedef void*  HANDLE;
typedef struct _CONTEXT CONTEXT;
MSVC_DBG_EXPORT size_t GetStackFrames(size_t skip, void* frames[], size_t maxFrames);
MSVC_DBG_EXPORT size_t GetStackFramesFromContext(HANDLE hProcess, HANDLE hThread, CONTEXT* context, size_t skip, void* frames[], size_t maxFrames);
MSVC_DBG_EXPORT size_t GetModuleNameFromAddress(void* address, char* moduleName, size_t size);
MSVC_DBG_EXPORT size_t GetModuleNameFromStack(size_t skip, char* moduleName, size_t size);
MSVC_DBG_EXPORT size_t GetSymbolNameFromAddress(void* address, char* symbolName, size_t size, size_t* offsetBytes);
MSVC_DBG_EXPORT size_t GetSymbolNameFromStack(size_t skip, char* symbolName, size_t size, size_t* offsetBytes);
MSVC_DBG_EXPORT size_t GetFileLineFromAddress(void* address, char* fileName, size_t size, size_t* lineNumber, size_t* offsetBytes);
MSVC_DBG_EXPORT size_t GetFileLineFromStack(size_t skip, char* fileName, size_t size, size_t* lineNumber, size_t* offsetBytes);
MSVC_DBG_EXPORT size_t GetDescriptionFromAddress(void* address, const char* format, char* description, size_t size);
MSVC_DBG_EXPORT size_t GetDescriptionFromStack(void*const frames[], size_t count, const char* format, char* description[], size_t size);
MSVC_DBG_EXPORT int    backtrace(void* addresses[], int count);
MSVC_DBG_EXPORT char** backtrace_symbols(void*const addresses[], int count);
#ifdef __cplusplus
  }
#endif
#endif
#else
#include <execinfo.h>
#endif
#endif
#ifdef SAVE_CALL_CHAIN
#if NARGS == 0 && NFRAMES % 2 == 0  \
    && defined(GC_HAVE_BUILTIN_BACKTRACE)
#ifdef REDIRECT_MALLOC
#ifdef THREADS
    __thread
#endif
    GC_bool GC_in_save_callers = FALSE;
#endif
GC_INNER void GC_save_callers(struct callinfo info[NFRAMES])
{
  void * tmp_info[NFRAMES + 1];
  int npcs, i;
#define IGNORE_FRAMES 1
#ifdef REDIRECT_MALLOC
    if (GC_in_save_callers) {
      info[0].ci_pc = (word)(&GC_save_callers);
      for (i = 1; i < NFRAMES; ++i) info[i].ci_pc = 0;
      return;
    }
    GC_in_save_callers = TRUE;
#endif
  GC_ASSERT(I_HOLD_LOCK());
  GC_STATIC_ASSERT(sizeof(struct callinfo) == sizeof(void *));
  npcs = backtrace((void **)tmp_info, NFRAMES + IGNORE_FRAMES);
  if (npcs > IGNORE_FRAMES)
    BCOPY(&tmp_info[IGNORE_FRAMES], info,
          (npcs - IGNORE_FRAMES) * sizeof(void *));
  for (i = npcs - IGNORE_FRAMES; i < NFRAMES; ++i) info[i].ci_pc = 0;
#ifdef REDIRECT_MALLOC
    GC_in_save_callers = FALSE;
#endif
}
#else
#if (defined(OPENBSD) || defined(NETBSD) || defined(FREEBSD)) && defined(SPARC)
#define FR_SAVFP fr_fp
#define FR_SAVPC fr_pc
#else
#define FR_SAVFP fr_savfp
#define FR_SAVPC fr_savpc
#endif
#if defined(SPARC) && (defined(__arch64__) || defined(__sparcv9))
#define BIAS 2047
#else
#define BIAS 0
#endif
GC_INNER void GC_save_callers(struct callinfo info[NFRAMES])
{
  struct frame *frame;
  struct frame *fp;
  int nframes = 0;
#ifdef I386
    asm("movl %%ebp,%0" : "=r"(frame));
    fp = frame;
#else
    frame = (struct frame *)GC_save_regs_in_stack();
    fp = (struct frame *)((long) frame -> FR_SAVFP + BIAS);
#endif
   for (; !((word)fp HOTTER_THAN (word)frame)
#ifndef THREADS
            && !((word)GC_stackbottom HOTTER_THAN (word)fp)
#elif defined(STACK_GROWS_UP)
            && fp != NULL
#endif
          && nframes < NFRAMES;
        fp = (struct frame *)((long) fp -> FR_SAVFP + BIAS), nframes++) {
#if NARGS > 0
        int i;
#endif
      info[nframes].ci_pc = fp->FR_SAVPC;
#if NARGS > 0
        for (i = 0; i < NARGS; i++) {
          info[nframes].ci_arg[i] = ~(fp->fr_arg[i]);
        }
#endif
  }
  if (nframes < NFRAMES) info[nframes].ci_pc = 0;
}
#endif
#endif
#ifdef NEED_CALLINFO
GC_INNER void GC_print_callers(struct callinfo info[NFRAMES])
{
    int i;
    static int reentry_count = 0;
    DCL_LOCK_STATE;
    LOCK();
      ++reentry_count;
    UNLOCK();
#if NFRAMES == 1
      GC_err_printf("\tCaller at allocation:\n");
#else
      GC_err_printf("\tCall chain at allocation:\n");
#endif
    for (i = 0; i < NFRAMES; i++) {
#if defined(LINUX) && !defined(SMALL_CONFIG)
          GC_bool stop = FALSE;
#endif
        if (0 == info[i].ci_pc)
          break;
#if NARGS > 0
        {
          int j;
          GC_err_printf("\t\targs: ");
          for (j = 0; j < NARGS; j++) {
            if (j != 0) GC_err_printf(", ");
            GC_err_printf("%d (0x%X)", ~(info[i].ci_arg[j]),
                                        ~(info[i].ci_arg[j]));
          }
          GC_err_printf("\n");
        }
#endif
        if (reentry_count > 1) {
            GC_err_printf("\t\t##PC##= 0x%lx\n",
                          (unsigned long)info[i].ci_pc);
            continue;
        }
        {
          char buf[40];
          char *name;
#if defined(GC_HAVE_BUILTIN_BACKTRACE) \
             && !defined(GC_BACKTRACE_SYMBOLS_BROKEN)
            char **sym_name =
              backtrace_symbols((void **)(&(info[i].ci_pc)), 1);
            if (sym_name != NULL) {
              name = sym_name[0];
            } else
#endif
           {
            (void)snprintf(buf, sizeof(buf), "##PC##= 0x%lx",
                           (unsigned long)info[i].ci_pc);
            buf[sizeof(buf) - 1] = '\0';
            name = buf;
          }
#if defined(LINUX) && !defined(SMALL_CONFIG)
            do {
                FILE *pipe;
#define EXE_SZ 100
                static char exe_name[EXE_SZ];
#define CMD_SZ 200
                char cmd_buf[CMD_SZ];
#define RESULT_SZ 200
                static char result_buf[RESULT_SZ];
                size_t result_len;
                char *old_preload;
#define PRELOAD_SZ 200
                char preload_buf[PRELOAD_SZ];
                static GC_bool found_exe_name = FALSE;
                static GC_bool will_fail = FALSE;
                if (will_fail)
                  break;
                if (!found_exe_name) {
                  int ret_code = readlink("/proc/self/exe", exe_name, EXE_SZ);
                  if (ret_code < 0 || ret_code >= EXE_SZ
                      || exe_name[0] != '/') {
                    will_fail = TRUE;
                    break;
                  }
                  exe_name[ret_code] = '\0';
                  found_exe_name = TRUE;
                }
                (void)snprintf(cmd_buf, sizeof(cmd_buf),
                               "/usr/bin/addr2line -f -e %s 0x%lx",
                               exe_name, (unsigned long)info[i].ci_pc);
                cmd_buf[sizeof(cmd_buf) - 1] = '\0';
                old_preload = GETENV("LD_PRELOAD");
                if (0 != old_preload) {
                  size_t old_len = strlen(old_preload);
                  if (old_len >= PRELOAD_SZ) {
                    will_fail = TRUE;
                    break;
                  }
                  BCOPY(old_preload, preload_buf, old_len + 1);
                  unsetenv ("LD_PRELOAD");
                }
                pipe = popen(cmd_buf, "r");
                if (0 != old_preload
                    && 0 != setenv ("LD_PRELOAD", preload_buf, 0)) {
                  WARN("Failed to reset LD_PRELOAD\n", 0);
                }
                if (NULL == pipe) {
                  will_fail = TRUE;
                  break;
                }
                result_len = fread(result_buf, 1, RESULT_SZ - 1, pipe);
                (void)pclose(pipe);
                if (0 == result_len) {
                  will_fail = TRUE;
                  break;
                }
                if (result_buf[result_len - 1] == '\n') --result_len;
                result_buf[result_len] = 0;
                if (result_buf[0] == '?'
                    || (result_buf[result_len-2] == ':'
                        && result_buf[result_len-1] == '0'))
                  break;
                {
                  char * nl = strchr(result_buf, '\n');
                  if (nl != NULL
                      && (word)nl < (word)(result_buf + result_len)) {
                    *nl = ':';
                  }
                  if (strncmp(result_buf, "main",
                              nl != NULL
                                ? (size_t)((word)nl
                                           - COVERT_DATAFLOW(result_buf))
                                : result_len) == 0) {
                    stop = TRUE;
                  }
                }
                if (result_len < RESULT_SZ - 25) {
                  (void)snprintf(&result_buf[result_len],
                                 sizeof(result_buf) - result_len,
                                 " [0x%lx]", (unsigned long)info[i].ci_pc);
                  result_buf[sizeof(result_buf) - 1] = '\0';
                }
#if defined(CPPCHECK)
                  GC_noop1((unsigned char)name[0]);
#endif
                name = result_buf;
            } while (0);
#endif
          GC_err_printf("\t\t%s\n", name);
#if defined(GC_HAVE_BUILTIN_BACKTRACE) \
             && !defined(GC_BACKTRACE_SYMBOLS_BROKEN)
            if (sym_name != NULL)
              free(sym_name);
#endif
        }
#if defined(LINUX) && !defined(SMALL_CONFIG)
          if (stop)
            break;
#endif
    }
    LOCK();
      --reentry_count;
    UNLOCK();
}
#endif
#if defined(LINUX) && defined(__ELF__) && !defined(SMALL_CONFIG)
  void GC_print_address_map(void)
  {
    const char *maps = GC_get_maps();
    GC_err_printf("---------- Begin address map ----------\n");
    GC_err_puts(maps);
    GC_err_printf("---------- End address map ----------\n");
  }
#endif
#if defined(THREAD_LOCAL_ALLOC)
#ifndef THREADS
#error "invalid config - THREAD_LOCAL_ALLOC requires GC_THREADS"
#endif
#ifndef GC_THREAD_LOCAL_ALLOC_H
#define GC_THREAD_LOCAL_ALLOC_H
#ifdef THREAD_LOCAL_ALLOC
#if defined(USE_HPUX_TLS)
#error USE_HPUX_TLS macro was replaced by USE_COMPILER_TLS
#endif
#include <stdlib.h>
EXTERN_C_BEGIN
#if !defined(USE_PTHREAD_SPECIFIC) && !defined(USE_WIN32_SPECIFIC) \
    && !defined(USE_WIN32_COMPILER_TLS) && !defined(USE_COMPILER_TLS) \
    && !defined(USE_CUSTOM_SPECIFIC)
#if defined(MSWIN32) || defined(MSWINCE) || defined(CYGWIN32)
#if defined(CYGWIN32) && GC_GNUC_PREREQ(4, 0)
#if defined(__clang__)
#define USE_PTHREAD_SPECIFIC
#else
#define USE_COMPILER_TLS
#endif
#elif defined(__GNUC__) || defined(MSWINCE)
#define USE_WIN32_SPECIFIC
#else
#define USE_WIN32_COMPILER_TLS
#endif
#elif (defined(LINUX) && !defined(ARM32) && !defined(AVR32) \
         && GC_GNUC_PREREQ(3, 3) \
         && !(defined(__clang__) && defined(HOST_ANDROID))) \
       || (defined(FREEBSD) \
           || (defined(NETBSD) && __NetBSD_Version__ >= 600000000 ) \
            && (GC_GNUC_PREREQ(4, 4) || GC_CLANG_PREREQ(3, 9))) \
       || (defined(HOST_ANDROID) && defined(ARM32) \
            && (GC_GNUC_PREREQ(4, 6) || GC_CLANG_PREREQ_FULL(3, 8, 256229)))
#define USE_COMPILER_TLS
#elif defined(GC_DGUX386_THREADS) || defined(GC_OSF1_THREADS) \
       || defined(GC_AIX_THREADS) || defined(GC_DARWIN_THREADS) \
       || defined(GC_FREEBSD_THREADS) || defined(GC_NETBSD_THREADS) \
       || defined(GC_LINUX_THREADS) || defined(GC_HAIKU_THREADS) \
       || defined(GC_RTEMS_PTHREADS)
#define USE_PTHREAD_SPECIFIC
#elif defined(GC_HPUX_THREADS)
#ifdef __GNUC__
#define USE_PTHREAD_SPECIFIC
#else
#define USE_COMPILER_TLS
#endif
#else
#define USE_CUSTOM_SPECIFIC
#endif
#endif
#ifndef THREAD_FREELISTS_KINDS
#ifdef ENABLE_DISCLAIM
#define THREAD_FREELISTS_KINDS (NORMAL+2)
#else
#define THREAD_FREELISTS_KINDS (NORMAL+1)
#endif
#endif
typedef struct thread_local_freelists {
  void * _freelists[THREAD_FREELISTS_KINDS][TINY_FREELISTS];
#define ptrfree_freelists _freelists[PTRFREE]
#define normal_freelists _freelists[NORMAL]
#ifdef GC_GCJ_SUPPORT
    void * gcj_freelists[TINY_FREELISTS];
#define ERROR_FL ((void *)GC_WORD_MAX)
#endif
#define DIRECT_GRANULES (HBLKSIZE/GRANULE_BYTES)
} *GC_tlfs;
#if defined(USE_PTHREAD_SPECIFIC)
#define GC_getspecific pthread_getspecific
#define GC_setspecific pthread_setspecific
#define GC_key_create pthread_key_create
#define GC_remove_specific(key) pthread_setspecific(key, NULL)
#define GC_remove_specific_after_fork(key, t) (void)0
  typedef pthread_key_t GC_key_t;
#elif defined(USE_COMPILER_TLS) || defined(USE_WIN32_COMPILER_TLS)
#define GC_getspecific(x) (x)
#define GC_setspecific(key, v) ((key) = (v), 0)
#define GC_key_create(key, d) 0
#define GC_remove_specific(key)
#define GC_remove_specific_after_fork(key, t) (void)0
  typedef void * GC_key_t;
#elif defined(USE_WIN32_SPECIFIC)
#define GC_getspecific TlsGetValue
#define GC_setspecific(key, v) !TlsSetValue(key, v)
#ifndef TLS_OUT_OF_INDEXES
#define TLS_OUT_OF_INDEXES (DWORD)0xFFFFFFFF
#endif
#define GC_key_create(key, d) \
        ((d) != 0 || (*(key) = TlsAlloc()) == TLS_OUT_OF_INDEXES ? -1 : 0)
#define GC_remove_specific(key)
#define GC_remove_specific_after_fork(key, t) (void)0
  typedef DWORD GC_key_t;
#elif defined(USE_CUSTOM_SPECIFIC)
  EXTERN_C_END
#ifndef GC_SPECIFIC_H
#define GC_SPECIFIC_H
#include <errno.h>
EXTERN_C_BEGIN
#define MALLOC_CLEAR(n) GC_INTERNAL_MALLOC(n, NORMAL)
#define TS_CACHE_SIZE 1024
#define CACHE_HASH(n) ((((n) >> 8) ^ (n)) & (TS_CACHE_SIZE - 1))
#define TS_HASH_SIZE 1024
#define HASH(p) \
          ((unsigned)((((word)(p)) >> 8) ^ (word)(p)) & (TS_HASH_SIZE - 1))
#ifdef GC_ASSERTIONS
  typedef GC_hidden_pointer ts_entry_value_t;
#define TS_HIDE_VALUE(p) GC_HIDE_POINTER(p)
#define TS_REVEAL_PTR(p) GC_REVEAL_POINTER(p)
#else
  typedef void * ts_entry_value_t;
#define TS_HIDE_VALUE(p) (p)
#define TS_REVEAL_PTR(p) (p)
#endif
typedef struct thread_specific_entry {
        volatile AO_t qtid;
        ts_entry_value_t value;
        struct thread_specific_entry *next;
        pthread_t thread;
} tse;
#define quick_thread_id() (((word)GC_approx_sp()) >> 12)
#define INVALID_QTID ((word)0)
#define INVALID_THREADID ((pthread_t)0)
union ptse_ao_u {
  tse *p;
  volatile AO_t ao;
};
typedef struct thread_specific_data {
    tse * volatile cache[TS_CACHE_SIZE];
    union ptse_ao_u hash[TS_HASH_SIZE];
    pthread_mutex_t lock;
} tsd;
typedef tsd * GC_key_t;
#define GC_key_create(key, d) GC_key_create_inner(key)
GC_INNER int GC_key_create_inner(tsd ** key_ptr);
GC_INNER int GC_setspecific(tsd * key, void * value);
#define GC_remove_specific(key) \
                        GC_remove_specific_after_fork(key, pthread_self())
GC_INNER void GC_remove_specific_after_fork(tsd * key, pthread_t t);
GC_INNER void * GC_slow_getspecific(tsd * key, word qtid,
                                    tse * volatile * cache_entry);
GC_INLINE void * GC_getspecific(tsd * key)
{
    word qtid = quick_thread_id();
    tse * volatile * entry_ptr = &key->cache[CACHE_HASH(qtid)];
    tse * entry = *entry_ptr;
    GC_ASSERT(qtid != INVALID_QTID);
    if (EXPECT(entry -> qtid == qtid, TRUE)) {
      GC_ASSERT(entry -> thread == pthread_self());
      return TS_REVEAL_PTR(entry -> value);
    }
    return GC_slow_getspecific(key, qtid, entry_ptr);
}
EXTERN_C_END
#endif
  EXTERN_C_BEGIN
#else
#error implement me
#endif
GC_INNER void GC_init_thread_local(GC_tlfs p);
GC_INNER void GC_destroy_thread_local(GC_tlfs p);
GC_INNER void GC_mark_thread_local_fls_for(GC_tlfs p);
#ifdef GC_ASSERTIONS
  GC_bool GC_is_thread_tsd_valid(void *tsd);
  void GC_check_tls_for(GC_tlfs p);
#if defined(USE_CUSTOM_SPECIFIC)
    void GC_check_tsd_marks(tsd *key);
#endif
#endif
#ifndef GC_ATTR_TLS_FAST
#define GC_ATTR_TLS_FAST
#endif
extern
#if defined(USE_COMPILER_TLS)
  __thread GC_ATTR_TLS_FAST
#elif defined(USE_WIN32_COMPILER_TLS)
  __declspec(thread) GC_ATTR_TLS_FAST
#endif
  GC_key_t GC_thread_key;
EXTERN_C_END
#endif
#endif
#include <stdlib.h>
#if defined(USE_COMPILER_TLS)
  __thread GC_ATTR_TLS_FAST
#elif defined(USE_WIN32_COMPILER_TLS)
  __declspec(thread) GC_ATTR_TLS_FAST
#endif
GC_key_t GC_thread_key;
static GC_bool keys_initialized;
static void return_single_freelist(void *fl, void **gfl)
{
    if (*gfl == 0) {
      *gfl = fl;
    } else {
      void *q, **qptr;
      GC_ASSERT(GC_size(fl) == GC_size(*gfl));
        qptr = &(obj_link(fl));
        while ((word)(q = *qptr) >= HBLKSIZE)
          qptr = &(obj_link(q));
        GC_ASSERT(0 == q);
        *qptr = *gfl;
        *gfl = fl;
    }
}
static void return_freelists(void **fl, void **gfl)
{
    int i;
    for (i = 1; i < TINY_FREELISTS; ++i) {
        if ((word)(fl[i]) >= HBLKSIZE) {
          return_single_freelist(fl[i], &gfl[i]);
        }
        fl[i] = (ptr_t)HBLKSIZE;
    }
#ifdef GC_GCJ_SUPPORT
      if (fl[0] == ERROR_FL) return;
#endif
    if ((word)(fl[0]) >= HBLKSIZE) {
        return_single_freelist(fl[0], &gfl[1]);
    }
}
#ifdef USE_PTHREAD_SPECIFIC
  static void reset_thread_key(void* v) {
    pthread_setspecific(GC_thread_key, v);
  }
#else
#define reset_thread_key 0
#endif
GC_INNER void GC_init_thread_local(GC_tlfs p)
{
    int i, j, res;
    GC_ASSERT(I_HOLD_LOCK());
    if (!EXPECT(keys_initialized, TRUE)) {
        GC_ASSERT((word)&GC_thread_key % sizeof(word) == 0);
        res = GC_key_create(&GC_thread_key, reset_thread_key);
        if (COVERT_DATAFLOW(res) != 0) {
            ABORT("Failed to create key for local allocator");
        }
        keys_initialized = TRUE;
    }
    res = GC_setspecific(GC_thread_key, p);
    if (COVERT_DATAFLOW(res) != 0) {
        ABORT("Failed to set thread specific allocation pointers");
    }
    for (j = 0; j < TINY_FREELISTS; ++j) {
        for (i = 0; i < THREAD_FREELISTS_KINDS; ++i) {
            p -> _freelists[i][j] = (void *)(word)1;
        }
#ifdef GC_GCJ_SUPPORT
            p -> gcj_freelists[j] = (void *)(word)1;
#endif
    }
#ifdef GC_GCJ_SUPPORT
        p -> gcj_freelists[0] = ERROR_FL;
#endif
}
GC_INNER void GC_destroy_thread_local(GC_tlfs p)
{
    int k;
    GC_STATIC_ASSERT(THREAD_FREELISTS_KINDS <= MAXOBJKINDS);
    for (k = 0; k < THREAD_FREELISTS_KINDS; ++k) {
        if (k == (int)GC_n_kinds)
            break;
        return_freelists(p -> _freelists[k], GC_obj_kinds[k].ok_freelist);
    }
#ifdef GC_GCJ_SUPPORT
        return_freelists(p -> gcj_freelists, (void **)GC_gcjobjfreelist);
#endif
}
GC_API GC_ATTR_MALLOC void * GC_CALL GC_malloc_kind(size_t bytes, int kind)
{
    size_t granules;
    void *tsd;
    void *result;
#if MAXOBJKINDS > THREAD_FREELISTS_KINDS
      if (EXPECT(kind >= THREAD_FREELISTS_KINDS, FALSE)) {
        return GC_malloc_kind_global(bytes, kind);
      }
#endif
#if !defined(USE_PTHREAD_SPECIFIC) && !defined(USE_WIN32_SPECIFIC)
    {
      GC_key_t k = GC_thread_key;
      if (EXPECT(0 == k, FALSE)) {
        return GC_malloc_kind_global(bytes, kind);
      }
      tsd = GC_getspecific(k);
    }
#else
      if (!EXPECT(keys_initialized, TRUE))
        return GC_malloc_kind_global(bytes, kind);
      tsd = GC_getspecific(GC_thread_key);
#endif
#if !defined(USE_COMPILER_TLS) && !defined(USE_WIN32_COMPILER_TLS)
      if (EXPECT(0 == tsd, FALSE)) {
        return GC_malloc_kind_global(bytes, kind);
      }
#endif
    GC_ASSERT(GC_is_initialized);
    GC_ASSERT(GC_is_thread_tsd_valid(tsd));
    granules = ROUNDED_UP_GRANULES(bytes);
#if defined(CPPCHECK)
#define MALLOC_KIND_PTRFREE_INIT (void*)1
#else
#define MALLOC_KIND_PTRFREE_INIT NULL
#endif
    GC_FAST_MALLOC_GRANS(result, granules,
                         ((GC_tlfs)tsd) -> _freelists[kind], DIRECT_GRANULES,
                         kind, GC_malloc_kind_global(bytes, kind),
                         (void)(kind == PTRFREE ? MALLOC_KIND_PTRFREE_INIT
                                               : (obj_link(result) = 0)));
#ifdef LOG_ALLOCS
      GC_log_printf("GC_malloc_kind(%lu, %d) returned %p, recent GC #%lu\n",
                    (unsigned long)bytes, kind, result,
                    (unsigned long)GC_gc_no);
#endif
    return result;
}
#ifdef GC_GCJ_SUPPORT
GC_API GC_ATTR_MALLOC void * GC_CALL GC_gcj_malloc(size_t bytes,
                                    void * ptr_to_struct_containing_descr)
{
  if (EXPECT(GC_incremental, FALSE)) {
    return GC_core_gcj_malloc(bytes, ptr_to_struct_containing_descr);
  } else {
    size_t granules = ROUNDED_UP_GRANULES(bytes);
    void *result;
    void **tiny_fl;
    GC_ASSERT(GC_gcjobjfreelist != NULL);
    tiny_fl = ((GC_tlfs)GC_getspecific(GC_thread_key))->gcj_freelists;
    GC_FAST_MALLOC_GRANS(result, granules, tiny_fl, DIRECT_GRANULES,
                         GC_gcj_kind,
                         GC_core_gcj_malloc(bytes,
                                            ptr_to_struct_containing_descr),
                         {AO_compiler_barrier();
                          *(void **)result = ptr_to_struct_containing_descr;});
    return result;
  }
}
#endif
GC_INNER void GC_mark_thread_local_fls_for(GC_tlfs p)
{
    ptr_t q;
    int i, j;
    for (j = 0; j < TINY_FREELISTS; ++j) {
      for (i = 0; i < THREAD_FREELISTS_KINDS; ++i) {
        q = (ptr_t)AO_load((volatile AO_t *)&p->_freelists[i][j]);
        if ((word)q > HBLKSIZE)
          GC_set_fl_marks(q);
      }
#ifdef GC_GCJ_SUPPORT
        if (EXPECT(j > 0, TRUE)) {
          q = (ptr_t)AO_load((volatile AO_t *)&p->gcj_freelists[j]);
          if ((word)q > HBLKSIZE)
            GC_set_fl_marks(q);
        }
#endif
    }
}
#if defined(GC_ASSERTIONS)
    void GC_check_tls_for(GC_tlfs p)
    {
        int i, j;
        for (j = 1; j < TINY_FREELISTS; ++j) {
          for (i = 0; i < THREAD_FREELISTS_KINDS; ++i) {
            GC_check_fl_marks(&p->_freelists[i][j]);
          }
#ifdef GC_GCJ_SUPPORT
            GC_check_fl_marks(&p->gcj_freelists[j]);
#endif
        }
    }
#endif
#endif
#ifndef GC_PTHREAD_SUPPORT_H
#define GC_PTHREAD_SUPPORT_H
#if defined(GC_PTHREADS) && !defined(GC_WIN32_THREADS)
#if defined(GC_DARWIN_THREADS)
#else
#ifndef GC_PTHREAD_STOP_WORLD_H
#define GC_PTHREAD_STOP_WORLD_H
EXTERN_C_BEGIN
struct thread_stop_info {
#if !defined(GC_OPENBSD_UTHREADS) && !defined(NACL) \
       && !defined(PLATFORM_STOP_WORLD) && !defined(SN_TARGET_PSP2)
      volatile AO_t last_stop_count;
#endif
    ptr_t stack_ptr;
#ifdef NACL
#ifdef ARM32
#define NACL_GC_REG_STORAGE_SIZE 9
#else
#define NACL_GC_REG_STORAGE_SIZE 20
#endif
      ptr_t reg_storage[NACL_GC_REG_STORAGE_SIZE];
#elif defined(PLATFORM_HAVE_GC_REG_STORAGE_SIZE)
      word registers[PLATFORM_GC_REG_STORAGE_SIZE];
#endif
};
GC_INNER void GC_stop_init(void);
EXTERN_C_END
#endif
#endif
#ifdef THREAD_LOCAL_ALLOC
#endif
#ifdef THREAD_SANITIZER
#endif
EXTERN_C_BEGIN
typedef struct GC_Thread_Rep {
#ifdef THREAD_SANITIZER
      char dummy[sizeof(oh)];
#endif
    struct GC_Thread_Rep * next;
    pthread_t id;
#ifdef USE_TKILL_ON_ANDROID
      pid_t kernel_id;
#endif
    struct thread_stop_info stop_info;
#if defined(GC_ENABLE_SUSPEND_THREAD) && !defined(GC_DARWIN_THREADS) \
        && !defined(GC_OPENBSD_UTHREADS) && !defined(NACL)
      volatile AO_t suspended_ext;
#endif
    unsigned char flags;
#define FINISHED 1
#define DETACHED 2
#define MAIN_THREAD 4
#define DISABLED_GC 0x10
    unsigned char thread_blocked;
    unsigned short finalizer_skipped;
    unsigned char finalizer_nested;
    ptr_t stack_end;
    ptr_t altstack;
    word altstack_size;
    ptr_t stack;
    word stack_size;
#if defined(GC_DARWIN_THREADS) && !defined(DARWIN_DONT_PARSE_STACK)
      ptr_t topOfStack;
#endif
#ifdef IA64
        ptr_t backing_store_end;
        ptr_t backing_store_ptr;
#endif
    struct GC_traced_stack_sect_s *traced_stack_sect;
    void * status;
#ifdef THREAD_LOCAL_ALLOC
        struct thread_local_freelists tlfs GC_ATTR_WORD_ALIGNED;
#endif
} * GC_thread;
#ifndef THREAD_TABLE_SZ
#define THREAD_TABLE_SZ 256
#endif
#if CPP_WORDSZ == 64
#define THREAD_TABLE_INDEX(id) \
    (int)(((((NUMERIC_THREAD_ID(id) >> 8) ^ NUMERIC_THREAD_ID(id)) >> 16) \
          ^ ((NUMERIC_THREAD_ID(id) >> 8) ^ NUMERIC_THREAD_ID(id))) \
         % THREAD_TABLE_SZ)
#else
#define THREAD_TABLE_INDEX(id) \
                (int)(((NUMERIC_THREAD_ID(id) >> 16) \
                       ^ (NUMERIC_THREAD_ID(id) >> 8) \
                       ^ NUMERIC_THREAD_ID(id)) % THREAD_TABLE_SZ)
#endif
GC_EXTERN volatile GC_thread GC_threads[THREAD_TABLE_SZ];
GC_EXTERN GC_bool GC_thr_initialized;
GC_INNER GC_thread GC_lookup_thread(pthread_t id);
#ifdef NACL
  GC_EXTERN __thread GC_thread GC_nacl_gc_thread_self;
  GC_INNER void GC_nacl_initialize_gc_thread(void);
  GC_INNER void GC_nacl_shutdown_gc_thread(void);
#endif
#ifdef GC_EXPLICIT_SIGNALS_UNBLOCK
  GC_INNER void GC_unblock_gc_signals(void);
#endif
#ifdef GC_PTHREAD_START_STANDALONE
#define GC_INNER_PTHRSTART
#else
#define GC_INNER_PTHRSTART GC_INNER
#endif
GC_INNER_PTHRSTART void * GC_CALLBACK GC_inner_start_routine(
                                        struct GC_stack_base *sb, void *arg);
GC_INNER_PTHRSTART GC_thread GC_start_rtn_prepare_thread(
                                        void *(**pstart)(void *),
                                        void **pstart_arg,
                                        struct GC_stack_base *sb, void *arg);
GC_INNER_PTHRSTART void GC_thread_exit_proc(void *);
EXTERN_C_END
#endif
#endif
#if defined(GC_DARWIN_THREADS)
#include <sys/sysctl.h>
#include <mach/machine.h>
#include <CoreFoundation/CoreFoundation.h>
#ifdef POWERPC
#if CPP_WORDSZ == 32
#define PPC_RED_ZONE_SIZE 224
#elif CPP_WORDSZ == 64
#define PPC_RED_ZONE_SIZE 320
#endif
#endif
#ifndef DARWIN_DONT_PARSE_STACK
typedef struct StackFrame {
  unsigned long savedSP;
  unsigned long savedCR;
  unsigned long savedLR;
  unsigned long reserved[2];
  unsigned long savedRTOC;
} StackFrame;
GC_INNER ptr_t GC_FindTopOfStack(unsigned long stack_start)
{
  StackFrame *frame = (StackFrame *)stack_start;
  if (stack_start == 0) {
#ifdef POWERPC
#if CPP_WORDSZ == 32
        __asm__ __volatile__ ("lwz %0,0(r1)" : "=r" (frame));
#else
        __asm__ __volatile__ ("ld %0,0(r1)" : "=r" (frame));
#endif
#elif defined(ARM32)
        volatile ptr_t sp_reg;
        __asm__ __volatile__ ("mov %0, r7\n" : "=r" (sp_reg));
        frame = (StackFrame *)sp_reg;
#elif defined(AARCH64)
        volatile ptr_t sp_reg;
        __asm__ __volatile__ ("mov %0, x29\n" : "=r" (sp_reg));
        frame = (StackFrame *)sp_reg;
#else
#if defined(CPPCHECK)
        GC_noop1((word)&frame);
#endif
      ABORT("GC_FindTopOfStack(0) is not implemented");
#endif
  }
#ifdef DEBUG_THREADS_EXTRA
    GC_log_printf("FindTopOfStack start at sp= %p\n", (void *)frame);
#endif
  while (frame->savedSP != 0) {
    frame = (StackFrame*)frame->savedSP;
    if ((frame->savedLR & ~0x3) == 0 || (frame->savedLR & ~0x3) == ~0x3UL)
      break;
  }
#ifdef DEBUG_THREADS_EXTRA
    GC_log_printf("FindTopOfStack finish at sp= %p\n", (void *)frame);
#endif
  return (ptr_t)frame;
}
#endif
#ifdef GC_NO_THREADS_DISCOVERY
#define GC_query_task_threads FALSE
#elif defined(GC_DISCOVER_TASK_THREADS)
#define GC_query_task_threads TRUE
#else
  STATIC GC_bool GC_query_task_threads = FALSE;
#endif
GC_API void GC_CALL GC_use_threads_discovery(void)
{
#if defined(GC_NO_THREADS_DISCOVERY) || defined(DARWIN_DONT_PARSE_STACK)
    ABORT("Darwin task-threads-based stop and push unsupported");
#else
#ifndef GC_ALWAYS_MULTITHREADED
      GC_ASSERT(!GC_need_to_lock);
#endif
#ifndef GC_DISCOVER_TASK_THREADS
      GC_query_task_threads = TRUE;
#endif
    GC_init_parallel();
#endif
}
#ifndef kCFCoreFoundationVersionNumber_iOS_8_0
#define kCFCoreFoundationVersionNumber_iOS_8_0 1140.1
#endif
STATIC ptr_t GC_stack_range_for(ptr_t *phi, thread_act_t thread, GC_thread p,
                                GC_bool thread_blocked, mach_port_t my_thread,
                                ptr_t *paltstack_lo,
                                ptr_t *paltstack_hi GC_ATTR_UNUSED)
{
  ptr_t lo;
  if (thread == my_thread) {
    GC_ASSERT(!thread_blocked);
    lo = GC_approx_sp();
#ifndef DARWIN_DONT_PARSE_STACK
      *phi = GC_FindTopOfStack(0);
#endif
  } else if (thread_blocked) {
#if defined(CPPCHECK)
      if (NULL == p) ABORT("Invalid GC_thread passed to GC_stack_range_for");
#endif
    lo = p->stop_info.stack_ptr;
#ifndef DARWIN_DONT_PARSE_STACK
      *phi = p->topOfStack;
#endif
  } else {
    kern_return_t kern_result;
    GC_THREAD_STATE_T state;
#if defined(ARM32) && defined(ARM_THREAD_STATE32)
      size_t size;
      static cpu_type_t cputype = 0;
      if (cputype == 0) {
        sysctlbyname("hw.cputype", &cputype, &size, NULL, 0);
      }
      if (cputype == CPU_TYPE_ARM64
          || kCFCoreFoundationVersionNumber
             >= kCFCoreFoundationVersionNumber_iOS_8_0) {
        arm_unified_thread_state_t unified_state;
        mach_msg_type_number_t unified_thread_state_count
                                        = ARM_UNIFIED_THREAD_STATE_COUNT;
#if defined(CPPCHECK)
#define GC_ARM_UNIFIED_THREAD_STATE 1
#else
#define GC_ARM_UNIFIED_THREAD_STATE ARM_UNIFIED_THREAD_STATE
#endif
        kern_result = thread_get_state(thread, GC_ARM_UNIFIED_THREAD_STATE,
                                       (natural_t *)&unified_state,
                                       &unified_thread_state_count);
#if !defined(CPPCHECK)
          if (unified_state.ash.flavor != ARM_THREAD_STATE32) {
            ABORT("unified_state flavor should be ARM_THREAD_STATE32");
          }
#endif
        state = unified_state;
      } else
#endif
     {
      mach_msg_type_number_t thread_state_count = GC_MACH_THREAD_STATE_COUNT;
      do {
        kern_result = thread_get_state(thread, GC_MACH_THREAD_STATE,
                                       (natural_t *)&state,
                                       &thread_state_count);
      } while (kern_result == KERN_ABORTED);
    }
#ifdef DEBUG_THREADS
      GC_log_printf("thread_get_state returns %d\n", kern_result);
#endif
    if (kern_result != KERN_SUCCESS)
      ABORT("thread_get_state failed");
#if defined(I386)
      lo = (ptr_t)state.THREAD_FLD(esp);
#ifndef DARWIN_DONT_PARSE_STACK
        *phi = GC_FindTopOfStack(state.THREAD_FLD(esp));
#endif
      GC_push_one(state.THREAD_FLD(eax));
      GC_push_one(state.THREAD_FLD(ebx));
      GC_push_one(state.THREAD_FLD(ecx));
      GC_push_one(state.THREAD_FLD(edx));
      GC_push_one(state.THREAD_FLD(edi));
      GC_push_one(state.THREAD_FLD(esi));
      GC_push_one(state.THREAD_FLD(ebp));
#elif defined(X86_64)
      lo = (ptr_t)state.THREAD_FLD(rsp);
#ifndef DARWIN_DONT_PARSE_STACK
        *phi = GC_FindTopOfStack(state.THREAD_FLD(rsp));
#endif
      GC_push_one(state.THREAD_FLD(rax));
      GC_push_one(state.THREAD_FLD(rbx));
      GC_push_one(state.THREAD_FLD(rcx));
      GC_push_one(state.THREAD_FLD(rdx));
      GC_push_one(state.THREAD_FLD(rdi));
      GC_push_one(state.THREAD_FLD(rsi));
      GC_push_one(state.THREAD_FLD(rbp));
      GC_push_one(state.THREAD_FLD(r8));
      GC_push_one(state.THREAD_FLD(r9));
      GC_push_one(state.THREAD_FLD(r10));
      GC_push_one(state.THREAD_FLD(r11));
      GC_push_one(state.THREAD_FLD(r12));
      GC_push_one(state.THREAD_FLD(r13));
      GC_push_one(state.THREAD_FLD(r14));
      GC_push_one(state.THREAD_FLD(r15));
#elif defined(POWERPC)
      lo = (ptr_t)(state.THREAD_FLD(r1) - PPC_RED_ZONE_SIZE);
#ifndef DARWIN_DONT_PARSE_STACK
        *phi = GC_FindTopOfStack(state.THREAD_FLD(r1));
#endif
      GC_push_one(state.THREAD_FLD(r0));
      GC_push_one(state.THREAD_FLD(r2));
      GC_push_one(state.THREAD_FLD(r3));
      GC_push_one(state.THREAD_FLD(r4));
      GC_push_one(state.THREAD_FLD(r5));
      GC_push_one(state.THREAD_FLD(r6));
      GC_push_one(state.THREAD_FLD(r7));
      GC_push_one(state.THREAD_FLD(r8));
      GC_push_one(state.THREAD_FLD(r9));
      GC_push_one(state.THREAD_FLD(r10));
      GC_push_one(state.THREAD_FLD(r11));
      GC_push_one(state.THREAD_FLD(r12));
      GC_push_one(state.THREAD_FLD(r13));
      GC_push_one(state.THREAD_FLD(r14));
      GC_push_one(state.THREAD_FLD(r15));
      GC_push_one(state.THREAD_FLD(r16));
      GC_push_one(state.THREAD_FLD(r17));
      GC_push_one(state.THREAD_FLD(r18));
      GC_push_one(state.THREAD_FLD(r19));
      GC_push_one(state.THREAD_FLD(r20));
      GC_push_one(state.THREAD_FLD(r21));
      GC_push_one(state.THREAD_FLD(r22));
      GC_push_one(state.THREAD_FLD(r23));
      GC_push_one(state.THREAD_FLD(r24));
      GC_push_one(state.THREAD_FLD(r25));
      GC_push_one(state.THREAD_FLD(r26));
      GC_push_one(state.THREAD_FLD(r27));
      GC_push_one(state.THREAD_FLD(r28));
      GC_push_one(state.THREAD_FLD(r29));
      GC_push_one(state.THREAD_FLD(r30));
      GC_push_one(state.THREAD_FLD(r31));
#elif defined(ARM32)
      lo = (ptr_t)state.THREAD_FLD(sp);
#ifndef DARWIN_DONT_PARSE_STACK
        *phi = GC_FindTopOfStack(state.THREAD_FLD(r[7]));
#endif
      {
        int j;
        for (j = 0; j < 7; j++)
          GC_push_one(state.THREAD_FLD(r[j]));
        j++;
        for (; j <= 12; j++)
          GC_push_one(state.THREAD_FLD(r[j]));
      }
      GC_push_one(state.THREAD_FLD(lr));
#elif defined(AARCH64)
      lo = (ptr_t)state.THREAD_FLD(sp);
#ifndef DARWIN_DONT_PARSE_STACK
        *phi = GC_FindTopOfStack(state.THREAD_FLD(fp));
#endif
      {
        int j;
        for (j = 0; j <= 28; j++) {
          GC_push_one(state.THREAD_FLD(x[j]));
        }
      }
      GC_push_one(state.THREAD_FLD(lr));
#elif defined(CPPCHECK)
      lo = NULL;
#else
#error FIXME for non-arm/ppc/x86 architectures
#endif
  }
#ifdef DARWIN_DONT_PARSE_STACK
    *phi = (p->flags & MAIN_THREAD) != 0 ? GC_stackbottom : p->stack_end;
#endif
#ifdef DARWIN_DONT_PARSE_STACK
  if (p->altstack != NULL && (word)p->altstack <= (word)lo
      && (word)lo <= (word)p->altstack + p->altstack_size) {
    *paltstack_lo = lo;
    *paltstack_hi = p->altstack + p->altstack_size;
    lo = p->stack;
    *phi = p->stack + p->stack_size;
  } else
#endif
   {
    *paltstack_lo = NULL;
  }
#ifdef DEBUG_THREADS
    GC_log_printf("Darwin: Stack for thread %p is [%p,%p)\n",
                  (void *)(word)thread, (void *)lo, (void *)(*phi));
#endif
  return lo;
}
GC_INNER void GC_push_all_stacks(void)
{
  ptr_t hi, altstack_lo, altstack_hi;
  task_t my_task = current_task();
  mach_port_t my_thread = mach_thread_self();
  GC_bool found_me = FALSE;
  int nthreads = 0;
  word total_size = 0;
  mach_msg_type_number_t listcount = (mach_msg_type_number_t)THREAD_TABLE_SZ;
  if (!EXPECT(GC_thr_initialized, TRUE))
    GC_thr_init();
#ifndef DARWIN_DONT_PARSE_STACK
    if (GC_query_task_threads) {
      int i;
      kern_return_t kern_result;
      thread_act_array_t act_list = 0;
      kern_result = task_threads(my_task, &act_list, &listcount);
      if (kern_result != KERN_SUCCESS)
        ABORT("task_threads failed");
      for (i = 0; i < (int)listcount; i++) {
        thread_act_t thread = act_list[i];
        ptr_t lo = GC_stack_range_for(&hi, thread, NULL, FALSE, my_thread,
                                      &altstack_lo, &altstack_hi);
        if (lo) {
          GC_ASSERT((word)lo <= (word)hi);
          total_size += hi - lo;
          GC_push_all_stack(lo, hi);
        }
        nthreads++;
        if (thread == my_thread)
          found_me = TRUE;
        mach_port_deallocate(my_task, thread);
      }
      vm_deallocate(my_task, (vm_address_t)act_list,
                    sizeof(thread_t) * listcount);
    } else
#endif
   {
    int i;
    for (i = 0; i < (int)listcount; i++) {
      GC_thread p;
      for (p = GC_threads[i]; p != NULL; p = p->next)
        if ((p->flags & FINISHED) == 0) {
          thread_act_t thread = (thread_act_t)p->stop_info.mach_thread;
          ptr_t lo = GC_stack_range_for(&hi, thread, p,
                                        (GC_bool)p->thread_blocked,
                                        my_thread, &altstack_lo,
                                        &altstack_hi);
          if (lo) {
            GC_ASSERT((word)lo <= (word)hi);
            total_size += hi - lo;
            GC_push_all_stack_sections(lo, hi, p->traced_stack_sect);
          }
          if (altstack_lo) {
            total_size += altstack_hi - altstack_lo;
            GC_push_all_stack(altstack_lo, altstack_hi);
          }
          nthreads++;
          if (thread == my_thread)
            found_me = TRUE;
        }
    }
  }
  mach_port_deallocate(my_task, my_thread);
  GC_VERBOSE_LOG_PRINTF("Pushed %d thread stacks\n", nthreads);
  if (!found_me && !GC_in_thread_creation)
    ABORT("Collecting from unknown thread");
  GC_total_stacksize = total_size;
}
#ifndef GC_NO_THREADS_DISCOVERY
#ifdef MPROTECT_VDB
    STATIC mach_port_t GC_mach_handler_thread = 0;
    STATIC GC_bool GC_use_mach_handler_thread = FALSE;
    GC_INNER void GC_darwin_register_mach_handler_thread(mach_port_t thread)
    {
      GC_mach_handler_thread = thread;
      GC_use_mach_handler_thread = TRUE;
    }
#endif
#ifndef GC_MAX_MACH_THREADS
#define GC_MAX_MACH_THREADS THREAD_TABLE_SZ
#endif
  struct GC_mach_thread {
    thread_act_t thread;
    GC_bool suspended;
  };
  struct GC_mach_thread GC_mach_threads[GC_MAX_MACH_THREADS];
  STATIC int GC_mach_threads_count = 0;
STATIC GC_bool GC_suspend_thread_list(thread_act_array_t act_list, int count,
                                      thread_act_array_t old_list,
                                      int old_count, task_t my_task,
                                      mach_port_t my_thread)
{
  int i;
  int j = -1;
  GC_bool changed = FALSE;
  for (i = 0; i < count; i++) {
    thread_act_t thread = act_list[i];
    GC_bool found;
    kern_return_t kern_result;
    if (thread == my_thread
#ifdef MPROTECT_VDB
          || (GC_mach_handler_thread == thread && GC_use_mach_handler_thread)
#endif
#ifdef PARALLEL_MARK
          || GC_is_mach_marker(thread)
#endif
        ) {
      mach_port_deallocate(my_task, thread);
      continue;
    }
    found = FALSE;
    {
      int last_found = j;
      while (++j < old_count)
        if (old_list[j] == thread) {
          found = TRUE;
          break;
        }
      if (!found) {
        for (j = 0; j < last_found; j++)
          if (old_list[j] == thread) {
            found = TRUE;
            break;
          }
      }
    }
    if (found) {
      mach_port_deallocate(my_task, thread);
      continue;
    }
    if (GC_mach_threads_count == GC_MAX_MACH_THREADS)
      ABORT("Too many threads");
    GC_mach_threads[GC_mach_threads_count].thread = thread;
    GC_mach_threads[GC_mach_threads_count].suspended = FALSE;
    changed = TRUE;
#ifdef DEBUG_THREADS
      GC_log_printf("Suspending %p\n", (void *)(word)thread);
#endif
    GC_acquire_dirty_lock();
    do {
      kern_result = thread_suspend(thread);
    } while (kern_result == KERN_ABORTED);
    GC_release_dirty_lock();
    if (kern_result != KERN_SUCCESS) {
      GC_mach_threads[GC_mach_threads_count].suspended = FALSE;
    } else {
      GC_mach_threads[GC_mach_threads_count].suspended = TRUE;
      if (GC_on_thread_event)
        GC_on_thread_event(GC_EVENT_THREAD_SUSPENDED, (void *)(word)thread);
    }
    GC_mach_threads_count++;
  }
  return changed;
}
#endif
GC_INNER void GC_stop_world(void)
{
  task_t my_task = current_task();
  mach_port_t my_thread = mach_thread_self();
  kern_return_t kern_result;
#ifdef DEBUG_THREADS
    GC_log_printf("Stopping the world from thread %p\n",
                  (void *)(word)my_thread);
#endif
#ifdef PARALLEL_MARK
    if (GC_parallel) {
      GC_acquire_mark_lock();
      GC_ASSERT(GC_fl_builder_count == 0);
    }
#endif
  if (GC_query_task_threads) {
#ifndef GC_NO_THREADS_DISCOVERY
      GC_bool changed;
      thread_act_array_t act_list, prev_list;
      mach_msg_type_number_t listcount, prevcount;
      GC_mach_threads_count = 0;
      changed = TRUE;
      prev_list = NULL;
      prevcount = 0;
      do {
        kern_result = task_threads(my_task, &act_list, &listcount);
        if (kern_result == KERN_SUCCESS) {
          changed = GC_suspend_thread_list(act_list, listcount, prev_list,
                                           prevcount, my_task, my_thread);
          if (prev_list != NULL) {
            vm_deallocate(my_task, (vm_address_t)prev_list,
                          sizeof(thread_t) * prevcount);
          }
          prev_list = act_list;
          prevcount = listcount;
        }
      } while (changed);
      GC_ASSERT(prev_list != 0);
      vm_deallocate(my_task, (vm_address_t)act_list,
                    sizeof(thread_t) * listcount);
#endif
  } else {
    unsigned i;
    for (i = 0; i < THREAD_TABLE_SZ; i++) {
      GC_thread p;
      for (p = GC_threads[i]; p != NULL; p = p->next) {
        if ((p->flags & FINISHED) == 0 && !p->thread_blocked &&
             p->stop_info.mach_thread != my_thread) {
          GC_acquire_dirty_lock();
          do {
            kern_result = thread_suspend(p->stop_info.mach_thread);
          } while (kern_result == KERN_ABORTED);
          GC_release_dirty_lock();
          if (kern_result != KERN_SUCCESS)
            ABORT("thread_suspend failed");
          if (GC_on_thread_event)
            GC_on_thread_event(GC_EVENT_THREAD_SUSPENDED,
                               (void *)(word)p->stop_info.mach_thread);
        }
      }
    }
  }
#ifdef MPROTECT_VDB
    if (GC_auto_incremental) {
      GC_mprotect_stop();
    }
#endif
#ifdef PARALLEL_MARK
    if (GC_parallel)
      GC_release_mark_lock();
#endif
#ifdef DEBUG_THREADS
    GC_log_printf("World stopped from %p\n", (void *)(word)my_thread);
#endif
  mach_port_deallocate(my_task, my_thread);
}
GC_INLINE void GC_thread_resume(thread_act_t thread)
{
  kern_return_t kern_result;
#if defined(DEBUG_THREADS) || defined(GC_ASSERTIONS)
    struct thread_basic_info info;
    mach_msg_type_number_t outCount = THREAD_BASIC_INFO_COUNT;
#if defined(CPPCHECK) && defined(DEBUG_THREADS)
      info.run_state = 0;
#endif
    kern_result = thread_info(thread, THREAD_BASIC_INFO,
                              (thread_info_t)&info, &outCount);
    if (kern_result != KERN_SUCCESS)
      ABORT("thread_info failed");
#endif
#ifdef DEBUG_THREADS
    GC_log_printf("Resuming thread %p with state %d\n", (void *)(word)thread,
                  info.run_state);
#endif
  kern_result = thread_resume(thread);
  if (kern_result != KERN_SUCCESS) {
    WARN("thread_resume(%p) failed: mach port invalid\n", thread);
  } else if (GC_on_thread_event) {
    GC_on_thread_event(GC_EVENT_THREAD_UNSUSPENDED, (void *)(word)thread);
  }
}
GC_INNER void GC_start_world(void)
{
  task_t my_task = current_task();
#ifdef DEBUG_THREADS
    GC_log_printf("World starting\n");
#endif
#ifdef MPROTECT_VDB
    if (GC_auto_incremental) {
      GC_mprotect_resume();
    }
#endif
  if (GC_query_task_threads) {
#ifndef GC_NO_THREADS_DISCOVERY
      int i, j;
      kern_return_t kern_result;
      thread_act_array_t act_list;
      mach_msg_type_number_t listcount;
      kern_result = task_threads(my_task, &act_list, &listcount);
      if (kern_result != KERN_SUCCESS)
        ABORT("task_threads failed");
      j = (int)listcount;
      for (i = 0; i < GC_mach_threads_count; i++) {
        thread_act_t thread = GC_mach_threads[i].thread;
        if (GC_mach_threads[i].suspended) {
          int last_found = j;
          while (++j < (int)listcount) {
            if (act_list[j] == thread)
              break;
          }
          if (j >= (int)listcount) {
            for (j = 0; j < last_found; j++) {
              if (act_list[j] == thread)
                break;
            }
          }
          if (j != last_found) {
            GC_thread_resume(thread);
          }
        } else {
#ifdef DEBUG_THREADS
            GC_log_printf("Not resuming thread %p as it is not suspended\n",
                          (void *)(word)thread);
#endif
        }
        mach_port_deallocate(my_task, thread);
      }
      for (i = 0; i < (int)listcount; i++)
        mach_port_deallocate(my_task, act_list[i]);
      vm_deallocate(my_task, (vm_address_t)act_list,
                    sizeof(thread_t) * listcount);
#endif
  } else {
    int i;
    mach_port_t my_thread = mach_thread_self();
    for (i = 0; i < THREAD_TABLE_SZ; i++) {
      GC_thread p;
      for (p = GC_threads[i]; p != NULL; p = p->next) {
        if ((p->flags & FINISHED) == 0 && !p->thread_blocked &&
             p->stop_info.mach_thread != my_thread)
          GC_thread_resume(p->stop_info.mach_thread);
      }
    }
    mach_port_deallocate(my_task, my_thread);
  }
#ifdef DEBUG_THREADS
    GC_log_printf("World started\n");
#endif
}
#endif
#if !defined(MACOS) && !defined(GC_NO_TYPES) && !defined(SN_TARGET_PSP2) \
    && !defined(_WIN32_WCE) && !defined(__CC_ARM)
#include <sys/types.h>
#endif
#undef GC_MUST_RESTORE_REDEFINED_DLOPEN
#if defined(GC_PTHREADS) && !defined(GC_NO_DLOPEN) \
    && !defined(GC_NO_THREAD_REDIRECTS) && !defined(GC_USE_LD_WRAP)
#undef dlopen
#define GC_MUST_RESTORE_REDEFINED_DLOPEN
#endif
STATIC GC_has_static_roots_func GC_has_static_roots = 0;
#if (defined(DYNAMIC_LOADING) || defined(MSWIN32) || defined(MSWINCE) \
    || defined(CYGWIN32)) && !defined(PCR)
#if !defined(DARWIN) && !defined(SCO_ELF) && !defined(SOLARISDL) \
    && !defined(AIX) && !defined(DGUX) && !defined(IRIX5) && !defined(HPUX) \
    && !defined(CYGWIN32) && !defined(MSWIN32) && !defined(MSWINCE) \
    && !(defined(ALPHA) && defined(OSF1)) \
    && !(defined(FREEBSD) && defined(__ELF__)) \
    && !(defined(LINUX) && defined(__ELF__)) \
    && !(defined(NETBSD) && defined(__ELF__)) \
    && !(defined(OPENBSD) && (defined(__ELF__) || defined(M68K))) \
    && !defined(HAIKU) && !defined(HURD) && !defined(NACL) \
    && !defined(CPPCHECK)
#error We only know how to find data segments of dynamic libraries for above.
#error Additional SVR4 variants might not be too hard to add.
#endif
#include <stdio.h>
#ifdef SOLARISDL
#include <sys/elf.h>
#include <dlfcn.h>
#include <link.h>
#endif
#if defined(NETBSD)
#include <sys/param.h>
#include <dlfcn.h>
#include <machine/elf_machdep.h>
#define ELFSIZE ARCH_ELFSIZE
#endif
#if defined(OPENBSD)
#include <sys/param.h>
#if (OpenBSD >= 200519) && !defined(HAVE_DL_ITERATE_PHDR)
#define HAVE_DL_ITERATE_PHDR
#endif
#endif
#if defined(SCO_ELF) || defined(DGUX) || defined(HURD) || defined(NACL) \
    || (defined(__ELF__) && (defined(LINUX) || defined(FREEBSD) \
                             || defined(NETBSD) || defined(OPENBSD)))
#include <stddef.h>
#if !defined(OPENBSD) && !defined(HOST_ANDROID)
#include <elf.h>
#endif
#ifdef HOST_ANDROID
#ifdef BIONIC_ELFDATA_REDEF_BUG
#include <asm/elf.h>
#include <linux/elf-em.h>
#undef ELF_DATA
#undef EM_ALPHA
#endif
#include <link.h>
#if !defined(GC_DONT_DEFINE_LINK_MAP) && !(__ANDROID_API__ >= 21)
      struct link_map {
        uintptr_t l_addr;
        char* l_name;
        uintptr_t l_ld;
        struct link_map* l_next;
        struct link_map* l_prev;
      };
      struct r_debug {
        int32_t r_version;
        struct link_map* r_map;
        void (*r_brk)(void);
        int32_t r_state;
        uintptr_t r_ldbase;
      };
#endif
#else
    EXTERN_C_BEGIN
#include <link.h>
    EXTERN_C_END
#endif
#endif
#ifndef ElfW
#if defined(FREEBSD)
#if __ELF_WORD_SIZE == 32
#define ElfW(type) Elf32_##type
#else
#define ElfW(type) Elf64_##type
#endif
#elif defined(NETBSD) || defined(OPENBSD)
#if ELFSIZE == 32
#define ElfW(type) Elf32_##type
#elif ELFSIZE == 64
#define ElfW(type) Elf64_##type
#else
#error Missing ELFSIZE define
#endif
#else
#if !defined(ELF_CLASS) || ELF_CLASS == ELFCLASS32
#define ElfW(type) Elf32_##type
#else
#define ElfW(type) Elf64_##type
#endif
#endif
#endif
#if defined(SOLARISDL) && !defined(USE_PROC_FOR_LIBRARIES)
  EXTERN_C_BEGIN
  extern ElfW(Dyn) _DYNAMIC;
  EXTERN_C_END
  STATIC struct link_map *
  GC_FirstDLOpenedLinkMap(void)
  {
    ElfW(Dyn) *dp;
    static struct link_map * cachedResult = 0;
    static ElfW(Dyn) *dynStructureAddr = 0;
#ifdef SUNOS53_SHARED_LIB
        if( dynStructureAddr == 0 ) {
          void* startupSyms = dlopen(0, RTLD_LAZY);
          dynStructureAddr = (ElfW(Dyn)*)(word)dlsym(startupSyms, "_DYNAMIC");
        }
#else
        dynStructureAddr = &_DYNAMIC;
#endif
    if (0 == COVERT_DATAFLOW(dynStructureAddr)) {
        return(0);
    }
    if (cachedResult == 0) {
        int tag;
        for( dp = ((ElfW(Dyn) *)(&_DYNAMIC)); (tag = dp->d_tag) != 0; dp++ ) {
            if (tag == DT_DEBUG) {
                struct r_debug *rd = (struct r_debug *)dp->d_un.d_ptr;
                if (rd != NULL) {
                    struct link_map *lm = rd->r_map;
                    if (lm != NULL)
                        cachedResult = lm->l_next;
                }
                break;
            }
        }
    }
    return cachedResult;
  }
#endif
#ifdef GC_MUST_RESTORE_REDEFINED_DLOPEN
#define dlopen GC_dlopen
#endif
#if defined(SOLARISDL)
#if !defined(PCR) && !defined(GC_SOLARIS_THREADS) && defined(THREADS) \
     && !defined(CPPCHECK)
#error Fix mutual exclusion with dlopen
#endif
#ifndef USE_PROC_FOR_LIBRARIES
GC_INNER void GC_register_dynamic_libraries(void)
{
  struct link_map *lm;
  for (lm = GC_FirstDLOpenedLinkMap(); lm != 0; lm = lm->l_next) {
        ElfW(Ehdr) * e;
        ElfW(Phdr) * p;
        unsigned long offset;
        char * start;
        int i;
        e = (ElfW(Ehdr) *) lm->l_addr;
        p = ((ElfW(Phdr) *)(((char *)(e)) + e->e_phoff));
        offset = ((unsigned long)(lm->l_addr));
        for( i = 0; i < (int)e->e_phnum; i++, p++ ) {
          switch( p->p_type ) {
            case PT_LOAD:
              {
                if( !(p->p_flags & PF_W) ) break;
                start = ((char *)(p->p_vaddr)) + offset;
                GC_add_roots_inner(start, start + p->p_memsz, TRUE);
              }
              break;
            default:
              break;
          }
        }
    }
}
#endif
#endif
#if defined(SCO_ELF) || defined(DGUX) || defined(HURD) || defined(NACL) \
    || (defined(__ELF__) && (defined(LINUX) || defined(FREEBSD) \
                             || defined(NETBSD) || defined(OPENBSD)))
#ifdef USE_PROC_FOR_LIBRARIES
#include <string.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#define MAPS_BUF_SIZE (32*1024)
static void sort_heap_sects(struct HeapSect *base, size_t number_of_elements)
{
    signed_word n = (signed_word)number_of_elements;
    signed_word nsorted = 1;
    while (nsorted < n) {
      signed_word i;
      while (nsorted < n &&
             (word)base[nsorted-1].hs_start < (word)base[nsorted].hs_start)
          ++nsorted;
      if (nsorted == n) break;
      GC_ASSERT((word)base[nsorted-1].hs_start > (word)base[nsorted].hs_start);
      i = nsorted - 1;
      while (i >= 0 && (word)base[i].hs_start > (word)base[i+1].hs_start) {
        struct HeapSect tmp = base[i];
        base[i] = base[i+1];
        base[i+1] = tmp;
        --i;
      }
      GC_ASSERT((word)base[nsorted-1].hs_start < (word)base[nsorted].hs_start);
      ++nsorted;
    }
}
STATIC void GC_register_map_entries(const char *maps)
{
    const char *prot;
    ptr_t start, end;
    unsigned int maj_dev;
    ptr_t least_ha, greatest_ha;
    unsigned i;
    GC_ASSERT(I_HOLD_LOCK());
    sort_heap_sects(GC_our_memory, GC_n_memory);
    least_ha = GC_our_memory[0].hs_start;
    greatest_ha = GC_our_memory[GC_n_memory-1].hs_start
                  + GC_our_memory[GC_n_memory-1].hs_bytes;
    for (;;) {
        maps = GC_parse_map_entry(maps, &start, &end, &prot, &maj_dev, 0);
        if (NULL == maps) break;
        if (prot[1] == 'w') {
            if ((word)start <= (word)GC_stackbottom
                && (word)end >= (word)GC_stackbottom) {
                continue;
            }
#ifdef THREADS
              if (GC_segment_is_thread_stack(start, end)) continue;
#endif
            if ((word)end <= (word)least_ha
                || (word)start >= (word)greatest_ha) {
              GC_add_roots_inner(start, end, TRUE);
              continue;
            }
              i = 0;
              while ((word)(GC_our_memory[i].hs_start
                                + GC_our_memory[i].hs_bytes) < (word)start)
                  ++i;
              GC_ASSERT(i < GC_n_memory);
              if ((word)GC_our_memory[i].hs_start <= (word)start) {
                  start = GC_our_memory[i].hs_start
                          + GC_our_memory[i].hs_bytes;
                  ++i;
              }
              while (i < GC_n_memory
                     && (word)GC_our_memory[i].hs_start < (word)end
                     && (word)start < (word)end) {
                  if ((word)start < (word)GC_our_memory[i].hs_start)
                    GC_add_roots_inner(start,
                                       GC_our_memory[i].hs_start, TRUE);
                  start = GC_our_memory[i].hs_start
                          + GC_our_memory[i].hs_bytes;
                  ++i;
              }
              if ((word)start < (word)end)
                  GC_add_roots_inner(start, end, TRUE);
        } else if (prot[0] == '-' && prot[1] == '-' && prot[2] == '-') {
            GC_remove_roots_subregion(start, end);
        }
    }
}
GC_INNER void GC_register_dynamic_libraries(void)
{
    GC_register_map_entries(GC_get_maps());
}
GC_INNER GC_bool GC_register_main_static_data(void)
{
    return FALSE;
}
#define HAVE_REGISTER_MAIN_STATIC_DATA
#else
#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 2) \
    || (__GLIBC__ == 2 && __GLIBC_MINOR__ == 2 && defined(DT_CONFIG)) \
    || defined(HOST_ANDROID)
#ifndef HAVE_DL_ITERATE_PHDR
#define HAVE_DL_ITERATE_PHDR
#endif
#ifdef HOST_ANDROID
    EXTERN_C_BEGIN
    extern int dl_iterate_phdr(int (*cb)(struct dl_phdr_info *,
                                         size_t, void *),
                               void *data);
    EXTERN_C_END
#endif
#endif
#if defined(__DragonFly__) || defined(__FreeBSD_kernel__) \
    || (defined(FREEBSD) && __FreeBSD__ >= 7)
#ifndef HAVE_DL_ITERATE_PHDR
#define HAVE_DL_ITERATE_PHDR
#endif
#define DL_ITERATE_PHDR_STRONG
#elif defined(HAVE_DL_ITERATE_PHDR)
  EXTERN_C_BEGIN
#pragma weak dl_iterate_phdr
  EXTERN_C_END
#endif
#if defined(HAVE_DL_ITERATE_PHDR)
#ifdef PT_GNU_RELRO
#define MAX_LOAD_SEGS MAX_ROOT_SETS
    static struct load_segment {
      ptr_t start;
      ptr_t end;
      ptr_t start2;
      ptr_t end2;
    } load_segs[MAX_LOAD_SEGS];
    static int n_load_segs;
    static GC_bool load_segs_overflow;
#endif
STATIC int GC_register_dynlib_callback(struct dl_phdr_info * info,
                                       size_t size, void * ptr)
{
  const ElfW(Phdr) * p;
  ptr_t start, end;
  int i;
  if (size < offsetof (struct dl_phdr_info, dlpi_phnum)
      + sizeof (info->dlpi_phnum))
    return -1;
  p = info->dlpi_phdr;
  for (i = 0; i < (int)info->dlpi_phnum; i++, p++) {
    if (p->p_type == PT_LOAD) {
      GC_has_static_roots_func callback = GC_has_static_roots;
      if ((p->p_flags & PF_W) == 0) continue;
      start = (ptr_t)p->p_vaddr + info->dlpi_addr;
      end = start + p->p_memsz;
      if (callback != 0 && !callback(info->dlpi_name, start, p->p_memsz))
        continue;
#ifdef PT_GNU_RELRO
#if CPP_WORDSZ == 64
          start = (ptr_t)((word)start & ~(word)(sizeof(word) - 1));
#endif
        if (n_load_segs >= MAX_LOAD_SEGS) {
          if (!load_segs_overflow) {
            WARN("Too many PT_LOAD segments;"
                 " registering as roots directly...\n", 0);
            load_segs_overflow = TRUE;
          }
          GC_add_roots_inner(start, end, TRUE);
        } else {
          load_segs[n_load_segs].start = start;
          load_segs[n_load_segs].end = end;
          load_segs[n_load_segs].start2 = 0;
          load_segs[n_load_segs].end2 = 0;
          ++n_load_segs;
        }
#else
        GC_add_roots_inner(start, end, TRUE);
#endif
    }
  }
#ifdef PT_GNU_RELRO
    p = info->dlpi_phdr;
    for (i = 0; i < (int)info->dlpi_phnum; i++, p++) {
      if (p->p_type == PT_GNU_RELRO) {
        int j;
        start = (ptr_t)p->p_vaddr + info->dlpi_addr;
        end = start + p->p_memsz;
        for (j = n_load_segs; --j >= 0; ) {
          if ((word)start >= (word)load_segs[j].start
              && (word)start < (word)load_segs[j].end) {
            if (load_segs[j].start2 != 0) {
              WARN("More than one GNU_RELRO segment per load one\n",0);
            } else {
              GC_ASSERT((word)end <=
                            (((word)load_segs[j].end + GC_page_size - 1) &
                             ~(GC_page_size - 1)));
              load_segs[j].end2 = load_segs[j].end;
              load_segs[j].end = start;
              load_segs[j].start2 = end;
            }
            break;
          }
          if (0 == j && 0 == GC_has_static_roots)
            WARN("Failed to find PT_GNU_RELRO segment"
                 " inside PT_LOAD region\n", 0);
        }
      }
    }
#endif
  *(int *)ptr = 1;
  return 0;
}
GC_INNER GC_bool GC_register_main_static_data(void)
{
#ifdef DL_ITERATE_PHDR_STRONG
    return FALSE;
#else
    return 0 == COVERT_DATAFLOW(dl_iterate_phdr);
#endif
}
STATIC GC_bool GC_register_dynamic_libraries_dl_iterate_phdr(void)
{
  int did_something;
  if (GC_register_main_static_data())
    return FALSE;
#ifdef PT_GNU_RELRO
    {
      static GC_bool excluded_segs = FALSE;
      n_load_segs = 0;
      load_segs_overflow = FALSE;
      if (!EXPECT(excluded_segs, TRUE)) {
        GC_exclude_static_roots_inner((ptr_t)load_segs,
                                      (ptr_t)load_segs + sizeof(load_segs));
        excluded_segs = TRUE;
      }
    }
#endif
  did_something = 0;
  dl_iterate_phdr(GC_register_dynlib_callback, &did_something);
  if (did_something) {
#ifdef PT_GNU_RELRO
      int i;
      for (i = 0; i < n_load_segs; ++i) {
        if ((word)load_segs[i].end > (word)load_segs[i].start) {
          GC_add_roots_inner(load_segs[i].start, load_segs[i].end, TRUE);
        }
        if ((word)load_segs[i].end2 > (word)load_segs[i].start2) {
          GC_add_roots_inner(load_segs[i].start2, load_segs[i].end2, TRUE);
        }
      }
#endif
  } else {
      ptr_t datastart, dataend;
#ifdef DATASTART_IS_FUNC
        static ptr_t datastart_cached = (ptr_t)GC_WORD_MAX;
        if (datastart_cached == (ptr_t)GC_WORD_MAX) {
          datastart_cached = DATASTART;
        }
        datastart = datastart_cached;
#else
        datastart = DATASTART;
#endif
#ifdef DATAEND_IS_FUNC
        {
          static ptr_t dataend_cached = 0;
          if (dataend_cached == 0) {
            dataend_cached = DATAEND;
          }
          dataend = dataend_cached;
        }
#else
        dataend = DATAEND;
#endif
      if (NULL == *(char * volatile *)&datastart
          || (word)datastart > (word)dataend)
        ABORT_ARG2("Wrong DATASTART/END pair",
                   ": %p .. %p", (void *)datastart, (void *)dataend);
      GC_add_roots_inner(datastart, dataend, TRUE);
#ifdef GC_HAVE_DATAREGION2
        if ((word)DATASTART2 - 1U >= (word)DATAEND2) {
          ABORT_ARG2("Wrong DATASTART/END2 pair",
                     ": %p .. %p", (void *)DATASTART2, (void *)DATAEND2);
        }
        GC_add_roots_inner(DATASTART2, DATAEND2, TRUE);
#endif
  }
  return TRUE;
}
#define HAVE_REGISTER_MAIN_STATIC_DATA
#else
#if defined(NETBSD) || defined(OPENBSD)
#include <sys/exec_elf.h>
#ifndef DT_DEBUG
#define DT_DEBUG   21
#endif
#ifndef PT_LOAD
#define PT_LOAD    1
#endif
#ifndef PF_W
#define PF_W       2
#endif
#elif !defined(HOST_ANDROID)
#include <elf.h>
#endif
#ifndef HOST_ANDROID
#include <link.h>
#endif
#endif
EXTERN_C_BEGIN
#ifdef __GNUC__
#pragma weak _DYNAMIC
#endif
extern ElfW(Dyn) _DYNAMIC[];
EXTERN_C_END
STATIC struct link_map *
GC_FirstDLOpenedLinkMap(void)
{
    static struct link_map *cachedResult = 0;
    if (0 == COVERT_DATAFLOW(_DYNAMIC)) {
        return(0);
    }
    if( cachedResult == 0 ) {
#if defined(NETBSD) && defined(RTLD_DI_LINKMAP)
#if defined(CPPCHECK)
#define GC_RTLD_DI_LINKMAP 2
#else
#define GC_RTLD_DI_LINKMAP RTLD_DI_LINKMAP
#endif
        struct link_map *lm = NULL;
        if (!dlinfo(RTLD_SELF, GC_RTLD_DI_LINKMAP, &lm) && lm != NULL) {
            while (lm->l_prev != NULL) {
                lm = lm->l_prev;
            }
            cachedResult = lm->l_next;
        }
#else
        ElfW(Dyn) *dp;
        int tag;
        for( dp = _DYNAMIC; (tag = dp->d_tag) != 0; dp++ ) {
            if (tag == DT_DEBUG) {
                struct r_debug *rd = (struct r_debug *)dp->d_un.d_ptr;
                if (rd != NULL) {
                    struct link_map *lm = rd->r_map;
                    if (lm != NULL)
                        cachedResult = lm->l_next;
                }
                break;
            }
        }
#endif
    }
    return cachedResult;
}
GC_INNER void GC_register_dynamic_libraries(void)
{
  struct link_map *lm;
#ifdef HAVE_DL_ITERATE_PHDR
    if (GC_register_dynamic_libraries_dl_iterate_phdr()) {
        return;
    }
#endif
  for (lm = GC_FirstDLOpenedLinkMap(); lm != 0; lm = lm->l_next)
    {
        ElfW(Ehdr) * e;
        ElfW(Phdr) * p;
        unsigned long offset;
        char * start;
        int i;
        e = (ElfW(Ehdr) *) lm->l_addr;
#ifdef HOST_ANDROID
          if (e == NULL)
            continue;
#endif
        p = ((ElfW(Phdr) *)(((char *)(e)) + e->e_phoff));
        offset = ((unsigned long)(lm->l_addr));
        for( i = 0; i < (int)e->e_phnum; i++, p++ ) {
          switch( p->p_type ) {
            case PT_LOAD:
              {
                if( !(p->p_flags & PF_W) ) break;
                start = ((char *)(p->p_vaddr)) + offset;
                GC_add_roots_inner(start, start + p->p_memsz, TRUE);
              }
              break;
            default:
              break;
          }
        }
    }
}
#endif
#endif
#if defined(IRIX5) || (defined(USE_PROC_FOR_LIBRARIES) && !defined(LINUX))
#include <sys/procfs.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <elf.h>
#include <errno.h>
#include <signal.h>
#ifndef _sigargs
#define IRIX6
#endif
GC_INNER void GC_register_dynamic_libraries(void)
{
    static int fd = -1;
    char buf[30];
    static prmap_t * addr_map = 0;
    static int current_sz = 0;
    int needed_sz = 0;
    int i;
    long flags;
    ptr_t start;
    ptr_t limit;
    ptr_t heap_start = HEAP_START;
    ptr_t heap_end = heap_start;
#ifdef SOLARISDL
#define MA_PHYS 0
#endif
    if (fd < 0) {
      (void)snprintf(buf, sizeof(buf), "/proc/%ld", (long)getpid());
      buf[sizeof(buf) - 1] = '\0';
      fd = open(buf, O_RDONLY);
      if (fd < 0) {
        ABORT("/proc open failed");
      }
    }
    if (ioctl(fd, PIOCNMAP, &needed_sz) < 0) {
        ABORT_ARG2("/proc PIOCNMAP ioctl failed",
                   ": fd= %d, errno= %d", fd, errno);
    }
    if (needed_sz >= current_sz) {
        GC_scratch_recycle_no_gww(addr_map,
                                  (size_t)current_sz * sizeof(prmap_t));
        current_sz = needed_sz * 2 + 1;
        addr_map = (prmap_t *)GC_scratch_alloc(
                                (size_t)current_sz * sizeof(prmap_t));
        if (addr_map == NULL)
          ABORT("Insufficient memory for address map");
    }
    if (ioctl(fd, PIOCMAP, addr_map) < 0) {
        ABORT_ARG3("/proc PIOCMAP ioctl failed",
                   ": errcode= %d, needed_sz= %d, addr_map= %p",
                   errno, needed_sz, (void *)addr_map);
    }
    if (GC_n_heap_sects > 0) {
        heap_end = GC_heap_sects[GC_n_heap_sects-1].hs_start
                        + GC_heap_sects[GC_n_heap_sects-1].hs_bytes;
        if ((word)heap_end < (word)GC_scratch_last_end_ptr)
          heap_end = GC_scratch_last_end_ptr;
    }
    for (i = 0; i < needed_sz; i++) {
        flags = addr_map[i].pr_mflags;
        if ((flags & (MA_BREAK | MA_STACK | MA_PHYS
                      | MA_FETCHOP | MA_NOTCACHED)) != 0) goto irrelevant;
        if ((flags & (MA_READ | MA_WRITE)) != (MA_READ | MA_WRITE))
            goto irrelevant;
        start = (ptr_t)(addr_map[i].pr_vaddr);
        if (GC_roots_present(start)) goto irrelevant;
        if ((word)start < (word)heap_end && (word)start >= (word)heap_start)
                goto irrelevant;
        limit = start + addr_map[i].pr_size;
#ifndef IRIX6
          if (addr_map[i].pr_off == 0 && strncmp(start, ELFMAG, 4) == 0) {
            caddr_t arg;
            int obj;
#define MAP_IRR_SZ 10
            static ptr_t map_irr[MAP_IRR_SZ];
            static int n_irr = 0;
            struct stat buf;
            int j;
            for (j = 0; j < n_irr; j++) {
                if (map_irr[j] == start) goto irrelevant;
            }
            arg = (caddr_t)start;
            obj = ioctl(fd, PIOCOPENM, &arg);
            if (obj >= 0) {
                fstat(obj, &buf);
                close(obj);
                if ((buf.st_mode & 0111) != 0) {
                    if (n_irr < MAP_IRR_SZ) {
                        map_irr[n_irr++] = start;
                    }
                    goto irrelevant;
                }
            }
          }
#endif
        GC_add_roots_inner(start, limit, TRUE);
      irrelevant: ;
    }
        if (close(fd) < 0) ABORT("Couldn't close /proc file");
        fd = -1;
}
#endif
#if defined(MSWIN32) || defined(MSWINCE) || defined(CYGWIN32)
#include <stdlib.h>
  STATIC void GC_cond_add_roots(char *base, char * limit)
  {
#ifdef GC_WIN32_THREADS
      char * curr_base = base;
      char * next_stack_lo;
      char * next_stack_hi;
      if (base == limit) return;
      for(;;) {
          GC_get_next_stack(curr_base, limit, &next_stack_lo, &next_stack_hi);
          if ((word)next_stack_lo >= (word)limit) break;
          if ((word)next_stack_lo > (word)curr_base)
            GC_add_roots_inner(curr_base, next_stack_lo, TRUE);
          curr_base = next_stack_hi;
      }
      if ((word)curr_base < (word)limit)
        GC_add_roots_inner(curr_base, limit, TRUE);
#else
      char * stack_top
         = (char *)((word)GC_approx_sp() &
                    ~(word)(GC_sysinfo.dwAllocationGranularity - 1));
      if (base == limit) return;
      if ((word)limit > (word)stack_top
          && (word)base < (word)GC_stackbottom) {
          return;
      }
      GC_add_roots_inner(base, limit, TRUE);
#endif
  }
#ifdef DYNAMIC_LOADING
  GC_INNER GC_bool GC_register_main_static_data(void)
  {
#if defined(MSWINCE) || defined(CYGWIN32)
      return FALSE;
#else
      return GC_no_win32_dlls;
#endif
  }
#define HAVE_REGISTER_MAIN_STATIC_DATA
#endif
#ifdef DEBUG_VIRTUALQUERY
  void GC_dump_meminfo(MEMORY_BASIC_INFORMATION *buf)
  {
    GC_printf("BaseAddress= 0x%lx, AllocationBase= 0x%lx,"
              " RegionSize= 0x%lx(%lu)\n",
              buf -> BaseAddress, buf -> AllocationBase,
              buf -> RegionSize, buf -> RegionSize);
    GC_printf("\tAllocationProtect= 0x%lx, State= 0x%lx, Protect= 0x%lx, "
              "Type= 0x%lx\n", buf -> AllocationProtect, buf -> State,
              buf -> Protect, buf -> Type);
  }
#endif
#if defined(MSWINCE) || defined(CYGWIN32)
#define GC_wnt TRUE
#endif
  GC_INNER void GC_register_dynamic_libraries(void)
  {
    MEMORY_BASIC_INFORMATION buf;
    DWORD protect;
    LPVOID p;
    char * base;
    char * limit, * new_limit;
#ifdef MSWIN32
      if (GC_no_win32_dlls) return;
#endif
    p = GC_sysinfo.lpMinimumApplicationAddress;
    base = limit = (char *)p;
    while ((word)p < (word)GC_sysinfo.lpMaximumApplicationAddress) {
        size_t result = VirtualQuery(p, &buf, sizeof(buf));
#ifdef MSWINCE
          if (result == 0) {
            new_limit = (char *)
                (((DWORD) p + GC_sysinfo.dwAllocationGranularity)
                 & ~(GC_sysinfo.dwAllocationGranularity-1));
          } else
#endif
         {
            if (result != sizeof(buf)) {
                ABORT("Weird VirtualQuery result");
            }
            new_limit = (char *)p + buf.RegionSize;
            protect = buf.Protect;
            if (buf.State == MEM_COMMIT
                && (protect == PAGE_EXECUTE_READWRITE
                    || protect == PAGE_EXECUTE_WRITECOPY
                    || protect == PAGE_READWRITE
                    || protect == PAGE_WRITECOPY)
                && (buf.Type == MEM_IMAGE
#ifdef GC_REGISTER_MEM_PRIVATE
                      || (protect == PAGE_READWRITE && buf.Type == MEM_PRIVATE)
#else
                      || (!GC_wnt && buf.Type == MEM_PRIVATE)
#endif
                   )
                && !GC_is_heap_base(buf.AllocationBase)) {
#ifdef DEBUG_VIRTUALQUERY
                  GC_dump_meminfo(&buf);
#endif
                if ((char *)p != limit) {
                    GC_cond_add_roots(base, limit);
                    base = (char *)p;
                }
                limit = new_limit;
            }
        }
        if ((word)p > (word)new_limit ) break;
        p = (LPVOID)new_limit;
    }
    GC_cond_add_roots(base, limit);
  }
#endif
#if defined(ALPHA) && defined(OSF1)
#include <loader.h>
EXTERN_C_BEGIN
extern char *sys_errlist[];
extern int sys_nerr;
extern int errno;
EXTERN_C_END
GC_INNER void GC_register_dynamic_libraries(void)
{
  ldr_module_t moduleid = LDR_NULL_MODULE;
  ldr_process_t mypid = ldr_my_process();
    while (TRUE) {
      ldr_module_info_t moduleinfo;
      size_t modulereturnsize;
      ldr_region_t region;
      ldr_region_info_t regioninfo;
      size_t regionreturnsize;
      int status = ldr_next_module(mypid, &moduleid);
        if (moduleid == LDR_NULL_MODULE)
            break;
        if (status != 0) {
          ABORT_ARG3("ldr_next_module failed",
                     ": status= %d, errcode= %d (%s)", status, errno,
                     errno < sys_nerr ? sys_errlist[errno] : "");
        }
        status = ldr_inq_module(mypid, moduleid, &moduleinfo,
                                sizeof(moduleinfo), &modulereturnsize);
        if (status != 0 )
            ABORT("ldr_inq_module failed");
          if (moduleinfo.lmi_flags & LDR_MAIN)
              continue;
#ifdef DL_VERBOSE
        GC_log_printf("---Module---\n");
        GC_log_printf("Module ID: %ld\n", moduleinfo.lmi_modid);
        GC_log_printf("Count of regions: %d\n", moduleinfo.lmi_nregion);
        GC_log_printf("Flags for module: %016lx\n", moduleinfo.lmi_flags);
        GC_log_printf("Module pathname: \"%s\"\n", moduleinfo.lmi_name);
#endif
        for (region = 0; region < moduleinfo.lmi_nregion; region++) {
            status = ldr_inq_region(mypid, moduleid, region, &regioninfo,
                                    sizeof(regioninfo), &regionreturnsize);
            if (status != 0 )
                ABORT("ldr_inq_region failed");
            if (! (regioninfo.lri_prot & LDR_W))
                continue;
#ifdef DL_VERBOSE
            GC_log_printf("--- Region ---\n");
            GC_log_printf("Region number: %ld\n", regioninfo.lri_region_no);
            GC_log_printf("Protection flags: %016x\n", regioninfo.lri_prot);
            GC_log_printf("Virtual address: %p\n", regioninfo.lri_vaddr);
            GC_log_printf("Mapped address: %p\n", regioninfo.lri_mapaddr);
            GC_log_printf("Region size: %ld\n", regioninfo.lri_size);
            GC_log_printf("Region name: \"%s\"\n", regioninfo.lri_name);
#endif
          GC_add_roots_inner((char *)regioninfo.lri_mapaddr,
                        (char *)regioninfo.lri_mapaddr + regioninfo.lri_size,
                        TRUE);
        }
    }
}
#endif
#if defined(HPUX)
#include <errno.h>
#include <dl.h>
EXTERN_C_BEGIN
extern char *sys_errlist[];
extern int sys_nerr;
EXTERN_C_END
GC_INNER void GC_register_dynamic_libraries(void)
{
  int index = 1;
    while (TRUE) {
      struct shl_descriptor *shl_desc;
      int status = shl_get(index, &shl_desc);
        if (status != 0) {
#ifdef GC_HPUX_THREADS
           break;
#else
          if (errno == EINVAL) {
            break;
          } else {
            ABORT_ARG3("shl_get failed",
                       ": status= %d, errcode= %d (%s)", status, errno,
                       errno < sys_nerr ? sys_errlist[errno] : "");
          }
#endif
        }
#ifdef DL_VERBOSE
        GC_log_printf("---Shared library---\n");
        GC_log_printf("filename= \"%s\"\n", shl_desc->filename);
        GC_log_printf("index= %d\n", index);
        GC_log_printf("handle= %08x\n", (unsigned long) shl_desc->handle);
        GC_log_printf("text seg.start= %08x\n", shl_desc->tstart);
        GC_log_printf("text seg.end= %08x\n", shl_desc->tend);
        GC_log_printf("data seg.start= %08x\n", shl_desc->dstart);
        GC_log_printf("data seg.end= %08x\n", shl_desc->dend);
        GC_log_printf("ref.count= %lu\n", shl_desc->ref_count);
#endif
        GC_add_roots_inner((char *) shl_desc->dstart,
                           (char *) shl_desc->dend, TRUE);
        index++;
    }
}
#endif
#ifdef AIX
#include <alloca.h>
#include <sys/ldr.h>
#include <sys/errno.h>
  GC_INNER void GC_register_dynamic_libraries(void)
  {
      int ldibuflen = 8192;
      for (;;) {
        int len;
        struct ld_info *ldi;
#if defined(CPPCHECK)
          char ldibuf[ldibuflen];
#else
          char *ldibuf = alloca(ldibuflen);
#endif
        len = loadquery(L_GETINFO, ldibuf, ldibuflen);
        if (len < 0) {
                if (errno != ENOMEM) {
                        ABORT("loadquery failed");
                }
                ldibuflen *= 2;
                continue;
        }
        ldi = (struct ld_info *)ldibuf;
        while (ldi) {
                len = ldi->ldinfo_next;
                GC_add_roots_inner(
                                ldi->ldinfo_dataorg,
                                (ptr_t)(unsigned long)ldi->ldinfo_dataorg
                                + ldi->ldinfo_datasize,
                                TRUE);
                ldi = len ? (struct ld_info *)((char *)ldi + len) : 0;
        }
        break;
      }
  }
#endif
#ifdef DARWIN
#ifndef __private_extern__
#define __private_extern__ extern
#include <mach-o/dyld.h>
#undef __private_extern__
#else
#include <mach-o/dyld.h>
#endif
#include <mach-o/getsect.h>
STATIC const struct dyld_sections_s {
    const char *seg;
    const char *sect;
} GC_dyld_sections[] = {
    { SEG_DATA, SECT_DATA },
    { SEG_DATA, "__static_data" },
    { SEG_DATA, SECT_BSS },
    { SEG_DATA, SECT_COMMON },
    { SEG_DATA, "__zobj_data" },
    { SEG_DATA, "__zobj_bss" }
};
STATIC const char * const GC_dyld_add_sect_fmts[] = {
  "__bss%u",
  "__pu_bss%u",
  "__zo_bss%u",
  "__zo_pu_bss%u"
};
#ifndef L2_MAX_OFILE_ALIGNMENT
#define L2_MAX_OFILE_ALIGNMENT 15
#endif
STATIC const char *GC_dyld_name_for_hdr(const struct GC_MACH_HEADER *hdr)
{
    unsigned long i, c;
    c = _dyld_image_count();
    for (i = 0; i < c; i++)
      if ((const struct GC_MACH_HEADER *)_dyld_get_image_header(i) == hdr)
        return _dyld_get_image_name(i);
    return NULL;
}
STATIC void GC_dyld_image_add(const struct GC_MACH_HEADER *hdr,
                              intptr_t slide)
{
  unsigned long start, end;
  unsigned i, j;
  const struct GC_MACH_SECTION *sec;
  const char *name;
  GC_has_static_roots_func callback = GC_has_static_roots;
  DCL_LOCK_STATE;
  if (GC_no_dls) return;
#ifdef DARWIN_DEBUG
    name = GC_dyld_name_for_hdr(hdr);
#else
    name = callback != 0 ? GC_dyld_name_for_hdr(hdr) : NULL;
#endif
  for (i = 0; i < sizeof(GC_dyld_sections)/sizeof(GC_dyld_sections[0]); i++) {
    sec = GC_GETSECTBYNAME(hdr, GC_dyld_sections[i].seg,
                           GC_dyld_sections[i].sect);
    if (sec == NULL || sec->size < sizeof(word))
      continue;
    start = slide + sec->addr;
    end = start + sec->size;
    LOCK();
    if (callback == 0 || callback(name, (void*)start, (size_t)sec->size)) {
#ifdef DARWIN_DEBUG
        GC_log_printf(
              "Adding section __DATA,%s at %p-%p (%lu bytes) from image %s\n",
               GC_dyld_sections[i].sect, (void*)start, (void*)end,
               (unsigned long)sec->size, name);
#endif
      GC_add_roots_inner((ptr_t)start, (ptr_t)end, FALSE);
    }
    UNLOCK();
  }
  for (j = 0; j < sizeof(GC_dyld_add_sect_fmts) / sizeof(char *); j++) {
    const char *fmt = GC_dyld_add_sect_fmts[j];
    for (i = 0; i <= L2_MAX_OFILE_ALIGNMENT; i++) {
      char secnam[16];
      (void)snprintf(secnam, sizeof(secnam), fmt, (unsigned)i);
      secnam[sizeof(secnam) - 1] = '\0';
      sec = GC_GETSECTBYNAME(hdr, SEG_DATA, secnam);
      if (sec == NULL || sec->size == 0)
        continue;
      start = slide + sec->addr;
      end = start + sec->size;
#ifdef DARWIN_DEBUG
        GC_log_printf("Adding on-demand section __DATA,%s at"
                      " %p-%p (%lu bytes) from image %s\n",
                      secnam, (void*)start, (void*)end,
                      (unsigned long)sec->size, name);
#endif
      GC_add_roots((char*)start, (char*)end);
    }
  }
#if defined(DARWIN_DEBUG) && !defined(NO_DEBUGGING)
    LOCK();
    GC_print_static_roots();
    UNLOCK();
#endif
}
STATIC void GC_dyld_image_remove(const struct GC_MACH_HEADER *hdr,
                                 intptr_t slide)
{
  unsigned long start, end;
  unsigned i, j;
  const struct GC_MACH_SECTION *sec;
#if defined(DARWIN_DEBUG) && !defined(NO_DEBUGGING)
    DCL_LOCK_STATE;
#endif
  for (i = 0; i < sizeof(GC_dyld_sections)/sizeof(GC_dyld_sections[0]); i++) {
    sec = GC_GETSECTBYNAME(hdr, GC_dyld_sections[i].seg,
                           GC_dyld_sections[i].sect);
    if (sec == NULL || sec->size == 0)
      continue;
    start = slide + sec->addr;
    end = start + sec->size;
#ifdef DARWIN_DEBUG
      GC_log_printf(
            "Removing section __DATA,%s at %p-%p (%lu bytes) from image %s\n",
            GC_dyld_sections[i].sect, (void*)start, (void*)end,
            (unsigned long)sec->size, GC_dyld_name_for_hdr(hdr));
#endif
    GC_remove_roots((char*)start, (char*)end);
  }
  for (j = 0; j < sizeof(GC_dyld_add_sect_fmts) / sizeof(char *); j++) {
    const char *fmt = GC_dyld_add_sect_fmts[j];
    for (i = 0; i <= L2_MAX_OFILE_ALIGNMENT; i++) {
      char secnam[16];
      (void)snprintf(secnam, sizeof(secnam), fmt, (unsigned)i);
      secnam[sizeof(secnam) - 1] = '\0';
      sec = GC_GETSECTBYNAME(hdr, SEG_DATA, secnam);
      if (sec == NULL || sec->size == 0)
        continue;
      start = slide + sec->addr;
      end = start + sec->size;
#ifdef DARWIN_DEBUG
        GC_log_printf("Removing on-demand section __DATA,%s at"
                      " %p-%p (%lu bytes) from image %s\n", secnam,
                      (void*)start, (void*)end, (unsigned long)sec->size,
                      GC_dyld_name_for_hdr(hdr));
#endif
      GC_remove_roots((char*)start, (char*)end);
    }
  }
#if defined(DARWIN_DEBUG) && !defined(NO_DEBUGGING)
    LOCK();
    GC_print_static_roots();
    UNLOCK();
#endif
}
GC_INNER void GC_register_dynamic_libraries(void)
{
}
GC_INNER void GC_init_dyld(void)
{
  static GC_bool initialized = FALSE;
  if (initialized) return;
#ifdef DARWIN_DEBUG
    GC_log_printf("Registering dyld callbacks...\n");
#endif
  _dyld_register_func_for_add_image(
        (void (*)(const struct mach_header*, intptr_t))GC_dyld_image_add);
  _dyld_register_func_for_remove_image(
        (void (*)(const struct mach_header*, intptr_t))GC_dyld_image_remove);
  initialized = TRUE;
#ifdef NO_DYLD_BIND_FULLY_IMAGE
#else
    if (GC_no_dls) return;
    if (GETENV("DYLD_BIND_AT_LAUNCH") == 0) {
#ifdef DARWIN_DEBUG
        GC_log_printf("Forcing full bind of GC code...\n");
#endif
      if (!_dyld_bind_fully_image_containing_address(
                                                  (unsigned long *)GC_malloc))
        ABORT("_dyld_bind_fully_image_containing_address failed");
    }
#endif
}
#define HAVE_REGISTER_MAIN_STATIC_DATA
GC_INNER GC_bool GC_register_main_static_data(void)
{
  return FALSE;
}
#endif
#if defined(HAIKU)
#include <kernel/image.h>
  GC_INNER void GC_register_dynamic_libraries(void)
  {
    image_info info;
    int32 cookie = 0;
    while (get_next_image_info(0, &cookie, &info) == B_OK) {
      ptr_t data = (ptr_t)info.data;
      GC_add_roots_inner(data, data + info.data_size, TRUE);
    }
  }
#endif
#elif defined(PCR)
  GC_INNER void GC_register_dynamic_libraries(void)
  {
    PCR_IL_LoadedFile * p = PCR_IL_GetLastLoadedFile();
    PCR_IL_LoadedSegment * q;
    while (p != NIL && !(p -> lf_commitPoint)) {
        p = p -> lf_prev;
    }
    for (; p != NIL; p = p -> lf_prev) {
      for (q = p -> lf_ls; q != NIL; q = q -> ls_next) {
        if ((q -> ls_flags & PCR_IL_SegFlags_Traced_MASK)
            == PCR_IL_SegFlags_Traced_on) {
          GC_add_roots_inner((ptr_t)q->ls_addr,
                             (ptr_t)q->ls_addr + q->ls_bytes, TRUE);
        }
      }
    }
  }
#endif
#if !defined(HAVE_REGISTER_MAIN_STATIC_DATA) && defined(DYNAMIC_LOADING)
  GC_INNER GC_bool GC_register_main_static_data(void)
  {
    return TRUE;
  }
#endif
GC_API void GC_CALL GC_register_has_static_roots_callback(
                                        GC_has_static_roots_func callback)
{
    GC_has_static_roots = callback;
}
#if defined(GC_PTHREADS) && !defined(GC_NO_DLOPEN)
#undef GC_MUST_RESTORE_REDEFINED_DLOPEN
#if defined(dlopen) && !defined(GC_USE_LD_WRAP)
#undef dlopen
#define GC_MUST_RESTORE_REDEFINED_DLOPEN
#endif
#ifndef USE_PROC_FOR_LIBRARIES
  static void disable_gc_for_dlopen(void)
  {
    DCL_LOCK_STATE;
    LOCK();
    while (GC_incremental && GC_collection_in_progress()) {
      ENTER_GC();
      GC_collect_a_little_inner(1000);
      EXIT_GC();
    }
    ++GC_dont_gc;
    UNLOCK();
  }
#endif
#ifdef GC_USE_LD_WRAP
#define WRAP_DLFUNC(f) __wrap_##f
#define REAL_DLFUNC(f) __real_##f
  void * REAL_DLFUNC(dlopen)(const char *, int);
#else
#define WRAP_DLFUNC(f) GC_##f
#define REAL_DLFUNC(f) f
#endif
GC_API void * WRAP_DLFUNC(dlopen)(const char *path, int mode)
{
  void * result;
#ifndef USE_PROC_FOR_LIBRARIES
    disable_gc_for_dlopen();
#endif
  result = REAL_DLFUNC(dlopen)(path, mode);
#ifndef USE_PROC_FOR_LIBRARIES
    GC_enable();
#endif
  return(result);
}
#ifdef GC_USE_LD_WRAP
  GC_API void *GC_dlopen(const char *path, int mode)
  {
    return dlopen(path, mode);
  }
#endif
#ifdef GC_MUST_RESTORE_REDEFINED_DLOPEN
#define dlopen GC_dlopen
#endif
#endif
#if !defined(PLATFORM_MACH_DEP)
#if !defined(PLATFORM_MACH_DEP) && !defined(SN_TARGET_PSP2)
#include <stdio.h>
#ifdef AMIGA
#ifndef __GNUC__
#include <dos.h>
#else
#include <machine/reg.h>
#endif
#endif
#if defined(MACOS) && defined(__MWERKS__)
#if defined(POWERPC)
#define NONVOLATILE_GPR_COUNT 19
  struct ppc_registers {
        unsigned long gprs[NONVOLATILE_GPR_COUNT];
  };
  typedef struct ppc_registers ppc_registers;
#if defined(CPPCHECK)
    void getRegisters(ppc_registers* regs);
#else
    asm static void getRegisters(register ppc_registers* regs)
    {
        stmw    r13,regs->gprs
        blr
    }
#endif
  static void PushMacRegisters(void)
  {
        ppc_registers regs;
        int i;
        getRegisters(&regs);
        for (i = 0; i < NONVOLATILE_GPR_COUNT; i++)
                GC_push_one(regs.gprs[i]);
  }
#else
  asm static void PushMacRegisters(void)
  {
    sub.w   #4,sp
    move.l  a2,(sp)
    jsr         GC_push_one
    move.l  a3,(sp)
    jsr         GC_push_one
    move.l  a4,(sp)
    jsr         GC_push_one
#if !__option(a6frames)
        move.l  a6,(sp)
        jsr             GC_push_one
#endif
    move.l  d2,(sp)
    jsr         GC_push_one
    move.l  d3,(sp)
    jsr         GC_push_one
    move.l  d4,(sp)
    jsr         GC_push_one
    move.l  d5,(sp)
    jsr         GC_push_one
    move.l  d6,(sp)
    jsr         GC_push_one
    move.l  d7,(sp)
    jsr         GC_push_one
    add.w   #4,sp
    rts
  }
#endif
#endif
#if defined(SPARC) || defined(IA64)
    GC_INNER ptr_t GC_save_regs_ret_val = NULL;
#endif
#undef HAVE_PUSH_REGS
#if defined(USE_ASM_PUSH_REGS)
#define HAVE_PUSH_REGS
#else
#ifdef STACK_NOT_SCANNED
    void GC_push_regs(void)
    {
    }
#define HAVE_PUSH_REGS
#elif defined(M68K) && defined(AMIGA)
    void GC_push_regs(void)
    {
#ifdef __GNUC__
          asm("subq.w &0x4,%sp");
          asm("mov.l %a2,(%sp)"); asm("jsr _GC_push_one");
          asm("mov.l %a3,(%sp)"); asm("jsr _GC_push_one");
          asm("mov.l %a4,(%sp)"); asm("jsr _GC_push_one");
          asm("mov.l %a5,(%sp)"); asm("jsr _GC_push_one");
          asm("mov.l %a6,(%sp)"); asm("jsr _GC_push_one");
          asm("mov.l %d2,(%sp)"); asm("jsr _GC_push_one");
          asm("mov.l %d3,(%sp)"); asm("jsr _GC_push_one");
          asm("mov.l %d4,(%sp)"); asm("jsr _GC_push_one");
          asm("mov.l %d5,(%sp)"); asm("jsr _GC_push_one");
          asm("mov.l %d6,(%sp)"); asm("jsr _GC_push_one");
          asm("mov.l %d7,(%sp)"); asm("jsr _GC_push_one");
          asm("addq.w &0x4,%sp");
#else
          GC_push_one(getreg(REG_A2));
          GC_push_one(getreg(REG_A3));
#ifndef __SASC
            GC_push_one(getreg(REG_A4));
#endif
          GC_push_one(getreg(REG_A5));
          GC_push_one(getreg(REG_A6));
          GC_push_one(getreg(REG_D2));
          GC_push_one(getreg(REG_D3));
          GC_push_one(getreg(REG_D4));
          GC_push_one(getreg(REG_D5));
          GC_push_one(getreg(REG_D6));
          GC_push_one(getreg(REG_D7));
#endif
    }
#define HAVE_PUSH_REGS
#elif defined(MACOS)
#if defined(M68K) && defined(THINK_C) && !defined(CPPCHECK)
#define PushMacReg(reg) \
              move.l  reg,(sp) \
              jsr             GC_push_one
      void GC_push_regs(void)
      {
          asm {
              sub.w   #4,sp          ; reserve space for one parameter.
              PushMacReg(a2);
              PushMacReg(a3);
              PushMacReg(a4);
              ; skip a5 (globals), a6 (frame pointer), and a7 (stack pointer)
              PushMacReg(d2);
              PushMacReg(d3);
              PushMacReg(d4);
              PushMacReg(d5);
              PushMacReg(d6);
              PushMacReg(d7);
              add.w   #4,sp          ; fix stack.
          }
      }
#define HAVE_PUSH_REGS
#undef PushMacReg
#elif defined(__MWERKS__)
      void GC_push_regs(void)
      {
          PushMacRegisters();
      }
#define HAVE_PUSH_REGS
#endif
#endif
#endif
#if defined(HAVE_PUSH_REGS) && defined(THREADS)
#error GC_push_regs cannot be used with threads
#undef HAVE_PUSH_REGS
#endif
#if !defined(HAVE_PUSH_REGS) && defined(UNIX_LIKE)
#include <signal.h>
#ifndef NO_GETCONTEXT
#if defined(DARWIN) \
       && (MAC_OS_X_VERSION_MAX_ALLOWED >= 1060 )
#include <sys/ucontext.h>
#else
#include <ucontext.h>
#endif
#ifdef GETCONTEXT_FPU_EXCMASK_BUG
#include <fenv.h>
#endif
#endif
#endif
GC_ATTR_NO_SANITIZE_ADDR
GC_INNER void GC_with_callee_saves_pushed(void (*fn)(ptr_t, void *),
                                          volatile ptr_t arg)
{
  volatile int dummy;
  volatile ptr_t context = 0;
#if defined(HAVE_PUSH_REGS)
    GC_push_regs();
#elif defined(EMSCRIPTEN)
#else
#if defined(UNIX_LIKE) && !defined(NO_GETCONTEXT)
      static signed char getcontext_works = 0;
      ucontext_t ctxt;
#ifdef GETCONTEXT_FPU_EXCMASK_BUG
#ifdef X86_64
          unsigned short old_fcw;
#if defined(CPPCHECK)
            GC_noop1((word)&old_fcw);
#endif
          __asm__ __volatile__ ("fstcw %0" : "=m" (*&old_fcw));
#else
          int except_mask = fegetexcept();
#endif
#endif
      if (getcontext_works >= 0) {
        if (getcontext(&ctxt) < 0) {
          WARN("getcontext failed:"
               " using another register retrieval method...\n", 0);
        } else {
          context = (ptr_t)&ctxt;
        }
        if (EXPECT(0 == getcontext_works, FALSE))
          getcontext_works = context != NULL ? 1 : -1;
      }
#ifdef GETCONTEXT_FPU_EXCMASK_BUG
#ifdef X86_64
          __asm__ __volatile__ ("fldcw %0" : : "m" (*&old_fcw));
          {
            unsigned mxcsr;
            __asm__ __volatile__ ("stmxcsr %0" : "=m" (*&mxcsr));
            mxcsr = (mxcsr & ~(FE_ALL_EXCEPT << 7)) |
                        ((old_fcw & FE_ALL_EXCEPT) << 7);
            __asm__ __volatile__ ("ldmxcsr %0" : : "m" (*&mxcsr));
          }
#else
          if (feenableexcept(except_mask) < 0)
            ABORT("feenableexcept failed");
#endif
#endif
#if defined(SPARC) || defined(IA64)
        GC_save_regs_ret_val = GC_save_regs_in_stack();
#endif
      if (NULL == context)
#endif
    {
#if defined(HAVE_BUILTIN_UNWIND_INIT)
        __builtin_unwind_init();
#elif defined(NO_CRT) && defined(MSWIN32)
        CONTEXT ctx;
        RtlCaptureContext(&ctx);
#else
        jmp_buf regs;
        word * i = (word *)&regs;
        ptr_t lim = (ptr_t)(&regs) + sizeof(regs);
        for (; (word)i < (word)lim; i++) {
            *i = 0;
        }
#if defined(MSWIN32) || defined(MSWINCE) || defined(UTS4) \
           || defined(OS2) || defined(CX_UX) || defined(__CC_ARM) \
           || defined(LINUX) || defined(EWS4800) || defined(RTEMS)
          (void) setjmp(regs);
#else
          (void) _setjmp(regs);
#endif
#endif
    }
#endif
  fn(arg, ( void *)context);
  GC_noop1(COVERT_DATAFLOW(&dummy));
}
#endif
#endif
#if !defined(PLATFORM_STOP_WORLD)
#if defined(GC_PTHREADS) && !defined(GC_WIN32_THREADS) && \
    !defined(GC_DARWIN_THREADS) && !defined(PLATFORM_STOP_WORLD) \
    && !defined(SN_TARGET_PSP2)
#ifdef NACL
#include <unistd.h>
#include <sys/time.h>
  STATIC int GC_nacl_num_gc_threads = 0;
  STATIC __thread int GC_nacl_thread_idx = -1;
  STATIC volatile int GC_nacl_park_threads_now = 0;
  STATIC volatile pthread_t GC_nacl_thread_parker = -1;
  GC_INNER __thread GC_thread GC_nacl_gc_thread_self = NULL;
  volatile int GC_nacl_thread_parked[MAX_NACL_GC_THREADS];
  int GC_nacl_thread_used[MAX_NACL_GC_THREADS];
#elif defined(GC_OPENBSD_UTHREADS)
#include <pthread_np.h>
#else
#include <signal.h>
#include <semaphore.h>
#include <errno.h>
#include <time.h>
#include <unistd.h>
#if (!defined(AO_HAVE_load_acquire) || !defined(AO_HAVE_store_release)) \
    && !defined(CPPCHECK)
#error AO_load_acquire and/or AO_store_release are missing;
#error please define AO_REQUIRE_CAS manually
#endif
#undef pthread_sigmask
#ifdef GC_ENABLE_SUSPEND_THREAD
  static void *GC_CALLBACK suspend_self_inner(void *client_data);
#endif
#ifdef DEBUG_THREADS
#ifndef NSIG
#if defined(MAXSIG)
#define NSIG (MAXSIG+1)
#elif defined(_NSIG)
#define NSIG _NSIG
#elif defined(__SIGRTMAX)
#define NSIG (__SIGRTMAX+1)
#else
#error define NSIG
#endif
#endif
  void GC_print_sig_mask(void)
  {
    sigset_t blocked;
    int i;
    if (pthread_sigmask(SIG_BLOCK, NULL, &blocked) != 0)
      ABORT("pthread_sigmask failed");
    for (i = 1; i < NSIG; i++) {
      if (sigismember(&blocked, i))
        GC_printf("Signal blocked: %d\n", i);
    }
  }
#endif
STATIC void GC_remove_allowed_signals(sigset_t *set)
{
    if (sigdelset(set, SIGINT) != 0
          || sigdelset(set, SIGQUIT) != 0
          || sigdelset(set, SIGABRT) != 0
          || sigdelset(set, SIGTERM) != 0) {
        ABORT("sigdelset failed");
    }
#ifdef MPROTECT_VDB
      if (sigdelset(set, SIGSEGV) != 0
#ifdef HAVE_SIGBUS
            || sigdelset(set, SIGBUS) != 0
#endif
          ) {
        ABORT("sigdelset failed");
      }
#endif
}
static sigset_t suspend_handler_mask;
#define THREAD_RESTARTED 0x1
STATIC volatile AO_t GC_stop_count = 0;
STATIC volatile AO_t GC_world_is_stopped = FALSE;
#ifndef NO_RETRY_SIGNALS
  STATIC GC_bool GC_retry_signals = TRUE;
#else
  STATIC GC_bool GC_retry_signals = FALSE;
#endif
#ifndef SIG_THR_RESTART
#if defined(GC_HPUX_THREADS) || defined(GC_OSF1_THREADS) \
     || defined(GC_NETBSD_THREADS) || defined(GC_USESIGRT_SIGNALS)
#if defined(_SIGRTMIN) && !defined(CPPCHECK)
#define SIG_THR_RESTART _SIGRTMIN + 5
#else
#define SIG_THR_RESTART SIGRTMIN + 5
#endif
#else
#define SIG_THR_RESTART SIGXCPU
#endif
#endif
#define SIGNAL_UNSET (-1)
STATIC int GC_sig_suspend = SIGNAL_UNSET;
STATIC int GC_sig_thr_restart = SIGNAL_UNSET;
GC_API void GC_CALL GC_set_suspend_signal(int sig)
{
  if (GC_is_initialized) return;
  GC_sig_suspend = sig;
}
GC_API void GC_CALL GC_set_thr_restart_signal(int sig)
{
  if (GC_is_initialized) return;
  GC_sig_thr_restart = sig;
}
GC_API int GC_CALL GC_get_suspend_signal(void)
{
  return GC_sig_suspend != SIGNAL_UNSET ? GC_sig_suspend : SIG_SUSPEND;
}
GC_API int GC_CALL GC_get_thr_restart_signal(void)
{
  return GC_sig_thr_restart != SIGNAL_UNSET
            ? GC_sig_thr_restart : SIG_THR_RESTART;
}
#if defined(GC_EXPLICIT_SIGNALS_UNBLOCK) \
    || !defined(NO_SIGNALS_UNBLOCK_IN_MAIN)
  GC_INNER void GC_unblock_gc_signals(void)
  {
    sigset_t set;
    sigemptyset(&set);
    GC_ASSERT(GC_sig_suspend != SIGNAL_UNSET);
    GC_ASSERT(GC_sig_thr_restart != SIGNAL_UNSET);
    sigaddset(&set, GC_sig_suspend);
    sigaddset(&set, GC_sig_thr_restart);
    if (pthread_sigmask(SIG_UNBLOCK, &set, NULL) != 0)
      ABORT("pthread_sigmask failed");
  }
#endif
STATIC sem_t GC_suspend_ack_sem;
STATIC void GC_suspend_handler_inner(ptr_t dummy, void *context);
#ifndef NO_SA_SIGACTION
  STATIC void GC_suspend_handler(int sig, siginfo_t * info GC_ATTR_UNUSED,
                                 void * context GC_ATTR_UNUSED)
#else
  STATIC void GC_suspend_handler(int sig)
#endif
{
  int old_errno = errno;
  if (sig != GC_sig_suspend) {
#if defined(GC_FREEBSD_THREADS)
      if (0 == sig) return;
#endif
    ABORT("Bad signal in suspend_handler");
  }
#if defined(IA64) || defined(HP_PA) || defined(M68K)
    GC_with_callee_saves_pushed(GC_suspend_handler_inner, NULL);
#else
    {
#ifdef NO_SA_SIGACTION
        void *context = 0;
#endif
      GC_suspend_handler_inner(NULL, context);
    }
#endif
  errno = old_errno;
}
#ifdef BASE_ATOMIC_OPS_EMULATED
#define ao_load_acquire_async(p) (*(p))
#define ao_load_async(p) ao_load_acquire_async(p)
#define ao_store_release_async(p, v) (void)(*(p) = (v))
#define ao_store_async(p, v) ao_store_release_async(p, v)
#else
#define ao_load_acquire_async(p) AO_load_acquire(p)
#define ao_load_async(p) AO_load(p)
#define ao_store_release_async(p, v) AO_store_release(p, v)
#define ao_store_async(p, v) AO_store(p, v)
#endif
#ifdef THREAD_SANITIZER
  GC_ATTR_NO_SANITIZE_THREAD
  static GC_thread GC_lookup_thread_async(pthread_t id)
  {
    GC_thread p = GC_threads[THREAD_TABLE_INDEX(id)];
    while (p != NULL && !THREAD_EQUAL(p->id, id))
      p = p->next;
    return p;
  }
#else
#define GC_lookup_thread_async GC_lookup_thread
#endif
GC_INLINE void GC_store_stack_ptr(GC_thread me)
{
#ifdef SPARC
    ao_store_async((volatile AO_t *)&me->stop_info.stack_ptr,
             (AO_t)GC_save_regs_in_stack());
#else
#ifdef IA64
      me -> backing_store_ptr = GC_save_regs_in_stack();
#endif
    ao_store_async((volatile AO_t *)&me->stop_info.stack_ptr,
                   (AO_t)GC_approx_sp());
#endif
}
STATIC void GC_suspend_handler_inner(ptr_t dummy GC_ATTR_UNUSED,
                                     void * context GC_ATTR_UNUSED)
{
  pthread_t self = pthread_self();
  GC_thread me;
  IF_CANCEL(int cancel_state;)
  AO_t my_stop_count = ao_load_acquire_async(&GC_stop_count);
  DISABLE_CANCEL(cancel_state);
#ifdef DEBUG_THREADS
    GC_log_printf("Suspending %p\n", (void *)self);
#endif
  GC_ASSERT(((word)my_stop_count & THREAD_RESTARTED) == 0);
  me = GC_lookup_thread_async(self);
#ifdef GC_ENABLE_SUSPEND_THREAD
    if (ao_load_async(&me->suspended_ext)) {
      GC_store_stack_ptr(me);
      sem_post(&GC_suspend_ack_sem);
      suspend_self_inner(me);
#ifdef DEBUG_THREADS
        GC_log_printf("Continuing %p on GC_resume_thread\n", (void *)self);
#endif
      RESTORE_CANCEL(cancel_state);
      return;
    }
#endif
  if (((word)me->stop_info.last_stop_count & ~(word)THREAD_RESTARTED)
        == (word)my_stop_count) {
      if (!GC_retry_signals) {
          WARN("Duplicate suspend signal in thread %p\n", self);
      }
      RESTORE_CANCEL(cancel_state);
      return;
  }
  GC_store_stack_ptr(me);
#ifdef THREAD_SANITIZER
    {
      sigset_t set;
      sigemptyset(&set);
      GC_ASSERT(GC_sig_suspend != SIGNAL_UNSET);
      GC_ASSERT(GC_sig_thr_restart != SIGNAL_UNSET);
      sigaddset(&set, GC_sig_suspend);
      sigaddset(&set, GC_sig_thr_restart);
      if (pthread_sigmask(SIG_UNBLOCK, &set, NULL) != 0)
        ABORT("pthread_sigmask failed in suspend handler");
    }
#endif
  sem_post(&GC_suspend_ack_sem);
  ao_store_release_async(&me->stop_info.last_stop_count, my_stop_count);
  do {
      sigsuspend (&suspend_handler_mask);
  } while (ao_load_acquire_async(&GC_world_is_stopped)
           && ao_load_async(&GC_stop_count) == my_stop_count);
#ifdef DEBUG_THREADS
    GC_log_printf("Continuing %p\n", (void *)self);
#endif
#ifndef GC_NETBSD_THREADS_WORKAROUND
    if (GC_retry_signals)
#endif
  {
    sem_post(&GC_suspend_ack_sem);
#ifdef GC_NETBSD_THREADS_WORKAROUND
      if (GC_retry_signals)
#endif
    {
      ao_store_release_async(&me->stop_info.last_stop_count,
                             (AO_t)((word)my_stop_count | THREAD_RESTARTED));
    }
  }
  RESTORE_CANCEL(cancel_state);
}
static void suspend_restart_barrier(int n_live_threads)
{
    int i;
    for (i = 0; i < n_live_threads; i++) {
      while (0 != sem_wait(&GC_suspend_ack_sem)) {
        if (errno != EINTR)
          ABORT("sem_wait failed");
      }
    }
#ifdef GC_ASSERTIONS
      sem_getvalue(&GC_suspend_ack_sem, &i);
      GC_ASSERT(0 == i);
#endif
}
static int resend_lost_signals(int n_live_threads,
                               int (*suspend_restart_all)(void))
{
#define WAIT_UNIT 3000
#define RETRY_INTERVAL 100000
    if (n_live_threads > 0) {
      unsigned long wait_usecs = 0;
      for (;;) {
        int ack_count;
        sem_getvalue(&GC_suspend_ack_sem, &ack_count);
        if (ack_count == n_live_threads)
          break;
        if (wait_usecs > RETRY_INTERVAL) {
          int newly_sent = suspend_restart_all();
          GC_COND_LOG_PRINTF("Resent %d signals after timeout\n", newly_sent);
          sem_getvalue(&GC_suspend_ack_sem, &ack_count);
          if (newly_sent < n_live_threads - ack_count) {
            WARN("Lost some threads while stopping or starting world?!\n", 0);
            n_live_threads = ack_count + newly_sent;
          }
          wait_usecs = 0;
        }
#ifdef LINT2
#undef WAIT_UNIT
#define WAIT_UNIT 1
          sched_yield();
#elif defined(CPPCHECK)
          {
            struct timespec ts;
            ts.tv_sec = 0;
            ts.tv_nsec = WAIT_UNIT * 1000;
            (void)nanosleep(&ts, NULL);
          }
#else
          usleep(WAIT_UNIT);
#endif
        wait_usecs += WAIT_UNIT;
      }
    }
    return n_live_threads;
}
#ifdef HAVE_CLOCK_GETTIME
#define TS_NSEC_ADD(ts, ns) \
                (ts.tv_nsec += (ns), \
                 (void)(ts.tv_nsec >= 1000000L*1000 ? \
                       (ts.tv_nsec -= 1000000L*1000, ts.tv_sec++, 0) : 0))
#endif
static void resend_lost_signals_retry(int n_live_threads,
                                      int (*suspend_restart_all)(void))
{
#if defined(HAVE_CLOCK_GETTIME) && !defined(DONT_TIMEDWAIT_ACK_SEM)
#define TIMEOUT_BEFORE_RESEND 10000
    int i;
    struct timespec ts;
    if (n_live_threads > 0 && clock_gettime(CLOCK_REALTIME, &ts) == 0) {
      TS_NSEC_ADD(ts, TIMEOUT_BEFORE_RESEND * 1000);
      for (i = 0; i < n_live_threads; i++) {
        if (0 != sem_timedwait(&GC_suspend_ack_sem, &ts))
          break;
      }
      n_live_threads -= i;
    }
#endif
  n_live_threads = resend_lost_signals(n_live_threads, suspend_restart_all);
  suspend_restart_barrier(n_live_threads);
}
STATIC void GC_restart_handler(int sig)
{
#if defined(DEBUG_THREADS)
    int old_errno = errno;
#endif
  if (sig != GC_sig_thr_restart)
    ABORT("Bad signal in restart handler");
#ifdef DEBUG_THREADS
    GC_log_printf("In GC_restart_handler for %p\n", (void *)pthread_self());
    errno = old_errno;
#endif
}
#ifdef USE_TKILL_ON_ANDROID
    EXTERN_C_BEGIN
    extern int tkill(pid_t tid, int sig);
    EXTERN_C_END
    static int android_thread_kill(pid_t tid, int sig)
    {
      int ret;
      int old_errno = errno;
      ret = tkill(tid, sig);
      if (ret < 0) {
          ret = errno;
          errno = old_errno;
      }
      return ret;
    }
#define THREAD_SYSTEM_ID(t) (t)->kernel_id
#define RAISE_SIGNAL(t, sig) android_thread_kill(THREAD_SYSTEM_ID(t), sig)
#else
#define THREAD_SYSTEM_ID(t) (t)->id
#define RAISE_SIGNAL(t, sig) pthread_kill(THREAD_SYSTEM_ID(t), sig)
#endif
#ifdef GC_ENABLE_SUSPEND_THREAD
#include <sys/time.h>
    STATIC void GC_brief_async_signal_safe_sleep(void)
    {
      struct timeval tv;
      tv.tv_sec = 0;
#if defined(GC_TIME_LIMIT) && !defined(CPPCHECK)
        tv.tv_usec = 1000 * GC_TIME_LIMIT / 2;
#else
        tv.tv_usec = 1000 * 50 / 2;
#endif
      (void)select(0, 0, 0, 0, &tv);
    }
    static void *GC_CALLBACK suspend_self_inner(void *client_data) {
      GC_thread me = (GC_thread)client_data;
      while (ao_load_acquire_async(&me->suspended_ext)) {
        GC_brief_async_signal_safe_sleep();
      }
      return NULL;
    }
    GC_API void GC_CALL GC_suspend_thread(GC_SUSPEND_THREAD_ID thread) {
      GC_thread t;
      IF_CANCEL(int cancel_state;)
      DCL_LOCK_STATE;
      LOCK();
      t = GC_lookup_thread((pthread_t)thread);
      if (t == NULL || t -> suspended_ext) {
        UNLOCK();
        return;
      }
      AO_store_release(&t->suspended_ext, TRUE);
      if (THREAD_EQUAL((pthread_t)thread, pthread_self())) {
        UNLOCK();
        (void)GC_do_blocking(suspend_self_inner, t);
        return;
      }
      if ((t -> flags & FINISHED) != 0) {
        UNLOCK();
        return;
      }
      DISABLE_CANCEL(cancel_state);
#ifdef PARALLEL_MARK
        if (GC_parallel)
          GC_wait_for_reclaim();
#endif
      if (GC_manual_vdb) {
        GC_acquire_dirty_lock();
      }
      switch (RAISE_SIGNAL(t, GC_sig_suspend)) {
      case 0:
        break;
      default:
        ABORT("pthread_kill failed");
      }
      GC_ASSERT(GC_thr_initialized);
      while (sem_wait(&GC_suspend_ack_sem) != 0) {
        if (errno != EINTR)
          ABORT("sem_wait for handler failed (suspend_self)");
      }
      if (GC_manual_vdb)
        GC_release_dirty_lock();
      RESTORE_CANCEL(cancel_state);
      UNLOCK();
    }
    GC_API void GC_CALL GC_resume_thread(GC_SUSPEND_THREAD_ID thread) {
      GC_thread t;
      DCL_LOCK_STATE;
      LOCK();
      t = GC_lookup_thread((pthread_t)thread);
      if (t != NULL)
        AO_store(&t->suspended_ext, FALSE);
      UNLOCK();
    }
    GC_API int GC_CALL GC_is_thread_suspended(GC_SUSPEND_THREAD_ID thread) {
      GC_thread t;
      int is_suspended = 0;
      DCL_LOCK_STATE;
      LOCK();
      t = GC_lookup_thread((pthread_t)thread);
      if (t != NULL && t -> suspended_ext)
        is_suspended = (int)TRUE;
      UNLOCK();
      return is_suspended;
    }
#endif
#undef ao_load_acquire_async
#undef ao_load_async
#undef ao_store_async
#undef ao_store_release_async
#endif
#ifdef IA64
#define IF_IA64(x) x
#else
#define IF_IA64(x)
#endif
GC_INNER void GC_push_all_stacks(void)
{
    GC_bool found_me = FALSE;
    size_t nthreads = 0;
    int i;
    GC_thread p;
    ptr_t lo, hi;
    IF_IA64(ptr_t bs_lo; ptr_t bs_hi;)
    struct GC_traced_stack_sect_s *traced_stack_sect;
    pthread_t self = pthread_self();
    word total_size = 0;
    if (!EXPECT(GC_thr_initialized, TRUE))
      GC_thr_init();
#ifdef DEBUG_THREADS
      GC_log_printf("Pushing stacks from thread %p\n", (void *)self);
#endif
    for (i = 0; i < THREAD_TABLE_SZ; i++) {
      for (p = GC_threads[i]; p != 0; p = p -> next) {
        if (p -> flags & FINISHED) continue;
        ++nthreads;
        traced_stack_sect = p -> traced_stack_sect;
        if (THREAD_EQUAL(p -> id, self)) {
            GC_ASSERT(!p->thread_blocked);
#ifdef SPARC
                lo = (ptr_t)GC_save_regs_in_stack();
#else
                lo = GC_approx_sp();
#endif
            found_me = TRUE;
            IF_IA64(bs_hi = (ptr_t)GC_save_regs_in_stack();)
        } else {
            lo = (ptr_t)AO_load((volatile AO_t *)&p->stop_info.stack_ptr);
            IF_IA64(bs_hi = p -> backing_store_ptr;)
            if (traced_stack_sect != NULL
                    && traced_stack_sect->saved_stack_ptr == lo) {
              traced_stack_sect = traced_stack_sect->prev;
            }
        }
        if ((p -> flags & MAIN_THREAD) == 0) {
            hi = p -> stack_end;
            IF_IA64(bs_lo = p -> backing_store_end);
        } else {
            hi = GC_stackbottom;
            IF_IA64(bs_lo = BACKING_STORE_BASE;)
        }
#ifdef DEBUG_THREADS
          GC_log_printf("Stack for thread %p is [%p,%p)\n",
                        (void *)p->id, (void *)lo, (void *)hi);
#endif
        if (0 == lo) ABORT("GC_push_all_stacks: sp not set!");
        if (p->altstack != NULL && (word)p->altstack <= (word)lo
            && (word)lo <= (word)p->altstack + p->altstack_size) {
          hi = p->altstack + p->altstack_size;
        }
        GC_push_all_stack_sections(lo, hi, traced_stack_sect);
#ifdef STACK_GROWS_UP
          total_size += lo - hi;
#else
          total_size += hi - lo;
#endif
#ifdef NACL
          GC_push_all_stack((ptr_t)p -> stop_info.reg_storage,
              (ptr_t)(p -> stop_info.reg_storage + NACL_GC_REG_STORAGE_SIZE));
          total_size += NACL_GC_REG_STORAGE_SIZE * sizeof(ptr_t);
#endif
#ifdef IA64
#ifdef DEBUG_THREADS
            GC_log_printf("Reg stack for thread %p is [%p,%p)\n",
                          (void *)p->id, (void *)bs_lo, (void *)bs_hi);
#endif
          GC_push_all_register_sections(bs_lo, bs_hi,
                                        THREAD_EQUAL(p -> id, self),
                                        traced_stack_sect);
          total_size += bs_hi - bs_lo;
#endif
      }
    }
    GC_VERBOSE_LOG_PRINTF("Pushed %d thread stacks\n", (int)nthreads);
    if (!found_me && !GC_in_thread_creation)
      ABORT("Collecting from unknown thread");
    GC_total_stacksize = total_size;
}
#ifdef DEBUG_THREADS
  pthread_t GC_stopping_thread;
  int GC_stopping_pid = 0;
#endif
STATIC int GC_suspend_all(void)
{
  int n_live_threads = 0;
  int i;
#ifndef NACL
    GC_thread p;
#ifndef GC_OPENBSD_UTHREADS
      int result;
#endif
    pthread_t self = pthread_self();
    for (i = 0; i < THREAD_TABLE_SZ; i++) {
      for (p = GC_threads[i]; p != 0; p = p -> next) {
        if (!THREAD_EQUAL(p -> id, self)) {
            if ((p -> flags & FINISHED) != 0) continue;
            if (p -> thread_blocked)  continue;
#ifndef GC_OPENBSD_UTHREADS
#ifdef GC_ENABLE_SUSPEND_THREAD
                if (p -> suspended_ext) continue;
#endif
              if (AO_load(&p->stop_info.last_stop_count) == GC_stop_count)
                continue;
              n_live_threads++;
#endif
#ifdef DEBUG_THREADS
              GC_log_printf("Sending suspend signal to %p\n", (void *)p->id);
#endif
#ifdef GC_OPENBSD_UTHREADS
              {
                stack_t stack;
                GC_acquire_dirty_lock();
                if (pthread_suspend_np(p -> id) != 0)
                  ABORT("pthread_suspend_np failed");
                GC_release_dirty_lock();
                if (pthread_stackseg_np(p->id, &stack))
                  ABORT("pthread_stackseg_np failed");
                p -> stop_info.stack_ptr = (ptr_t)stack.ss_sp - stack.ss_size;
                if (GC_on_thread_event)
                  GC_on_thread_event(GC_EVENT_THREAD_SUSPENDED,
                                     (void *)p->id);
              }
#else
              result = RAISE_SIGNAL(p, GC_sig_suspend);
              switch(result) {
                case ESRCH:
                    n_live_threads--;
                    break;
                case 0:
                    if (GC_on_thread_event)
                      GC_on_thread_event(GC_EVENT_THREAD_SUSPENDED,
                                         (void *)(word)THREAD_SYSTEM_ID(p));
                    break;
                default:
                    ABORT_ARG1("pthread_kill failed at suspend",
                               ": errcode= %d", result);
              }
#endif
        }
      }
    }
#else
#ifndef NACL_PARK_WAIT_NANOSECONDS
#define NACL_PARK_WAIT_NANOSECONDS (100 * 1000)
#endif
#define NANOS_PER_SECOND (1000UL * 1000 * 1000)
    unsigned long num_sleeps = 0;
#ifdef DEBUG_THREADS
      GC_log_printf("pthread_stop_world: number of threads: %d\n",
                    GC_nacl_num_gc_threads - 1);
#endif
    GC_nacl_thread_parker = pthread_self();
    GC_nacl_park_threads_now = 1;
    if (GC_manual_vdb)
      GC_acquire_dirty_lock();
    while (1) {
      int num_threads_parked = 0;
      struct timespec ts;
      int num_used = 0;
      for (i = 0; i < MAX_NACL_GC_THREADS
                  && num_used < GC_nacl_num_gc_threads; i++) {
        if (GC_nacl_thread_used[i] == 1) {
          num_used++;
          if (GC_nacl_thread_parked[i] == 1) {
            num_threads_parked++;
            if (GC_on_thread_event)
              GC_on_thread_event(GC_EVENT_THREAD_SUSPENDED, (void *)(word)i);
          }
        }
      }
      if (num_threads_parked >= GC_nacl_num_gc_threads - 1)
        break;
      ts.tv_sec = 0;
      ts.tv_nsec = NACL_PARK_WAIT_NANOSECONDS;
#ifdef DEBUG_THREADS
        GC_log_printf("Sleep waiting for %d threads to park...\n",
                      GC_nacl_num_gc_threads - num_threads_parked - 1);
#endif
      nanosleep(&ts, 0);
      if (++num_sleeps > NANOS_PER_SECOND / NACL_PARK_WAIT_NANOSECONDS) {
        WARN("GC appears stalled waiting for %" WARN_PRIdPTR
             " threads to park...\n",
             GC_nacl_num_gc_threads - num_threads_parked - 1);
        num_sleeps = 0;
      }
    }
    if (GC_manual_vdb)
      GC_release_dirty_lock();
#endif
  return n_live_threads;
}
GC_INNER void GC_stop_world(void)
{
#if !defined(GC_OPENBSD_UTHREADS) && !defined(NACL)
    int n_live_threads;
#endif
  GC_ASSERT(I_HOLD_LOCK());
#ifdef DEBUG_THREADS
    GC_stopping_thread = pthread_self();
    GC_stopping_pid = getpid();
    GC_log_printf("Stopping the world from %p\n", (void *)GC_stopping_thread);
#endif
#ifdef PARALLEL_MARK
    if (GC_parallel) {
      GC_acquire_mark_lock();
      GC_ASSERT(GC_fl_builder_count == 0);
    }
#endif
#if defined(GC_OPENBSD_UTHREADS) || defined(NACL)
    (void)GC_suspend_all();
#else
    AO_store(&GC_stop_count,
             (AO_t)((word)GC_stop_count + (THREAD_RESTARTED+1)));
    if (GC_manual_vdb) {
      GC_acquire_dirty_lock();
    }
    AO_store_release(&GC_world_is_stopped, TRUE);
    n_live_threads = GC_suspend_all();
    if (GC_retry_signals) {
      resend_lost_signals_retry(n_live_threads, GC_suspend_all);
    } else {
      suspend_restart_barrier(n_live_threads);
    }
    if (GC_manual_vdb)
      GC_release_dirty_lock();
#endif
#ifdef PARALLEL_MARK
    if (GC_parallel)
      GC_release_mark_lock();
#endif
#ifdef DEBUG_THREADS
    GC_log_printf("World stopped from %p\n", (void *)pthread_self());
    GC_stopping_thread = 0;
#endif
}
#ifdef NACL
#if defined(__x86_64__)
#define NACL_STORE_REGS() \
        do { \
          __asm__ __volatile__ ("push %rbx"); \
          __asm__ __volatile__ ("push %rbp"); \
          __asm__ __volatile__ ("push %r12"); \
          __asm__ __volatile__ ("push %r13"); \
          __asm__ __volatile__ ("push %r14"); \
          __asm__ __volatile__ ("push %r15"); \
          __asm__ __volatile__ ("mov %%esp, %0" \
                    : "=m" (GC_nacl_gc_thread_self->stop_info.stack_ptr)); \
          BCOPY(GC_nacl_gc_thread_self->stop_info.stack_ptr, \
                GC_nacl_gc_thread_self->stop_info.reg_storage, \
                NACL_GC_REG_STORAGE_SIZE * sizeof(ptr_t)); \
          __asm__ __volatile__ ("naclasp $48, %r15"); \
        } while (0)
#elif defined(__i386__)
#define NACL_STORE_REGS() \
        do { \
          __asm__ __volatile__ ("push %ebx"); \
          __asm__ __volatile__ ("push %ebp"); \
          __asm__ __volatile__ ("push %esi"); \
          __asm__ __volatile__ ("push %edi"); \
          __asm__ __volatile__ ("mov %%esp, %0" \
                    : "=m" (GC_nacl_gc_thread_self->stop_info.stack_ptr)); \
          BCOPY(GC_nacl_gc_thread_self->stop_info.stack_ptr, \
                GC_nacl_gc_thread_self->stop_info.reg_storage, \
                NACL_GC_REG_STORAGE_SIZE * sizeof(ptr_t));\
          __asm__ __volatile__ ("add $16, %esp"); \
        } while (0)
#elif defined(__arm__)
#define NACL_STORE_REGS() \
        do { \
          __asm__ __volatile__ ("push {r4-r8,r10-r12,lr}"); \
          __asm__ __volatile__ ("mov r0, %0" \
                : : "r" (&GC_nacl_gc_thread_self->stop_info.stack_ptr)); \
          __asm__ __volatile__ ("bic r0, r0, #0xc0000000"); \
          __asm__ __volatile__ ("str sp, [r0]"); \
          BCOPY(GC_nacl_gc_thread_self->stop_info.stack_ptr, \
                GC_nacl_gc_thread_self->stop_info.reg_storage, \
                NACL_GC_REG_STORAGE_SIZE * sizeof(ptr_t)); \
          __asm__ __volatile__ ("add sp, sp, #40"); \
          __asm__ __volatile__ ("bic sp, sp, #0xc0000000"); \
        } while (0)
#else
#error TODO Please port NACL_STORE_REGS
#endif
  GC_API_OSCALL void nacl_pre_syscall_hook(void)
  {
    if (GC_nacl_thread_idx != -1) {
      NACL_STORE_REGS();
      GC_nacl_gc_thread_self->stop_info.stack_ptr = GC_approx_sp();
      GC_nacl_thread_parked[GC_nacl_thread_idx] = 1;
    }
  }
  GC_API_OSCALL void __nacl_suspend_thread_if_needed(void)
  {
    if (GC_nacl_park_threads_now) {
      pthread_t self = pthread_self();
      if (GC_nacl_thread_parker == self)
        return;
      if (GC_nacl_thread_idx < 0)
        return;
      if (!GC_nacl_thread_parked[GC_nacl_thread_idx]) {
        NACL_STORE_REGS();
        GC_nacl_gc_thread_self->stop_info.stack_ptr = GC_approx_sp();
      }
      GC_nacl_thread_parked[GC_nacl_thread_idx] = 1;
      while (GC_nacl_park_threads_now) {
      }
      GC_nacl_thread_parked[GC_nacl_thread_idx] = 0;
      BZERO(GC_nacl_gc_thread_self->stop_info.reg_storage,
            NACL_GC_REG_STORAGE_SIZE * sizeof(ptr_t));
    }
  }
  GC_API_OSCALL void nacl_post_syscall_hook(void)
  {
    __nacl_suspend_thread_if_needed();
    if (GC_nacl_thread_idx != -1) {
      GC_nacl_thread_parked[GC_nacl_thread_idx] = 0;
    }
  }
  STATIC GC_bool GC_nacl_thread_parking_inited = FALSE;
  STATIC pthread_mutex_t GC_nacl_thread_alloc_lock = PTHREAD_MUTEX_INITIALIZER;
  struct nacl_irt_blockhook {
    int (*register_block_hooks)(void (*pre)(void), void (*post)(void));
  };
  EXTERN_C_BEGIN
  extern size_t nacl_interface_query(const char *interface_ident,
                                     void *table, size_t tablesize);
  EXTERN_C_END
  GC_INNER void GC_nacl_initialize_gc_thread(void)
  {
    int i;
    static struct nacl_irt_blockhook gc_hook;
    pthread_mutex_lock(&GC_nacl_thread_alloc_lock);
    if (!EXPECT(GC_nacl_thread_parking_inited, TRUE)) {
      BZERO(GC_nacl_thread_parked, sizeof(GC_nacl_thread_parked));
      BZERO(GC_nacl_thread_used, sizeof(GC_nacl_thread_used));
      nacl_interface_query("nacl-irt-blockhook-0.1",
                           &gc_hook, sizeof(gc_hook));
      gc_hook.register_block_hooks(nacl_pre_syscall_hook,
                                   nacl_post_syscall_hook);
      GC_nacl_thread_parking_inited = TRUE;
    }
    GC_ASSERT(GC_nacl_num_gc_threads <= MAX_NACL_GC_THREADS);
    for (i = 0; i < MAX_NACL_GC_THREADS; i++) {
      if (GC_nacl_thread_used[i] == 0) {
        GC_nacl_thread_used[i] = 1;
        GC_nacl_thread_idx = i;
        GC_nacl_num_gc_threads++;
        break;
      }
    }
    pthread_mutex_unlock(&GC_nacl_thread_alloc_lock);
  }
  GC_INNER void GC_nacl_shutdown_gc_thread(void)
  {
    pthread_mutex_lock(&GC_nacl_thread_alloc_lock);
    GC_ASSERT(GC_nacl_thread_idx >= 0);
    GC_ASSERT(GC_nacl_thread_idx < MAX_NACL_GC_THREADS);
    GC_ASSERT(GC_nacl_thread_used[GC_nacl_thread_idx] != 0);
    GC_nacl_thread_used[GC_nacl_thread_idx] = 0;
    GC_nacl_thread_idx = -1;
    GC_nacl_num_gc_threads--;
    pthread_mutex_unlock(&GC_nacl_thread_alloc_lock);
  }
#else
  STATIC int GC_restart_all(void)
  {
    int n_live_threads = 0;
    int i;
    pthread_t self = pthread_self();
    GC_thread p;
#ifndef GC_OPENBSD_UTHREADS
      int result;
#endif
    for (i = 0; i < THREAD_TABLE_SZ; i++) {
      for (p = GC_threads[i]; p != NULL; p = p -> next) {
        if (!THREAD_EQUAL(p -> id, self)) {
          if ((p -> flags & FINISHED) != 0) continue;
          if (p -> thread_blocked) continue;
#ifndef GC_OPENBSD_UTHREADS
#ifdef GC_ENABLE_SUSPEND_THREAD
              if (p -> suspended_ext) continue;
#endif
            if (GC_retry_signals
                && AO_load(&p->stop_info.last_stop_count)
                    == (AO_t)((word)GC_stop_count | THREAD_RESTARTED))
              continue;
            n_live_threads++;
#endif
#ifdef DEBUG_THREADS
            GC_log_printf("Sending restart signal to %p\n", (void *)p->id);
#endif
#ifdef GC_OPENBSD_UTHREADS
            if (pthread_resume_np(p -> id) != 0)
              ABORT("pthread_resume_np failed");
            if (GC_on_thread_event)
              GC_on_thread_event(GC_EVENT_THREAD_UNSUSPENDED, (void *)p->id);
#else
            result = RAISE_SIGNAL(p, GC_sig_thr_restart);
            switch(result) {
            case ESRCH:
              n_live_threads--;
              break;
            case 0:
              if (GC_on_thread_event)
                GC_on_thread_event(GC_EVENT_THREAD_UNSUSPENDED,
                                   (void *)(word)THREAD_SYSTEM_ID(p));
              break;
            default:
              ABORT_ARG1("pthread_kill failed at resume",
                         ": errcode= %d", result);
            }
#endif
        }
      }
    }
    return n_live_threads;
  }
#endif
GC_INNER void GC_start_world(void)
{
#ifndef NACL
    int n_live_threads;
    GC_ASSERT(I_HOLD_LOCK());
#ifdef DEBUG_THREADS
      GC_log_printf("World starting\n");
#endif
#ifndef GC_OPENBSD_UTHREADS
      AO_store_release(&GC_world_is_stopped, FALSE);
#endif
    n_live_threads = GC_restart_all();
#ifdef GC_OPENBSD_UTHREADS
      (void)n_live_threads;
#else
      if (GC_retry_signals) {
        resend_lost_signals_retry(n_live_threads, GC_restart_all);
      }
#ifdef GC_NETBSD_THREADS_WORKAROUND
        else {
          suspend_restart_barrier(n_live_threads);
        }
#endif
#endif
#ifdef DEBUG_THREADS
      GC_log_printf("World started\n");
#endif
#else
#ifdef DEBUG_THREADS
      GC_log_printf("World starting...\n");
#endif
    GC_nacl_park_threads_now = 0;
    if (GC_on_thread_event)
      GC_on_thread_event(GC_EVENT_THREAD_UNSUSPENDED, NULL);
#endif
}
GC_INNER void GC_stop_init(void)
{
#if !defined(GC_OPENBSD_UTHREADS) && !defined(NACL)
    struct sigaction act;
    char *str;
    if (SIGNAL_UNSET == GC_sig_suspend)
        GC_sig_suspend = SIG_SUSPEND;
    if (SIGNAL_UNSET == GC_sig_thr_restart)
        GC_sig_thr_restart = SIG_THR_RESTART;
    if (GC_sig_suspend == GC_sig_thr_restart)
        ABORT("Cannot use same signal for thread suspend and resume");
    if (sem_init(&GC_suspend_ack_sem, GC_SEM_INIT_PSHARED, 0) != 0)
        ABORT("sem_init failed");
#ifdef SA_RESTART
      act.sa_flags = SA_RESTART
#else
      act.sa_flags = 0
#endif
#ifndef NO_SA_SIGACTION
                     | SA_SIGINFO
#endif
        ;
    if (sigfillset(&act.sa_mask) != 0) {
        ABORT("sigfillset failed");
    }
#ifdef GC_RTEMS_PTHREADS
      if(sigprocmask(SIG_UNBLOCK, &act.sa_mask, NULL) != 0) {
        ABORT("sigprocmask failed");
      }
#endif
    GC_remove_allowed_signals(&act.sa_mask);
#ifndef NO_SA_SIGACTION
      act.sa_sigaction = GC_suspend_handler;
#else
      act.sa_handler = GC_suspend_handler;
#endif
    if (sigaction(GC_sig_suspend, &act, NULL) != 0) {
        ABORT("Cannot set SIG_SUSPEND handler");
    }
#ifndef NO_SA_SIGACTION
      act.sa_flags &= ~SA_SIGINFO;
#endif
    act.sa_handler = GC_restart_handler;
    if (sigaction(GC_sig_thr_restart, &act, NULL) != 0) {
        ABORT("Cannot set SIG_THR_RESTART handler");
    }
    if (sigfillset(&suspend_handler_mask) != 0) ABORT("sigfillset failed");
    GC_remove_allowed_signals(&suspend_handler_mask);
    if (sigdelset(&suspend_handler_mask, GC_sig_thr_restart) != 0)
        ABORT("sigdelset failed");
    str = GETENV("GC_RETRY_SIGNALS");
    if (str != NULL) {
        if (*str == '0' && *(str + 1) == '\0') {
            GC_retry_signals = FALSE;
        } else {
            GC_retry_signals = TRUE;
        }
    }
    if (GC_retry_signals) {
      GC_COND_LOG_PRINTF(
                "Will retry suspend and restart signals if necessary\n");
    }
#ifndef NO_SIGNALS_UNBLOCK_IN_MAIN
      GC_unblock_gc_signals();
#endif
#endif
}
#endif
#endif
#if defined(GC_PTHREADS) && !defined(GC_WIN32_THREADS)
#include <stdlib.h>
#include <pthread.h>
#include <sched.h>
#include <time.h>
#include <errno.h>
#include <unistd.h>
#if !defined(SN_TARGET_ORBIS) && !defined(SN_TARGET_PSP2)
#if !defined(GC_RTEMS_PTHREADS)
#include <sys/mman.h>
#endif
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#endif
#include <signal.h>
#if defined(GC_DARWIN_THREADS)
#ifndef GC_DARWIN_SEMAPHORE_H
#define GC_DARWIN_SEMAPHORE_H
#if !defined(GC_DARWIN_THREADS)
#error darwin_semaphore.h included with GC_DARWIN_THREADS not defined
#endif
#ifdef __cplusplus
  extern "C" {
#endif
typedef struct {
    pthread_mutex_t mutex;
    pthread_cond_t cond;
    int value;
} sem_t;
GC_INLINE int sem_init(sem_t *sem, int pshared, int value) {
    if (pshared != 0) {
        errno = EPERM;
        return -1;
    }
    sem->value = value;
    if (pthread_mutex_init(&sem->mutex, NULL) != 0)
      return -1;
    if (pthread_cond_init(&sem->cond, NULL) != 0) {
      (void)pthread_mutex_destroy(&sem->mutex);
      return -1;
    }
    return 0;
}
GC_INLINE int sem_post(sem_t *sem) {
    if (pthread_mutex_lock(&sem->mutex) != 0)
      return -1;
    sem->value++;
    if (pthread_cond_signal(&sem->cond) != 0) {
      (void)pthread_mutex_unlock(&sem->mutex);
      return -1;
    }
    return pthread_mutex_unlock(&sem->mutex) != 0 ? -1 : 0;
}
GC_INLINE int sem_wait(sem_t *sem) {
    if (pthread_mutex_lock(&sem->mutex) != 0)
      return -1;
    while (sem->value == 0) {
        if (pthread_cond_wait(&sem->cond, &sem->mutex) != 0) {
            (void)pthread_mutex_unlock(&sem->mutex);
            return -1;
        }
    }
    sem->value--;
    return pthread_mutex_unlock(&sem->mutex) != 0 ? -1 : 0;
}
GC_INLINE int sem_destroy(sem_t *sem) {
    return pthread_cond_destroy(&sem->cond) != 0
           || pthread_mutex_destroy(&sem->mutex) != 0 ? -1 : 0;
}
#ifdef __cplusplus
  }
#endif
#endif
#else
#include <semaphore.h>
#endif
#if defined(GC_DARWIN_THREADS) || defined(GC_FREEBSD_THREADS)
#include <sys/sysctl.h>
#endif
#if defined(GC_NETBSD_THREADS) || defined(GC_OPENBSD_THREADS)
#include <sys/param.h>
#include <sys/sysctl.h>
#endif
#if !defined(USE_SPIN_LOCK)
  GC_INNER pthread_mutex_t GC_allocate_ml = PTHREAD_MUTEX_INITIALIZER;
#endif
#ifdef GC_ASSERTIONS
  GC_INNER unsigned long GC_lock_holder = NO_THREAD;
#endif
#if defined(GC_DGUX386_THREADS)
#include <sys/dg_sys_info.h>
#include <sys/_int_psem.h>
  typedef unsigned int sem_t;
#endif
#undef pthread_create
#ifndef GC_NO_PTHREAD_SIGMASK
#undef pthread_sigmask
#endif
#ifndef GC_NO_PTHREAD_CANCEL
#undef pthread_cancel
#endif
#ifdef GC_HAVE_PTHREAD_EXIT
#undef pthread_exit
#endif
#undef pthread_join
#undef pthread_detach
#if defined(GC_OSF1_THREADS) && defined(_PTHREAD_USE_MANGLED_NAMES_) \
     && !defined(_PTHREAD_USE_PTDNAM_)
#define pthread_create __pthread_create
#define pthread_join __pthread_join
#define pthread_detach __pthread_detach
#ifndef GC_NO_PTHREAD_CANCEL
#define pthread_cancel __pthread_cancel
#endif
#ifdef GC_HAVE_PTHREAD_EXIT
#define pthread_exit __pthread_exit
#endif
#endif
#ifdef GC_USE_LD_WRAP
#define WRAP_FUNC(f) __wrap_##f
#define REAL_FUNC(f) __real_##f
    int REAL_FUNC(pthread_create)(pthread_t *,
                                  GC_PTHREAD_CREATE_CONST pthread_attr_t *,
                                  void *(*start_routine)(void *), void *);
    int REAL_FUNC(pthread_join)(pthread_t, void **);
    int REAL_FUNC(pthread_detach)(pthread_t);
#ifndef GC_NO_PTHREAD_SIGMASK
      int REAL_FUNC(pthread_sigmask)(int, const sigset_t *, sigset_t *);
#endif
#ifndef GC_NO_PTHREAD_CANCEL
      int REAL_FUNC(pthread_cancel)(pthread_t);
#endif
#ifdef GC_HAVE_PTHREAD_EXIT
      void REAL_FUNC(pthread_exit)(void *) GC_PTHREAD_EXIT_ATTRIBUTE;
#endif
#else
#ifdef GC_USE_DLOPEN_WRAP
#include <dlfcn.h>
#define WRAP_FUNC(f) f
#define REAL_FUNC(f) GC_real_##f
      typedef int (* GC_pthread_create_t)(pthread_t *,
                                    GC_PTHREAD_CREATE_CONST pthread_attr_t *,
                                    void * (*)(void *), void *);
      static GC_pthread_create_t REAL_FUNC(pthread_create);
#ifndef GC_NO_PTHREAD_SIGMASK
        typedef int (* GC_pthread_sigmask_t)(int, const sigset_t *,
                                             sigset_t *);
        static GC_pthread_sigmask_t REAL_FUNC(pthread_sigmask);
#endif
      typedef int (* GC_pthread_join_t)(pthread_t, void **);
      static GC_pthread_join_t REAL_FUNC(pthread_join);
      typedef int (* GC_pthread_detach_t)(pthread_t);
      static GC_pthread_detach_t REAL_FUNC(pthread_detach);
#ifndef GC_NO_PTHREAD_CANCEL
        typedef int (* GC_pthread_cancel_t)(pthread_t);
        static GC_pthread_cancel_t REAL_FUNC(pthread_cancel);
#endif
#ifdef GC_HAVE_PTHREAD_EXIT
        typedef void (* GC_pthread_exit_t)(void *) GC_PTHREAD_EXIT_ATTRIBUTE;
        static GC_pthread_exit_t REAL_FUNC(pthread_exit);
#endif
#else
#define WRAP_FUNC(f) GC_##f
#if !defined(GC_DGUX386_THREADS)
#define REAL_FUNC(f) f
#else
#define REAL_FUNC(f) __d10_##f
#endif
#endif
#endif
#if defined(GC_USE_LD_WRAP) || defined(GC_USE_DLOPEN_WRAP)
  GC_API int GC_pthread_create(pthread_t * t,
                               GC_PTHREAD_CREATE_CONST pthread_attr_t *a,
                               void * (* fn)(void *), void * arg)
  {
    return pthread_create(t, a, fn, arg);
  }
#ifndef GC_NO_PTHREAD_SIGMASK
    GC_API int GC_pthread_sigmask(int how, const sigset_t *mask,
                                  sigset_t *old)
    {
      return pthread_sigmask(how, mask, old);
    }
#endif
  GC_API int GC_pthread_join(pthread_t t, void **res)
  {
    return pthread_join(t, res);
  }
  GC_API int GC_pthread_detach(pthread_t t)
  {
    return pthread_detach(t);
  }
#ifndef GC_NO_PTHREAD_CANCEL
    GC_API int GC_pthread_cancel(pthread_t t)
    {
      return pthread_cancel(t);
    }
#endif
#ifdef GC_HAVE_PTHREAD_EXIT
    GC_API GC_PTHREAD_EXIT_ATTRIBUTE void GC_pthread_exit(void *retval)
    {
      pthread_exit(retval);
    }
#endif
#endif
#ifdef GC_USE_DLOPEN_WRAP
  STATIC GC_bool GC_syms_initialized = FALSE;
  STATIC void GC_init_real_syms(void)
  {
    void *dl_handle;
    if (GC_syms_initialized) return;
#ifdef RTLD_NEXT
      dl_handle = RTLD_NEXT;
#else
      dl_handle = dlopen("libpthread.so.0", RTLD_LAZY);
      if (NULL == dl_handle) {
        dl_handle = dlopen("libpthread.so", RTLD_LAZY);
      }
      if (NULL == dl_handle) ABORT("Couldn't open libpthread");
#endif
    REAL_FUNC(pthread_create) = (GC_pthread_create_t)(word)
                                dlsym(dl_handle, "pthread_create");
#ifdef RTLD_NEXT
      if (REAL_FUNC(pthread_create) == 0)
        ABORT("pthread_create not found"
              " (probably -lgc is specified after -lpthread)");
#endif
#ifndef GC_NO_PTHREAD_SIGMASK
      REAL_FUNC(pthread_sigmask) = (GC_pthread_sigmask_t)(word)
                                dlsym(dl_handle, "pthread_sigmask");
#endif
    REAL_FUNC(pthread_join) = (GC_pthread_join_t)(word)
                                dlsym(dl_handle, "pthread_join");
    REAL_FUNC(pthread_detach) = (GC_pthread_detach_t)(word)
                                  dlsym(dl_handle, "pthread_detach");
#ifndef GC_NO_PTHREAD_CANCEL
      REAL_FUNC(pthread_cancel) = (GC_pthread_cancel_t)(word)
                                    dlsym(dl_handle, "pthread_cancel");
#endif
#ifdef GC_HAVE_PTHREAD_EXIT
      REAL_FUNC(pthread_exit) = (GC_pthread_exit_t)(word)
                                  dlsym(dl_handle, "pthread_exit");
#endif
    GC_syms_initialized = TRUE;
  }
#define INIT_REAL_SYMS() if (EXPECT(GC_syms_initialized, TRUE)) {} \
                            else GC_init_real_syms()
#define ASSERT_SYMS_INITIALIZED() GC_ASSERT(GC_syms_initialized)
#else
#define INIT_REAL_SYMS() (void)0
#define ASSERT_SYMS_INITIALIZED() GC_ASSERT(parallel_initialized)
#endif
static GC_bool parallel_initialized = FALSE;
#ifndef GC_ALWAYS_MULTITHREADED
  GC_INNER GC_bool GC_need_to_lock = FALSE;
#endif
STATIC int GC_nprocs = 1;
#ifdef THREAD_LOCAL_ALLOC
  GC_INNER void GC_mark_thread_local_free_lists(void)
  {
    int i;
    GC_thread p;
    for (i = 0; i < THREAD_TABLE_SZ; ++i) {
      for (p = GC_threads[i]; 0 != p; p = p -> next) {
        if (!(p -> flags & FINISHED))
          GC_mark_thread_local_fls_for(&(p->tlfs));
      }
    }
  }
#if defined(GC_ASSERTIONS)
    void GC_check_tls(void)
    {
        int i;
        GC_thread p;
        for (i = 0; i < THREAD_TABLE_SZ; ++i) {
          for (p = GC_threads[i]; 0 != p; p = p -> next) {
            if (!(p -> flags & FINISHED))
              GC_check_tls_for(&(p->tlfs));
          }
        }
#if defined(USE_CUSTOM_SPECIFIC)
          if (GC_thread_key != 0)
            GC_check_tsd_marks(GC_thread_key);
#endif
    }
#endif
#endif
#ifndef MAX_MARKERS
#define MAX_MARKERS 16
#endif
#ifdef PARALLEL_MARK
static ptr_t marker_sp[MAX_MARKERS - 1] = {0};
#ifdef IA64
  static ptr_t marker_bsp[MAX_MARKERS - 1] = {0};
#endif
#if defined(GC_DARWIN_THREADS) && !defined(GC_NO_THREADS_DISCOVERY)
  static mach_port_t marker_mach_threads[MAX_MARKERS - 1] = {0};
  GC_INNER GC_bool GC_is_mach_marker(thread_act_t thread)
  {
    int i;
    for (i = 0; i < GC_markers_m1; i++) {
      if (marker_mach_threads[i] == thread)
        return TRUE;
    }
    return FALSE;
  }
#endif
#ifdef HAVE_PTHREAD_SETNAME_NP_WITH_TID_AND_ARG
  static void set_marker_thread_name(unsigned id)
  {
    int err = pthread_setname_np(pthread_self(), "GC-marker-%zu",
                                 (void*)(size_t)id);
    if (err != 0)
      WARN("pthread_setname_np failed, errno= %" WARN_PRIdPTR "\n", err);
  }
#elif defined(HAVE_PTHREAD_SETNAME_NP_WITH_TID) \
      || defined(HAVE_PTHREAD_SETNAME_NP_WITHOUT_TID)
  static void set_marker_thread_name(unsigned id)
  {
    char name_buf[16];
    int len = sizeof("GC-marker-") - 1;
    BCOPY("GC-marker-", name_buf, len);
    if (id >= 10)
      name_buf[len++] = (char)('0' + (id / 10) % 10);
    name_buf[len] = (char)('0' + id % 10);
    name_buf[len + 1] = '\0';
#ifdef HAVE_PTHREAD_SETNAME_NP_WITHOUT_TID
      (void)pthread_setname_np(name_buf);
#else
      if (pthread_setname_np(pthread_self(), name_buf) != 0)
        WARN("pthread_setname_np failed\n", 0);
#endif
  }
#else
#define set_marker_thread_name(id) (void)(id)
#endif
STATIC void * GC_mark_thread(void * id)
{
  word my_mark_no = 0;
  IF_CANCEL(int cancel_state;)
  if ((word)id == GC_WORD_MAX) return 0;
  DISABLE_CANCEL(cancel_state);
  set_marker_thread_name((unsigned)(word)id);
  marker_sp[(word)id] = GC_approx_sp();
#ifdef IA64
    marker_bsp[(word)id] = GC_save_regs_in_stack();
#endif
#if defined(GC_DARWIN_THREADS) && !defined(GC_NO_THREADS_DISCOVERY)
    marker_mach_threads[(word)id] = mach_thread_self();
#endif
  GC_acquire_mark_lock();
  if (0 == --GC_fl_builder_count)
    GC_notify_all_builder();
  for (;; ++my_mark_no) {
    if (my_mark_no < GC_mark_no || my_mark_no > GC_mark_no + 2) {
        my_mark_no = GC_mark_no;
    }
#ifdef DEBUG_THREADS
      GC_log_printf("Starting mark helper for mark number %lu\n",
                    (unsigned long)my_mark_no);
#endif
    GC_help_marker(my_mark_no);
  }
}
STATIC pthread_t GC_mark_threads[MAX_MARKERS];
#ifdef CAN_HANDLE_FORK
  static int available_markers_m1 = 0;
  static pthread_cond_t mark_cv;
#else
#define available_markers_m1 GC_markers_m1
  static pthread_cond_t mark_cv = PTHREAD_COND_INITIALIZER;
#endif
GC_INNER void GC_start_mark_threads_inner(void)
{
    int i;
    pthread_attr_t attr;
#ifndef NO_MARKER_SPECIAL_SIGMASK
      sigset_t set, oldset;
#endif
    GC_ASSERT(I_DONT_HOLD_LOCK());
    if (available_markers_m1 <= 0) return;
#ifdef CAN_HANDLE_FORK
      if (GC_parallel) return;
      {
        pthread_cond_t mark_cv_local = PTHREAD_COND_INITIALIZER;
        BCOPY(&mark_cv_local, &mark_cv, sizeof(mark_cv));
      }
#endif
    GC_ASSERT(GC_fl_builder_count == 0);
    INIT_REAL_SYMS();
    if (0 != pthread_attr_init(&attr)) ABORT("pthread_attr_init failed");
    if (0 != pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED))
        ABORT("pthread_attr_setdetachstate failed");
#ifdef DEFAULT_STACK_MAYBE_SMALL
      {
        size_t old_size;
        if (pthread_attr_getstacksize(&attr, &old_size) != 0)
          ABORT("pthread_attr_getstacksize failed");
        if (old_size < MIN_STACK_SIZE
            && old_size != 0 ) {
          if (pthread_attr_setstacksize(&attr, MIN_STACK_SIZE) != 0)
            ABORT("pthread_attr_setstacksize failed");
        }
      }
#endif
#ifndef NO_MARKER_SPECIAL_SIGMASK
      if (sigfillset(&set) != 0)
        ABORT("sigfillset failed");
#if !defined(GC_DARWIN_THREADS) && !defined(GC_OPENBSD_UTHREADS) \
         && !defined(NACL)
        if (sigdelset(&set, GC_get_suspend_signal()) != 0
            || sigdelset(&set, GC_get_thr_restart_signal()) != 0)
          ABORT("sigdelset failed");
#endif
      if (REAL_FUNC(pthread_sigmask)(SIG_BLOCK, &set, &oldset) < 0) {
        WARN("pthread_sigmask set failed, no markers started,"
             " errno= %" WARN_PRIdPTR "\n", errno);
        GC_markers_m1 = 0;
        (void)pthread_attr_destroy(&attr);
        return;
      }
#endif
#ifdef CAN_HANDLE_FORK
      GC_markers_m1 = available_markers_m1;
#endif
    for (i = 0; i < available_markers_m1; ++i) {
      if (0 != REAL_FUNC(pthread_create)(GC_mark_threads + i, &attr,
                              GC_mark_thread, (void *)(word)i)) {
        WARN("Marker thread creation failed, errno= %" WARN_PRIdPTR "\n",
             errno);
        GC_markers_m1 = i;
        break;
      }
    }
#ifndef NO_MARKER_SPECIAL_SIGMASK
      if (REAL_FUNC(pthread_sigmask)(SIG_SETMASK, &oldset, NULL) < 0) {
        WARN("pthread_sigmask restore failed, errno= %" WARN_PRIdPTR "\n",
             errno);
      }
#endif
    (void)pthread_attr_destroy(&attr);
    GC_wait_for_markers_init();
    GC_COND_LOG_PRINTF("Started %d mark helper threads\n", GC_markers_m1);
}
#endif
GC_INNER GC_bool GC_thr_initialized = FALSE;
GC_INNER volatile GC_thread GC_threads[THREAD_TABLE_SZ] = {0};
void GC_push_thread_structures(void)
{
    GC_ASSERT(I_HOLD_LOCK());
    GC_PUSH_ALL_SYM(GC_threads);
#if defined(THREAD_LOCAL_ALLOC)
      GC_PUSH_ALL_SYM(GC_thread_key);
#endif
}
#ifdef DEBUG_THREADS
  STATIC int GC_count_threads(void)
  {
    int i;
    int count = 0;
    GC_ASSERT(I_HOLD_LOCK());
    for (i = 0; i < THREAD_TABLE_SZ; ++i) {
        GC_thread th = GC_threads[i];
        while (th) {
            if (!(th->flags & FINISHED))
                ++count;
            th = th->next;
        }
    }
    return count;
  }
#endif
static struct GC_Thread_Rep first_thread;
STATIC GC_thread GC_new_thread(pthread_t id)
{
    int hv = THREAD_TABLE_INDEX(id);
    GC_thread result;
    static GC_bool first_thread_used = FALSE;
#ifdef DEBUG_THREADS
        GC_log_printf("Creating thread %p\n", (void *)id);
        for (result = GC_threads[hv]; result != NULL; result = result->next)
          if (!THREAD_EQUAL(result->id, id)) {
            GC_log_printf("Hash collision at GC_threads[%d]\n", hv);
            break;
          }
#endif
    GC_ASSERT(I_HOLD_LOCK());
    if (!EXPECT(first_thread_used, TRUE)) {
        result = &first_thread;
        first_thread_used = TRUE;
        GC_ASSERT(NULL == GC_threads[hv]);
#if defined(THREAD_SANITIZER) && defined(CPPCHECK)
          GC_noop1(result->dummy[0]);
#endif
    } else {
        result = (struct GC_Thread_Rep *)
                 GC_INTERNAL_MALLOC(sizeof(struct GC_Thread_Rep), NORMAL);
        if (result == 0) return(0);
    }
    result -> id = id;
#ifdef USE_TKILL_ON_ANDROID
      result -> kernel_id = gettid();
#endif
    result -> next = GC_threads[hv];
    GC_threads[hv] = result;
#ifdef NACL
      GC_nacl_gc_thread_self = result;
      GC_nacl_initialize_gc_thread();
#endif
    GC_ASSERT(result -> flags == 0 && result -> thread_blocked == 0);
    if (EXPECT(result != &first_thread, TRUE))
      GC_dirty(result);
    return(result);
}
STATIC void GC_delete_thread(pthread_t id)
{
    int hv = THREAD_TABLE_INDEX(id);
    GC_thread p = GC_threads[hv];
    GC_thread prev = NULL;
#ifdef DEBUG_THREADS
      GC_log_printf("Deleting thread %p, n_threads= %d\n",
                    (void *)id, GC_count_threads());
#endif
#ifdef NACL
      GC_nacl_shutdown_gc_thread();
      GC_nacl_gc_thread_self = NULL;
#endif
    GC_ASSERT(I_HOLD_LOCK());
    while (!THREAD_EQUAL(p -> id, id)) {
        prev = p;
        p = p -> next;
    }
    if (prev == 0) {
        GC_threads[hv] = p -> next;
    } else {
        GC_ASSERT(prev != &first_thread);
        prev -> next = p -> next;
        GC_dirty(prev);
    }
    if (p != &first_thread) {
#ifdef GC_DARWIN_THREADS
        mach_port_deallocate(mach_task_self(), p->stop_info.mach_thread);
#endif
      GC_INTERNAL_FREE(p);
    }
}
STATIC void GC_delete_gc_thread(GC_thread t)
{
    pthread_t id = t -> id;
    int hv = THREAD_TABLE_INDEX(id);
    GC_thread p = GC_threads[hv];
    GC_thread prev = NULL;
    GC_ASSERT(I_HOLD_LOCK());
    while (p != t) {
        prev = p;
        p = p -> next;
    }
    if (prev == 0) {
        GC_threads[hv] = p -> next;
    } else {
        GC_ASSERT(prev != &first_thread);
        prev -> next = p -> next;
        GC_dirty(prev);
    }
#ifdef GC_DARWIN_THREADS
        mach_port_deallocate(mach_task_self(), p->stop_info.mach_thread);
#endif
    GC_INTERNAL_FREE(p);
#ifdef DEBUG_THREADS
      GC_log_printf("Deleted thread %p, n_threads= %d\n",
                    (void *)id, GC_count_threads());
#endif
}
GC_INNER GC_thread GC_lookup_thread(pthread_t id)
{
    GC_thread p = GC_threads[THREAD_TABLE_INDEX(id)];
    while (p != 0 && !THREAD_EQUAL(p -> id, id)) p = p -> next;
    return(p);
}
GC_INNER void GC_reset_finalizer_nested(void)
{
  GC_thread me = GC_lookup_thread(pthread_self());
  me->finalizer_nested = 0;
}
GC_INNER unsigned char *GC_check_finalizer_nested(void)
{
  GC_thread me = GC_lookup_thread(pthread_self());
  unsigned nesting_level = me->finalizer_nested;
  if (nesting_level) {
    if (++me->finalizer_skipped < (1U << nesting_level)) return NULL;
    me->finalizer_skipped = 0;
  }
  me->finalizer_nested = (unsigned char)(nesting_level + 1);
  return &me->finalizer_nested;
}
#if defined(GC_ASSERTIONS) && defined(THREAD_LOCAL_ALLOC)
  GC_bool GC_is_thread_tsd_valid(void *tsd)
  {
    GC_thread me;
    DCL_LOCK_STATE;
    LOCK();
    me = GC_lookup_thread(pthread_self());
    UNLOCK();
    return (word)tsd >= (word)(&me->tlfs)
            && (word)tsd < (word)(&me->tlfs) + sizeof(me->tlfs);
  }
#endif
GC_API int GC_CALL GC_thread_is_registered(void)
{
    pthread_t self = pthread_self();
    GC_thread me;
    DCL_LOCK_STATE;
    LOCK();
    me = GC_lookup_thread(self);
    UNLOCK();
    return me != NULL;
}
static pthread_t main_pthread_id;
static void *main_stack, *main_altstack;
static word main_stack_size, main_altstack_size;
GC_API void GC_CALL GC_register_altstack(void *stack, GC_word stack_size,
                                         void *altstack,
                                         GC_word altstack_size)
{
  GC_thread me;
  pthread_t self = pthread_self();
  DCL_LOCK_STATE;
  LOCK();
  me = GC_lookup_thread(self);
  if (me != NULL) {
    me->stack = (ptr_t)stack;
    me->stack_size = stack_size;
    me->altstack = (ptr_t)altstack;
    me->altstack_size = altstack_size;
  } else {
    main_pthread_id = self;
    main_stack = stack;
    main_stack_size = stack_size;
    main_altstack = altstack;
    main_altstack_size = altstack_size;
  }
  UNLOCK();
}
#ifdef CAN_HANDLE_FORK
#ifdef CAN_CALL_ATFORK
    GC_ATTR_NO_SANITIZE_THREAD
#endif
  static void store_to_threads_table(int hv, GC_thread me)
  {
    GC_threads[hv] = me;
  }
STATIC void GC_remove_all_threads_but_me(void)
{
    pthread_t self = pthread_self();
    int hv;
    for (hv = 0; hv < THREAD_TABLE_SZ; ++hv) {
      GC_thread p, next;
      GC_thread me = NULL;
      for (p = GC_threads[hv]; 0 != p; p = next) {
        next = p -> next;
        if (THREAD_EQUAL(p -> id, self)
            && me == NULL) {
          me = p;
          p -> next = 0;
#ifdef GC_DARWIN_THREADS
            me -> stop_info.mach_thread = mach_thread_self();
#endif
#ifdef USE_TKILL_ON_ANDROID
            me -> kernel_id = gettid();
#endif
#if defined(THREAD_LOCAL_ALLOC) && !defined(USE_CUSTOM_SPECIFIC)
          {
            int res;
            res = GC_setspecific(GC_thread_key, &me->tlfs);
            if (COVERT_DATAFLOW(res) != 0)
              ABORT("GC_setspecific failed (in child)");
          }
#endif
        } else {
#ifdef THREAD_LOCAL_ALLOC
            if (!(p -> flags & FINISHED)) {
              GC_remove_specific_after_fork(GC_thread_key, p -> id);
            }
#endif
#if !defined(THREAD_SANITIZER) || !defined(CAN_CALL_ATFORK)
            if (p != &first_thread) GC_INTERNAL_FREE(p);
#endif
        }
      }
      store_to_threads_table(hv, me);
    }
}
#endif
#ifdef USE_PROC_FOR_LIBRARIES
  GC_INNER GC_bool GC_segment_is_thread_stack(ptr_t lo, ptr_t hi)
  {
    int i;
    GC_thread p;
    GC_ASSERT(I_HOLD_LOCK());
#ifdef PARALLEL_MARK
      for (i = 0; i < GC_markers_m1; ++i) {
        if ((word)marker_sp[i] > (word)lo && (word)marker_sp[i] < (word)hi)
          return TRUE;
#ifdef IA64
          if ((word)marker_bsp[i] > (word)lo
              && (word)marker_bsp[i] < (word)hi)
            return TRUE;
#endif
      }
#endif
    for (i = 0; i < THREAD_TABLE_SZ; i++) {
      for (p = GC_threads[i]; p != 0; p = p -> next) {
        if (0 != p -> stack_end) {
#ifdef STACK_GROWS_UP
            if ((word)p->stack_end >= (word)lo
                && (word)p->stack_end < (word)hi)
              return TRUE;
#else
            if ((word)p->stack_end > (word)lo
                && (word)p->stack_end <= (word)hi)
              return TRUE;
#endif
        }
      }
    }
    return FALSE;
  }
#endif
#ifdef IA64
  GC_INNER ptr_t GC_greatest_stack_base_below(ptr_t bound)
  {
    int i;
    GC_thread p;
    ptr_t result = 0;
    GC_ASSERT(I_HOLD_LOCK());
#ifdef PARALLEL_MARK
      for (i = 0; i < GC_markers_m1; ++i) {
        if ((word)marker_sp[i] > (word)result
            && (word)marker_sp[i] < (word)bound)
          result = marker_sp[i];
      }
#endif
    for (i = 0; i < THREAD_TABLE_SZ; i++) {
      for (p = GC_threads[i]; p != 0; p = p -> next) {
        if ((word)p->stack_end > (word)result
            && (word)p->stack_end < (word)bound) {
          result = p -> stack_end;
        }
      }
    }
    return result;
  }
#endif
#ifndef STAT_READ
#define STAT_READ read
#endif
#ifdef GC_HPUX_THREADS
#define GC_get_nprocs() pthread_num_processors_np()
#elif defined(GC_OSF1_THREADS) || defined(GC_AIX_THREADS) \
      || defined(GC_HAIKU_THREADS) || defined(GC_SOLARIS_THREADS) \
      || defined(HURD) || defined(HOST_ANDROID) || defined(NACL)
  GC_INLINE int GC_get_nprocs(void)
  {
    int nprocs = (int)sysconf(_SC_NPROCESSORS_ONLN);
    return nprocs > 0 ? nprocs : 1;
  }
#elif defined(GC_IRIX_THREADS)
  GC_INLINE int GC_get_nprocs(void)
  {
    int nprocs = (int)sysconf(_SC_NPROC_ONLN);
    return nprocs > 0 ? nprocs : 1;
  }
#elif defined(GC_LINUX_THREADS)
  STATIC int GC_get_nprocs(void)
  {
#define PROC_STAT_BUF_SZ ((1 + MAX_MARKERS) * 100)
    char stat_buf[PROC_STAT_BUF_SZ+1];
    int f;
    int result, i, len;
    f = open("/proc/stat", O_RDONLY);
    if (f < 0) {
      WARN("Could not open /proc/stat\n", 0);
      return 1;
    }
    len = STAT_READ(f, stat_buf, sizeof(stat_buf)-1);
    if (len < 0) {
      WARN("Failed to read /proc/stat, errno= %" WARN_PRIdPTR "\n", errno);
      close(f);
      return 1;
    }
    stat_buf[len] = '\0';
    close(f);
    result = 1;
    for (i = 0; i < len - 4; ++i) {
      if (stat_buf[i] == '\n' && stat_buf[i+1] == 'c'
          && stat_buf[i+2] == 'p' && stat_buf[i+3] == 'u') {
        int cpu_no = atoi(&stat_buf[i + 4]);
        if (cpu_no >= result)
          result = cpu_no + 1;
      }
    }
    return result;
  }
#elif defined(GC_DGUX386_THREADS)
  STATIC int GC_get_nprocs(void)
  {
    int numCpus;
    struct dg_sys_info_pm_info pm_sysinfo;
    int status = 0;
    status = dg_sys_info((long int *) &pm_sysinfo,
        DG_SYS_INFO_PM_INFO_TYPE, DG_SYS_INFO_PM_CURRENT_VERSION);
    if (status < 0)
       numCpus = -1;
    else
      numCpus = pm_sysinfo.idle_vp_count;
    return(numCpus);
  }
#elif defined(GC_DARWIN_THREADS) || defined(GC_FREEBSD_THREADS) \
      || defined(GC_NETBSD_THREADS) || defined(GC_OPENBSD_THREADS)
  STATIC int GC_get_nprocs(void)
  {
    int mib[] = {CTL_HW,HW_NCPU};
    int res;
    size_t len = sizeof(res);
    sysctl(mib, sizeof(mib)/sizeof(int), &res, &len, NULL, 0);
    return res;
  }
#else
#define GC_get_nprocs() 1
#endif
#if defined(ARM32) && defined(GC_LINUX_THREADS) && !defined(NACL)
  STATIC int GC_get_nprocs_present(void)
  {
    char stat_buf[16];
    int f;
    int len;
    f = open("/sys/devices/system/cpu/present", O_RDONLY);
    if (f < 0)
      return -1;
    len = STAT_READ(f, stat_buf, sizeof(stat_buf));
    close(f);
    if (len < 2 || stat_buf[0] != '0' || stat_buf[len - 1] != '\n') {
      return 0;
    } else if (len == 2) {
      return 1;
    } else if (stat_buf[1] != '-') {
      return 0;
    }
    stat_buf[len - 1] = '\0';
    return atoi(&stat_buf[2]) + 1;
  }
#endif
STATIC void GC_wait_for_gc_completion(GC_bool wait_for_all)
{
    DCL_LOCK_STATE;
#if !defined(THREAD_SANITIZER) || !defined(CAN_CALL_ATFORK)
      GC_ASSERT(I_HOLD_LOCK());
#endif
    ASSERT_CANCEL_DISABLED();
    if (GC_incremental && GC_collection_in_progress()) {
        word old_gc_no = GC_gc_no;
        while (GC_incremental && GC_collection_in_progress()
               && (wait_for_all || old_gc_no == GC_gc_no)) {
            ENTER_GC();
            GC_in_thread_creation = TRUE;
            GC_collect_a_little_inner(1);
            GC_in_thread_creation = FALSE;
            EXIT_GC();
            UNLOCK();
            sched_yield();
            LOCK();
        }
    }
}
#ifdef CAN_HANDLE_FORK
IF_CANCEL(static int fork_cancel_state;)
#if defined(GC_ASSERTIONS) && defined(CAN_CALL_ATFORK)
  GC_ATTR_NO_SANITIZE_THREAD
#endif
static void fork_prepare_proc(void)
{
      LOCK();
      DISABLE_CANCEL(fork_cancel_state);
#if defined(PARALLEL_MARK)
        if (GC_parallel)
          GC_wait_for_reclaim();
#endif
      GC_wait_for_gc_completion(TRUE);
#if defined(PARALLEL_MARK)
        if (GC_parallel)
          GC_acquire_mark_lock();
#endif
      GC_acquire_dirty_lock();
}
#if defined(GC_ASSERTIONS) && defined(CAN_CALL_ATFORK)
  GC_ATTR_NO_SANITIZE_THREAD
#endif
static void fork_parent_proc(void)
{
    GC_release_dirty_lock();
#if defined(PARALLEL_MARK)
      if (GC_parallel)
        GC_release_mark_lock();
#endif
    RESTORE_CANCEL(fork_cancel_state);
    UNLOCK();
}
#if defined(GC_ASSERTIONS) && defined(CAN_CALL_ATFORK)
  GC_ATTR_NO_SANITIZE_THREAD
#endif
static void fork_child_proc(void)
{
    GC_release_dirty_lock();
#if defined(PARALLEL_MARK)
      if (GC_parallel)
        GC_release_mark_lock();
#endif
    GC_remove_all_threads_but_me();
#ifdef PARALLEL_MARK
        GC_parallel = FALSE;
#endif
#ifndef GC_DISABLE_INCREMENTAL
      GC_dirty_update_child();
#endif
    RESTORE_CANCEL(fork_cancel_state);
    UNLOCK();
#ifdef USE_PTHREAD_LOCKS
      GC_ASSERT(I_DONT_HOLD_LOCK());
      (void)pthread_mutex_destroy(&GC_allocate_ml);
      if (0 != pthread_mutex_init(&GC_allocate_ml, NULL))
        ABORT("pthread_mutex_init failed (in child)");
#endif
}
  GC_API void GC_CALL GC_atfork_prepare(void)
  {
    if (!EXPECT(GC_is_initialized, TRUE)) GC_init();
#if defined(GC_DARWIN_THREADS) && defined(MPROTECT_VDB)
      if (GC_auto_incremental) {
        GC_ASSERT(0 == GC_handle_fork);
        ABORT("Unable to fork while mprotect_thread is running");
      }
#endif
    if (GC_handle_fork <= 0)
      fork_prepare_proc();
  }
  GC_API void GC_CALL GC_atfork_parent(void)
  {
    if (GC_handle_fork <= 0)
      fork_parent_proc();
  }
  GC_API void GC_CALL GC_atfork_child(void)
  {
    if (GC_handle_fork <= 0)
      fork_child_proc();
  }
#endif
#ifdef INCLUDE_LINUX_THREAD_DESCR
  __thread int GC_dummy_thread_local;
#endif
#ifdef PARALLEL_MARK
  static void setup_mark_lock(void);
  static unsigned required_markers_cnt = 0;
#endif
GC_API void GC_CALL GC_set_markers_count(unsigned markers GC_ATTR_UNUSED)
{
#ifdef PARALLEL_MARK
    required_markers_cnt = markers < MAX_MARKERS ? markers : MAX_MARKERS;
#endif
}
GC_INNER void GC_thr_init(void)
{
  GC_ASSERT(I_HOLD_LOCK());
  if (GC_thr_initialized) return;
  GC_thr_initialized = TRUE;
  GC_ASSERT((word)&GC_threads % sizeof(word) == 0);
#ifdef CAN_HANDLE_FORK
    if (GC_handle_fork) {
#ifdef CAN_CALL_ATFORK
        if (pthread_atfork(fork_prepare_proc, fork_parent_proc,
                           fork_child_proc) == 0) {
          GC_handle_fork = 1;
        } else
#endif
       if (GC_handle_fork != -1)
        ABORT("pthread_atfork failed");
    }
#endif
#ifdef INCLUDE_LINUX_THREAD_DESCR
    {
      ptr_t thread_local_addr = (ptr_t)(&GC_dummy_thread_local);
      ptr_t main_thread_start, main_thread_end;
      if (!GC_enclosing_mapping(thread_local_addr, &main_thread_start,
                                &main_thread_end)) {
        ABORT("Failed to find mapping for main thread thread locals");
      } else {
        GC_add_roots_inner(main_thread_start, main_thread_end, FALSE);
      }
    }
#endif
  {
    pthread_t self = pthread_self();
    GC_thread t = GC_new_thread(self);
    if (t == NULL)
      ABORT("Failed to allocate memory for the initial thread");
#ifdef GC_DARWIN_THREADS
      t -> stop_info.mach_thread = mach_thread_self();
#else
      t -> stop_info.stack_ptr = GC_approx_sp();
#endif
    t -> flags = DETACHED | MAIN_THREAD;
    if (THREAD_EQUAL(self, main_pthread_id)) {
      t -> stack = (ptr_t)main_stack;
      t -> stack_size = main_stack_size;
      t -> altstack = (ptr_t)main_altstack;
      t -> altstack_size = main_altstack_size;
    }
  }
  {
    char * nprocs_string = GETENV("GC_NPROCS");
    GC_nprocs = -1;
    if (nprocs_string != NULL) GC_nprocs = atoi(nprocs_string);
  }
  if (GC_nprocs <= 0
#if defined(ARM32) && defined(GC_LINUX_THREADS) && !defined(NACL)
        && (GC_nprocs = GC_get_nprocs_present()) <= 1
#endif
      )
  {
    GC_nprocs = GC_get_nprocs();
  }
  if (GC_nprocs <= 0) {
    WARN("GC_get_nprocs() returned %" WARN_PRIdPTR "\n", GC_nprocs);
    GC_nprocs = 2;
#ifdef PARALLEL_MARK
      available_markers_m1 = 0;
#endif
  } else {
#ifdef PARALLEL_MARK
      {
        char * markers_string = GETENV("GC_MARKERS");
        int markers = required_markers_cnt;
        if (markers_string != NULL) {
          markers = atoi(markers_string);
          if (markers <= 0 || markers > MAX_MARKERS) {
            WARN("Too big or invalid number of mark threads: %" WARN_PRIdPTR
                 "; using maximum threads\n", (signed_word)markers);
            markers = MAX_MARKERS;
          }
        } else if (0 == markers) {
          markers = GC_nprocs;
#if defined(GC_MIN_MARKERS) && !defined(CPPCHECK)
            if (markers < GC_MIN_MARKERS)
              markers = GC_MIN_MARKERS;
#endif
          if (markers > MAX_MARKERS)
            markers = MAX_MARKERS;
        }
        available_markers_m1 = markers - 1;
      }
#endif
  }
  GC_COND_LOG_PRINTF("Number of processors: %d\n", GC_nprocs);
#if defined(BASE_ATOMIC_OPS_EMULATED) && !defined(GC_DARWIN_THREADS) \
     && !defined(GC_OPENBSD_UTHREADS) && !defined(NACL) \
     && !defined(PLATFORM_STOP_WORLD) && !defined(SN_TARGET_PSP2)
    {
      cpu_set_t mask;
      int cpu_set_cnt = 0;
      int cpu_lowest_set = 0;
      int i = GC_nprocs > 1 ? GC_nprocs : 2;
      if (sched_getaffinity(0 ,
                            sizeof(mask), &mask) == -1)
        ABORT_ARG1("sched_getaffinity failed", ": errno= %d", errno);
      while (i-- > 0)
        if (CPU_ISSET(i, &mask)) {
          cpu_lowest_set = i;
          cpu_set_cnt++;
        }
      if (0 == cpu_set_cnt)
        ABORT("sched_getaffinity returned empty mask");
      if (cpu_set_cnt > 1) {
        CPU_ZERO(&mask);
        CPU_SET(cpu_lowest_set, &mask);
        if (sched_setaffinity(0, sizeof(mask), &mask) == -1)
          ABORT_ARG1("sched_setaffinity failed", ": errno= %d", errno);
        WARN("CPU affinity mask is set to %p\n", (word)1 << cpu_lowest_set);
      }
    }
#endif
#ifndef GC_DARWIN_THREADS
    GC_stop_init();
#endif
#ifdef PARALLEL_MARK
    if (available_markers_m1 <= 0) {
      GC_parallel = FALSE;
      GC_COND_LOG_PRINTF(
                "Single marker thread, turning off parallel marking\n");
    } else {
      setup_mark_lock();
    }
#endif
}
GC_INNER void GC_init_parallel(void)
{
#if defined(THREAD_LOCAL_ALLOC)
      DCL_LOCK_STATE;
#endif
    if (parallel_initialized) return;
    parallel_initialized = TRUE;
    if (!GC_is_initialized) GC_init();
#if defined(THREAD_LOCAL_ALLOC)
      LOCK();
      GC_init_thread_local(&(GC_lookup_thread(pthread_self())->tlfs));
      UNLOCK();
#endif
}
#ifndef GC_NO_PTHREAD_SIGMASK
  GC_API int WRAP_FUNC(pthread_sigmask)(int how, const sigset_t *set,
                                        sigset_t *oset)
  {
    sigset_t fudged_set;
    INIT_REAL_SYMS();
    if (set != NULL && (how == SIG_BLOCK || how == SIG_SETMASK)) {
        int sig_suspend = GC_get_suspend_signal();
        fudged_set = *set;
        GC_ASSERT(sig_suspend >= 0);
        if (sigdelset(&fudged_set, sig_suspend) != 0)
            ABORT("sigdelset failed");
        set = &fudged_set;
    }
    return(REAL_FUNC(pthread_sigmask)(how, set, oset));
  }
#endif
GC_INNER void GC_do_blocking_inner(ptr_t data, void * context GC_ATTR_UNUSED)
{
    struct blocking_data * d = (struct blocking_data *) data;
    pthread_t self = pthread_self();
    GC_thread me;
#if defined(SPARC) || defined(IA64)
        ptr_t stack_ptr = GC_save_regs_in_stack();
#endif
#if defined(GC_DARWIN_THREADS) && !defined(DARWIN_DONT_PARSE_STACK)
        GC_bool topOfStackUnset = FALSE;
#endif
    DCL_LOCK_STATE;
    LOCK();
    me = GC_lookup_thread(self);
    GC_ASSERT(!(me -> thread_blocked));
#ifdef SPARC
        me -> stop_info.stack_ptr = stack_ptr;
#else
        me -> stop_info.stack_ptr = GC_approx_sp();
#endif
#if defined(GC_DARWIN_THREADS) && !defined(DARWIN_DONT_PARSE_STACK)
        if (me -> topOfStack == NULL) {
            topOfStackUnset = TRUE;
            me -> topOfStack = GC_FindTopOfStack(0);
        }
#endif
#ifdef IA64
        me -> backing_store_ptr = stack_ptr;
#endif
    me -> thread_blocked = (unsigned char)TRUE;
    UNLOCK();
    d -> client_data = (d -> fn)(d -> client_data);
    LOCK();
#if defined(CPPCHECK)
      GC_noop1((word)&me->thread_blocked);
#endif
    me -> thread_blocked = FALSE;
#if defined(GC_DARWIN_THREADS) && !defined(DARWIN_DONT_PARSE_STACK)
        if (topOfStackUnset)
            me -> topOfStack = NULL;
#endif
    UNLOCK();
}
GC_API void GC_CALL GC_set_stackbottom(void *gc_thread_handle,
                                       const struct GC_stack_base *sb)
{
    GC_thread t = (GC_thread)gc_thread_handle;
    GC_ASSERT(sb -> mem_base != NULL);
    if (!EXPECT(GC_is_initialized, TRUE)) {
        GC_ASSERT(NULL == t);
    } else {
        GC_ASSERT(I_HOLD_LOCK());
        if (NULL == t)
            t = GC_lookup_thread(pthread_self());
        GC_ASSERT((t -> flags & FINISHED) == 0);
        GC_ASSERT(!(t -> thread_blocked)
                  && NULL == t -> traced_stack_sect);
        if ((t -> flags & MAIN_THREAD) == 0) {
            t -> stack_end = (ptr_t)sb->mem_base;
#ifdef IA64
                t -> backing_store_end = (ptr_t)sb->reg_base;
#endif
            return;
        }
    }
    GC_stackbottom = (char*)sb->mem_base;
#ifdef IA64
        GC_register_stackbottom = (ptr_t)sb->reg_base;
#endif
}
GC_API void * GC_CALL GC_get_my_stackbottom(struct GC_stack_base *sb)
{
    pthread_t self = pthread_self();
    GC_thread me;
    DCL_LOCK_STATE;
    LOCK();
    me = GC_lookup_thread(self);
    if ((me -> flags & MAIN_THREAD) == 0) {
        sb -> mem_base = me -> stack_end;
#ifdef IA64
            sb -> reg_base = me -> backing_store_end;
#endif
    } else {
        sb -> mem_base = GC_stackbottom;
#ifdef IA64
            sb -> reg_base = GC_register_stackbottom;
#endif
    }
    UNLOCK();
    return (void *)me;
}
GC_API void * GC_CALL GC_call_with_gc_active(GC_fn_type fn,
                                             void * client_data)
{
    struct GC_traced_stack_sect_s stacksect;
    pthread_t self = pthread_self();
    GC_thread me;
    DCL_LOCK_STATE;
    LOCK();
    me = GC_lookup_thread(self);
    if ((me -> flags & MAIN_THREAD) == 0) {
      GC_ASSERT(me -> stack_end != NULL);
      if ((word)me->stack_end HOTTER_THAN (word)(&stacksect))
        me -> stack_end = (ptr_t)(&stacksect);
    } else {
      if ((word)GC_stackbottom HOTTER_THAN (word)(&stacksect))
        GC_stackbottom = (ptr_t)COVERT_DATAFLOW(&stacksect);
    }
    if (!me->thread_blocked) {
      UNLOCK();
      client_data = fn(client_data);
      GC_noop1(COVERT_DATAFLOW(&stacksect));
      return client_data;
    }
    stacksect.saved_stack_ptr = me -> stop_info.stack_ptr;
#ifdef IA64
      stacksect.backing_store_end = GC_save_regs_in_stack();
      stacksect.saved_backing_store_ptr = me -> backing_store_ptr;
#endif
    stacksect.prev = me -> traced_stack_sect;
    me -> thread_blocked = FALSE;
    me -> traced_stack_sect = &stacksect;
    UNLOCK();
    client_data = fn(client_data);
    GC_ASSERT(me -> thread_blocked == FALSE);
    GC_ASSERT(me -> traced_stack_sect == &stacksect);
#if defined(CPPCHECK)
      GC_noop1((word)me->traced_stack_sect);
#endif
    LOCK();
    me -> traced_stack_sect = stacksect.prev;
#ifdef IA64
      me -> backing_store_ptr = stacksect.saved_backing_store_ptr;
#endif
    me -> thread_blocked = (unsigned char)TRUE;
    me -> stop_info.stack_ptr = stacksect.saved_stack_ptr;
    UNLOCK();
    return client_data;
}
STATIC void GC_unregister_my_thread_inner(GC_thread me)
{
    GC_ASSERT(I_HOLD_LOCK());
#ifdef DEBUG_THREADS
      GC_log_printf(
                "Unregistering thread %p, gc_thread= %p, n_threads= %d\n",
                (void *)me->id, (void *)me, GC_count_threads());
#endif
    GC_ASSERT(!(me -> flags & FINISHED));
#if defined(THREAD_LOCAL_ALLOC)
      GC_ASSERT(GC_getspecific(GC_thread_key) == &me->tlfs);
      GC_destroy_thread_local(&(me->tlfs));
#endif
#if defined(GC_HAVE_PTHREAD_EXIT) || !defined(GC_NO_PTHREAD_CANCEL)
      if ((me -> flags & DISABLED_GC) != 0) {
        GC_dont_gc--;
      }
#endif
    if (me -> flags & DETACHED) {
        GC_delete_thread(pthread_self());
    } else {
        me -> flags |= FINISHED;
    }
#if defined(THREAD_LOCAL_ALLOC)
      GC_remove_specific(GC_thread_key);
#endif
}
GC_API int GC_CALL GC_unregister_my_thread(void)
{
    pthread_t self = pthread_self();
    GC_thread me;
    IF_CANCEL(int cancel_state;)
    DCL_LOCK_STATE;
    LOCK();
    DISABLE_CANCEL(cancel_state);
    GC_wait_for_gc_completion(FALSE);
    me = GC_lookup_thread(self);
#ifdef DEBUG_THREADS
        GC_log_printf(
                "Called GC_unregister_my_thread on %p, gc_thread= %p\n",
                (void *)self, (void *)me);
#endif
    GC_ASSERT(THREAD_EQUAL(me->id, self));
    GC_unregister_my_thread_inner(me);
    RESTORE_CANCEL(cancel_state);
    UNLOCK();
    return GC_SUCCESS;
}
GC_INNER_PTHRSTART void GC_thread_exit_proc(void *arg)
{
    IF_CANCEL(int cancel_state;)
    DCL_LOCK_STATE;
#ifdef DEBUG_THREADS
        GC_log_printf("Called GC_thread_exit_proc on %p, gc_thread= %p\n",
                      (void *)((GC_thread)arg)->id, arg);
#endif
    LOCK();
    DISABLE_CANCEL(cancel_state);
    GC_wait_for_gc_completion(FALSE);
    GC_unregister_my_thread_inner((GC_thread)arg);
    RESTORE_CANCEL(cancel_state);
    UNLOCK();
}
#if !defined(SN_TARGET_ORBIS) && !defined(SN_TARGET_PSP2)
  GC_API int WRAP_FUNC(pthread_join)(pthread_t thread, void **retval)
  {
    int result;
    GC_thread t;
    DCL_LOCK_STATE;
    ASSERT_SYMS_INITIALIZED();
    LOCK();
    t = (GC_thread)COVERT_DATAFLOW(GC_lookup_thread(thread));
    UNLOCK();
    result = REAL_FUNC(pthread_join)(thread, retval);
#if defined(GC_FREEBSD_THREADS)
    if (result == EINTR) result = 0;
#endif
    if (result == 0) {
        LOCK();
        if ((t -> flags & FINISHED) != 0)
          GC_delete_gc_thread(t);
        UNLOCK();
    }
    return result;
  }
  GC_API int WRAP_FUNC(pthread_detach)(pthread_t thread)
  {
    int result;
    GC_thread t;
    DCL_LOCK_STATE;
    ASSERT_SYMS_INITIALIZED();
    LOCK();
    t = (GC_thread)COVERT_DATAFLOW(GC_lookup_thread(thread));
    UNLOCK();
    result = REAL_FUNC(pthread_detach)(thread);
    if (result == 0) {
      LOCK();
      t -> flags |= DETACHED;
      if ((t -> flags & FINISHED) != 0) {
        GC_delete_gc_thread(t);
      }
      UNLOCK();
    }
    return result;
  }
#endif
#ifndef GC_NO_PTHREAD_CANCEL
  GC_API int WRAP_FUNC(pthread_cancel)(pthread_t thread)
  {
#ifdef CANCEL_SAFE
      GC_thread t;
      DCL_LOCK_STATE;
#endif
    INIT_REAL_SYMS();
#ifdef CANCEL_SAFE
      LOCK();
      t = GC_lookup_thread(thread);
      if (t != NULL && (t -> flags & DISABLED_GC) == 0) {
        t -> flags |= DISABLED_GC;
        GC_dont_gc++;
      }
      UNLOCK();
#endif
    return REAL_FUNC(pthread_cancel)(thread);
  }
#endif
#ifdef GC_HAVE_PTHREAD_EXIT
  GC_API GC_PTHREAD_EXIT_ATTRIBUTE void WRAP_FUNC(pthread_exit)(void *retval)
  {
    pthread_t self = pthread_self();
    GC_thread me;
    DCL_LOCK_STATE;
    INIT_REAL_SYMS();
    LOCK();
    me = GC_lookup_thread(self);
    if (me != 0 && (me -> flags & DISABLED_GC) == 0) {
      me -> flags |= DISABLED_GC;
      GC_dont_gc++;
    }
    UNLOCK();
    REAL_FUNC(pthread_exit)(retval);
  }
#endif
GC_INNER GC_bool GC_in_thread_creation = FALSE;
GC_INLINE void GC_record_stack_base(GC_thread me,
                                    const struct GC_stack_base *sb)
{
#ifndef GC_DARWIN_THREADS
      me -> stop_info.stack_ptr = (ptr_t)sb->mem_base;
#endif
    me -> stack_end = (ptr_t)sb->mem_base;
    if (me -> stack_end == NULL)
      ABORT("Bad stack base in GC_register_my_thread");
#ifdef IA64
      me -> backing_store_end = (ptr_t)sb->reg_base;
#endif
}
STATIC GC_thread GC_register_my_thread_inner(const struct GC_stack_base *sb,
                                             pthread_t my_pthread)
{
    GC_thread me;
    GC_in_thread_creation = TRUE;
    me = GC_new_thread(my_pthread);
    GC_in_thread_creation = FALSE;
    if (me == 0)
      ABORT("Failed to allocate memory for thread registering");
#ifdef GC_DARWIN_THREADS
      me -> stop_info.mach_thread = mach_thread_self();
#endif
    GC_record_stack_base(me, sb);
#ifdef GC_EXPLICIT_SIGNALS_UNBLOCK
      GC_unblock_gc_signals();
#endif
    return me;
}
GC_API void GC_CALL GC_allow_register_threads(void)
{
    GC_ASSERT(GC_lookup_thread(pthread_self()) != 0);
    set_need_to_lock();
}
GC_API int GC_CALL GC_register_my_thread(const struct GC_stack_base *sb)
{
    pthread_t self = pthread_self();
    GC_thread me;
    DCL_LOCK_STATE;
    if (GC_need_to_lock == FALSE)
        ABORT("Threads explicit registering is not previously enabled");
    LOCK();
    me = GC_lookup_thread(self);
    if (0 == me) {
        me = GC_register_my_thread_inner(sb, self);
#if defined(CPPCHECK)
          GC_noop1(me->flags);
#endif
        me -> flags |= DETACHED;
#if defined(THREAD_LOCAL_ALLOC)
          GC_init_thread_local(&(me->tlfs));
#endif
        UNLOCK();
        return GC_SUCCESS;
    } else if ((me -> flags & FINISHED) != 0) {
#ifdef GC_DARWIN_THREADS
          me -> stop_info.mach_thread = mach_thread_self();
#endif
        GC_record_stack_base(me, sb);
        me -> flags &= ~FINISHED;
#ifdef GC_EXPLICIT_SIGNALS_UNBLOCK
          GC_unblock_gc_signals();
#endif
#if defined(THREAD_LOCAL_ALLOC)
          GC_init_thread_local(&(me->tlfs));
#endif
        UNLOCK();
        return GC_SUCCESS;
    } else {
        UNLOCK();
        return GC_DUPLICATE;
    }
}
struct start_info {
    void *(*start_routine)(void *);
    void *arg;
    word flags;
    sem_t registered;
};
GC_INNER_PTHRSTART GC_thread GC_start_rtn_prepare_thread(
                                        void *(**pstart)(void *),
                                        void **pstart_arg,
                                        struct GC_stack_base *sb, void *arg)
{
    struct start_info * si = (struct start_info *)arg;
    pthread_t self = pthread_self();
    GC_thread me;
    DCL_LOCK_STATE;
#ifdef DEBUG_THREADS
      GC_log_printf("Starting thread %p, pid= %ld, sp= %p\n",
                    (void *)self, (long)getpid(), (void *)&arg);
#endif
    LOCK();
    me = GC_register_my_thread_inner(sb, self);
    me -> flags = si -> flags;
#if defined(THREAD_LOCAL_ALLOC)
      GC_init_thread_local(&(me->tlfs));
#endif
    UNLOCK();
    *pstart = si -> start_routine;
#ifdef DEBUG_THREADS
      GC_log_printf("start_routine= %p\n", (void *)(signed_word)(*pstart));
#endif
    *pstart_arg = si -> arg;
    sem_post(&(si -> registered));
    return me;
}
#if !defined(SN_TARGET_ORBIS) && !defined(SN_TARGET_PSP2)
  STATIC void * GC_start_routine(void * arg)
  {
#ifdef INCLUDE_LINUX_THREAD_DESCR
      struct GC_stack_base sb;
#ifdef REDIRECT_MALLOC
        GC_disable();
#endif
      if (GC_get_stack_base(&sb) != GC_SUCCESS)
        ABORT("Failed to get thread stack base");
#ifdef REDIRECT_MALLOC
        GC_enable();
#endif
      return GC_inner_start_routine(&sb, arg);
#else
      return GC_call_with_stack_base(GC_inner_start_routine, arg);
#endif
  }
  GC_API int WRAP_FUNC(pthread_create)(pthread_t *new_thread,
                       GC_PTHREAD_CREATE_CONST pthread_attr_t *attr,
                       void *(*start_routine)(void *), void *arg)
  {
    int result;
    int detachstate;
    word my_flags = 0;
    struct start_info si;
    DCL_LOCK_STATE;
    INIT_REAL_SYMS();
    if (!EXPECT(parallel_initialized, TRUE))
      GC_init_parallel();
    if (sem_init(&si.registered, GC_SEM_INIT_PSHARED, 0) != 0)
      ABORT("sem_init failed");
    si.start_routine = start_routine;
    si.arg = arg;
    LOCK();
    if (!EXPECT(GC_thr_initialized, TRUE))
      GC_thr_init();
#ifdef GC_ASSERTIONS
      {
        size_t stack_size = 0;
        if (NULL != attr) {
          if (pthread_attr_getstacksize(attr, &stack_size) != 0)
            ABORT("pthread_attr_getstacksize failed");
        }
        if (0 == stack_size) {
           pthread_attr_t my_attr;
           if (pthread_attr_init(&my_attr) != 0)
             ABORT("pthread_attr_init failed");
           if (pthread_attr_getstacksize(&my_attr, &stack_size) != 0)
             ABORT("pthread_attr_getstacksize failed");
           (void)pthread_attr_destroy(&my_attr);
        }
        if (0 == stack_size) {
#ifndef SOLARIS
              WARN("Failed to get stack size for assertion checking\n", 0);
#endif
            stack_size = 1000000;
        }
        GC_ASSERT(stack_size >= 65536);
      }
#endif
    if (NULL == attr) {
        detachstate = PTHREAD_CREATE_JOINABLE;
    } else {
        pthread_attr_getdetachstate(attr, &detachstate);
    }
    if (PTHREAD_CREATE_DETACHED == detachstate) my_flags |= DETACHED;
    si.flags = my_flags;
    UNLOCK();
#ifdef DEBUG_THREADS
      GC_log_printf("About to start new thread from thread %p\n",
                    (void *)pthread_self());
#endif
    set_need_to_lock();
    result = REAL_FUNC(pthread_create)(new_thread, attr, GC_start_routine,
                                       &si);
    if (0 == result) {
        IF_CANCEL(int cancel_state;)
#ifdef DEBUG_THREADS
            GC_log_printf("Started thread %p\n", (void *)(*new_thread));
#endif
        DISABLE_CANCEL(cancel_state);
        while (0 != sem_wait(&si.registered)) {
#if defined(GC_HAIKU_THREADS)
              if (EACCES == errno) continue;
#endif
            if (EINTR != errno) ABORT("sem_wait failed");
        }
        RESTORE_CANCEL(cancel_state);
    }
    sem_destroy(&si.registered);
    return(result);
  }
#endif
#if defined(USE_SPIN_LOCK) || !defined(NO_PTHREAD_TRYLOCK)
#define GC_PAUSE_SPIN_CYCLES 10
STATIC void GC_pause(void)
{
    int i;
    for (i = 0; i < GC_PAUSE_SPIN_CYCLES; ++i) {
#if defined(AO_HAVE_compiler_barrier) \
         && !defined(BASE_ATOMIC_OPS_EMULATED)
        AO_compiler_barrier();
#else
        GC_noop1(i);
#endif
    }
}
#endif
#ifndef SPIN_MAX
#define SPIN_MAX 128
#endif
GC_INNER volatile GC_bool GC_collecting = FALSE;
#if (!defined(USE_SPIN_LOCK) && !defined(NO_PTHREAD_TRYLOCK)) \
        || defined(PARALLEL_MARK)
#ifdef LOCK_STATS
  volatile AO_t GC_spin_count = 0;
  volatile AO_t GC_block_count = 0;
  volatile AO_t GC_unlocked_count = 0;
#endif
STATIC void GC_generic_lock(pthread_mutex_t * lock)
{
#ifndef NO_PTHREAD_TRYLOCK
    unsigned pause_length = 1;
    unsigned i;
    if (0 == pthread_mutex_trylock(lock)) {
#ifdef LOCK_STATS
            (void)AO_fetch_and_add1(&GC_unlocked_count);
#endif
        return;
    }
    for (; pause_length <= SPIN_MAX; pause_length <<= 1) {
        for (i = 0; i < pause_length; ++i) {
            GC_pause();
        }
        switch(pthread_mutex_trylock(lock)) {
            case 0:
#ifdef LOCK_STATS
                    (void)AO_fetch_and_add1(&GC_spin_count);
#endif
                return;
            case EBUSY:
                break;
            default:
                ABORT("Unexpected error from pthread_mutex_trylock");
        }
    }
#endif
#ifdef LOCK_STATS
        (void)AO_fetch_and_add1(&GC_block_count);
#endif
    pthread_mutex_lock(lock);
}
#endif
#if defined(AO_HAVE_char_load) && !defined(BASE_ATOMIC_OPS_EMULATED)
#define is_collecting() \
                ((GC_bool)AO_char_load((unsigned char *)&GC_collecting))
#else
#define is_collecting() GC_collecting
#endif
#if defined(USE_SPIN_LOCK)
GC_INNER volatile AO_TS_t GC_allocate_lock = AO_TS_INITIALIZER;
#define low_spin_max 30
#define high_spin_max SPIN_MAX
  static volatile AO_t spin_max = low_spin_max;
  static volatile AO_t last_spins = 0;
GC_INNER void GC_lock(void)
{
    unsigned my_spin_max;
    unsigned my_last_spins;
    unsigned i;
    if (AO_test_and_set_acquire(&GC_allocate_lock) == AO_TS_CLEAR) {
        return;
    }
    my_spin_max = (unsigned)AO_load(&spin_max);
    my_last_spins = (unsigned)AO_load(&last_spins);
    for (i = 0; i < my_spin_max; i++) {
        if (is_collecting() || GC_nprocs == 1)
          goto yield;
        if (i < my_last_spins/2) {
            GC_pause();
            continue;
        }
        if (AO_test_and_set_acquire(&GC_allocate_lock) == AO_TS_CLEAR) {
            AO_store(&last_spins, (AO_t)i);
            AO_store(&spin_max, (AO_t)high_spin_max);
            return;
        }
    }
    AO_store(&spin_max, (AO_t)low_spin_max);
yield:
    for (i = 0;; ++i) {
        if (AO_test_and_set_acquire(&GC_allocate_lock) == AO_TS_CLEAR) {
            return;
        }
#define SLEEP_THRESHOLD 12
        if (i < SLEEP_THRESHOLD) {
            sched_yield();
        } else {
            struct timespec ts;
            if (i > 24) i = 24;
            ts.tv_sec = 0;
            ts.tv_nsec = 1 << i;
            nanosleep(&ts, 0);
        }
    }
}
#elif defined(USE_PTHREAD_LOCKS)
#ifndef NO_PTHREAD_TRYLOCK
    GC_INNER void GC_lock(void)
    {
      if (1 == GC_nprocs || is_collecting()) {
        pthread_mutex_lock(&GC_allocate_ml);
      } else {
        GC_generic_lock(&GC_allocate_ml);
      }
    }
#elif defined(GC_ASSERTIONS)
    GC_INNER void GC_lock(void)
    {
      pthread_mutex_lock(&GC_allocate_ml);
    }
#endif
#endif
#ifdef PARALLEL_MARK
#ifdef GC_ASSERTIONS
    STATIC unsigned long GC_mark_lock_holder = NO_THREAD;
#define SET_MARK_LOCK_HOLDER \
                (void)(GC_mark_lock_holder = NUMERIC_THREAD_ID(pthread_self()))
#define UNSET_MARK_LOCK_HOLDER \
                do { \
                  GC_ASSERT(GC_mark_lock_holder \
                                == NUMERIC_THREAD_ID(pthread_self())); \
                  GC_mark_lock_holder = NO_THREAD; \
                } while (0)
#else
#define SET_MARK_LOCK_HOLDER (void)0
#define UNSET_MARK_LOCK_HOLDER (void)0
#endif
#ifdef GLIBC_2_1_MUTEX_HACK
  static pthread_mutex_t mark_mutex =
        {0, 0, 0, PTHREAD_MUTEX_ERRORCHECK_NP, {0, 0}};
#else
  static pthread_mutex_t mark_mutex = PTHREAD_MUTEX_INITIALIZER;
#endif
static pthread_cond_t builder_cv = PTHREAD_COND_INITIALIZER;
static void setup_mark_lock(void)
{
#ifdef GLIBC_2_19_TSX_BUG
    pthread_mutexattr_t mattr;
    int glibc_minor = -1;
    int glibc_major = GC_parse_version(&glibc_minor, gnu_get_libc_version());
    if (glibc_major > 2 || (glibc_major == 2 && glibc_minor >= 19)) {
      if (0 != pthread_mutexattr_init(&mattr)) {
        ABORT("pthread_mutexattr_init failed");
      }
      if (0 != pthread_mutexattr_settype(&mattr, PTHREAD_MUTEX_NORMAL)) {
        ABORT("pthread_mutexattr_settype failed");
      }
      if (0 != pthread_mutex_init(&mark_mutex, &mattr)) {
        ABORT("pthread_mutex_init failed");
      }
      (void)pthread_mutexattr_destroy(&mattr);
    }
#endif
}
GC_INNER void GC_acquire_mark_lock(void)
{
#if defined(NUMERIC_THREAD_ID_UNIQUE) && !defined(THREAD_SANITIZER)
      GC_ASSERT(GC_mark_lock_holder != NUMERIC_THREAD_ID(pthread_self()));
#endif
    GC_generic_lock(&mark_mutex);
    SET_MARK_LOCK_HOLDER;
}
GC_INNER void GC_release_mark_lock(void)
{
    UNSET_MARK_LOCK_HOLDER;
    if (pthread_mutex_unlock(&mark_mutex) != 0) {
        ABORT("pthread_mutex_unlock failed");
    }
}
STATIC void GC_wait_builder(void)
{
    ASSERT_CANCEL_DISABLED();
    UNSET_MARK_LOCK_HOLDER;
    if (pthread_cond_wait(&builder_cv, &mark_mutex) != 0) {
        ABORT("pthread_cond_wait failed");
    }
    GC_ASSERT(GC_mark_lock_holder == NO_THREAD);
    SET_MARK_LOCK_HOLDER;
}
GC_INNER void GC_wait_for_reclaim(void)
{
    GC_acquire_mark_lock();
    while (GC_fl_builder_count > 0) {
        GC_wait_builder();
    }
    GC_release_mark_lock();
}
GC_INNER void GC_notify_all_builder(void)
{
    GC_ASSERT(GC_mark_lock_holder == NUMERIC_THREAD_ID(pthread_self()));
    if (pthread_cond_broadcast(&builder_cv) != 0) {
        ABORT("pthread_cond_broadcast failed");
    }
}
GC_INNER void GC_wait_marker(void)
{
    ASSERT_CANCEL_DISABLED();
    GC_ASSERT(GC_parallel);
    UNSET_MARK_LOCK_HOLDER;
    if (pthread_cond_wait(&mark_cv, &mark_mutex) != 0) {
        ABORT("pthread_cond_wait failed");
    }
    GC_ASSERT(GC_mark_lock_holder == NO_THREAD);
    SET_MARK_LOCK_HOLDER;
}
GC_INNER void GC_notify_all_marker(void)
{
    GC_ASSERT(GC_parallel);
    if (pthread_cond_broadcast(&mark_cv) != 0) {
        ABORT("pthread_cond_broadcast failed");
    }
}
#endif
#ifdef PTHREAD_REGISTER_CANCEL_WEAK_STUBS
  EXTERN_C_BEGIN
  extern void __pthread_register_cancel(void) __attribute__((__weak__));
  extern void __pthread_unregister_cancel(void) __attribute__((__weak__));
  EXTERN_C_END
  void __pthread_register_cancel(void) {}
  void __pthread_unregister_cancel(void) {}
#endif
#endif
#if defined(USE_CUSTOM_SPECIFIC)
static const tse invalid_tse = {INVALID_QTID, 0, 0, INVALID_THREADID};
GC_INNER int GC_key_create_inner(tsd ** key_ptr)
{
    int i;
    int ret;
    tsd * result;
    GC_ASSERT(I_HOLD_LOCK());
    GC_ASSERT((word)(&invalid_tse.next) % sizeof(tse *) == 0);
    result = (tsd *)MALLOC_CLEAR(sizeof(tsd));
    if (NULL == result) return ENOMEM;
    ret = pthread_mutex_init(&result->lock, NULL);
    if (ret != 0) return ret;
    for (i = 0; i < TS_CACHE_SIZE; ++i) {
      result -> cache[i] = ( tse *)&invalid_tse;
    }
#ifdef GC_ASSERTIONS
      for (i = 0; i < TS_HASH_SIZE; ++i) {
        GC_ASSERT(result -> hash[i].p == 0);
      }
#endif
    *key_ptr = result;
    return 0;
}
GC_INNER int GC_setspecific(tsd * key, void * value)
{
    pthread_t self = pthread_self();
    unsigned hash_val = HASH(self);
    volatile tse * entry;
    GC_ASSERT(I_HOLD_LOCK());
    GC_ASSERT(self != INVALID_THREADID);
    GC_dont_gc++;
    entry = (volatile tse *)MALLOC_CLEAR(sizeof(tse));
    GC_dont_gc--;
    if (0 == entry) return ENOMEM;
    pthread_mutex_lock(&(key -> lock));
    entry -> next = key->hash[hash_val].p;
    entry -> thread = self;
    entry -> value = TS_HIDE_VALUE(value);
    GC_ASSERT(entry -> qtid == INVALID_QTID);
    AO_store_release(&key->hash[hash_val].ao, (AO_t)entry);
    GC_dirty(( void *)entry);
    GC_dirty(key->hash + hash_val);
    if (pthread_mutex_unlock(&key->lock) != 0)
      ABORT("pthread_mutex_unlock failed (setspecific)");
    return 0;
}
GC_INNER void GC_remove_specific_after_fork(tsd * key, pthread_t t)
{
    unsigned hash_val = HASH(t);
    tse *entry;
    tse *prev = NULL;
#ifdef CAN_HANDLE_FORK
      GC_ASSERT(I_HOLD_LOCK());
#endif
    pthread_mutex_lock(&(key -> lock));
    entry = key->hash[hash_val].p;
    while (entry != NULL && !THREAD_EQUAL(entry->thread, t)) {
      prev = entry;
      entry = entry->next;
    }
    if (entry != NULL) {
      entry -> qtid = INVALID_QTID;
      if (NULL == prev) {
        key->hash[hash_val].p = entry->next;
        GC_dirty(key->hash + hash_val);
      } else {
        prev->next = entry->next;
        GC_dirty(prev);
      }
    }
#ifdef LINT2
      GC_noop1((word)entry);
#endif
    if (pthread_mutex_unlock(&key->lock) != 0)
      ABORT("pthread_mutex_unlock failed (remove_specific after fork)");
}
GC_INNER void * GC_slow_getspecific(tsd * key, word qtid,
                                    tse * volatile * cache_ptr)
{
    pthread_t self = pthread_self();
    tse *entry = key->hash[HASH(self)].p;
    GC_ASSERT(qtid != INVALID_QTID);
    while (entry != NULL && !THREAD_EQUAL(entry->thread, self)) {
      entry = entry -> next;
    }
    if (entry == NULL) return NULL;
    entry -> qtid = (AO_t)qtid;
    *cache_ptr = entry;
    return TS_REVEAL_PTR(entry -> value);
}
#ifdef GC_ASSERTIONS
  void GC_check_tsd_marks(tsd *key)
  {
    int i;
    tse *p;
    if (!GC_is_marked(GC_base(key))) {
      ABORT("Unmarked thread-specific-data table");
    }
    for (i = 0; i < TS_HASH_SIZE; ++i) {
      for (p = key->hash[i].p; p != 0; p = p -> next) {
        if (!GC_is_marked(GC_base(p))) {
          ABORT_ARG1("Unmarked thread-specific-data entry",
                     " at %p", (void *)p);
        }
      }
    }
    for (i = 0; i < TS_CACHE_SIZE; ++i) {
      p = key -> cache[i];
      if (p != &invalid_tse && !GC_is_marked(GC_base(p))) {
        ABORT_ARG1("Unmarked cached thread-specific-data entry",
                   " at %p", (void *)p);
      }
    }
  }
#endif
#endif
#if defined(GC_WIN32_THREADS)
#ifdef THREAD_LOCAL_ALLOC
#endif
#if !defined(USE_PTHREAD_LOCKS)
  GC_INNER CRITICAL_SECTION GC_allocate_ml;
#ifdef GC_ASSERTIONS
    GC_INNER DWORD GC_lock_holder = NO_THREAD;
#endif
#else
  GC_INNER pthread_mutex_t GC_allocate_ml = PTHREAD_MUTEX_INITIALIZER;
#ifdef GC_ASSERTIONS
    GC_INNER unsigned long GC_lock_holder = NO_THREAD;
#endif
#endif
#undef CreateThread
#undef ExitThread
#undef _beginthreadex
#undef _endthreadex
#ifdef GC_PTHREADS
#include <errno.h>
#undef pthread_create
#undef pthread_join
#undef pthread_detach
#ifndef GC_NO_PTHREAD_SIGMASK
#undef pthread_sigmask
#endif
  STATIC void * GC_pthread_start(void * arg);
  STATIC void GC_thread_exit_proc(void *arg);
#include <pthread.h>
#ifdef CAN_CALL_ATFORK
#include <unistd.h>
#endif
#elif !defined(MSWINCE)
#include <process.h>
#include <errno.h>
#endif
static ptr_t copy_ptr_regs(word *regs, const CONTEXT *pcontext);
#if defined(I386)
#ifdef WOW64_THREAD_CONTEXT_WORKAROUND
#define PUSHED_REGS_COUNT 9
#else
#define PUSHED_REGS_COUNT 7
#endif
#elif defined(X86_64) || defined(SHx)
#define PUSHED_REGS_COUNT 15
#elif defined(ARM32)
#define PUSHED_REGS_COUNT 13
#elif defined(AARCH64)
#define PUSHED_REGS_COUNT 30
#elif defined(MIPS) || defined(ALPHA)
#define PUSHED_REGS_COUNT 28
#elif defined(PPC)
#define PUSHED_REGS_COUNT 29
#endif
#if (defined(GC_DLL) || defined(GC_INSIDE_DLL)) && !defined(NO_CRT) \
        && !defined(GC_NO_THREADS_DISCOVERY) && !defined(MSWINCE) \
        && !defined(THREAD_LOCAL_ALLOC) && !defined(GC_PTHREADS)
#ifdef GC_DISCOVER_TASK_THREADS
#define GC_win32_dll_threads TRUE
#else
    STATIC GC_bool GC_win32_dll_threads = FALSE;
#endif
#else
#ifndef GC_NO_THREADS_DISCOVERY
#define GC_NO_THREADS_DISCOVERY
#endif
#define GC_win32_dll_threads FALSE
#undef MAX_THREADS
#define MAX_THREADS 1
#endif
typedef LONG * IE_t;
STATIC GC_bool GC_thr_initialized = FALSE;
#ifndef GC_ALWAYS_MULTITHREADED
  GC_INNER GC_bool GC_need_to_lock = FALSE;
#endif
static GC_bool parallel_initialized = FALSE;
GC_API void GC_CALL GC_use_threads_discovery(void)
{
#ifdef GC_NO_THREADS_DISCOVERY
    ABORT("GC DllMain-based thread registration unsupported");
#else
    GC_ASSERT(!parallel_initialized);
#ifndef GC_DISCOVER_TASK_THREADS
      GC_win32_dll_threads = TRUE;
#endif
    GC_init_parallel();
#endif
}
#define ADDR_LIMIT ((ptr_t)GC_WORD_MAX)
struct GC_Thread_Rep {
  union {
#ifndef GC_NO_THREADS_DISCOVERY
      volatile AO_t in_use;
      LONG long_in_use;
#endif
    struct GC_Thread_Rep * next;
  } tm;
  DWORD id;
#ifdef MSWINCE
#define THREAD_HANDLE(t) (HANDLE)(word)(t)->id
#else
    HANDLE handle;
#define THREAD_HANDLE(t) (t)->handle
#endif
  ptr_t stack_base;
  ptr_t last_stack_min;
#ifdef IA64
    ptr_t backing_store_end;
    ptr_t backing_store_ptr;
#elif defined(I386)
    ptr_t initial_stack_base;
#endif
  ptr_t thread_blocked_sp;
  struct GC_traced_stack_sect_s *traced_stack_sect;
  unsigned short finalizer_skipped;
  unsigned char finalizer_nested;
  unsigned char suspended;
#ifdef GC_PTHREADS
    unsigned char flags;
#define FINISHED 1
#define DETACHED 2
#define KNOWN_FINISHED(t) (((t) -> flags) & FINISHED)
    pthread_t pthread_id;
    void *status;
#else
#define KNOWN_FINISHED(t) 0
#endif
#ifdef THREAD_LOCAL_ALLOC
    struct thread_local_freelists tlfs;
#endif
#ifdef RETRY_GET_THREAD_CONTEXT
    ptr_t context_sp;
    word context_regs[PUSHED_REGS_COUNT];
#endif
};
typedef struct GC_Thread_Rep * GC_thread;
typedef volatile struct GC_Thread_Rep * GC_vthread;
#ifndef GC_NO_THREADS_DISCOVERY
  STATIC DWORD GC_main_thread = 0;
  STATIC volatile AO_t GC_attached_thread = FALSE;
  STATIC volatile GC_bool GC_please_stop = FALSE;
#elif defined(GC_ASSERTIONS)
  STATIC GC_bool GC_please_stop = FALSE;
#endif
#if defined(WRAP_MARK_SOME) && !defined(GC_PTHREADS)
  GC_INNER GC_bool GC_started_thread_while_stopped(void)
  {
#ifndef GC_NO_THREADS_DISCOVERY
      if (GC_win32_dll_threads) {
#ifdef AO_HAVE_compare_and_swap_release
          if (AO_compare_and_swap_release(&GC_attached_thread, TRUE,
                                          FALSE ))
            return TRUE;
#else
          AO_nop_full();
          if (AO_load(&GC_attached_thread)) {
            AO_store(&GC_attached_thread, FALSE);
            return TRUE;
          }
#endif
      }
#endif
    return FALSE;
  }
#endif
#ifndef MAX_THREADS
#define MAX_THREADS 512
#endif
volatile struct GC_Thread_Rep dll_thread_table[MAX_THREADS];
STATIC volatile LONG GC_max_thread_index = 0;
#ifndef THREAD_TABLE_SZ
#define THREAD_TABLE_SZ 256
#endif
#define THREAD_TABLE_INDEX(id)  \
                (int)((((id) >> 8) ^ (id)) % THREAD_TABLE_SZ)
STATIC GC_thread GC_threads[THREAD_TABLE_SZ];
static struct GC_Thread_Rep first_thread;
static GC_bool first_thread_used = FALSE;
STATIC GC_thread GC_new_thread(DWORD id)
{
  int hv = THREAD_TABLE_INDEX(id);
  GC_thread result;
#ifdef DEBUG_THREADS
    GC_log_printf("Creating thread 0x%lx\n", (long)id);
    if (GC_threads[hv] != NULL)
      GC_log_printf("Hash collision at GC_threads[%d]\n", hv);
#endif
  GC_ASSERT(I_HOLD_LOCK());
  if (!EXPECT(first_thread_used, TRUE)) {
    result = &first_thread;
    first_thread_used = TRUE;
    GC_ASSERT(NULL == GC_threads[hv]);
  } else {
    GC_ASSERT(!GC_win32_dll_threads);
    result = (struct GC_Thread_Rep *)
                GC_INTERNAL_MALLOC(sizeof(struct GC_Thread_Rep), NORMAL);
    if (result == 0) return(0);
  }
  result -> tm.next = GC_threads[hv];
  GC_threads[hv] = result;
#ifdef GC_PTHREADS
    GC_ASSERT(result -> flags == 0);
#endif
  GC_ASSERT(result -> thread_blocked_sp == NULL);
  if (EXPECT(result != &first_thread, TRUE))
    GC_dirty(result);
  return(result);
}
GC_INNER GC_bool GC_in_thread_creation = FALSE;
GC_INLINE void GC_record_stack_base(GC_vthread me,
                                    const struct GC_stack_base *sb)
{
  me -> stack_base = (ptr_t)sb->mem_base;
#ifdef IA64
    me -> backing_store_end = (ptr_t)sb->reg_base;
#elif defined(I386)
    me -> initial_stack_base = (ptr_t)sb->mem_base;
#endif
  if (me -> stack_base == NULL)
    ABORT("Bad stack base in GC_register_my_thread");
}
STATIC GC_thread GC_register_my_thread_inner(const struct GC_stack_base *sb,
                                             DWORD thread_id)
{
  GC_vthread me;
#if defined(MPROTECT_VDB) && !defined(CYGWIN32)
    if (GC_auto_incremental
#ifdef GWW_VDB
          && !GC_gww_dirty_init()
#endif
        )
      GC_set_write_fault_handler();
#endif
#ifndef GC_NO_THREADS_DISCOVERY
    if (GC_win32_dll_threads) {
      int i;
      for (i = 0;
           InterlockedExchange(&dll_thread_table[i].tm.long_in_use, 1) != 0;
           i++) {
        if (i == MAX_THREADS - 1)
          ABORT("Too many threads");
      }
      while (i > GC_max_thread_index) {
        InterlockedIncrement((IE_t)&GC_max_thread_index);
      }
      if (GC_max_thread_index >= MAX_THREADS) {
        GC_max_thread_index = MAX_THREADS - 1;
      }
      me = dll_thread_table + i;
    } else
#endif
    {
    GC_ASSERT(I_HOLD_LOCK());
    GC_in_thread_creation = TRUE;
    me = GC_new_thread(thread_id);
    GC_in_thread_creation = FALSE;
    if (me == 0)
      ABORT("Failed to allocate memory for thread registering");
  }
#ifdef GC_PTHREADS
    me -> pthread_id = pthread_self();
#endif
#ifndef MSWINCE
    if (!DuplicateHandle(GetCurrentProcess(), GetCurrentThread(),
                        GetCurrentProcess(),
                        (HANDLE*)&(me -> handle),
                        0 , FALSE ,
                        DUPLICATE_SAME_ACCESS)) {
        ABORT_ARG1("DuplicateHandle failed",
                   ": errcode= 0x%X", (unsigned)GetLastError());
    }
#endif
  me -> last_stack_min = ADDR_LIMIT;
  GC_record_stack_base(me, sb);
  me -> id = thread_id;
#if defined(THREAD_LOCAL_ALLOC)
    GC_init_thread_local((GC_tlfs)(&(me->tlfs)));
#endif
#ifndef GC_NO_THREADS_DISCOVERY
    if (GC_win32_dll_threads) {
      if (GC_please_stop) {
        AO_store(&GC_attached_thread, TRUE);
        AO_nop_full();
      }
    } else
#endif
   {
    GC_ASSERT(!GC_please_stop);
  }
  return (GC_thread)(me);
}
GC_INLINE LONG GC_get_max_thread_index(void)
{
  LONG my_max = GC_max_thread_index;
  if (my_max >= MAX_THREADS) return MAX_THREADS - 1;
  return my_max;
}
STATIC GC_thread GC_lookup_thread_inner(DWORD thread_id)
{
#ifndef GC_NO_THREADS_DISCOVERY
    if (GC_win32_dll_threads) {
      int i;
      LONG my_max = GC_get_max_thread_index();
      for (i = 0; i <= my_max &&
                  (!AO_load_acquire(&dll_thread_table[i].tm.in_use)
                  || dll_thread_table[i].id != thread_id);
           i++) {
      }
      return i <= my_max ? (GC_thread)(dll_thread_table + i) : NULL;
    } else
#endif
   {
    GC_thread p = GC_threads[THREAD_TABLE_INDEX(thread_id)];
    GC_ASSERT(I_HOLD_LOCK());
    while (p != 0 && p -> id != thread_id) p = p -> tm.next;
    return(p);
  }
}
#ifdef LINT2
#define CHECK_LOOKUP_MY_THREAD(me) \
        if (!(me)) ABORT("GC_lookup_thread_inner(GetCurrentThreadId) failed")
#else
#define CHECK_LOOKUP_MY_THREAD(me)
#endif
GC_INNER void GC_reset_finalizer_nested(void)
{
  GC_thread me = GC_lookup_thread_inner(GetCurrentThreadId());
  CHECK_LOOKUP_MY_THREAD(me);
  me->finalizer_nested = 0;
}
GC_INNER unsigned char *GC_check_finalizer_nested(void)
{
  GC_thread me = GC_lookup_thread_inner(GetCurrentThreadId());
  unsigned nesting_level;
  CHECK_LOOKUP_MY_THREAD(me);
  nesting_level = me->finalizer_nested;
  if (nesting_level) {
    if (++me->finalizer_skipped < (1U << nesting_level)) return NULL;
    me->finalizer_skipped = 0;
  }
  me->finalizer_nested = (unsigned char)(nesting_level + 1);
  return &me->finalizer_nested;
}
#if defined(GC_ASSERTIONS) && defined(THREAD_LOCAL_ALLOC)
  GC_bool GC_is_thread_tsd_valid(void *tsd)
  {
    GC_thread me;
    DCL_LOCK_STATE;
    LOCK();
    me = GC_lookup_thread_inner(GetCurrentThreadId());
    UNLOCK();
    return (word)tsd >= (word)(&me->tlfs)
            && (word)tsd < (word)(&me->tlfs) + sizeof(me->tlfs);
  }
#endif
GC_API int GC_CALL GC_thread_is_registered(void)
{
    DWORD thread_id = GetCurrentThreadId();
    GC_thread me;
    DCL_LOCK_STATE;
    LOCK();
    me = GC_lookup_thread_inner(thread_id);
    UNLOCK();
    return me != NULL;
}
GC_API void GC_CALL GC_register_altstack(void *stack GC_ATTR_UNUSED,
                                         GC_word stack_size GC_ATTR_UNUSED,
                                         void *altstack GC_ATTR_UNUSED,
                                         GC_word altstack_size GC_ATTR_UNUSED)
{
}
#if defined(MPROTECT_VDB)
#define UNPROTECT_THREAD(t) \
    if (!GC_win32_dll_threads && GC_auto_incremental \
        && t != &first_thread) { \
      GC_ASSERT(SMALL_OBJ(GC_size(t))); \
      GC_remove_protection(HBLKPTR(t), 1, FALSE); \
    } else (void)0
#else
#define UNPROTECT_THREAD(t) (void)0
#endif
#ifdef CYGWIN32
#define GC_PTHREAD_PTRVAL(pthread_id) pthread_id
#elif defined(GC_WIN32_PTHREADS) || defined(GC_PTHREADS_PARAMARK)
#include <pthread.h>
#if defined(__WINPTHREADS_VERSION_MAJOR)
#define GC_PTHREAD_PTRVAL(pthread_id) pthread_id
#else
#define GC_PTHREAD_PTRVAL(pthread_id) pthread_id.p
#endif
#endif
STATIC void GC_delete_gc_thread_no_free(GC_vthread t)
{
#ifndef MSWINCE
    CloseHandle(t->handle);
#endif
#ifndef GC_NO_THREADS_DISCOVERY
    if (GC_win32_dll_threads) {
      t -> stack_base = 0;
      t -> id = 0;
      t -> suspended = FALSE;
#ifdef RETRY_GET_THREAD_CONTEXT
        t -> context_sp = NULL;
#endif
      AO_store_release(&t->tm.in_use, FALSE);
    } else
#endif
   {
    DWORD id = ((GC_thread)t) -> id;
    int hv = THREAD_TABLE_INDEX(id);
    GC_thread p = GC_threads[hv];
    GC_thread prev = NULL;
    GC_ASSERT(I_HOLD_LOCK());
    while (p != (GC_thread)t) {
      prev = p;
      p = p -> tm.next;
    }
    if (prev == 0) {
      GC_threads[hv] = p -> tm.next;
    } else {
      GC_ASSERT(prev != &first_thread);
      prev -> tm.next = p -> tm.next;
      GC_dirty(prev);
    }
  }
}
STATIC void GC_delete_thread(DWORD id)
{
  if (GC_win32_dll_threads) {
    GC_vthread t = GC_lookup_thread_inner(id);
    if (0 == t) {
      WARN("Removing nonexistent thread, id= %" WARN_PRIdPTR "\n", id);
    } else {
      GC_delete_gc_thread_no_free(t);
    }
  } else {
    int hv = THREAD_TABLE_INDEX(id);
    GC_thread p = GC_threads[hv];
    GC_thread prev = NULL;
    GC_ASSERT(I_HOLD_LOCK());
    while (p -> id != id) {
      prev = p;
      p = p -> tm.next;
    }
#ifndef MSWINCE
      CloseHandle(p->handle);
#endif
    if (prev == 0) {
      GC_threads[hv] = p -> tm.next;
    } else {
      GC_ASSERT(prev != &first_thread);
      prev -> tm.next = p -> tm.next;
      GC_dirty(prev);
    }
    if (EXPECT(p != &first_thread, TRUE)) {
      GC_INTERNAL_FREE(p);
    }
  }
}
GC_API void GC_CALL GC_allow_register_threads(void)
{
  GC_ASSERT(GC_lookup_thread_inner(GetCurrentThreadId()) != 0);
#if !defined(GC_ALWAYS_MULTITHREADED) && !defined(PARALLEL_MARK) \
     && !defined(GC_NO_THREADS_DISCOVERY)
    parallel_initialized = TRUE;
#endif
  set_need_to_lock();
}
GC_API int GC_CALL GC_register_my_thread(const struct GC_stack_base *sb)
{
  GC_thread me;
  DWORD thread_id = GetCurrentThreadId();
  DCL_LOCK_STATE;
  if (GC_need_to_lock == FALSE)
    ABORT("Threads explicit registering is not previously enabled");
  LOCK();
  me = GC_lookup_thread_inner(thread_id);
  if (me == 0) {
#ifdef GC_PTHREADS
      me = GC_register_my_thread_inner(sb, thread_id);
#if defined(CPPCHECK)
        GC_noop1(me->flags);
#endif
      me -> flags |= DETACHED;
#else
      GC_register_my_thread_inner(sb, thread_id);
#endif
    UNLOCK();
    return GC_SUCCESS;
  } else
#ifdef GC_PTHREADS
       if ((me -> flags & FINISHED) != 0) {
        GC_record_stack_base(me, sb);
        me -> flags &= ~FINISHED;
#ifdef THREAD_LOCAL_ALLOC
          GC_init_thread_local((GC_tlfs)(&me->tlfs));
#endif
        UNLOCK();
        return GC_SUCCESS;
      } else
#endif
   {
    UNLOCK();
    return GC_DUPLICATE;
  }
}
STATIC void GC_wait_for_gc_completion(GC_bool wait_for_all)
{
  GC_ASSERT(I_HOLD_LOCK());
  if (GC_incremental && GC_collection_in_progress()) {
    word old_gc_no = GC_gc_no;
    do {
      ENTER_GC();
      GC_in_thread_creation = TRUE;
      GC_collect_a_little_inner(1);
      GC_in_thread_creation = FALSE;
      EXIT_GC();
      UNLOCK();
      Sleep(0);
      LOCK();
    } while (GC_incremental && GC_collection_in_progress()
             && (wait_for_all || old_gc_no == GC_gc_no));
  }
}
GC_API int GC_CALL GC_unregister_my_thread(void)
{
  DCL_LOCK_STATE;
#ifdef DEBUG_THREADS
    GC_log_printf("Unregistering thread 0x%lx\n", (long)GetCurrentThreadId());
#endif
  if (GC_win32_dll_threads) {
#if defined(THREAD_LOCAL_ALLOC)
      GC_ASSERT(FALSE);
#else
      GC_delete_thread(GetCurrentThreadId());
#endif
  } else {
#if defined(THREAD_LOCAL_ALLOC) || defined(GC_PTHREADS)
      GC_thread me;
#endif
    DWORD thread_id = GetCurrentThreadId();
    LOCK();
    GC_wait_for_gc_completion(FALSE);
#if defined(THREAD_LOCAL_ALLOC) || defined(GC_PTHREADS)
      me = GC_lookup_thread_inner(thread_id);
      CHECK_LOOKUP_MY_THREAD(me);
      GC_ASSERT(!KNOWN_FINISHED(me));
#endif
#if defined(THREAD_LOCAL_ALLOC)
      GC_ASSERT(GC_getspecific(GC_thread_key) == &me->tlfs);
      GC_destroy_thread_local(&(me->tlfs));
#endif
#ifdef GC_PTHREADS
      if ((me -> flags & DETACHED) == 0) {
        me -> flags |= FINISHED;
      } else
#endif
     {
      GC_delete_thread(thread_id);
    }
#if defined(THREAD_LOCAL_ALLOC)
      GC_remove_specific(GC_thread_key);
#endif
    UNLOCK();
  }
  return GC_SUCCESS;
}
GC_INNER void GC_do_blocking_inner(ptr_t data, void * context GC_ATTR_UNUSED)
{
  struct blocking_data * d = (struct blocking_data *) data;
  DWORD thread_id = GetCurrentThreadId();
  GC_thread me;
#ifdef IA64
    ptr_t stack_ptr = GC_save_regs_in_stack();
#endif
  DCL_LOCK_STATE;
  LOCK();
  me = GC_lookup_thread_inner(thread_id);
  CHECK_LOOKUP_MY_THREAD(me);
  GC_ASSERT(me -> thread_blocked_sp == NULL);
#ifdef IA64
    me -> backing_store_ptr = stack_ptr;
#endif
  me -> thread_blocked_sp = (ptr_t) &d;
  UNLOCK();
  d -> client_data = (d -> fn)(d -> client_data);
  LOCK();
#if defined(CPPCHECK)
    GC_noop1((word)me->thread_blocked_sp);
#endif
  me -> thread_blocked_sp = NULL;
  UNLOCK();
}
GC_API void * GC_CALL GC_call_with_gc_active(GC_fn_type fn,
                                             void * client_data)
{
  struct GC_traced_stack_sect_s stacksect;
  DWORD thread_id = GetCurrentThreadId();
  GC_thread me;
  DCL_LOCK_STATE;
  LOCK();
  me = GC_lookup_thread_inner(thread_id);
  CHECK_LOOKUP_MY_THREAD(me);
  GC_ASSERT(me -> stack_base != NULL);
  if ((word)me->stack_base < (word)(&stacksect)) {
    me -> stack_base = (ptr_t)(&stacksect);
#if defined(I386)
      me -> initial_stack_base = me -> stack_base;
#endif
  }
  if (me -> thread_blocked_sp == NULL) {
    UNLOCK();
    client_data = fn(client_data);
    GC_noop1(COVERT_DATAFLOW(&stacksect));
    return client_data;
  }
  stacksect.saved_stack_ptr = me -> thread_blocked_sp;
#ifdef IA64
    stacksect.backing_store_end = GC_save_regs_in_stack();
    stacksect.saved_backing_store_ptr = me -> backing_store_ptr;
#endif
  stacksect.prev = me -> traced_stack_sect;
  me -> thread_blocked_sp = NULL;
  me -> traced_stack_sect = &stacksect;
  UNLOCK();
  client_data = fn(client_data);
  GC_ASSERT(me -> thread_blocked_sp == NULL);
  GC_ASSERT(me -> traced_stack_sect == &stacksect);
  LOCK();
#if defined(CPPCHECK)
    GC_noop1((word)me->traced_stack_sect);
#endif
  me -> traced_stack_sect = stacksect.prev;
#ifdef IA64
    me -> backing_store_ptr = stacksect.saved_backing_store_ptr;
#endif
  me -> thread_blocked_sp = stacksect.saved_stack_ptr;
  UNLOCK();
  return client_data;
}
GC_API void GC_CALL GC_set_stackbottom(void *gc_thread_handle,
                                       const struct GC_stack_base *sb)
{
  GC_thread t = (GC_thread)gc_thread_handle;
  GC_ASSERT(sb -> mem_base != NULL);
  if (!EXPECT(GC_is_initialized, TRUE)) {
    GC_ASSERT(NULL == t);
    GC_stackbottom = (char *)sb->mem_base;
#ifdef IA64
      GC_register_stackbottom = (ptr_t)sb->reg_base;
#endif
    return;
  }
  GC_ASSERT(I_HOLD_LOCK());
  if (NULL == t) {
    t = GC_lookup_thread_inner(GetCurrentThreadId());
    CHECK_LOOKUP_MY_THREAD(t);
  }
  GC_ASSERT(!KNOWN_FINISHED(t));
  GC_ASSERT(NULL == t -> thread_blocked_sp
            && NULL == t -> traced_stack_sect);
  t -> stack_base = (ptr_t)sb->mem_base;
  t -> last_stack_min = ADDR_LIMIT;
#ifdef IA64
    t -> backing_store_end = (ptr_t)sb->reg_base;
#endif
}
GC_API void * GC_CALL GC_get_my_stackbottom(struct GC_stack_base *sb)
{
  DWORD thread_id = GetCurrentThreadId();
  GC_thread me;
  DCL_LOCK_STATE;
  LOCK();
  me = GC_lookup_thread_inner(thread_id);
  CHECK_LOOKUP_MY_THREAD(me);
  sb -> mem_base = me -> stack_base;
#ifdef IA64
    sb -> reg_base = me -> backing_store_end;
#endif
  UNLOCK();
  return (void *)me;
}
#ifdef GC_PTHREADS
#define PTHREAD_MAP_SIZE 512
  DWORD GC_pthread_map_cache[PTHREAD_MAP_SIZE] = {0};
#define PTHREAD_MAP_INDEX(pthread_id) \
                ((NUMERIC_THREAD_ID(pthread_id) >> 5) % PTHREAD_MAP_SIZE)
#define SET_PTHREAD_MAP_CACHE(pthread_id, win32_id) \
      (void)(GC_pthread_map_cache[PTHREAD_MAP_INDEX(pthread_id)] = (win32_id))
#define GET_PTHREAD_MAP_CACHE(pthread_id) \
          GC_pthread_map_cache[PTHREAD_MAP_INDEX(pthread_id)]
  STATIC GC_thread GC_lookup_pthread(pthread_t id)
  {
#ifndef GC_NO_THREADS_DISCOVERY
      if (GC_win32_dll_threads) {
        int i;
        LONG my_max = GC_get_max_thread_index();
        for (i = 0; i <= my_max &&
                    (!AO_load_acquire(&dll_thread_table[i].tm.in_use)
                    || THREAD_EQUAL(dll_thread_table[i].pthread_id, id));
             i++) {
        }
        return i <= my_max ? (GC_thread)(dll_thread_table + i) : NULL;
      } else
#endif
     {
      DWORD win32_id = GET_PTHREAD_MAP_CACHE(id);
      int hv_guess = THREAD_TABLE_INDEX(win32_id);
      int hv;
      GC_thread p;
      DCL_LOCK_STATE;
      LOCK();
      for (p = GC_threads[hv_guess]; 0 != p; p = p -> tm.next) {
        if (THREAD_EQUAL(p -> pthread_id, id))
          goto foundit;
      }
      for (hv = 0; hv < THREAD_TABLE_SZ; ++hv) {
        for (p = GC_threads[hv]; 0 != p; p = p -> tm.next) {
          if (THREAD_EQUAL(p -> pthread_id, id))
            goto foundit;
        }
      }
      p = 0;
     foundit:
      UNLOCK();
      return p;
    }
  }
#endif
#ifdef CAN_HANDLE_FORK
    STATIC void GC_remove_all_threads_but_me(void)
    {
      int hv;
      GC_thread me = NULL;
      DWORD thread_id;
      pthread_t pthread_id = pthread_self();
      GC_ASSERT(!GC_win32_dll_threads);
      for (hv = 0; hv < THREAD_TABLE_SZ; ++hv) {
        GC_thread p, next;
        for (p = GC_threads[hv]; 0 != p; p = next) {
          next = p -> tm.next;
          if (THREAD_EQUAL(p -> pthread_id, pthread_id)
              && me == NULL) {
            me = p;
            p -> tm.next = 0;
          } else {
#ifdef THREAD_LOCAL_ALLOC
              if ((p -> flags & FINISHED) == 0) {
                GC_remove_specific_after_fork(GC_thread_key, p -> pthread_id);
              }
#endif
            if (&first_thread != p)
              GC_INTERNAL_FREE(p);
          }
        }
        GC_threads[hv] = NULL;
      }
      GC_ASSERT(me != NULL);
      thread_id = GetCurrentThreadId();
      GC_threads[THREAD_TABLE_INDEX(thread_id)] = me;
      me -> id = thread_id;
#ifndef MSWINCE
        if (!DuplicateHandle(GetCurrentProcess(), GetCurrentThread(),
                        GetCurrentProcess(), (HANDLE *)&me->handle,
                        0 , FALSE ,
                        DUPLICATE_SAME_ACCESS))
          ABORT("DuplicateHandle failed");
#endif
#if defined(THREAD_LOCAL_ALLOC) && !defined(USE_CUSTOM_SPECIFIC)
        if (GC_setspecific(GC_thread_key, &me->tlfs) != 0)
          ABORT("GC_setspecific failed (in child)");
#endif
    }
    static void fork_prepare_proc(void)
    {
      LOCK();
#ifdef PARALLEL_MARK
        if (GC_parallel)
          GC_wait_for_reclaim();
#endif
      GC_wait_for_gc_completion(TRUE);
#ifdef PARALLEL_MARK
        if (GC_parallel)
          GC_acquire_mark_lock();
#endif
    }
    static void fork_parent_proc(void)
    {
#ifdef PARALLEL_MARK
        if (GC_parallel)
          GC_release_mark_lock();
#endif
      UNLOCK();
    }
    static void fork_child_proc(void)
    {
#ifdef PARALLEL_MARK
        if (GC_parallel) {
          GC_release_mark_lock();
          GC_parallel = FALSE;
        }
#endif
      GC_remove_all_threads_but_me();
      UNLOCK();
    }
  GC_API void GC_CALL GC_atfork_prepare(void)
  {
    if (!EXPECT(GC_is_initialized, TRUE)) GC_init();
    if (GC_handle_fork <= 0)
      fork_prepare_proc();
  }
  GC_API void GC_CALL GC_atfork_parent(void)
  {
    if (GC_handle_fork <= 0)
      fork_parent_proc();
  }
  GC_API void GC_CALL GC_atfork_child(void)
  {
    if (GC_handle_fork <= 0)
      fork_child_proc();
  }
#endif
void GC_push_thread_structures(void)
{
  GC_ASSERT(I_HOLD_LOCK());
#ifndef GC_NO_THREADS_DISCOVERY
    if (GC_win32_dll_threads) {
    } else
#endif
   {
    GC_PUSH_ALL_SYM(GC_threads);
  }
#if defined(THREAD_LOCAL_ALLOC)
    GC_PUSH_ALL_SYM(GC_thread_key);
#endif
}
#ifdef WOW64_THREAD_CONTEXT_WORKAROUND
#ifndef CONTEXT_EXCEPTION_ACTIVE
#define CONTEXT_EXCEPTION_ACTIVE    0x08000000
#define CONTEXT_EXCEPTION_REQUEST   0x40000000
#define CONTEXT_EXCEPTION_REPORTING 0x80000000
#endif
  static BOOL isWow64;
#define GET_THREAD_CONTEXT_FLAGS (isWow64 \
                        ? CONTEXT_INTEGER | CONTEXT_CONTROL \
                          | CONTEXT_EXCEPTION_REQUEST | CONTEXT_SEGMENTS \
                        : CONTEXT_INTEGER | CONTEXT_CONTROL)
#else
#define GET_THREAD_CONTEXT_FLAGS (CONTEXT_INTEGER | CONTEXT_CONTROL)
#endif
STATIC void GC_suspend(GC_thread t)
{
#ifndef MSWINCE
    DWORD exitCode;
#ifdef RETRY_GET_THREAD_CONTEXT
      int retry_cnt;
#define MAX_SUSPEND_THREAD_RETRIES (1000 * 1000)
#endif
#endif
#ifdef DEBUG_THREADS
    GC_log_printf("Suspending 0x%x\n", (int)t->id);
#endif
  UNPROTECT_THREAD(t);
  GC_acquire_dirty_lock();
#ifdef MSWINCE
    while (SuspendThread(THREAD_HANDLE(t)) == (DWORD)-1) {
      GC_release_dirty_lock();
      Sleep(10);
      GC_acquire_dirty_lock();
    }
#elif defined(RETRY_GET_THREAD_CONTEXT)
    for (retry_cnt = 0;;) {
      if (GetExitCodeThread(t -> handle, &exitCode)
          && exitCode != STILL_ACTIVE) {
        GC_release_dirty_lock();
#ifdef GC_PTHREADS
          t -> stack_base = 0;
#else
          GC_ASSERT(GC_win32_dll_threads);
          GC_delete_gc_thread_no_free(t);
#endif
        return;
      }
      if (SuspendThread(t->handle) != (DWORD)-1) {
        CONTEXT context;
        context.ContextFlags = GET_THREAD_CONTEXT_FLAGS;
        if (GetThreadContext(t->handle, &context)) {
          t->context_sp = copy_ptr_regs(t->context_regs, &context);
          break;
        }
        if (ResumeThread(t->handle) == (DWORD)-1)
          ABORT("ResumeThread failed in suspend loop");
      }
      if (retry_cnt > 1) {
        GC_release_dirty_lock();
        Sleep(0);
        GC_acquire_dirty_lock();
      }
      if (++retry_cnt >= MAX_SUSPEND_THREAD_RETRIES)
        ABORT("SuspendThread loop failed");
    }
#else
    if (GetExitCodeThread(t -> handle, &exitCode)
        && exitCode != STILL_ACTIVE) {
      GC_release_dirty_lock();
#ifdef GC_PTHREADS
        t -> stack_base = 0;
#else
        GC_ASSERT(GC_win32_dll_threads);
        GC_delete_gc_thread_no_free(t);
#endif
      return;
    }
    if (SuspendThread(t -> handle) == (DWORD)-1)
      ABORT("SuspendThread failed");
#endif
  t -> suspended = (unsigned char)TRUE;
  GC_release_dirty_lock();
  if (GC_on_thread_event)
    GC_on_thread_event(GC_EVENT_THREAD_SUSPENDED, THREAD_HANDLE(t));
}
#if defined(GC_ASSERTIONS) \
    && ((defined(MSWIN32) && !defined(CONSOLE_LOG)) || defined(MSWINCE))
  GC_INNER GC_bool GC_write_disabled = FALSE;
#endif
GC_INNER void GC_stop_world(void)
{
  DWORD thread_id = GetCurrentThreadId();
  if (!GC_thr_initialized)
    ABORT("GC_stop_world() called before GC_thr_init()");
  GC_ASSERT(I_HOLD_LOCK());
#ifdef PARALLEL_MARK
    if (GC_parallel) {
      GC_acquire_mark_lock();
      GC_ASSERT(GC_fl_builder_count == 0);
    }
#endif
#if !defined(GC_NO_THREADS_DISCOVERY) || defined(GC_ASSERTIONS)
    GC_please_stop = TRUE;
#endif
#if (defined(MSWIN32) && !defined(CONSOLE_LOG)) || defined(MSWINCE)
    GC_ASSERT(!GC_write_disabled);
    EnterCriticalSection(&GC_write_cs);
#ifdef GC_ASSERTIONS
      GC_write_disabled = TRUE;
#endif
#endif
#ifndef GC_NO_THREADS_DISCOVERY
    if (GC_win32_dll_threads) {
      int i;
      int my_max;
      AO_store(&GC_attached_thread, FALSE);
      my_max = (int)GC_get_max_thread_index();
      for (i = 0; i <= my_max; i++) {
        GC_vthread t = dll_thread_table + i;
        if (t -> stack_base != 0 && t -> thread_blocked_sp == NULL
            && t -> id != thread_id) {
          GC_suspend((GC_thread)t);
        }
      }
    } else
#endif
   {
    GC_thread t;
    int i;
    for (i = 0; i < THREAD_TABLE_SZ; i++) {
      for (t = GC_threads[i]; t != 0; t = t -> tm.next) {
        if (t -> stack_base != 0 && t -> thread_blocked_sp == NULL
            && !KNOWN_FINISHED(t) && t -> id != thread_id) {
          GC_suspend(t);
        }
      }
    }
  }
#if (defined(MSWIN32) && !defined(CONSOLE_LOG)) || defined(MSWINCE)
#ifdef GC_ASSERTIONS
      GC_write_disabled = FALSE;
#endif
    LeaveCriticalSection(&GC_write_cs);
#endif
#ifdef PARALLEL_MARK
    if (GC_parallel)
      GC_release_mark_lock();
#endif
}
GC_INNER void GC_start_world(void)
{
#ifdef GC_ASSERTIONS
    DWORD thread_id = GetCurrentThreadId();
#endif
  GC_ASSERT(I_HOLD_LOCK());
  if (GC_win32_dll_threads) {
    LONG my_max = GC_get_max_thread_index();
    int i;
    for (i = 0; i <= my_max; i++) {
      GC_thread t = (GC_thread)(dll_thread_table + i);
      if (t -> suspended) {
#ifdef DEBUG_THREADS
          GC_log_printf("Resuming 0x%x\n", (int)t->id);
#endif
        GC_ASSERT(t -> stack_base != 0 && t -> id != thread_id);
        if (ResumeThread(THREAD_HANDLE(t)) == (DWORD)-1)
          ABORT("ResumeThread failed");
        t -> suspended = FALSE;
        if (GC_on_thread_event)
          GC_on_thread_event(GC_EVENT_THREAD_UNSUSPENDED, THREAD_HANDLE(t));
      }
    }
  } else {
    GC_thread t;
    int i;
    for (i = 0; i < THREAD_TABLE_SZ; i++) {
      for (t = GC_threads[i]; t != 0; t = t -> tm.next) {
        if (t -> suspended) {
#ifdef DEBUG_THREADS
            GC_log_printf("Resuming 0x%x\n", (int)t->id);
#endif
          GC_ASSERT(t -> stack_base != 0 && t -> id != thread_id);
          if (ResumeThread(THREAD_HANDLE(t)) == (DWORD)-1)
            ABORT("ResumeThread failed");
          UNPROTECT_THREAD(t);
          t -> suspended = FALSE;
          if (GC_on_thread_event)
            GC_on_thread_event(GC_EVENT_THREAD_UNSUSPENDED, THREAD_HANDLE(t));
        } else {
#ifdef DEBUG_THREADS
            GC_log_printf("Not resuming thread 0x%x as it is not suspended\n",
                          (int)t->id);
#endif
        }
      }
    }
  }
#if !defined(GC_NO_THREADS_DISCOVERY) || defined(GC_ASSERTIONS)
    GC_please_stop = FALSE;
#endif
}
#ifdef MSWINCE
#define GC_wince_evaluate_stack_min(s) \
                        (ptr_t)(((word)(s) - 1) & ~(word)0xFFFF)
#elif defined(GC_ASSERTIONS)
#define GC_dont_query_stack_min FALSE
#endif
static ptr_t last_address = 0;
static MEMORY_BASIC_INFORMATION last_info;
STATIC ptr_t GC_get_stack_min(ptr_t s)
{
  ptr_t bottom;
  GC_ASSERT(I_HOLD_LOCK());
  if (s != last_address) {
    VirtualQuery(s, &last_info, sizeof(last_info));
    last_address = s;
  }
  do {
    bottom = (ptr_t)last_info.BaseAddress;
    VirtualQuery(bottom - 1, &last_info, sizeof(last_info));
    last_address = bottom - 1;
  } while ((last_info.Protect & PAGE_READWRITE)
           && !(last_info.Protect & PAGE_GUARD));
  return(bottom);
}
static GC_bool may_be_in_stack(ptr_t s)
{
  GC_ASSERT(I_HOLD_LOCK());
  if (s != last_address) {
    VirtualQuery(s, &last_info, sizeof(last_info));
    last_address = s;
  }
  return (last_info.Protect & PAGE_READWRITE)
          && !(last_info.Protect & PAGE_GUARD);
}
static ptr_t copy_ptr_regs(word *regs, const CONTEXT *pcontext) {
    ptr_t sp;
    int cnt = 0;
#define context (*pcontext)
#define PUSH1(reg) (regs[cnt++] = (word)pcontext->reg)
#define PUSH2(r1,r2) (PUSH1(r1), PUSH1(r2))
#define PUSH4(r1,r2,r3,r4) (PUSH2(r1,r2), PUSH2(r3,r4))
#if defined(I386)
#ifdef WOW64_THREAD_CONTEXT_WORKAROUND
        PUSH2(ContextFlags, SegFs);
#endif
      PUSH4(Edi,Esi,Ebx,Edx), PUSH2(Ecx,Eax), PUSH1(Ebp);
      sp = (ptr_t)context.Esp;
#elif defined(X86_64)
      PUSH4(Rax,Rcx,Rdx,Rbx); PUSH2(Rbp, Rsi); PUSH1(Rdi);
      PUSH4(R8, R9, R10, R11); PUSH4(R12, R13, R14, R15);
      sp = (ptr_t)context.Rsp;
#elif defined(ARM32)
      PUSH4(R0,R1,R2,R3),PUSH4(R4,R5,R6,R7),PUSH4(R8,R9,R10,R11);
      PUSH1(R12);
      sp = (ptr_t)context.Sp;
#elif defined(AARCH64)
      PUSH4(X0,X1,X2,X3),PUSH4(X4,X5,X6,X7),PUSH4(X8,X9,X10,X11);
      PUSH4(X12,X13,X14,X15),PUSH4(X16,X17,X18,X19),PUSH4(X20,X21,X22,X23);
      PUSH4(X24,X25,X26,X27),PUSH1(X28);
      PUSH1(Lr);
      sp = (ptr_t)context.Sp;
#elif defined(SHx)
      PUSH4(R0,R1,R2,R3), PUSH4(R4,R5,R6,R7), PUSH4(R8,R9,R10,R11);
      PUSH2(R12,R13), PUSH1(R14);
      sp = (ptr_t)context.R15;
#elif defined(MIPS)
      PUSH4(IntAt,IntV0,IntV1,IntA0), PUSH4(IntA1,IntA2,IntA3,IntT0);
      PUSH4(IntT1,IntT2,IntT3,IntT4), PUSH4(IntT5,IntT6,IntT7,IntS0);
      PUSH4(IntS1,IntS2,IntS3,IntS4), PUSH4(IntS5,IntS6,IntS7,IntT8);
      PUSH4(IntT9,IntK0,IntK1,IntS8);
      sp = (ptr_t)context.IntSp;
#elif defined(PPC)
      PUSH4(Gpr0, Gpr3, Gpr4, Gpr5),  PUSH4(Gpr6, Gpr7, Gpr8, Gpr9);
      PUSH4(Gpr10,Gpr11,Gpr12,Gpr14), PUSH4(Gpr15,Gpr16,Gpr17,Gpr18);
      PUSH4(Gpr19,Gpr20,Gpr21,Gpr22), PUSH4(Gpr23,Gpr24,Gpr25,Gpr26);
      PUSH4(Gpr27,Gpr28,Gpr29,Gpr30), PUSH1(Gpr31);
      sp = (ptr_t)context.Gpr1;
#elif defined(ALPHA)
      PUSH4(IntV0,IntT0,IntT1,IntT2), PUSH4(IntT3,IntT4,IntT5,IntT6);
      PUSH4(IntT7,IntS0,IntS1,IntS2), PUSH4(IntS3,IntS4,IntS5,IntFp);
      PUSH4(IntA0,IntA1,IntA2,IntA3), PUSH4(IntA4,IntA5,IntT8,IntT9);
      PUSH4(IntT10,IntT11,IntT12,IntAt);
      sp = (ptr_t)context.IntSp;
#elif defined(CPPCHECK)
      sp = (ptr_t)(word)cnt;
#else
#error Architecture is not supported
#endif
#undef context
    GC_ASSERT(cnt == PUSHED_REGS_COUNT);
    return sp;
}
STATIC word GC_push_stack_for(GC_thread thread, DWORD me)
{
  ptr_t sp, stack_min;
  struct GC_traced_stack_sect_s *traced_stack_sect =
                                      thread -> traced_stack_sect;
  if (thread -> id == me) {
    GC_ASSERT(thread -> thread_blocked_sp == NULL);
    sp = GC_approx_sp();
  } else if ((sp = thread -> thread_blocked_sp) == NULL) {
#ifdef RETRY_GET_THREAD_CONTEXT
      word *regs = thread->context_regs;
      if (thread->suspended) {
        sp = thread->context_sp;
      } else
#else
      word regs[PUSHED_REGS_COUNT];
#endif
       {
        CONTEXT context;
        context.ContextFlags = GET_THREAD_CONTEXT_FLAGS;
        if (GetThreadContext(THREAD_HANDLE(thread), &context)) {
          sp = copy_ptr_regs(regs, &context);
        } else {
#ifdef RETRY_GET_THREAD_CONTEXT
            sp = thread->context_sp;
            if (NULL == sp) {
              return 0;
            }
#else
            ABORT("GetThreadContext failed");
#endif
        }
      }
#ifdef THREAD_LOCAL_ALLOC
      GC_ASSERT(thread->suspended || !GC_world_stopped);
#endif
#ifndef WOW64_THREAD_CONTEXT_WORKAROUND
      GC_push_many_regs(regs, PUSHED_REGS_COUNT);
#else
      GC_push_many_regs(regs + 2, PUSHED_REGS_COUNT - 2);
      if (isWow64) {
        DWORD ContextFlags = (DWORD)regs[0];
        WORD SegFs = (WORD)regs[1];
        if ((ContextFlags & CONTEXT_EXCEPTION_REPORTING) != 0
            && (ContextFlags & (CONTEXT_EXCEPTION_ACTIVE
                                )) != 0) {
          LDT_ENTRY selector;
          PNT_TIB tib;
          if (!GetThreadSelectorEntry(THREAD_HANDLE(thread), SegFs, &selector))
            ABORT("GetThreadSelectorEntry failed");
          tib = (PNT_TIB)(selector.BaseLow
                          | (selector.HighWord.Bits.BaseMid << 16)
                          | (selector.HighWord.Bits.BaseHi << 24));
#ifdef DEBUG_THREADS
            GC_log_printf("TIB stack limit/base: %p .. %p\n",
                          (void *)tib->StackLimit, (void *)tib->StackBase);
#endif
          GC_ASSERT(!((word)thread->stack_base
                      COOLER_THAN (word)tib->StackBase));
          if (thread->stack_base != thread->initial_stack_base
              && ((word)thread->stack_base <= (word)tib->StackLimit
                  || (word)tib->StackBase < (word)thread->stack_base)) {
            WARN("GetThreadContext might return stale register values"
                 " including ESP= %p\n", sp);
          } else {
            sp = (ptr_t)tib->StackLimit;
          }
        }
#ifdef DEBUG_THREADS
          else {
            static GC_bool logged;
            if (!logged
                && (ContextFlags & CONTEXT_EXCEPTION_REPORTING) == 0) {
              GC_log_printf("CONTEXT_EXCEPTION_REQUEST not supported\n");
              logged = TRUE;
            }
          }
#endif
      }
#endif
  }
  if (thread -> last_stack_min == ADDR_LIMIT) {
#ifdef MSWINCE
      if (GC_dont_query_stack_min) {
        stack_min = GC_wince_evaluate_stack_min(traced_stack_sect != NULL ?
                      (ptr_t)traced_stack_sect : thread -> stack_base);
      } else
#endif
     {
      stack_min = GC_get_stack_min(traced_stack_sect != NULL ?
                      (ptr_t)traced_stack_sect : thread -> stack_base);
      UNPROTECT_THREAD(thread);
      thread -> last_stack_min = stack_min;
    }
  } else {
    if (traced_stack_sect != NULL &&
        (word)thread->last_stack_min > (word)traced_stack_sect) {
      UNPROTECT_THREAD(thread);
      thread -> last_stack_min = (ptr_t)traced_stack_sect;
    }
    if ((word)sp < (word)thread->stack_base
        && (word)sp >= (word)thread->last_stack_min) {
      stack_min = sp;
    } else {
      if (may_be_in_stack(thread -> id == me &&
                          (word)sp < (word)thread->last_stack_min ?
                          sp : thread -> last_stack_min)) {
        stack_min = (ptr_t)last_info.BaseAddress;
        if ((word)sp < (word)stack_min
            || (word)sp >= (word)thread->stack_base)
          stack_min = GC_get_stack_min(thread -> last_stack_min);
      } else {
        stack_min = GC_get_stack_min(thread -> stack_base);
      }
      UNPROTECT_THREAD(thread);
      thread -> last_stack_min = stack_min;
    }
  }
  GC_ASSERT(GC_dont_query_stack_min
            || stack_min == GC_get_stack_min(thread -> stack_base)
            || ((word)sp >= (word)stack_min
                && (word)stack_min < (word)thread->stack_base
                && (word)stack_min
                        > (word)GC_get_stack_min(thread -> stack_base)));
  if ((word)sp >= (word)stack_min && (word)sp < (word)thread->stack_base) {
#ifdef DEBUG_THREADS
      GC_log_printf("Pushing stack for 0x%x from sp %p to %p from 0x%x\n",
                    (int)thread->id, (void *)sp, (void *)thread->stack_base,
                    (int)me);
#endif
    GC_push_all_stack_sections(sp, thread->stack_base, traced_stack_sect);
  } else {
    if (thread -> id == me || (word)sp >= (word)thread->stack_base
        || (word)(sp + GC_page_size) < (word)stack_min)
      WARN("Thread stack pointer %p out of range, pushing everything\n",
           sp);
#ifdef DEBUG_THREADS
      GC_log_printf("Pushing stack for 0x%x from (min) %p to %p from 0x%x\n",
                    (int)thread->id, (void *)stack_min,
                    (void *)thread->stack_base, (int)me);
#endif
    GC_push_all_stack(stack_min, thread->stack_base);
  }
  return thread->stack_base - sp;
}
GC_INNER void GC_push_all_stacks(void)
{
  DWORD thread_id = GetCurrentThreadId();
  GC_bool found_me = FALSE;
#ifndef SMALL_CONFIG
    unsigned nthreads = 0;
#endif
  word total_size = 0;
#ifndef GC_NO_THREADS_DISCOVERY
    if (GC_win32_dll_threads) {
      int i;
      LONG my_max = GC_get_max_thread_index();
      for (i = 0; i <= my_max; i++) {
        GC_thread t = (GC_thread)(dll_thread_table + i);
        if (t -> tm.in_use && t -> stack_base) {
#ifndef SMALL_CONFIG
            ++nthreads;
#endif
          total_size += GC_push_stack_for(t, thread_id);
          if (t -> id == thread_id) found_me = TRUE;
        }
      }
    } else
#endif
   {
    int i;
    for (i = 0; i < THREAD_TABLE_SZ; i++) {
      GC_thread t;
      for (t = GC_threads[i]; t != 0; t = t -> tm.next) {
        if (!KNOWN_FINISHED(t) && t -> stack_base) {
#ifndef SMALL_CONFIG
            ++nthreads;
#endif
          total_size += GC_push_stack_for(t, thread_id);
          if (t -> id == thread_id) found_me = TRUE;
        }
      }
    }
  }
#ifndef SMALL_CONFIG
    GC_VERBOSE_LOG_PRINTF("Pushed %d thread stacks%s\n", nthreads,
                          GC_win32_dll_threads ?
                                " based on DllMain thread tracking" : "");
#endif
  if (!found_me && !GC_in_thread_creation)
    ABORT("Collecting from unknown thread");
  GC_total_stacksize = total_size;
}
#ifdef PARALLEL_MARK
#ifndef MAX_MARKERS
#define MAX_MARKERS 16
#endif
  static ptr_t marker_sp[MAX_MARKERS - 1];
#ifdef IA64
    static ptr_t marker_bsp[MAX_MARKERS - 1];
#endif
  static ptr_t marker_last_stack_min[MAX_MARKERS - 1];
#endif
GC_INNER void GC_get_next_stack(char *start, char *limit,
                                char **lo, char **hi)
{
  int i;
  char * current_min = ADDR_LIMIT;
  ptr_t *plast_stack_min = NULL;
  GC_thread thread = NULL;
  if (GC_win32_dll_threads) {
    LONG my_max = GC_get_max_thread_index();
    for (i = 0; i <= my_max; i++) {
      ptr_t s = (ptr_t)(dll_thread_table[i].stack_base);
      if ((word)s > (word)start && (word)s < (word)current_min) {
        plast_stack_min = (ptr_t * )
                            &dll_thread_table[i].last_stack_min;
        current_min = s;
#if defined(CPPCHECK)
          thread = (GC_thread)&dll_thread_table[i];
#endif
      }
    }
  } else {
    for (i = 0; i < THREAD_TABLE_SZ; i++) {
      GC_thread t;
      for (t = GC_threads[i]; t != 0; t = t -> tm.next) {
        ptr_t s = t -> stack_base;
        if ((word)s > (word)start && (word)s < (word)current_min) {
          plast_stack_min = &t -> last_stack_min;
          thread = t;
          current_min = s;
        }
      }
    }
#ifdef PARALLEL_MARK
      for (i = 0; i < GC_markers_m1; ++i) {
        ptr_t s = marker_sp[i];
#ifdef IA64
#endif
        if ((word)s > (word)start && (word)s < (word)current_min) {
          GC_ASSERT(marker_last_stack_min[i] != NULL);
          plast_stack_min = &marker_last_stack_min[i];
          current_min = s;
          thread = NULL;
        }
      }
#endif
  }
  *hi = current_min;
  if (current_min == ADDR_LIMIT) {
      *lo = ADDR_LIMIT;
      return;
  }
  GC_ASSERT((word)current_min > (word)start && plast_stack_min != NULL);
#ifdef MSWINCE
    if (GC_dont_query_stack_min) {
      *lo = GC_wince_evaluate_stack_min(current_min);
      return;
    }
#endif
  if ((word)current_min > (word)limit && !may_be_in_stack(limit)) {
    *lo = ADDR_LIMIT;
    return;
  }
  if (*plast_stack_min == ADDR_LIMIT
      || !may_be_in_stack(*plast_stack_min)) {
    *lo = GC_get_stack_min(current_min);
  } else {
    *lo = GC_get_stack_min(*plast_stack_min);
  }
  if (thread != NULL) {
    UNPROTECT_THREAD(thread);
  }
  *plast_stack_min = *lo;
}
#ifdef PARALLEL_MARK
#if defined(GC_PTHREADS) && !defined(GC_PTHREADS_PARAMARK)
#if !defined(__MINGW32__)
#define GC_PTHREADS_PARAMARK
#endif
#endif
#if !defined(GC_PTHREADS_PARAMARK)
    STATIC HANDLE GC_marker_cv[MAX_MARKERS - 1] = {0};
    STATIC DWORD GC_marker_Id[MAX_MARKERS - 1] = {0};
#endif
#if defined(GC_PTHREADS) && defined(HAVE_PTHREAD_SETNAME_NP_WITH_TID)
    static void set_marker_thread_name(unsigned id)
    {
      char name_buf[16];
      int len = sizeof("GC-marker-") - 1;
      BCOPY("GC-marker-", name_buf, len);
      if (id >= 10)
        name_buf[len++] = (char)('0' + (id / 10) % 10);
      name_buf[len] = (char)('0' + id % 10);
      name_buf[len + 1] = '\0';
      if (pthread_setname_np(pthread_self(), name_buf) != 0)
        WARN("pthread_setname_np failed\n", 0);
    }
#elif !defined(MSWINCE)
    static FARPROC setThreadDescription_fn;
    static void set_marker_thread_name(unsigned id)
    {
      WCHAR name_buf[16];
      int len = sizeof(L"GC-marker-") / sizeof(WCHAR) - 1;
      HRESULT hr;
      if (!setThreadDescription_fn) return;
      BCOPY(L"GC-marker-", name_buf, len * sizeof(WCHAR));
      if (id >= 10)
        name_buf[len++] = (WCHAR)('0' + (id / 10) % 10);
      name_buf[len] = (WCHAR)('0' + id % 10);
      name_buf[len + 1] = 0;
      hr = (*(HRESULT (WINAPI *)(HANDLE, const WCHAR *))
            (word)setThreadDescription_fn)(GetCurrentThread(), name_buf);
      if (FAILED(hr))
        WARN("SetThreadDescription failed\n", 0);
    }
#else
#define set_marker_thread_name(id) (void)(id)
#endif
#ifdef GC_PTHREADS_PARAMARK
    STATIC void * GC_mark_thread(void * id)
#elif defined(MSWINCE)
    STATIC DWORD WINAPI GC_mark_thread(LPVOID id)
#else
    STATIC unsigned __stdcall GC_mark_thread(void * id)
#endif
  {
    word my_mark_no = 0;
    if ((word)id == GC_WORD_MAX) return 0;
    set_marker_thread_name((unsigned)(word)id);
    marker_sp[(word)id] = GC_approx_sp();
#ifdef IA64
      marker_bsp[(word)id] = GC_save_regs_in_stack();
#endif
#if !defined(GC_PTHREADS_PARAMARK)
      GC_marker_Id[(word)id] = GetCurrentThreadId();
#endif
    GC_acquire_mark_lock();
    if (0 == --GC_fl_builder_count)
      GC_notify_all_builder();
    for (;; ++my_mark_no) {
      if (my_mark_no - GC_mark_no > (word)2) {
        my_mark_no = GC_mark_no;
      }
#ifdef DEBUG_THREADS
        GC_log_printf("Starting mark helper for mark number %lu\n",
                      (unsigned long)my_mark_no);
#endif
      GC_help_marker(my_mark_no);
    }
  }
#ifndef GC_ASSERTIONS
#define SET_MARK_LOCK_HOLDER (void)0
#define UNSET_MARK_LOCK_HOLDER (void)0
#endif
#ifdef CAN_HANDLE_FORK
    static int available_markers_m1 = 0;
#else
#define available_markers_m1 GC_markers_m1
#endif
#ifdef GC_PTHREADS_PARAMARK
#include <pthread.h>
#if defined(GC_ASSERTIONS) && !defined(NUMERIC_THREAD_ID)
#define NUMERIC_THREAD_ID(id) (unsigned long)(word)GC_PTHREAD_PTRVAL(id)
#endif
#ifdef CAN_HANDLE_FORK
      static pthread_cond_t mark_cv;
#else
      static pthread_cond_t mark_cv = PTHREAD_COND_INITIALIZER;
#endif
    GC_INNER void GC_start_mark_threads_inner(void)
    {
      int i;
      pthread_attr_t attr;
      pthread_t new_thread;
#ifndef NO_MARKER_SPECIAL_SIGMASK
        sigset_t set, oldset;
#endif
      GC_ASSERT(I_DONT_HOLD_LOCK());
      if (available_markers_m1 <= 0) return;
#ifdef CAN_HANDLE_FORK
        if (GC_parallel) return;
        {
          pthread_cond_t mark_cv_local = PTHREAD_COND_INITIALIZER;
          BCOPY(&mark_cv_local, &mark_cv, sizeof(mark_cv));
        }
#endif
      GC_ASSERT(GC_fl_builder_count == 0);
      if (0 != pthread_attr_init(&attr)) ABORT("pthread_attr_init failed");
      if (0 != pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED))
        ABORT("pthread_attr_setdetachstate failed");
#ifndef NO_MARKER_SPECIAL_SIGMASK
        if (sigfillset(&set) != 0)
          ABORT("sigfillset failed");
        if (pthread_sigmask(SIG_BLOCK, &set, &oldset) < 0) {
          WARN("pthread_sigmask set failed, no markers started,"
               " errno= %" WARN_PRIdPTR "\n", errno);
          GC_markers_m1 = 0;
          (void)pthread_attr_destroy(&attr);
          return;
        }
#endif
#ifdef CAN_HANDLE_FORK
        GC_markers_m1 = available_markers_m1;
#endif
      for (i = 0; i < available_markers_m1; ++i) {
        marker_last_stack_min[i] = ADDR_LIMIT;
        if (0 != pthread_create(&new_thread, &attr,
                                GC_mark_thread, (void *)(word)i)) {
          WARN("Marker thread creation failed\n", 0);
          GC_markers_m1 = i;
          break;
        }
      }
#ifndef NO_MARKER_SPECIAL_SIGMASK
        if (pthread_sigmask(SIG_SETMASK, &oldset, NULL) < 0) {
          WARN("pthread_sigmask restore failed, errno= %" WARN_PRIdPTR "\n",
               errno);
        }
#endif
      (void)pthread_attr_destroy(&attr);
      GC_wait_for_markers_init();
      GC_COND_LOG_PRINTF("Started %d mark helper threads\n", GC_markers_m1);
    }
#ifdef GC_ASSERTIONS
      STATIC unsigned long GC_mark_lock_holder = NO_THREAD;
#define SET_MARK_LOCK_HOLDER \
                (void)(GC_mark_lock_holder = NUMERIC_THREAD_ID(pthread_self()))
#define UNSET_MARK_LOCK_HOLDER \
                do { \
                  GC_ASSERT(GC_mark_lock_holder \
                                == NUMERIC_THREAD_ID(pthread_self())); \
                  GC_mark_lock_holder = NO_THREAD; \
                } while (0)
#endif
    static pthread_mutex_t mark_mutex = PTHREAD_MUTEX_INITIALIZER;
    static pthread_cond_t builder_cv = PTHREAD_COND_INITIALIZER;
#ifdef LOCK_STATS
      volatile AO_t GC_block_count = 0;
#endif
    GC_INNER void GC_acquire_mark_lock(void)
    {
#if defined(NUMERIC_THREAD_ID_UNIQUE) && !defined(THREAD_SANITIZER)
        GC_ASSERT(GC_mark_lock_holder != NUMERIC_THREAD_ID(pthread_self()));
#endif
      if (pthread_mutex_lock(&mark_mutex) != 0) {
        ABORT("pthread_mutex_lock failed");
      }
#ifdef LOCK_STATS
        (void)AO_fetch_and_add1(&GC_block_count);
#endif
      SET_MARK_LOCK_HOLDER;
    }
    GC_INNER void GC_release_mark_lock(void)
    {
      UNSET_MARK_LOCK_HOLDER;
      if (pthread_mutex_unlock(&mark_mutex) != 0) {
        ABORT("pthread_mutex_unlock failed");
      }
    }
    STATIC void GC_wait_builder(void)
    {
      UNSET_MARK_LOCK_HOLDER;
      if (pthread_cond_wait(&builder_cv, &mark_mutex) != 0) {
        ABORT("pthread_cond_wait failed");
      }
      GC_ASSERT(GC_mark_lock_holder == NO_THREAD);
      SET_MARK_LOCK_HOLDER;
    }
    GC_INNER void GC_wait_for_reclaim(void)
    {
      GC_acquire_mark_lock();
      while (GC_fl_builder_count > 0) {
        GC_wait_builder();
      }
      GC_release_mark_lock();
    }
    GC_INNER void GC_notify_all_builder(void)
    {
      GC_ASSERT(GC_mark_lock_holder == NUMERIC_THREAD_ID(pthread_self()));
      if (pthread_cond_broadcast(&builder_cv) != 0) {
        ABORT("pthread_cond_broadcast failed");
      }
    }
    GC_INNER void GC_wait_marker(void)
    {
      GC_ASSERT(GC_parallel);
      UNSET_MARK_LOCK_HOLDER;
      if (pthread_cond_wait(&mark_cv, &mark_mutex) != 0) {
        ABORT("pthread_cond_wait failed");
      }
      GC_ASSERT(GC_mark_lock_holder == NO_THREAD);
      SET_MARK_LOCK_HOLDER;
    }
    GC_INNER void GC_notify_all_marker(void)
    {
      GC_ASSERT(GC_parallel);
      if (pthread_cond_broadcast(&mark_cv) != 0) {
        ABORT("pthread_cond_broadcast failed");
      }
    }
#else
#ifndef MARK_THREAD_STACK_SIZE
#define MARK_THREAD_STACK_SIZE 0
#endif
    static HANDLE mark_mutex_event = (HANDLE)0;
    static HANDLE builder_cv = (HANDLE)0;
    static HANDLE mark_cv = (HANDLE)0;
    GC_INNER void GC_start_mark_threads_inner(void)
    {
      int i;
      GC_ASSERT(I_DONT_HOLD_LOCK());
      if (available_markers_m1 <= 0) return;
      GC_ASSERT(GC_fl_builder_count == 0);
      for (i = 0; i < GC_markers_m1; ++i) {
        if ((GC_marker_cv[i] = CreateEvent(NULL ,
                                        TRUE ,
                                        FALSE ,
                                        NULL )) == (HANDLE)0)
          ABORT("CreateEvent failed");
      }
      for (i = 0; i < GC_markers_m1; ++i) {
#if defined(MSWINCE) || defined(MSWIN_XBOX1)
          HANDLE handle;
          DWORD thread_id;
          marker_last_stack_min[i] = ADDR_LIMIT;
          handle = CreateThread(NULL ,
                                MARK_THREAD_STACK_SIZE ,
                                GC_mark_thread, (LPVOID)(word)i,
                                0 , &thread_id);
          if (handle == NULL) {
            WARN("Marker thread creation failed\n", 0);
            break;
          } else {
            CloseHandle(handle);
          }
#else
          GC_uintptr_t handle;
          unsigned thread_id;
          marker_last_stack_min[i] = ADDR_LIMIT;
          handle = _beginthreadex(NULL ,
                                MARK_THREAD_STACK_SIZE, GC_mark_thread,
                                (void *)(word)i, 0 , &thread_id);
          if (!handle || handle == (GC_uintptr_t)-1L) {
            WARN("Marker thread creation failed\n", 0);
            break;
          } else {
          }
#endif
      }
      while (GC_markers_m1 > i) {
        GC_markers_m1--;
        CloseHandle(GC_marker_cv[GC_markers_m1]);
      }
      GC_wait_for_markers_init();
      GC_COND_LOG_PRINTF("Started %d mark helper threads\n", GC_markers_m1);
      if (i == 0) {
        CloseHandle(mark_cv);
        CloseHandle(builder_cv);
        CloseHandle(mark_mutex_event);
      }
    }
#ifdef GC_ASSERTIONS
      STATIC DWORD GC_mark_lock_holder = NO_THREAD;
#define SET_MARK_LOCK_HOLDER \
                (void)(GC_mark_lock_holder = GetCurrentThreadId())
#define UNSET_MARK_LOCK_HOLDER \
                do { \
                  GC_ASSERT(GC_mark_lock_holder == GetCurrentThreadId()); \
                  GC_mark_lock_holder = NO_THREAD; \
                } while (0)
#endif
    STATIC  LONG GC_mark_mutex_state = 0;
#ifdef LOCK_STATS
      volatile AO_t GC_block_count = 0;
      volatile AO_t GC_unlocked_count = 0;
#endif
    GC_INNER void GC_acquire_mark_lock(void)
    {
#ifndef THREAD_SANITIZER
        GC_ASSERT(GC_mark_lock_holder != GetCurrentThreadId());
#endif
      if (InterlockedExchange(&GC_mark_mutex_state, 1 ) != 0) {
#ifdef LOCK_STATS
          (void)AO_fetch_and_add1(&GC_block_count);
#endif
        while (InterlockedExchange(&GC_mark_mutex_state,
                                   -1 ) != 0) {
          if (WaitForSingleObject(mark_mutex_event, INFINITE) == WAIT_FAILED)
            ABORT("WaitForSingleObject failed");
        }
      }
#ifdef LOCK_STATS
        else {
          (void)AO_fetch_and_add1(&GC_unlocked_count);
        }
#endif
      GC_ASSERT(GC_mark_lock_holder == NO_THREAD);
      SET_MARK_LOCK_HOLDER;
    }
    GC_INNER void GC_release_mark_lock(void)
    {
      UNSET_MARK_LOCK_HOLDER;
      if (InterlockedExchange(&GC_mark_mutex_state, 0 ) < 0) {
        if (SetEvent(mark_mutex_event) == FALSE)
          ABORT("SetEvent failed");
      }
    }
    GC_INNER void GC_wait_for_reclaim(void)
    {
      GC_ASSERT(builder_cv != 0);
      for (;;) {
        GC_acquire_mark_lock();
        if (GC_fl_builder_count == 0)
          break;
        if (ResetEvent(builder_cv) == FALSE)
          ABORT("ResetEvent failed");
        GC_release_mark_lock();
        if (WaitForSingleObject(builder_cv, INFINITE) == WAIT_FAILED)
          ABORT("WaitForSingleObject failed");
      }
      GC_release_mark_lock();
    }
    GC_INNER void GC_notify_all_builder(void)
    {
      GC_ASSERT(GC_mark_lock_holder == GetCurrentThreadId());
      GC_ASSERT(builder_cv != 0);
      GC_ASSERT(GC_fl_builder_count == 0);
      if (SetEvent(builder_cv) == FALSE)
        ABORT("SetEvent failed");
    }
    GC_INNER void GC_wait_marker(void)
    {
      HANDLE event = mark_cv;
      DWORD thread_id = GetCurrentThreadId();
      int i = GC_markers_m1;
      while (i-- > 0) {
        if (GC_marker_Id[i] == thread_id) {
          event = GC_marker_cv[i];
          break;
        }
      }
      if (ResetEvent(event) == FALSE)
        ABORT("ResetEvent failed");
      GC_release_mark_lock();
      if (WaitForSingleObject(event, INFINITE) == WAIT_FAILED)
        ABORT("WaitForSingleObject failed");
      GC_acquire_mark_lock();
    }
    GC_INNER void GC_notify_all_marker(void)
    {
      DWORD thread_id = GetCurrentThreadId();
      int i = GC_markers_m1;
      while (i-- > 0) {
        if (SetEvent(GC_marker_Id[i] != thread_id ? GC_marker_cv[i] :
                     mark_cv) == FALSE)
          ABORT("SetEvent failed");
      }
    }
#endif
  static unsigned required_markers_cnt = 0;
#endif
  typedef struct {
    LPTHREAD_START_ROUTINE start;
    LPVOID param;
  } thread_args;
  STATIC void * GC_CALLBACK GC_win32_start_inner(struct GC_stack_base *sb,
                                                 void *arg)
  {
    void * ret;
    LPTHREAD_START_ROUTINE start = ((thread_args *)arg)->start;
    LPVOID param = ((thread_args *)arg)->param;
    GC_register_my_thread(sb);
#ifdef DEBUG_THREADS
      GC_log_printf("thread 0x%lx starting...\n", (long)GetCurrentThreadId());
#endif
    GC_free(arg);
#if !defined(__GNUC__) && !defined(NO_CRT)
      ret = NULL;
      __try
#endif
    {
      ret = (void *)(word)(*start)(param);
    }
#if !defined(__GNUC__) && !defined(NO_CRT)
      __finally
#endif
    {
      GC_unregister_my_thread();
    }
#ifdef DEBUG_THREADS
      GC_log_printf("thread 0x%lx returned from start routine\n",
                    (long)GetCurrentThreadId());
#endif
    return ret;
  }
  STATIC DWORD WINAPI GC_win32_start(LPVOID arg)
  {
    return (DWORD)(word)GC_call_with_stack_base(GC_win32_start_inner, arg);
  }
  GC_API HANDLE WINAPI GC_CreateThread(
                        LPSECURITY_ATTRIBUTES lpThreadAttributes,
                        GC_WIN32_SIZE_T dwStackSize,
                        LPTHREAD_START_ROUTINE lpStartAddress,
                        LPVOID lpParameter, DWORD dwCreationFlags,
                        LPDWORD lpThreadId)
  {
    if (!EXPECT(parallel_initialized, TRUE))
      GC_init_parallel();
#ifdef DEBUG_THREADS
      GC_log_printf("About to create a thread from 0x%lx\n",
                    (long)GetCurrentThreadId());
#endif
    if (GC_win32_dll_threads) {
      return CreateThread(lpThreadAttributes, dwStackSize, lpStartAddress,
                          lpParameter, dwCreationFlags, lpThreadId);
    } else {
      thread_args *args =
                (thread_args *)GC_malloc_uncollectable(sizeof(thread_args));
      HANDLE thread_h;
      if (NULL == args) {
        SetLastError(ERROR_NOT_ENOUGH_MEMORY);
        return NULL;
      }
      args -> start = lpStartAddress;
      args -> param = lpParameter;
      GC_dirty(args);
      REACHABLE_AFTER_DIRTY(lpParameter);
      set_need_to_lock();
      thread_h = CreateThread(lpThreadAttributes, dwStackSize, GC_win32_start,
                              args, dwCreationFlags, lpThreadId);
      if (thread_h == 0) GC_free(args);
      return thread_h;
    }
  }
  GC_API DECLSPEC_NORETURN void WINAPI GC_ExitThread(DWORD dwExitCode)
  {
    GC_unregister_my_thread();
    ExitThread(dwExitCode);
  }
#if !defined(CYGWIN32) && !defined(MSWINCE) && !defined(MSWIN_XBOX1) \
     && !defined(NO_CRT)
    GC_API GC_uintptr_t GC_CALL GC_beginthreadex(
                                  void *security, unsigned stack_size,
                                  unsigned (__stdcall *start_address)(void *),
                                  void *arglist, unsigned initflag,
                                  unsigned *thrdaddr)
    {
      if (!EXPECT(parallel_initialized, TRUE))
        GC_init_parallel();
#ifdef DEBUG_THREADS
        GC_log_printf("About to create a thread from 0x%lx\n",
                      (long)GetCurrentThreadId());
#endif
      if (GC_win32_dll_threads) {
        return _beginthreadex(security, stack_size, start_address,
                              arglist, initflag, thrdaddr);
      } else {
        GC_uintptr_t thread_h;
        thread_args *args =
                (thread_args *)GC_malloc_uncollectable(sizeof(thread_args));
        if (NULL == args) {
          errno = EAGAIN;
          return 0;
        }
        args -> start = (LPTHREAD_START_ROUTINE)start_address;
        args -> param = arglist;
        GC_dirty(args);
        REACHABLE_AFTER_DIRTY(arglist);
        set_need_to_lock();
        thread_h = _beginthreadex(security, stack_size,
                        (unsigned (__stdcall *)(void *))GC_win32_start,
                        args, initflag, thrdaddr);
        if (thread_h == 0) GC_free(args);
        return thread_h;
      }
    }
    GC_API void GC_CALL GC_endthreadex(unsigned retval)
    {
      GC_unregister_my_thread();
      _endthreadex(retval);
    }
#endif
#ifdef GC_WINMAIN_REDIRECT
#if defined(MSWINCE) && defined(UNDER_CE)
#define WINMAIN_LPTSTR LPWSTR
#else
#define WINMAIN_LPTSTR LPSTR
#endif
#undef WinMain
  int WINAPI GC_WinMain(HINSTANCE, HINSTANCE, WINMAIN_LPTSTR, int);
  typedef struct {
    HINSTANCE hInstance;
    HINSTANCE hPrevInstance;
    WINMAIN_LPTSTR lpCmdLine;
    int nShowCmd;
  } main_thread_args;
  static DWORD WINAPI main_thread_start(LPVOID arg)
  {
    main_thread_args * args = (main_thread_args *) arg;
    return (DWORD)GC_WinMain(args->hInstance, args->hPrevInstance,
                             args->lpCmdLine, args->nShowCmd);
  }
  STATIC void * GC_waitForSingleObjectInfinite(void * handle)
  {
    return (void *)(word)WaitForSingleObject((HANDLE)handle, INFINITE);
  }
#ifndef WINMAIN_THREAD_STACK_SIZE
#define WINMAIN_THREAD_STACK_SIZE 0
#endif
  int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
                     WINMAIN_LPTSTR lpCmdLine, int nShowCmd)
  {
    DWORD exit_code = 1;
    main_thread_args args = {
                hInstance, hPrevInstance, lpCmdLine, nShowCmd
    };
    HANDLE thread_h;
    DWORD thread_id;
    GC_INIT();
    thread_h = GC_CreateThread(NULL ,
                        WINMAIN_THREAD_STACK_SIZE ,
                        main_thread_start, &args, 0 ,
                        &thread_id);
    if (thread_h != NULL) {
      if ((DWORD)(word)GC_do_blocking(GC_waitForSingleObjectInfinite,
                                      (void *)thread_h) == WAIT_FAILED)
        ABORT("WaitForSingleObject(main_thread) failed");
      GetExitCodeThread (thread_h, &exit_code);
      CloseHandle (thread_h);
    } else {
      ABORT("GC_CreateThread(main_thread) failed");
    }
#ifdef MSWINCE
      GC_deinit();
#endif
    return (int) exit_code;
  }
#endif
GC_API void GC_CALL GC_set_markers_count(unsigned markers GC_ATTR_UNUSED)
{
#ifdef PARALLEL_MARK
    required_markers_cnt = markers < MAX_MARKERS ? markers : MAX_MARKERS;
#endif
}
GC_INNER void GC_thr_init(void)
{
  struct GC_stack_base sb;
#if (!defined(HAVE_PTHREAD_SETNAME_NP_WITH_TID) && !defined(MSWINCE) \
      && defined(PARALLEL_MARK)) || defined(WOW64_THREAD_CONTEXT_WORKAROUND)
    HMODULE hK32 = GetModuleHandle(TEXT("kernel32.dll"));
#endif
  GC_ASSERT(I_HOLD_LOCK());
  if (GC_thr_initialized) return;
  GC_ASSERT((word)&GC_threads % sizeof(word) == 0);
#ifdef GC_NO_THREADS_DISCOVERY
#define GC_main_thread GetCurrentThreadId()
#else
    GC_main_thread = GetCurrentThreadId();
#endif
  GC_thr_initialized = TRUE;
#ifdef CAN_HANDLE_FORK
    if (GC_handle_fork) {
#ifdef CAN_CALL_ATFORK
        if (pthread_atfork(fork_prepare_proc, fork_parent_proc,
                           fork_child_proc) == 0) {
          GC_handle_fork = 1;
        } else
#endif
       if (GC_handle_fork != -1)
        ABORT("pthread_atfork failed");
    }
#endif
#ifdef WOW64_THREAD_CONTEXT_WORKAROUND
      if (hK32) {
        FARPROC pfn = GetProcAddress(hK32, "IsWow64Process");
        if (pfn
            && !(*(BOOL (WINAPI*)(HANDLE, BOOL*))(word)pfn)(
                                        GetCurrentProcess(), &isWow64))
          isWow64 = FALSE;
      }
#endif
  sb.mem_base = GC_stackbottom;
  GC_ASSERT(sb.mem_base != NULL);
#ifdef IA64
    sb.reg_base = GC_register_stackbottom;
#endif
#if defined(PARALLEL_MARK)
    {
      char * markers_string = GETENV("GC_MARKERS");
      int markers = required_markers_cnt;
      if (markers_string != NULL) {
        markers = atoi(markers_string);
        if (markers <= 0 || markers > MAX_MARKERS) {
          WARN("Too big or invalid number of mark threads: %" WARN_PRIdPTR
               "; using maximum threads\n", (signed_word)markers);
          markers = MAX_MARKERS;
        }
      } else if (0 == markers) {
#ifdef MSWINCE
          markers = (int)GC_sysinfo.dwNumberOfProcessors;
#else
#ifdef _WIN64
            DWORD_PTR procMask = 0;
            DWORD_PTR sysMask;
#else
            DWORD procMask = 0;
            DWORD sysMask;
#endif
          int ncpu = 0;
          if (
#ifdef __cplusplus
              GetProcessAffinityMask(GetCurrentProcess(), &procMask, &sysMask)
#else
              GetProcessAffinityMask(GetCurrentProcess(),
                                     (void *)&procMask, (void *)&sysMask)
#endif
              && procMask) {
            do {
              ncpu++;
            } while ((procMask &= procMask - 1) != 0);
          }
          markers = ncpu;
#endif
#if defined(GC_MIN_MARKERS) && !defined(CPPCHECK)
          if (markers < GC_MIN_MARKERS)
            markers = GC_MIN_MARKERS;
#endif
        if (markers > MAX_MARKERS)
          markers = MAX_MARKERS;
      }
      available_markers_m1 = markers - 1;
    }
      if (GC_win32_dll_threads || available_markers_m1 <= 0) {
        GC_parallel = FALSE;
        GC_COND_LOG_PRINTF(
                "Single marker thread, turning off parallel marking\n");
      } else {
#ifndef GC_PTHREADS_PARAMARK
          mark_mutex_event = CreateEvent(NULL ,
                                FALSE ,
                                FALSE , NULL );
          builder_cv = CreateEvent(NULL ,
                                TRUE ,
                                FALSE , NULL );
          mark_cv = CreateEvent(NULL , TRUE ,
                                FALSE , NULL );
          if (mark_mutex_event == (HANDLE)0 || builder_cv == (HANDLE)0
              || mark_cv == (HANDLE)0)
            ABORT("CreateEvent failed");
#endif
#if !defined(HAVE_PTHREAD_SETNAME_NP_WITH_TID) && !defined(MSWINCE)
          if (hK32)
            setThreadDescription_fn = GetProcAddress(hK32,
                                                     "SetThreadDescription");
#endif
      }
#endif
  GC_ASSERT(0 == GC_lookup_thread_inner(GC_main_thread));
  GC_register_my_thread_inner(&sb, GC_main_thread);
#undef GC_main_thread
}
#ifdef GC_PTHREADS
  struct start_info {
    void *(*start_routine)(void *);
    void *arg;
    GC_bool detached;
  };
  GC_API int GC_pthread_join(pthread_t pthread_id, void **retval)
  {
    int result;
#ifndef GC_WIN32_PTHREADS
      GC_thread t;
#endif
    DCL_LOCK_STATE;
    GC_ASSERT(!GC_win32_dll_threads);
#ifdef DEBUG_THREADS
      GC_log_printf("thread %p(0x%lx) is joining thread %p\n",
                    (void *)GC_PTHREAD_PTRVAL(pthread_self()),
                    (long)GetCurrentThreadId(),
                    (void *)GC_PTHREAD_PTRVAL(pthread_id));
#endif
#ifndef GC_WIN32_PTHREADS
      while ((t = GC_lookup_pthread(pthread_id)) == 0)
        Sleep(10);
#endif
    result = pthread_join(pthread_id, retval);
    if (0 == result) {
#ifdef GC_WIN32_PTHREADS
        GC_thread t = GC_lookup_pthread(pthread_id);
        if (NULL == t) ABORT("Thread not registered");
#endif
      LOCK();
      if ((t -> flags & FINISHED) != 0) {
        GC_delete_gc_thread_no_free(t);
        GC_INTERNAL_FREE(t);
      }
      UNLOCK();
    }
#ifdef DEBUG_THREADS
      GC_log_printf("thread %p(0x%lx) join with thread %p %s\n",
                    (void *)GC_PTHREAD_PTRVAL(pthread_self()),
                    (long)GetCurrentThreadId(),
                    (void *)GC_PTHREAD_PTRVAL(pthread_id),
                    result != 0 ? "failed" : "succeeded");
#endif
    return result;
  }
  GC_API int GC_pthread_create(pthread_t *new_thread,
                               GC_PTHREAD_CREATE_CONST pthread_attr_t *attr,
                               void *(*start_routine)(void *), void *arg)
  {
    int result;
    struct start_info * si;
    if (!EXPECT(parallel_initialized, TRUE))
      GC_init_parallel();
    GC_ASSERT(!GC_win32_dll_threads);
      si = (struct start_info *)GC_malloc_uncollectable(
                                                sizeof(struct start_info));
      if (NULL == si)
        return EAGAIN;
      si -> start_routine = start_routine;
      si -> arg = arg;
      GC_dirty(si);
      REACHABLE_AFTER_DIRTY(arg);
      if (attr != 0 &&
          pthread_attr_getdetachstate(attr, &si->detached)
          == PTHREAD_CREATE_DETACHED) {
        si->detached = TRUE;
      }
#ifdef DEBUG_THREADS
        GC_log_printf("About to create a thread from %p(0x%lx)\n",
                      (void *)GC_PTHREAD_PTRVAL(pthread_self()),
                      (long)GetCurrentThreadId());
#endif
      set_need_to_lock();
      result = pthread_create(new_thread, attr, GC_pthread_start, si);
      if (result) {
          GC_free(si);
      }
      return(result);
  }
  STATIC void * GC_CALLBACK GC_pthread_start_inner(struct GC_stack_base *sb,
                                                   void * arg)
  {
    struct start_info * si = (struct start_info *)arg;
    void * result;
    void *(*start)(void *);
    void *start_arg;
    DWORD thread_id = GetCurrentThreadId();
    pthread_t pthread_id = pthread_self();
    GC_thread me;
    DCL_LOCK_STATE;
#ifdef DEBUG_THREADS
      GC_log_printf("thread %p(0x%x) starting...\n",
                    (void *)GC_PTHREAD_PTRVAL(pthread_id), (int)thread_id);
#endif
    GC_ASSERT(!GC_win32_dll_threads);
    LOCK();
    me = GC_register_my_thread_inner(sb, thread_id);
    SET_PTHREAD_MAP_CACHE(pthread_id, thread_id);
    GC_ASSERT(me != &first_thread);
    me -> pthread_id = pthread_id;
    if (si->detached) me -> flags |= DETACHED;
    UNLOCK();
    start = si -> start_routine;
    start_arg = si -> arg;
    GC_free(si);
    pthread_cleanup_push(GC_thread_exit_proc, (void *)me);
    result = (*start)(start_arg);
    me -> status = result;
    GC_dirty(me);
    pthread_cleanup_pop(1);
#ifdef DEBUG_THREADS
      GC_log_printf("thread %p(0x%x) returned from start routine\n",
                    (void *)GC_PTHREAD_PTRVAL(pthread_id), (int)thread_id);
#endif
    return(result);
  }
  STATIC void * GC_pthread_start(void * arg)
  {
    return GC_call_with_stack_base(GC_pthread_start_inner, arg);
  }
  STATIC void GC_thread_exit_proc(void *arg)
  {
    GC_thread me = (GC_thread)arg;
    DCL_LOCK_STATE;
    GC_ASSERT(!GC_win32_dll_threads);
#ifdef DEBUG_THREADS
      GC_log_printf("thread %p(0x%lx) called pthread_exit()\n",
                    (void *)GC_PTHREAD_PTRVAL(pthread_self()),
                    (long)GetCurrentThreadId());
#endif
    LOCK();
    GC_wait_for_gc_completion(FALSE);
#if defined(THREAD_LOCAL_ALLOC)
      GC_ASSERT(GC_getspecific(GC_thread_key) == &me->tlfs);
      GC_destroy_thread_local(&(me->tlfs));
#endif
    if (me -> flags & DETACHED) {
      GC_delete_thread(GetCurrentThreadId());
    } else {
      me -> flags |= FINISHED;
    }
#if defined(THREAD_LOCAL_ALLOC)
      GC_remove_specific(GC_thread_key);
#endif
    UNLOCK();
  }
#ifndef GC_NO_PTHREAD_SIGMASK
    GC_API int GC_pthread_sigmask(int how, const sigset_t *set,
                                  sigset_t *oset)
    {
      return pthread_sigmask(how, set, oset);
    }
#endif
  GC_API int GC_pthread_detach(pthread_t thread)
  {
    int result;
    GC_thread t;
    DCL_LOCK_STATE;
    GC_ASSERT(!GC_win32_dll_threads);
    while ((t = GC_lookup_pthread(thread)) == NULL)
      Sleep(10);
    result = pthread_detach(thread);
    if (result == 0) {
      LOCK();
      t -> flags |= DETACHED;
      if ((t -> flags & FINISHED) != 0) {
        GC_delete_gc_thread_no_free(t);
        GC_INTERNAL_FREE(t);
      }
      UNLOCK();
    }
    return result;
  }
#elif !defined(GC_NO_THREADS_DISCOVERY)
#ifdef GC_INSIDE_DLL
    GC_API
#else
#define GC_DllMain DllMain
#endif
  BOOL WINAPI GC_DllMain(HINSTANCE inst GC_ATTR_UNUSED, ULONG reason,
                         LPVOID reserved GC_ATTR_UNUSED)
  {
      DWORD thread_id;
      if (!GC_win32_dll_threads && parallel_initialized) return TRUE;
      switch (reason) {
       case DLL_THREAD_ATTACH:
#ifdef PARALLEL_MARK
          if (GC_parallel) {
            break;
          }
#endif
       case DLL_PROCESS_ATTACH:
        thread_id = GetCurrentThreadId();
        if (parallel_initialized && GC_main_thread != thread_id) {
#ifdef PARALLEL_MARK
            ABORT("Cannot initialize parallel marker from DllMain");
#else
            struct GC_stack_base sb;
#ifdef GC_ASSERTIONS
              int sb_result =
#endif
                        GC_get_stack_base(&sb);
            GC_ASSERT(sb_result == GC_SUCCESS);
            GC_register_my_thread_inner(&sb, thread_id);
#endif
        }
        break;
       case DLL_THREAD_DETACH:
        GC_ASSERT(parallel_initialized);
        if (GC_win32_dll_threads) {
          GC_delete_thread(GetCurrentThreadId());
        }
        break;
       case DLL_PROCESS_DETACH:
        if (GC_win32_dll_threads) {
          int i;
          int my_max = (int)GC_get_max_thread_index();
          for (i = 0; i <= my_max; ++i) {
           if (AO_load(&(dll_thread_table[i].tm.in_use)))
             GC_delete_gc_thread_no_free(&dll_thread_table[i]);
          }
          GC_deinit();
        }
        break;
      }
      return TRUE;
  }
#endif
GC_INNER void GC_init_parallel(void)
{
#if defined(THREAD_LOCAL_ALLOC)
    GC_thread me;
    DCL_LOCK_STATE;
#endif
  if (parallel_initialized) return;
  parallel_initialized = TRUE;
  if (!GC_is_initialized) GC_init();
#if defined(CPPCHECK) && !defined(GC_NO_THREADS_DISCOVERY)
    GC_noop1((word)&GC_DllMain);
#endif
  if (GC_win32_dll_threads) {
    set_need_to_lock();
  }
#if defined(THREAD_LOCAL_ALLOC)
    LOCK();
    me = GC_lookup_thread_inner(GetCurrentThreadId());
    CHECK_LOOKUP_MY_THREAD(me);
    GC_init_thread_local(&me->tlfs);
    UNLOCK();
#endif
}
#if defined(USE_PTHREAD_LOCKS)
  GC_INNER void GC_lock(void)
  {
    pthread_mutex_lock(&GC_allocate_ml);
  }
#endif
#if defined(THREAD_LOCAL_ALLOC)
  GC_INNER void GC_mark_thread_local_free_lists(void)
  {
    int i;
    GC_thread p;
    for (i = 0; i < THREAD_TABLE_SZ; ++i) {
      for (p = GC_threads[i]; 0 != p; p = p -> tm.next) {
        if (!KNOWN_FINISHED(p)) {
#ifdef DEBUG_THREADS
            GC_log_printf("Marking thread locals for 0x%x\n", (int)p -> id);
#endif
          GC_mark_thread_local_fls_for(&(p->tlfs));
        }
      }
    }
  }
#if defined(GC_ASSERTIONS)
    void GC_check_tls(void)
    {
        int i;
        GC_thread p;
        for (i = 0; i < THREAD_TABLE_SZ; ++i) {
          for (p = GC_threads[i]; 0 != p; p = p -> tm.next) {
            if (!KNOWN_FINISHED(p))
              GC_check_tls_for(&(p->tlfs));
          }
        }
#if defined(USE_CUSTOM_SPECIFIC)
          if (GC_thread_key != 0)
            GC_check_tsd_marks(GC_thread_key);
#endif
    }
#endif
#endif
#ifndef GC_NO_THREAD_REDIRECTS
#define CreateThread GC_CreateThread
#define ExitThread GC_ExitThread
#undef _beginthreadex
#define _beginthreadex GC_beginthreadex
#undef _endthreadex
#define _endthreadex GC_endthreadex
#endif
#endif
#ifndef GC_PTHREAD_START_STANDALONE
#if defined(__GNUC__) && defined(__linux__)
#undef __EXCEPTIONS
#endif
#if defined(GC_PTHREADS) && !defined(GC_WIN32_THREADS)
#include <pthread.h>
#include <sched.h>
GC_INNER_PTHRSTART void * GC_CALLBACK GC_inner_start_routine(
                                        struct GC_stack_base *sb, void *arg)
{
  void * (*start)(void *);
  void * start_arg;
  void * result;
  volatile GC_thread me =
                GC_start_rtn_prepare_thread(&start, &start_arg, sb, arg);
#ifndef NACL
    pthread_cleanup_push(GC_thread_exit_proc, me);
#endif
  result = (*start)(start_arg);
#if defined(DEBUG_THREADS) && !defined(GC_PTHREAD_START_STANDALONE)
    GC_log_printf("Finishing thread %p\n", (void *)pthread_self());
#endif
  me -> status = result;
  GC_end_stubborn_change(me);
#ifndef NACL
    pthread_cleanup_pop(1);
#endif
  return result;
}
#endif
#endif
#ifndef GC_NO_THREAD_REDIRECTS
#define GC_PTHREAD_REDIRECTS_ONLY
#endif
