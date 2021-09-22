/*
 * Copyright 1988, 1989 Hans-J. Boehm, Alan J. Demers
 * Copyright (c) 1991-1995 by Xerox Corporation.  All rights reserved.
 * Copyright 1996-1999 by Silicon Graphics.  All rights reserved.
 * Copyright 1999 by Hewlett-Packard Company.  All rights reserved.
 * Copyright (C) 2007 Free Software Foundation, Inc
 * Copyright (c) 2000-2011 by Hewlett-Packard Development Company.
 * Copyright (c) 2009-2020 Ivan Maidanski
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

#ifndef GC_H
#define GC_H
#if (defined(WIN64)&&!defined(_WIN64))&&defined(_MSC_VER)
#pragma message("Warning:Expecting _WIN64 for x64 targets!Notice the leading underscore!")
#endif
#if defined(GC_H)
#define GC_TMP_VERSION_MAJOR 8
#define GC_TMP_VERSION_MINOR 1
#define GC_TMP_VERSION_MICRO 0
#ifdef GC_VERSION_MAJOR
#if GC_TMP_VERSION_MAJOR!=GC_VERSION_MAJOR||GC_TMP_VERSION_MINOR!=GC_VERSION_MINOR||GC_TMP_VERSION_MICRO!=GC_VERSION_MICRO
#error Inconsistent version info. Check README.md,include/gc_version.h and configure.ac.
#endif
#else
#define GC_VERSION_MAJOR GC_TMP_VERSION_MAJOR
#define GC_VERSION_MINOR GC_TMP_VERSION_MINOR
#define GC_VERSION_MICRO GC_TMP_VERSION_MICRO
#endif
#endif
#if defined(GC_H)
#if defined(__GNUC__)&&defined(__GNUC_MINOR__)
#define GC_GNUC_PREREQ(major,minor)((__GNUC__<<16)+__GNUC_MINOR__>=((major)<<16)+(minor))
#else
#define GC_GNUC_PREREQ(major,minor)0
#endif
#if defined(SOLARIS_THREADS)||defined(_SOLARIS_THREADS)||defined(_SOLARIS_PTHREADS)||defined(GC_SOLARIS_PTHREADS)
#ifndef GC_SOLARIS_THREADS
#define GC_SOLARIS_THREADS
#endif
#endif
#if defined(IRIX_THREADS)
#define GC_IRIX_THREADS
#endif
#if defined(DGUX_THREADS)&&!defined(GC_DGUX386_THREADS)
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
#if defined(GC_WIN32_PTHREADS)&&!defined(GC_WIN32_THREADS)
#define GC_WIN32_THREADS
#endif
#if defined(GC_AIX_THREADS)||defined(GC_DARWIN_THREADS)||defined(GC_DGUX386_THREADS)||defined(GC_FREEBSD_THREADS)||defined(GC_HPUX_THREADS)||defined(GC_IRIX_THREADS)||defined(GC_LINUX_THREADS)||defined(GC_NETBSD_THREADS)||defined(GC_OPENBSD_THREADS)||defined(GC_OSF1_THREADS)||defined(GC_SOLARIS_THREADS)||defined(GC_WIN32_THREADS)||defined(GC_RTEMS_PTHREADS)
#ifndef GC_THREADS
#define GC_THREADS
#endif
#elif defined(GC_THREADS)
#if defined(__linux__)
#define GC_LINUX_THREADS
#elif defined(__OpenBSD__)
#define GC_OPENBSD_THREADS
#elif defined(_PA_RISC1_1)||defined(_PA_RISC2_0)||defined(hppa)||defined(__HPPA)||(defined(__ia64)&&defined(_HPUX_SOURCE))
#define GC_HPUX_THREADS
#elif defined(__HAIKU__)
#define GC_HAIKU_THREADS
#elif defined(__DragonFly__)||defined(__FreeBSD_kernel__)||(defined(__FreeBSD__)&&!defined(SN_TARGET_ORBIS))
#define GC_FREEBSD_THREADS
#elif defined(__NetBSD__)
#define GC_NETBSD_THREADS
#elif defined(__alpha)||defined(__alpha__)
#define GC_OSF1_THREADS
#elif (defined(mips)||defined(__mips)||defined(_mips))&&!(defined(nec_ews)||defined(_nec_ews)||defined(ultrix)||defined(__ultrix))
#define GC_IRIX_THREADS
#elif defined(__sparc)||((defined(sun)||defined(__sun))&&(defined(i386)||defined(__i386__)||defined(__amd64)||defined(__amd64__)))
#define GC_SOLARIS_THREADS
#elif defined(__APPLE__)&&defined(__MACH__)
#define GC_DARWIN_THREADS
#endif
#if defined(DGUX)&&(defined(i386)||defined(__i386__))
#define GC_DGUX386_THREADS
#endif
#if defined(_AIX)
#define GC_AIX_THREADS
#endif
#if (defined(_WIN32)||defined(_MSC_VER)||defined(__BORLANDC__)||defined(__CYGWIN32__)||defined(__CYGWIN__)||defined(__CEGCC__)||defined(_WIN32_WCE)||defined(__MINGW32__))&&!defined(GC_WIN32_THREADS)
#define GC_WIN32_THREADS
#endif
#if defined(__rtems__)&&(defined(i386)||defined(__i386__))
#define GC_RTEMS_PTHREADS
#endif
#endif
#undef GC_PTHREADS
#if (!defined(GC_WIN32_THREADS)||defined(GC_WIN32_PTHREADS)||defined(__CYGWIN32__)||defined(__CYGWIN__))&&defined(GC_THREADS)&&!defined(NN_PLATFORM_CTR)&&!defined(NN_BUILD_TARGET_PLATFORM_NX)
#define GC_PTHREADS
#endif
#if!defined(_PTHREADS)&&defined(GC_NETBSD_THREADS)
#define _PTHREADS
#endif
#if defined(GC_DGUX386_THREADS)&&!defined(_POSIX4A_DRAFT10_SOURCE)
#define _POSIX4A_DRAFT10_SOURCE 1
#endif
#if!defined(_REENTRANT)&&defined(GC_PTHREADS)&&!defined(GC_WIN32_THREADS)
#define _REENTRANT 1
#endif
#define __GC
#if!defined(_WIN32_WCE)||defined(__GNUC__)
#include <stddef.h>
#if defined(__MINGW32__)&&!defined(_WIN32_WCE)
#include <stdint.h>
#endif
#else
#include <stdlib.h>
#ifndef _PTRDIFF_T_DEFINED
#define _PTRDIFF_T_DEFINED
typedef long ptrdiff_t;
#endif
#endif
#if!defined(GC_NOT_DLL)&&!defined(GC_DLL)&&((defined(_DLL)&&!defined(__GNUC__))||(defined(DLL_EXPORT)&&defined(GC_BUILD)))
#define GC_DLL
#endif
#if defined(GC_DLL)&&!defined(GC_API)
#if defined(__CEGCC__)
#if defined(GC_BUILD)
#define GC_API __declspec(dllexport)
#else
#define GC_API __declspec(dllimport)
#endif
#elif defined(__MINGW32__)
#if defined(__cplusplus)&&defined(GC_BUILD)
#define GC_API extern __declspec(dllexport)
#elif defined(GC_BUILD)||defined(__MINGW32_DELAY_LOAD__)
#define GC_API __declspec(dllexport)
#else
#define GC_API extern __declspec(dllimport)
#endif
#elif defined(_MSC_VER)||defined(__DMC__)||defined(__BORLANDC__)||defined(__CYGWIN__)
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
#if defined(GC_BUILD)&&!defined(GC_NO_VISIBILITY)&&(GC_GNUC_PREREQ(4,0)||defined(GC_VISIBILITY_HIDDEN_SET))
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
#elif GC_GNUC_PREREQ(3,1)
#define GC_ATTR_MALLOC __attribute__((__malloc__))
#elif defined(_MSC_VER)&&(_MSC_VER>=1900)&&!defined(__EDG__)
#define GC_ATTR_MALLOC __declspec(allocator)__declspec(noalias)__declspec(restrict)
#elif defined(_MSC_VER)&&_MSC_VER>=1400
#define GC_ATTR_MALLOC __declspec(noalias)__declspec(restrict)
#else
#define GC_ATTR_MALLOC
#endif
#endif
#ifndef GC_ATTR_ALLOC_SIZE
#undef GC_ATTR_CALLOC_SIZE
#ifdef __clang__
#if __has_attribute(__alloc_size__)
#define GC_ATTR_ALLOC_SIZE(argnum)__attribute__((__alloc_size__(argnum)))
#define GC_ATTR_CALLOC_SIZE(n,s)__attribute__((__alloc_size__(n,s)))
#else
#define GC_ATTR_ALLOC_SIZE(argnum)
#endif
#elif GC_GNUC_PREREQ(4,3)&&!defined(__ICC)
#define GC_ATTR_ALLOC_SIZE(argnum)__attribute__((__alloc_size__(argnum)))
#define GC_ATTR_CALLOC_SIZE(n,s)__attribute__((__alloc_size__(n,s)))
#else
#define GC_ATTR_ALLOC_SIZE(argnum)
#endif
#endif
#ifndef GC_ATTR_CALLOC_SIZE
#define GC_ATTR_CALLOC_SIZE(n,s)
#endif
#ifndef GC_ATTR_NONNULL
#if GC_GNUC_PREREQ(4,0)
#define GC_ATTR_NONNULL(argnum)__attribute__((__nonnull__(argnum)))
#else
#define GC_ATTR_NONNULL(argnum)
#endif
#endif
#ifndef GC_ATTR_CONST
#if GC_GNUC_PREREQ(4,0)
#define GC_ATTR_CONST __attribute__((__const__))
#else
#define GC_ATTR_CONST
#endif
#endif
#ifndef GC_ATTR_DEPRECATED
#ifdef GC_BUILD
#undef GC_ATTR_DEPRECATED
#define GC_ATTR_DEPRECATED
#elif GC_GNUC_PREREQ(4,0)
#define GC_ATTR_DEPRECATED __attribute__((__deprecated__))
#elif defined(_MSC_VER)&&_MSC_VER>=1200
#define GC_ATTR_DEPRECATED __declspec(deprecated)
#else
#define GC_ATTR_DEPRECATED
#endif
#endif
#if defined(__sgi)&&!defined(__GNUC__)&&_COMPILER_VERSION>=720
#define GC_ADD_CALLER
#define GC_RETURN_ADDR (GC_word)__return_address
#endif
#if defined(__linux__)||defined(__GLIBC__)
#if!defined(__native_client__)
#include <features.h>
#endif
#if (__GLIBC__==2&&__GLIBC_MINOR__>=1||__GLIBC__ > 2)&&!defined(__ia64__)&&!defined(GC_MISSING_EXECINFO_H)&&!defined(GC_HAVE_BUILTIN_BACKTRACE)
#define GC_HAVE_BUILTIN_BACKTRACE
#endif
#if defined(__i386__)||defined(__amd64__)||defined(__x86_64__)
#define GC_CAN_SAVE_CALL_STACKS
#endif
#endif
#if defined(_MSC_VER)&&_MSC_VER>=1200&&!defined(_AMD64_)&&!defined(_M_X64)&&!defined(_WIN32_WCE)&&!defined(GC_HAVE_NO_BUILTIN_BACKTRACE)&&!defined(GC_HAVE_BUILTIN_BACKTRACE)
#define GC_HAVE_BUILTIN_BACKTRACE
#endif
#if defined(GC_HAVE_BUILTIN_BACKTRACE)&&!defined(GC_CAN_SAVE_CALL_STACKS)
#define GC_CAN_SAVE_CALL_STACKS
#endif
#if defined(__sparc__)
#define GC_CAN_SAVE_CALL_STACKS
#endif
#if (defined(__linux__)||defined(__DragonFly__)||defined(__FreeBSD__)||defined(__FreeBSD_kernel__)||defined(__HAIKU__)||defined(__NetBSD__)||defined(__OpenBSD__)||defined(HOST_ANDROID)||defined(__ANDROID__))&&!defined(GC_CAN_SAVE_CALL_STACKS)
#define GC_ADD_CALLER
#if GC_GNUC_PREREQ(2,95)
#define GC_RETURN_ADDR (GC_word)__builtin_return_address(0)
#if GC_GNUC_PREREQ(4,0)&&(defined(__i386__)||defined(__amd64__)||defined(__x86_64__))
#define GC_HAVE_RETURN_ADDR_PARENT
#define GC_RETURN_ADDR_PARENT (GC_word)__builtin_extract_return_addr(__builtin_return_address(1))
#endif
#else
#define GC_RETURN_ADDR 0
#endif
#endif
#ifdef GC_PTHREADS
#if (defined(GC_DARWIN_THREADS)||defined(GC_WIN32_PTHREADS)||defined(__native_client__)||defined(GC_RTEMS_PTHREADS))&&!defined(GC_NO_DLOPEN)
#define GC_NO_DLOPEN
#endif
#if (defined(GC_DARWIN_THREADS)||defined(GC_WIN32_PTHREADS)||defined(GC_OPENBSD_THREADS)||defined(__native_client__))&&!defined(GC_NO_PTHREAD_SIGMASK)
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
#if!defined(GC_HAVE_PTHREAD_EXIT)&&!defined(HOST_ANDROID)&&!defined(__ANDROID__)&&(defined(GC_LINUX_THREADS)||defined(GC_SOLARIS_THREADS))
#define GC_HAVE_PTHREAD_EXIT
#if GC_GNUC_PREREQ(2,7)
#define GC_PTHREAD_EXIT_ATTRIBUTE __attribute__((__noreturn__))
#elif defined(__NORETURN)
#define GC_PTHREAD_EXIT_ATTRIBUTE __NORETURN
#else
#define GC_PTHREAD_EXIT_ATTRIBUTE
#endif
#endif
#if (!defined(GC_HAVE_PTHREAD_EXIT)||defined(__native_client__))&&!defined(GC_NO_PTHREAD_CANCEL)
#define GC_NO_PTHREAD_CANCEL
#endif
#endif
#ifdef __cplusplus
#ifndef GC_ATTR_EXPLICIT
#if __cplusplus>=201103L&&!defined(__clang__)||_MSVC_LANG>=201103L||defined(CPPCHECK)
#define GC_ATTR_EXPLICIT explicit
#else
#define GC_ATTR_EXPLICIT
#endif
#endif
#ifndef GC_NOEXCEPT
#if defined(__DMC__)||(defined(__BORLANDC__)&&(defined(_RWSTD_NO_EXCEPTIONS)||defined(_RWSTD_NO_EX_SPEC)))||(defined(_MSC_VER)&&defined(_HAS_EXCEPTIONS)&&!_HAS_EXCEPTIONS)||(defined(__WATCOMC__)&&!defined(_CPPUNWIND))
#define GC_NOEXCEPT
#ifndef GC_NEW_ABORTS_ON_OOM
#define GC_NEW_ABORTS_ON_OOM
#endif
#elif __cplusplus>=201103L||_MSVC_LANG>=201103L
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
typedef void*GC_PTR;
#ifdef _WIN64
#if defined(__int64)&&!defined(CPPCHECK)
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
typedef void*(GC_CALLBACK*GC_oom_func)(size_t);
GC_API GC_ATTR_DEPRECATED GC_oom_func GC_oom_fn;
GC_API void GC_CALL GC_set_oom_fn(GC_oom_func)GC_ATTR_NONNULL(1);
GC_API GC_oom_func GC_CALL GC_get_oom_fn(void);
typedef void (GC_CALLBACK*GC_on_heap_resize_proc)(GC_word);
GC_API GC_ATTR_DEPRECATED GC_on_heap_resize_proc GC_on_heap_resize;
GC_API void GC_CALL GC_set_on_heap_resize(GC_on_heap_resize_proc);
GC_API GC_on_heap_resize_proc GC_CALL GC_get_on_heap_resize(void);
typedef enum {
GC_EVENT_START,
GC_EVENT_MARK_START,
GC_EVENT_MARK_END,
GC_EVENT_RECLAIM_START,
GC_EVENT_RECLAIM_END,
GC_EVENT_END,
GC_EVENT_PRE_STOP_WORLD,
GC_EVENT_POST_STOP_WORLD,
GC_EVENT_PRE_START_WORLD,
GC_EVENT_POST_START_WORLD,
GC_EVENT_THREAD_SUSPENDED,
GC_EVENT_THREAD_UNSUSPENDED
} GC_EventType;
typedef void (GC_CALLBACK*GC_on_collection_event_proc)(GC_EventType);
GC_API void GC_CALL GC_set_on_collection_event(GC_on_collection_event_proc);
GC_API GC_on_collection_event_proc GC_CALL GC_get_on_collection_event(void);
#if defined(GC_THREADS)||(defined(GC_BUILD)&&defined(NN_PLATFORM_CTR))
typedef void (GC_CALLBACK*GC_on_thread_event_proc)(GC_EventType,
void*);
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
typedef void (GC_CALLBACK*GC_finalizer_notifier_proc)(void);
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
GC_API GC_ATTR_DEPRECATED char*GC_stackbottom;
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
GC_API void GC_CALL GC_set_handle_fork(int);
GC_API void GC_CALL GC_atfork_prepare(void);
GC_API void GC_CALL GC_atfork_parent(void);
GC_API void GC_CALL GC_atfork_child(void);
GC_API void GC_CALL GC_init(void);
GC_API int GC_CALL GC_is_init_called(void);
GC_API void GC_CALL GC_deinit(void);
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1)void*GC_CALL
GC_malloc(size_t);
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1)void*GC_CALL
GC_malloc_atomic(size_t);
GC_API GC_ATTR_MALLOC char*GC_CALL GC_strdup(const char*);
GC_API GC_ATTR_MALLOC char*GC_CALL
GC_strndup(const char*,size_t)GC_ATTR_NONNULL(1);
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1)void*GC_CALL
GC_malloc_uncollectable(size_t);
GC_API GC_ATTR_DEPRECATED void*GC_CALL GC_malloc_stubborn(size_t);
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(2)void*GC_CALL
GC_memalign(size_t,size_t);
GC_API int GC_CALL GC_posix_memalign(void**,size_t,
size_t)GC_ATTR_NONNULL(1);
GC_API void GC_CALL GC_free(void*);
#define GC_MALLOC_STUBBORN(sz)GC_MALLOC(sz)
#define GC_NEW_STUBBORN(t)GC_NEW(t)
#define GC_CHANGE_STUBBORN(p)GC_change_stubborn(p)
GC_API GC_ATTR_DEPRECATED void GC_CALL GC_change_stubborn(const void*);
GC_API void GC_CALL GC_end_stubborn_change(const void*)GC_ATTR_NONNULL(1);
GC_API void*GC_CALL GC_base(void*);
GC_API int GC_CALL GC_is_heap_ptr(const void*);
GC_API size_t GC_CALL GC_size(const void*)GC_ATTR_NONNULL(1);
GC_API void*GC_CALL GC_realloc(void*,
size_t)
GC_ATTR_ALLOC_SIZE(2);
GC_API int GC_CALL GC_expand_hp(size_t);
GC_API void GC_CALL GC_set_max_heap_size(GC_word);
GC_API void GC_CALL GC_exclude_static_roots(void*,
void*);
GC_API void GC_CALL GC_clear_exclusion_table(void);
GC_API void GC_CALL GC_clear_roots(void);
GC_API void GC_CALL GC_add_roots(void*,
void*);
GC_API void GC_CALL GC_remove_roots(void*,
void*);
GC_API void GC_CALL GC_register_displacement(size_t);
GC_API void GC_CALL GC_debug_register_displacement(size_t);
GC_API void GC_CALL GC_gcollect(void);
GC_API void GC_CALL GC_gcollect_and_unmap(void);
typedef int (GC_CALLBACK*GC_stop_func)(void);
GC_API int GC_CALL GC_try_to_collect(GC_stop_func)
GC_ATTR_NONNULL(1);
GC_API void GC_CALL GC_set_stop_func(GC_stop_func)
GC_ATTR_NONNULL(1);
GC_API GC_stop_func GC_CALL GC_get_stop_func(void);
GC_API size_t GC_CALL GC_get_heap_size(void);
GC_API size_t GC_CALL GC_get_free_bytes(void);
GC_API size_t GC_CALL GC_get_unmapped_bytes(void);
GC_API size_t GC_CALL GC_get_bytes_since_gc(void);
GC_API size_t GC_CALL GC_get_expl_freed_bytes_since_gc(void);
GC_API size_t GC_CALL GC_get_total_bytes(void);
GC_API void GC_CALL GC_get_heap_usage_safe(GC_word*,
GC_word*,
GC_word*,
GC_word*,
GC_word*);
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
};
GC_API size_t GC_CALL GC_get_prof_stats(struct GC_prof_stats_s*,
size_t);
#ifdef GC_THREADS
GC_API size_t GC_CALL GC_get_prof_stats_unsafe(struct GC_prof_stats_s*,
size_t);
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
#define GC_PROTECTS_POINTER_HEAP 1
#define GC_PROTECTS_PTRFREE_HEAP 2
#define GC_PROTECTS_STATIC_DATA 4
#define GC_PROTECTS_STACK 8
#define GC_PROTECTS_NONE 0
GC_API int GC_CALL GC_incremental_protection_needs(void);
GC_API int GC_CALL GC_collect_a_little(void);
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1)void*GC_CALL
GC_malloc_ignore_off_page(size_t);
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1)void*GC_CALL
GC_malloc_atomic_ignore_off_page(size_t);
#ifdef GC_ADD_CALLER
#define GC_EXTRAS GC_RETURN_ADDR,__FILE__,__LINE__
#define GC_EXTRA_PARAMS GC_word ra,const char*s,int i
#else
#define GC_EXTRAS __FILE__,__LINE__
#define GC_EXTRA_PARAMS const char*s,int i
#endif
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1)void*GC_CALL
GC_malloc_atomic_uncollectable(size_t);
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1)void*GC_CALL
GC_debug_malloc_atomic_uncollectable(size_t,GC_EXTRA_PARAMS);
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1)void*GC_CALL
GC_debug_malloc(size_t,GC_EXTRA_PARAMS);
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1)void*GC_CALL
GC_debug_malloc_atomic(size_t,GC_EXTRA_PARAMS);
GC_API GC_ATTR_MALLOC char*GC_CALL
GC_debug_strdup(const char*,GC_EXTRA_PARAMS);
GC_API GC_ATTR_MALLOC char*GC_CALL
GC_debug_strndup(const char*,size_t,GC_EXTRA_PARAMS)
GC_ATTR_NONNULL(1);
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1)void*GC_CALL
GC_debug_malloc_uncollectable(size_t,
GC_EXTRA_PARAMS);
GC_API GC_ATTR_DEPRECATED void*GC_CALL
GC_debug_malloc_stubborn(size_t,GC_EXTRA_PARAMS);
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1)void*GC_CALL
GC_debug_malloc_ignore_off_page(size_t,
GC_EXTRA_PARAMS);
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1)void*GC_CALL
GC_debug_malloc_atomic_ignore_off_page(size_t,
GC_EXTRA_PARAMS);
GC_API void GC_CALL GC_debug_free(void*);
GC_API void*GC_CALL GC_debug_realloc(void*,
size_t,GC_EXTRA_PARAMS)
GC_ATTR_ALLOC_SIZE(2);
GC_API GC_ATTR_DEPRECATED void GC_CALL GC_debug_change_stubborn(const void*);
GC_API void GC_CALL GC_debug_end_stubborn_change(const void*)
GC_ATTR_NONNULL(1);
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1)void*GC_CALL
GC_debug_malloc_replacement(size_t);
GC_API GC_ATTR_ALLOC_SIZE(2)void*GC_CALL
GC_debug_realloc_replacement(void*,
size_t);
#ifdef GC_DEBUG_REPLACEMENT
#define GC_MALLOC(sz)GC_debug_malloc_replacement(sz)
#define GC_REALLOC(old,sz)GC_debug_realloc_replacement(old,sz)
#elif defined(GC_DEBUG)
#define GC_MALLOC(sz)GC_debug_malloc(sz,GC_EXTRAS)
#define GC_REALLOC(old,sz)GC_debug_realloc(old,sz,GC_EXTRAS)
#else
#define GC_MALLOC(sz)GC_malloc(sz)
#define GC_REALLOC(old,sz)GC_realloc(old,sz)
#endif
#ifdef GC_DEBUG
#define GC_MALLOC_ATOMIC(sz)GC_debug_malloc_atomic(sz,GC_EXTRAS)
#define GC_STRDUP(s)GC_debug_strdup(s,GC_EXTRAS)
#define GC_STRNDUP(s,sz)GC_debug_strndup(s,sz,GC_EXTRAS)
#define GC_MALLOC_ATOMIC_UNCOLLECTABLE(sz)GC_debug_malloc_atomic_uncollectable(sz,GC_EXTRAS)
#define GC_MALLOC_UNCOLLECTABLE(sz)GC_debug_malloc_uncollectable(sz,GC_EXTRAS)
#define GC_MALLOC_IGNORE_OFF_PAGE(sz)GC_debug_malloc_ignore_off_page(sz,GC_EXTRAS)
#define GC_MALLOC_ATOMIC_IGNORE_OFF_PAGE(sz)GC_debug_malloc_atomic_ignore_off_page(sz,GC_EXTRAS)
#define GC_FREE(p)GC_debug_free(p)
#define GC_REGISTER_FINALIZER(p,f,d,of,od)GC_debug_register_finalizer(p,f,d,of,od)
#define GC_REGISTER_FINALIZER_IGNORE_SELF(p,f,d,of,od)GC_debug_register_finalizer_ignore_self(p,f,d,of,od)
#define GC_REGISTER_FINALIZER_NO_ORDER(p,f,d,of,od)GC_debug_register_finalizer_no_order(p,f,d,of,od)
#define GC_REGISTER_FINALIZER_UNREACHABLE(p,f,d,of,od)GC_debug_register_finalizer_unreachable(p,f,d,of,od)
#define GC_END_STUBBORN_CHANGE(p)GC_debug_end_stubborn_change(p)
#define GC_PTR_STORE_AND_DIRTY(p,q)GC_debug_ptr_store_and_dirty(p,q)
#define GC_GENERAL_REGISTER_DISAPPEARING_LINK(link,obj)GC_general_register_disappearing_link(link,GC_base(( void*)(obj)))
#define GC_REGISTER_LONG_LINK(link,obj)GC_register_long_link(link,GC_base(( void*)(obj)))
#define GC_REGISTER_DISPLACEMENT(n)GC_debug_register_displacement(n)
#else
#define GC_MALLOC_ATOMIC(sz)GC_malloc_atomic(sz)
#define GC_STRDUP(s)GC_strdup(s)
#define GC_STRNDUP(s,sz)GC_strndup(s,sz)
#define GC_MALLOC_ATOMIC_UNCOLLECTABLE(sz)GC_malloc_atomic_uncollectable(sz)
#define GC_MALLOC_UNCOLLECTABLE(sz)GC_malloc_uncollectable(sz)
#define GC_MALLOC_IGNORE_OFF_PAGE(sz)GC_malloc_ignore_off_page(sz)
#define GC_MALLOC_ATOMIC_IGNORE_OFF_PAGE(sz)GC_malloc_atomic_ignore_off_page(sz)
#define GC_FREE(p)GC_free(p)
#define GC_REGISTER_FINALIZER(p,f,d,of,od)GC_register_finalizer(p,f,d,of,od)
#define GC_REGISTER_FINALIZER_IGNORE_SELF(p,f,d,of,od)GC_register_finalizer_ignore_self(p,f,d,of,od)
#define GC_REGISTER_FINALIZER_NO_ORDER(p,f,d,of,od)GC_register_finalizer_no_order(p,f,d,of,od)
#define GC_REGISTER_FINALIZER_UNREACHABLE(p,f,d,of,od)GC_register_finalizer_unreachable(p,f,d,of,od)
#define GC_END_STUBBORN_CHANGE(p)GC_end_stubborn_change(p)
#define GC_PTR_STORE_AND_DIRTY(p,q)GC_ptr_store_and_dirty(p,q)
#define GC_GENERAL_REGISTER_DISAPPEARING_LINK(link,obj)GC_general_register_disappearing_link(link,obj)
#define GC_REGISTER_LONG_LINK(link,obj)GC_register_long_link(link,obj)
#define GC_REGISTER_DISPLACEMENT(n)GC_register_displacement(n)
#endif
#define GC_NEW(t)((t*)GC_MALLOC(sizeof(t)))
#define GC_NEW_ATOMIC(t)((t*)GC_MALLOC_ATOMIC(sizeof(t)))
#define GC_NEW_UNCOLLECTABLE(t)((t*)GC_MALLOC_UNCOLLECTABLE(sizeof(t)))
#ifdef GC_REQUIRE_WCSDUP
GC_API GC_ATTR_MALLOC wchar_t*GC_CALL
GC_wcsdup(const wchar_t*)GC_ATTR_NONNULL(1);
GC_API GC_ATTR_MALLOC wchar_t*GC_CALL
GC_debug_wcsdup(const wchar_t*,GC_EXTRA_PARAMS)GC_ATTR_NONNULL(1);
#ifdef GC_DEBUG
#define GC_WCSDUP(s)GC_debug_wcsdup(s,GC_EXTRAS)
#else
#define GC_WCSDUP(s)GC_wcsdup(s)
#endif
#endif
typedef void (GC_CALLBACK*GC_finalization_proc)(void*,
void*);
GC_API void GC_CALL GC_register_finalizer(void*,
GC_finalization_proc,void*,
GC_finalization_proc*,void**)
GC_ATTR_NONNULL(1);
GC_API void GC_CALL GC_debug_register_finalizer(void*,
GC_finalization_proc,void*,
GC_finalization_proc*,void**)
GC_ATTR_NONNULL(1);
GC_API void GC_CALL GC_register_finalizer_ignore_self(void*,
GC_finalization_proc,void*,
GC_finalization_proc*,void**)
GC_ATTR_NONNULL(1);
GC_API void GC_CALL GC_debug_register_finalizer_ignore_self(void*,
GC_finalization_proc,void*,
GC_finalization_proc*,void**)
GC_ATTR_NONNULL(1);
GC_API void GC_CALL GC_register_finalizer_no_order(void*,
GC_finalization_proc,void*,
GC_finalization_proc*,void**)
GC_ATTR_NONNULL(1);
GC_API void GC_CALL GC_debug_register_finalizer_no_order(void*,
GC_finalization_proc,void*,
GC_finalization_proc*,void**)
GC_ATTR_NONNULL(1);
GC_API void GC_CALL GC_register_finalizer_unreachable(void*,
GC_finalization_proc,void*,
GC_finalization_proc*,void**)
GC_ATTR_NONNULL(1);
GC_API void GC_CALL GC_debug_register_finalizer_unreachable(void*,
GC_finalization_proc,void*,
GC_finalization_proc*,void**)
GC_ATTR_NONNULL(1);
#define GC_NO_MEMORY 2
GC_API int GC_CALL GC_register_disappearing_link(void**)
GC_ATTR_NONNULL(1);
GC_API int GC_CALL GC_general_register_disappearing_link(void**,
const void*)
GC_ATTR_NONNULL(1)GC_ATTR_NONNULL(2);
GC_API int GC_CALL GC_move_disappearing_link(void**,
void**)
GC_ATTR_NONNULL(2);
GC_API int GC_CALL GC_unregister_disappearing_link(void**);
GC_API int GC_CALL GC_register_long_link(void**,
const void*)
GC_ATTR_NONNULL(1)GC_ATTR_NONNULL(2);
GC_API int GC_CALL GC_move_long_link(void**,
void**)
GC_ATTR_NONNULL(2);
GC_API int GC_CALL GC_unregister_long_link(void**);
typedef enum {
GC_TOGGLE_REF_DROP,
GC_TOGGLE_REF_STRONG,
GC_TOGGLE_REF_WEAK
} GC_ToggleRefStatus;
typedef GC_ToggleRefStatus (GC_CALLBACK*GC_toggleref_func)(void*);
GC_API void GC_CALL GC_set_toggleref_func(GC_toggleref_func);
GC_API GC_toggleref_func GC_CALL GC_get_toggleref_func(void);
GC_API int GC_CALL GC_toggleref_add(void*,int)
GC_ATTR_NONNULL(1);
typedef void (GC_CALLBACK*GC_await_finalize_proc)(void*);
GC_API void GC_CALL GC_set_await_finalize_proc(GC_await_finalize_proc);
GC_API GC_await_finalize_proc GC_CALL GC_get_await_finalize_proc(void);
GC_API int GC_CALL GC_should_invoke_finalizers(void);
GC_API int GC_CALL GC_invoke_finalizers(void);
#if defined(__GNUC__)&&!defined(__INTEL_COMPILER)
#define GC_reachable_here(ptr)__asm__ __volatile__(" "::"X"(ptr):"memory")
#else
GC_API void GC_CALL GC_noop1(GC_word);
#ifdef LINT2
#define GC_reachable_here(ptr)GC_noop1(~(GC_word)(ptr)^(~(GC_word)0))
#else
#define GC_reachable_here(ptr)GC_noop1((GC_word)(ptr))
#endif
#endif
typedef void (GC_CALLBACK*GC_warn_proc)(char*,
GC_word);
GC_API void GC_CALL GC_set_warn_proc(GC_warn_proc)GC_ATTR_NONNULL(1);
GC_API GC_warn_proc GC_CALL GC_get_warn_proc(void);
GC_API void GC_CALLBACK GC_ignore_warn_proc(char*,GC_word);
GC_API void GC_CALL GC_set_log_fd(int);
typedef void (GC_CALLBACK*GC_abort_func)(const char*);
GC_API void GC_CALL GC_set_abort_func(GC_abort_func)GC_ATTR_NONNULL(1);
GC_API GC_abort_func GC_CALL GC_get_abort_func(void);
GC_API void GC_CALL GC_abort_on_oom(void);
typedef GC_word GC_hidden_pointer;
#define GC_HIDE_POINTER(p)(~(GC_hidden_pointer)(p))
#define GC_REVEAL_POINTER(p)((void*)GC_HIDE_POINTER(p))
#if defined(I_HIDE_POINTERS)||defined(GC_I_HIDE_POINTERS)
#define HIDE_POINTER(p)GC_HIDE_POINTER(p)
#define REVEAL_POINTER(p)GC_REVEAL_POINTER(p)
#endif
#ifdef GC_THREADS
GC_API void GC_CALL GC_alloc_lock(void);
GC_API void GC_CALL GC_alloc_unlock(void);
#else
#define GC_alloc_lock()(void)0
#define GC_alloc_unlock()(void)0
#endif
typedef void*(GC_CALLBACK*GC_fn_type)(void*);
GC_API void*GC_CALL GC_call_with_alloc_lock(GC_fn_type,
void*)GC_ATTR_NONNULL(1);
struct GC_stack_base {
void*mem_base;
#if defined(__ia64)||defined(__ia64__)||defined(_M_IA64)
void*reg_base;
#endif
};
typedef void*(GC_CALLBACK*GC_stack_base_func)(
struct GC_stack_base*,void*);
GC_API void*GC_CALL GC_call_with_stack_base(GC_stack_base_func,
void*)GC_ATTR_NONNULL(1);
#define GC_SUCCESS 0
#define GC_DUPLICATE 1
#define GC_NO_THREADS 2
#define GC_UNIMPLEMENTED 3
#define GC_NOT_FOUND 4
#if defined(GC_DARWIN_THREADS)||defined(GC_WIN32_THREADS)
GC_API void GC_CALL GC_use_threads_discovery(void);
#endif
#ifdef GC_THREADS
GC_API void GC_CALL GC_set_suspend_signal(int);
GC_API void GC_CALL GC_set_thr_restart_signal(int);
GC_API int GC_CALL GC_get_suspend_signal(void);
GC_API int GC_CALL GC_get_thr_restart_signal(void);
GC_API void GC_CALL GC_start_mark_threads(void);
GC_API void GC_CALL GC_allow_register_threads(void);
GC_API int GC_CALL GC_register_my_thread(const struct GC_stack_base*)
GC_ATTR_NONNULL(1);
GC_API int GC_CALL GC_thread_is_registered(void);
GC_API void GC_CALL GC_register_altstack(void*,
GC_word,
void*,
GC_word);
GC_API int GC_CALL GC_unregister_my_thread(void);
GC_API void GC_CALL GC_stop_world_external(void);
GC_API void GC_CALL GC_start_world_external(void);
#endif
GC_API void*GC_CALL GC_do_blocking(GC_fn_type,
void*)GC_ATTR_NONNULL(1);
GC_API void*GC_CALL GC_call_with_gc_active(GC_fn_type,
void*)GC_ATTR_NONNULL(1);
GC_API int GC_CALL GC_get_stack_base(struct GC_stack_base*)
GC_ATTR_NONNULL(1);
GC_API void*GC_CALL GC_get_my_stackbottom(struct GC_stack_base*)
GC_ATTR_NONNULL(1);
GC_API void GC_CALL GC_set_stackbottom(void*,
const struct GC_stack_base*)
GC_ATTR_NONNULL(2);
GC_API void*GC_CALL GC_same_obj(void*,void*);
GC_API void*GC_CALL GC_pre_incr(void**,ptrdiff_t)
GC_ATTR_NONNULL(1);
GC_API void*GC_CALL GC_post_incr(void**,ptrdiff_t)
GC_ATTR_NONNULL(1);
GC_API void*GC_CALL GC_is_visible(void*);
GC_API void*GC_CALL GC_is_valid_displacement(void*);
GC_API void GC_CALL GC_dump(void);
GC_API void GC_CALL GC_dump_named(const char*);
GC_API void GC_CALL GC_dump_regions(void);
GC_API void GC_CALL GC_dump_finalization(void);
#if defined(GC_DEBUG)&&defined(__GNUC__)
#define GC_PTR_ADD3(x,n,type_of_result)((type_of_result)GC_same_obj((x)+(n),(x)))
#define GC_PRE_INCR3(x,n,type_of_result)((type_of_result)GC_pre_incr((void**)(&(x)),(n)*sizeof(*x)))
#define GC_POST_INCR3(x,n,type_of_result)((type_of_result)GC_post_incr((void**)(&(x)),(n)*sizeof(*x)))
#define GC_PTR_ADD(x,n)GC_PTR_ADD3(x,n,__typeof__(x))
#define GC_PRE_INCR(x,n)GC_PRE_INCR3(x,n,__typeof__(x))
#define GC_POST_INCR(x)GC_POST_INCR3(x,1,__typeof__(x))
#define GC_POST_DECR(x)GC_POST_INCR3(x,-1,__typeof__(x))
#else
#define GC_PTR_ADD(x,n)((x)+(n))
#define GC_PRE_INCR(x,n)((x)+=(n))
#define GC_POST_INCR(x)((x)++)
#define GC_POST_DECR(x)((x)--)
#endif
#ifdef GC_DEBUG
#define GC_PTR_STORE(p,q)(*(void**)GC_is_visible((void*)(p))=GC_is_valid_displacement((void*)(q)))
#else
#define GC_PTR_STORE(p,q)(*(void**)(p)=(void*)(q))
#endif
GC_API void GC_CALL GC_ptr_store_and_dirty(void*,
const void*);
GC_API void GC_CALL GC_debug_ptr_store_and_dirty(void*,
const void*);
GC_API void (GC_CALLBACK*GC_same_obj_print_proc)(void*,
void*);
GC_API void (GC_CALLBACK*GC_is_valid_displacement_print_proc)(void*);
GC_API void (GC_CALLBACK*GC_is_visible_print_proc)(void*);
#ifdef GC_PTHREADS
#ifdef __cplusplus
}
#endif
#ifdef __cplusplus
extern "C" {
#endif
#endif
GC_API GC_ATTR_MALLOC void*GC_CALL GC_malloc_many(size_t);
#define GC_NEXT(p)(*(void**)(p))
typedef int (GC_CALLBACK*GC_has_static_roots_func)(
const char*,
void*,
size_t);
GC_API void GC_CALL GC_register_has_static_roots_callback(
GC_has_static_roots_func);
#if!defined(CPPCHECK)&&!defined(GC_WINDOWS_H_INCLUDED)&&defined(WINAPI)
#define GC_WINDOWS_H_INCLUDED
#endif
#if defined(GC_WIN32_THREADS)&&(!defined(GC_PTHREADS)||defined(GC_BUILD)||defined(GC_WINDOWS_H_INCLUDED))
#if (!defined(GC_NO_THREAD_DECLS)||defined(GC_BUILD))&&!defined(GC_DONT_INCL_WINDOWS_H)
#ifdef __cplusplus
}
#endif
#if!defined(_WIN32_WCE)&&!defined(__CEGCC__)
#include <process.h>
#endif
#if defined(GC_BUILD)||!defined(GC_DONT_INCLUDE_WINDOWS_H)
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
#if!defined(_UINTPTR_T)&&!defined(_UINTPTR_T_DEFINED)&&!defined(UINTPTR_MAX)
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
GC_API BOOL WINAPI GC_DllMain(HINSTANCE,
ULONG,
LPVOID);
#else
GC_API int __stdcall GC_DllMain(void*,unsigned long,void*);
#endif
#endif
#ifdef GC_WINDOWS_H_INCLUDED
GC_API HANDLE WINAPI GC_CreateThread(
LPSECURITY_ATTRIBUTES,
GC_WIN32_SIZE_T,
LPTHREAD_START_ROUTINE,
LPVOID,DWORD,
LPDWORD);
GC_API DECLSPEC_NORETURN void WINAPI GC_ExitThread(
DWORD);
#else
struct _SECURITY_ATTRIBUTES;
GC_API void*__stdcall GC_CreateThread(struct _SECURITY_ATTRIBUTES*,
GC_WIN32_SIZE_T,
unsigned long (__stdcall*)(void*),
void*,unsigned long,unsigned long*);
GC_API DECLSPEC_NORETURN void __stdcall GC_ExitThread(unsigned long);
#endif
#if!defined(_WIN32_WCE)&&!defined(__CEGCC__)
GC_API GC_uintptr_t GC_CALL GC_beginthreadex(
void*,unsigned,
unsigned (__stdcall*)(void*),
void*,unsigned,
unsigned*);
GC_API void GC_CALL GC_endthreadex(unsigned);
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
#if defined(__CYGWIN32__)||defined(__CYGWIN__)
#ifdef __x86_64__
extern int __data_start__[],__data_end__[];
extern int __bss_start__[],__bss_end__[];
#define GC_DATASTART ((GC_word)__data_start__ < (GC_word)__bss_start__?(void*)__data_start__:(void*)__bss_start__)
#define GC_DATAEND ((GC_word)__data_end__ > (GC_word)__bss_end__?(void*)__data_end__:(void*)__bss_end__)
#else
extern int _data_start__[],_data_end__[],_bss_start__[],_bss_end__[];
#define GC_DATASTART ((GC_word)_data_start__ < (GC_word)_bss_start__?(void*)_data_start__:(void*)_bss_start__)
#define GC_DATAEND ((GC_word)_data_end__ > (GC_word)_bss_end__?(void*)_data_end__:(void*)_bss_end__)
#endif
#define GC_INIT_CONF_ROOTS GC_add_roots(GC_DATASTART,GC_DATAEND);GC_gcollect()
#elif defined(_AIX)
extern int _data[],_end[];
#define GC_DATASTART ((void*)_data)
#define GC_DATAEND ((void*)_end)
#define GC_INIT_CONF_ROOTS GC_add_roots(GC_DATASTART,GC_DATAEND)
#elif (defined(HOST_ANDROID)||defined(__ANDROID__))&&defined(IGNORE_DYNAMIC_LOADING)
#pragma weak __dso_handle
extern int __dso_handle[];
GC_API void*GC_CALL GC_find_limit(void*,int);
#define GC_INIT_CONF_ROOTS (void)(__dso_handle!=0?(GC_add_roots(__dso_handle,GC_find_limit(__dso_handle,1)),0):0)
#else
#define GC_INIT_CONF_ROOTS
#endif
#ifdef GC_DONT_EXPAND
#define GC_INIT_CONF_DONT_EXPAND GC_set_dont_expand(1)
#else
#define GC_INIT_CONF_DONT_EXPAND
#endif
#ifdef GC_FORCE_UNMAP_ON_GCOLLECT
#define GC_INIT_CONF_FORCE_UNMAP_ON_GCOLLECT GC_set_force_unmap_on_gcollect(1)
#else
#define GC_INIT_CONF_FORCE_UNMAP_ON_GCOLLECT
#endif
#ifdef GC_DONT_GC
#define GC_INIT_CONF_MAX_RETRIES (void)(GC_dont_gc=1)
#elif defined(GC_MAX_RETRIES)&&!defined(CPPCHECK)
#define GC_INIT_CONF_MAX_RETRIES GC_set_max_retries(GC_MAX_RETRIES)
#else
#define GC_INIT_CONF_MAX_RETRIES
#endif
#if defined(GC_ALLOCD_BYTES_PER_FINALIZER)&&!defined(CPPCHECK)
#define GC_INIT_CONF_ALLOCD_BYTES_PER_FINALIZER GC_set_allocd_bytes_per_finalizer(GC_ALLOCD_BYTES_PER_FINALIZER)
#else
#define GC_INIT_CONF_ALLOCD_BYTES_PER_FINALIZER
#endif
#if defined(GC_FREE_SPACE_DIVISOR)&&!defined(CPPCHECK)
#define GC_INIT_CONF_FREE_SPACE_DIVISOR GC_set_free_space_divisor(GC_FREE_SPACE_DIVISOR)
#else
#define GC_INIT_CONF_FREE_SPACE_DIVISOR
#endif
#if defined(GC_FULL_FREQ)&&!defined(CPPCHECK)
#define GC_INIT_CONF_FULL_FREQ GC_set_full_freq(GC_FULL_FREQ)
#else
#define GC_INIT_CONF_FULL_FREQ
#endif
#if defined(GC_TIME_LIMIT)&&!defined(CPPCHECK)
#define GC_INIT_CONF_TIME_LIMIT GC_set_time_limit(GC_TIME_LIMIT)
#else
#define GC_INIT_CONF_TIME_LIMIT
#endif
#if defined(GC_MARKERS)&&defined(GC_THREADS)&&!defined(CPPCHECK)
#define GC_INIT_CONF_MARKERS GC_set_markers_count(GC_MARKERS)
#else
#define GC_INIT_CONF_MARKERS
#endif
#if defined(GC_SIG_SUSPEND)&&defined(GC_THREADS)&&!defined(CPPCHECK)
#define GC_INIT_CONF_SUSPEND_SIGNAL GC_set_suspend_signal(GC_SIG_SUSPEND)
#else
#define GC_INIT_CONF_SUSPEND_SIGNAL
#endif
#if defined(GC_SIG_THR_RESTART)&&defined(GC_THREADS)&&!defined(CPPCHECK)
#define GC_INIT_CONF_THR_RESTART_SIGNAL GC_set_thr_restart_signal(GC_SIG_THR_RESTART)
#else
#define GC_INIT_CONF_THR_RESTART_SIGNAL
#endif
#if defined(GC_MAXIMUM_HEAP_SIZE)&&!defined(CPPCHECK)
#define GC_INIT_CONF_MAXIMUM_HEAP_SIZE GC_set_max_heap_size(GC_MAXIMUM_HEAP_SIZE)
#else
#define GC_INIT_CONF_MAXIMUM_HEAP_SIZE
#endif
#ifdef GC_IGNORE_WARN
#define GC_INIT_CONF_IGNORE_WARN GC_set_warn_proc(GC_ignore_warn_proc)
#else
#define GC_INIT_CONF_IGNORE_WARN
#endif
#if defined(GC_INITIAL_HEAP_SIZE)&&!defined(CPPCHECK)
#define GC_INIT_CONF_INITIAL_HEAP_SIZE { size_t heap_size=GC_get_heap_size();if (heap_size < (GC_INITIAL_HEAP_SIZE))(void)GC_expand_hp((GC_INITIAL_HEAP_SIZE)- heap_size);}
#else
#define GC_INIT_CONF_INITIAL_HEAP_SIZE
#endif
#define GC_INIT(){ GC_INIT_CONF_DONT_EXPAND;GC_INIT_CONF_FORCE_UNMAP_ON_GCOLLECT;GC_INIT_CONF_MAX_RETRIES;GC_INIT_CONF_ALLOCD_BYTES_PER_FINALIZER;GC_INIT_CONF_FREE_SPACE_DIVISOR;GC_INIT_CONF_FULL_FREQ;GC_INIT_CONF_TIME_LIMIT;GC_INIT_CONF_MARKERS;GC_INIT_CONF_SUSPEND_SIGNAL;GC_INIT_CONF_THR_RESTART_SIGNAL;GC_INIT_CONF_MAXIMUM_HEAP_SIZE;GC_init();GC_INIT_CONF_ROOTS;GC_INIT_CONF_IGNORE_WARN;GC_INIT_CONF_INITIAL_HEAP_SIZE;}
GC_API void GC_CALL GC_win32_free_heap(void);
#if defined(__SYMBIAN32__)
void GC_init_global_static_roots(void);
#endif
#if defined(_AMIGA)&&!defined(GC_AMIGA_MAKINGLIB)
void*GC_amiga_realloc(void*,size_t);
#define GC_realloc(a,b)GC_amiga_realloc(a,b)
void GC_amiga_set_toany(void (*)(void));
extern int GC_amiga_free_space_divisor_inc;
extern void*(*GC_amiga_allocwrapper_do)(size_t,void*(GC_CALL*)(size_t));
#define GC_malloc(a)(*GC_amiga_allocwrapper_do)(a,GC_malloc)
#define GC_malloc_atomic(a)(*GC_amiga_allocwrapper_do)(a,GC_malloc_atomic)
#define GC_malloc_uncollectable(a)(*GC_amiga_allocwrapper_do)(a,GC_malloc_uncollectable)
#define GC_malloc_atomic_uncollectable(a)(*GC_amiga_allocwrapper_do)(a,GC_malloc_atomic_uncollectable)
#define GC_malloc_ignore_off_page(a)(*GC_amiga_allocwrapper_do)(a,GC_malloc_ignore_off_page)
#define GC_malloc_atomic_ignore_off_page(a)(*GC_amiga_allocwrapper_do)(a,GC_malloc_atomic_ignore_off_page)
#endif
#ifdef __cplusplus
}
#endif
#endif
