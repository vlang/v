/*
 * Copyright (c) 1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1996 by Silicon Graphics.  All rights reserved.
 * Copyright (c) 1998 by Fergus Henderson.  All rights reserved.
 * Copyright (c) 2000-2009 by Hewlett-Packard Development Company.
 * All rights reserved.
 * Copyright (c) 2008-2020 Ivan Maidanski
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

/* This should never be included directly; it is included only from gc.h. */
/* We separate it only to make gc.h more suitable as documentation.       */
#if defined(GC_H)

/* Convenient internal macro to test version of GCC.    */
#if defined(__GNUC__) && defined(__GNUC_MINOR__)
# define GC_GNUC_PREREQ(major, minor) \
            ((__GNUC__ << 16) + __GNUC_MINOR__ >= ((major) << 16) + (minor))
#else
# define GC_GNUC_PREREQ(major, minor) 0 /* FALSE */
#endif

/* Some tests for old macros.  These violate our namespace rules and    */
/* will disappear shortly.  Use the GC_ names.                          */
#if defined(SOLARIS_THREADS) || defined(_SOLARIS_THREADS) \
    || defined(_SOLARIS_PTHREADS) || defined(GC_SOLARIS_PTHREADS)
  /* We no longer support old style Solaris threads.            */
  /* GC_SOLARIS_THREADS now means pthreads.                     */
# ifndef GC_SOLARIS_THREADS
#   define GC_SOLARIS_THREADS
# endif
#endif
#if defined(IRIX_THREADS)
# define GC_IRIX_THREADS
#endif
#if defined(DGUX_THREADS) && !defined(GC_DGUX386_THREADS)
# define GC_DGUX386_THREADS
#endif
#if defined(AIX_THREADS)
# define GC_AIX_THREADS
#endif
#if defined(HPUX_THREADS)
# define GC_HPUX_THREADS
#endif
#if defined(OSF1_THREADS)
# define GC_OSF1_THREADS
#endif
#if defined(LINUX_THREADS)
# define GC_LINUX_THREADS
#endif
#if defined(WIN32_THREADS)
# define GC_WIN32_THREADS
#endif
#if defined(RTEMS_THREADS)
# define GC_RTEMS_PTHREADS
#endif
#if defined(USE_LD_WRAP)
# define GC_USE_LD_WRAP
#endif

#if defined(GC_WIN32_PTHREADS) && !defined(GC_WIN32_THREADS)
  /* Using pthreads-win32 library (or other Win32 implementation).  */
# define GC_WIN32_THREADS
#endif

#if defined(GC_AIX_THREADS) || defined(GC_DARWIN_THREADS) \
    || defined(GC_DGUX386_THREADS) || defined(GC_FREEBSD_THREADS) \
    || defined(GC_HPUX_THREADS) \
    || defined(GC_IRIX_THREADS) || defined(GC_LINUX_THREADS) \
    || defined(GC_NETBSD_THREADS) || defined(GC_OPENBSD_THREADS) \
    || defined(GC_OSF1_THREADS) || defined(GC_SOLARIS_THREADS) \
    || defined(GC_WIN32_THREADS) || defined(GC_RTEMS_PTHREADS)
# ifndef GC_THREADS
#   define GC_THREADS
# endif
#elif defined(GC_THREADS)
# if defined(__linux__)
#   define GC_LINUX_THREADS
# elif defined(__OpenBSD__)
#   define GC_OPENBSD_THREADS
# elif defined(_PA_RISC1_1) || defined(_PA_RISC2_0) || defined(hppa) \
       || defined(__HPPA) || (defined(__ia64) && defined(_HPUX_SOURCE))
#   define GC_HPUX_THREADS
# elif defined(__HAIKU__)
#   define GC_HAIKU_THREADS
# elif (defined(__DragonFly__) || defined(__FreeBSD_kernel__) \
        || defined(__FreeBSD__)) && !defined(GC_NO_FREEBSD)
#   define GC_FREEBSD_THREADS
# elif defined(__NetBSD__)
#   define GC_NETBSD_THREADS
# elif defined(__alpha) || defined(__alpha__) /* && !Linux && !xBSD */
#   define GC_OSF1_THREADS
# elif (defined(mips) || defined(__mips) || defined(_mips)) \
        && !(defined(nec_ews) || defined(_nec_ews) \
             || defined(ultrix) || defined(__ultrix))
#   define GC_IRIX_THREADS
# elif defined(__sparc) /* && !Linux */ \
       || ((defined(sun) || defined(__sun)) \
           && (defined(i386) || defined(__i386__) \
               || defined(__amd64) || defined(__amd64__)))
#   define GC_SOLARIS_THREADS
# elif defined(__APPLE__) && defined(__MACH__)
#   define GC_DARWIN_THREADS
# endif
# if defined(DGUX) && (defined(i386) || defined(__i386__))
#   define GC_DGUX386_THREADS
# endif
# if defined(_AIX)
#   define GC_AIX_THREADS
# endif
# if (defined(_WIN32) || defined(_MSC_VER) || defined(__BORLANDC__) \
      || defined(__CYGWIN32__) || defined(__CYGWIN__) || defined(__CEGCC__) \
      || defined(_WIN32_WCE) || defined(__MINGW32__)) \
     && !defined(GC_WIN32_THREADS)
    /* Either posix or native Win32 threads. */
#   define GC_WIN32_THREADS
# endif
# if defined(__rtems__) && (defined(i386) || defined(__i386__))
#   define GC_RTEMS_PTHREADS
# endif
#endif /* GC_THREADS */

#undef GC_PTHREADS
#if (!defined(GC_WIN32_THREADS) || defined(GC_WIN32_PTHREADS) \
     || defined(__CYGWIN32__) || defined(__CYGWIN__)) && defined(GC_THREADS) \
    && !defined(NN_PLATFORM_CTR) && !defined(NN_BUILD_TARGET_PLATFORM_NX)
  /* Posix threads. */
# define GC_PTHREADS
#endif

#if !defined(_PTHREADS) && defined(GC_NETBSD_THREADS)
# define _PTHREADS
#endif

#if defined(GC_DGUX386_THREADS) && !defined(_POSIX4A_DRAFT10_SOURCE)
# define _POSIX4A_DRAFT10_SOURCE 1
#endif

#if !defined(_REENTRANT) && defined(GC_PTHREADS) && !defined(GC_WIN32_THREADS)
  /* Better late than never.  This fails if system headers that depend  */
  /* on this were previously included.                                  */
# define _REENTRANT 1
#endif

#define __GC
#if !defined(_WIN32_WCE) || defined(__GNUC__)
# include <stddef.h>
# if defined(__MINGW32__) && !defined(_WIN32_WCE)
#   include <stdint.h>
    /* We mention uintptr_t.                                            */
    /* Perhaps this should be included in pure msft environments        */
    /* as well?                                                         */
# endif
#else /* _WIN32_WCE */
  /* Yet more kludges for WinCE.        */
# include <stdlib.h> /* size_t is defined here */
# ifndef _PTRDIFF_T_DEFINED
    /* ptrdiff_t is not defined */
#   define _PTRDIFF_T_DEFINED
    typedef long ptrdiff_t;
# endif
#endif /* _WIN32_WCE */

#if !defined(GC_NOT_DLL) && !defined(GC_DLL) \
    && ((defined(_DLL) && !defined(__GNUC__)) \
        || (defined(DLL_EXPORT) && defined(GC_BUILD)))
# define GC_DLL
#endif

#if defined(GC_DLL) && !defined(GC_API)

# if defined(__CEGCC__)
#   if defined(GC_BUILD)
#     define GC_API __declspec(dllexport)
#   else
#     define GC_API __declspec(dllimport)
#   endif

# elif defined(__MINGW32__)
#   if defined(__cplusplus) && defined(GC_BUILD)
#     define GC_API extern __declspec(dllexport)
#   elif defined(GC_BUILD) || defined(__MINGW32_DELAY_LOAD__)
#     define GC_API __declspec(dllexport)
#   else
#     define GC_API extern __declspec(dllimport)
#   endif

# elif defined(_MSC_VER) || defined(__DMC__) || defined(__BORLANDC__) \
        || defined(__CYGWIN__)
#   ifdef GC_BUILD
#     define GC_API extern __declspec(dllexport)
#   else
#     define GC_API __declspec(dllimport)
#   endif

# elif defined(__WATCOMC__)
#   ifdef GC_BUILD
#     define GC_API extern __declspec(dllexport)
#   else
#     define GC_API extern __declspec(dllimport)
#   endif

# elif defined(__SYMBIAN32__)
#   ifdef GC_BUILD
#     define GC_API extern EXPORT_C
#   else
#     define GC_API extern IMPORT_C
#   endif

# elif defined(__GNUC__)
    /* Only matters if used in conjunction with -fvisibility=hidden option. */
#   if defined(GC_BUILD) && !defined(GC_NO_VISIBILITY) \
            && (GC_GNUC_PREREQ(4, 0) || defined(GC_VISIBILITY_HIDDEN_SET))
#     define GC_API extern __attribute__((__visibility__("default")))
#   endif
# endif
#endif /* GC_DLL */

#ifndef GC_API
# define GC_API extern
#endif

#ifndef GC_CALL
# define GC_CALL
#endif

#ifndef GC_CALLBACK
# define GC_CALLBACK GC_CALL
#endif

#ifndef GC_ATTR_MALLOC
  /* 'malloc' attribute should be used for all malloc-like functions    */
  /* (to tell the compiler that a function may be treated as if any     */
  /* non-NULL pointer it returns cannot alias any other pointer valid   */
  /* when the function returns).  If the client code violates this rule */
  /* by using custom GC_oom_func then define GC_OOM_FUNC_RETURNS_ALIAS. */
# ifdef GC_OOM_FUNC_RETURNS_ALIAS
#   define GC_ATTR_MALLOC /* empty */
# elif GC_GNUC_PREREQ(3, 1)
#   define GC_ATTR_MALLOC __attribute__((__malloc__))
# elif defined(_MSC_VER) && (_MSC_VER >= 1900) && !defined(__EDG__)
#   define GC_ATTR_MALLOC \
                __declspec(allocator) __declspec(noalias) __declspec(restrict)
# elif defined(_MSC_VER) && _MSC_VER >= 1400
#   define GC_ATTR_MALLOC __declspec(noalias) __declspec(restrict)
# else
#   define GC_ATTR_MALLOC
# endif
#endif

#ifndef GC_ATTR_ALLOC_SIZE
  /* 'alloc_size' attribute improves __builtin_object_size correctness. */
# undef GC_ATTR_CALLOC_SIZE
# ifdef __clang__
#   if __has_attribute(__alloc_size__)
#     define GC_ATTR_ALLOC_SIZE(argnum) __attribute__((__alloc_size__(argnum)))
#     define GC_ATTR_CALLOC_SIZE(n, s) __attribute__((__alloc_size__(n, s)))
#   else
#     define GC_ATTR_ALLOC_SIZE(argnum) /* empty */
#   endif
# elif GC_GNUC_PREREQ(4, 3) && !defined(__ICC)
#   define GC_ATTR_ALLOC_SIZE(argnum) __attribute__((__alloc_size__(argnum)))
#   define GC_ATTR_CALLOC_SIZE(n, s) __attribute__((__alloc_size__(n, s)))
# else
#   define GC_ATTR_ALLOC_SIZE(argnum) /* empty */
# endif
#endif

#ifndef GC_ATTR_CALLOC_SIZE
# define GC_ATTR_CALLOC_SIZE(n, s) /* empty */
#endif

#ifndef GC_ATTR_NONNULL
# if GC_GNUC_PREREQ(4, 0)
#   define GC_ATTR_NONNULL(argnum) __attribute__((__nonnull__(argnum)))
# else
#   define GC_ATTR_NONNULL(argnum) /* empty */
# endif
#endif

#ifndef GC_ATTR_CONST
# if GC_GNUC_PREREQ(4, 0)
#   define GC_ATTR_CONST __attribute__((__const__))
# else
#   define GC_ATTR_CONST /* empty */
# endif
#endif

#ifndef GC_ATTR_DEPRECATED
# ifdef GC_BUILD
#   undef GC_ATTR_DEPRECATED
#   define GC_ATTR_DEPRECATED /* empty */
# elif GC_GNUC_PREREQ(4, 0)
#   define GC_ATTR_DEPRECATED __attribute__((__deprecated__))
# elif defined(_MSC_VER) && _MSC_VER >= 1200
#   define GC_ATTR_DEPRECATED __declspec(deprecated)
# else
#   define GC_ATTR_DEPRECATED /* empty */
# endif
#endif

#if defined(__sgi) && !defined(__GNUC__) && _COMPILER_VERSION >= 720
# define GC_ADD_CALLER
# define GC_RETURN_ADDR (GC_word)__return_address
#endif

#if defined(__linux__) || defined(__GLIBC__)
# if !defined(__native_client__)
#   include <features.h>
# endif
# if (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 1 || __GLIBC__ > 2) \
        && !defined(__ia64__) \
        && !defined(GC_MISSING_EXECINFO_H) \
        && !defined(GC_HAVE_BUILTIN_BACKTRACE)
#   define GC_HAVE_BUILTIN_BACKTRACE
# endif
# if defined(__i386__) || defined(__amd64__) || defined(__x86_64__)
#   define GC_CAN_SAVE_CALL_STACKS
# endif
#endif /* GLIBC */

#if defined(_MSC_VER) && _MSC_VER >= 1200 /* version 12.0+ (MSVC 6.0+) */ \
        && !defined(_AMD64_) && !defined(_M_X64) && !defined(_WIN32_WCE) \
        && !defined(GC_HAVE_NO_BUILTIN_BACKTRACE) \
        && !defined(GC_HAVE_BUILTIN_BACKTRACE)
# define GC_HAVE_BUILTIN_BACKTRACE
#endif

#if defined(GC_HAVE_BUILTIN_BACKTRACE) && !defined(GC_CAN_SAVE_CALL_STACKS)
# define GC_CAN_SAVE_CALL_STACKS
#endif

#if defined(__sparc__)
# define GC_CAN_SAVE_CALL_STACKS
#endif

/* If we're on a platform on which we can't save call stacks, but       */
/* gcc is normally used, we go ahead and define GC_ADD_CALLER.          */
/* We make this decision independent of whether gcc is actually being   */
/* used, in order to keep the interface consistent, and allow mixing    */
/* of compilers.                                                        */
/* This may also be desirable if it is possible but expensive to        */
/* retrieve the call chain.                                             */
#if (defined(__linux__) || defined(__DragonFly__) || defined(__FreeBSD__) \
     || defined(__FreeBSD_kernel__) || defined(__HAIKU__) \
     || defined(__NetBSD__) || defined(__OpenBSD__) \
     || defined(HOST_ANDROID) || defined(__ANDROID__)) \
    && !defined(GC_CAN_SAVE_CALL_STACKS)
# define GC_ADD_CALLER
# if GC_GNUC_PREREQ(2, 95)
    /* gcc knows how to retrieve return address, but we don't know      */
    /* how to generate call stacks.                                     */
#   define GC_RETURN_ADDR (GC_word)__builtin_return_address(0)
#   if GC_GNUC_PREREQ(4, 0) && (defined(__i386__) || defined(__amd64__) \
                        || defined(__x86_64__) /* and probably others... */) \
       && !defined(GC_NO_RETURN_ADDR_PARENT)
#     define GC_HAVE_RETURN_ADDR_PARENT
#     define GC_RETURN_ADDR_PARENT \
        (GC_word)__builtin_extract_return_addr(__builtin_return_address(1))
            /* Note: a compiler might complain that calling                 */
            /* __builtin_return_address with a nonzero argument is unsafe.  */
#   endif
# else
    /* Just pass 0 for gcc compatibility.       */
#   define GC_RETURN_ADDR 0
# endif
#endif /* !GC_CAN_SAVE_CALL_STACKS */

#ifdef GC_PTHREADS

# if (defined(GC_DARWIN_THREADS) || defined(GC_WIN32_PTHREADS) \
      || defined(__native_client__) || defined(GC_RTEMS_PTHREADS)) \
      && !defined(GC_NO_DLOPEN)
    /* Either there is no dlopen() or we do not need to intercept it.   */
#   define GC_NO_DLOPEN
# endif

# if (defined(GC_DARWIN_THREADS) || defined(GC_WIN32_PTHREADS) \
      || defined(GC_OPENBSD_THREADS) || defined(__native_client__)) \
     && !defined(GC_NO_PTHREAD_SIGMASK)
    /* Either there is no pthread_sigmask() or no need to intercept it. */
#   define GC_NO_PTHREAD_SIGMASK
# endif

# if defined(__native_client__)
    /* At present, NaCl pthread_create() prototype does not have        */
    /* "const" for its "attr" argument; also, NaCl pthread_exit() one   */
    /* does not have "noreturn" attribute.                              */
#   ifndef GC_PTHREAD_CREATE_CONST
#     define GC_PTHREAD_CREATE_CONST /* empty */
#   endif
#   ifndef GC_HAVE_PTHREAD_EXIT
#     define GC_HAVE_PTHREAD_EXIT
#     define GC_PTHREAD_EXIT_ATTRIBUTE /* empty */
#   endif
# endif

# if !defined(GC_HAVE_PTHREAD_EXIT) \
     && !defined(HOST_ANDROID) && !defined(__ANDROID__) \
     && (defined(GC_LINUX_THREADS) || defined(GC_SOLARIS_THREADS))
#   define GC_HAVE_PTHREAD_EXIT
    /* Intercept pthread_exit on Linux and Solaris.     */
#   if GC_GNUC_PREREQ(2, 7)
#     define GC_PTHREAD_EXIT_ATTRIBUTE __attribute__((__noreturn__))
#   elif defined(__NORETURN) /* used in Solaris */
#     define GC_PTHREAD_EXIT_ATTRIBUTE __NORETURN
#   else
#     define GC_PTHREAD_EXIT_ATTRIBUTE /* empty */
#   endif
# endif

# if (!defined(GC_HAVE_PTHREAD_EXIT) || defined(__native_client__)) \
     && !defined(GC_NO_PTHREAD_CANCEL)
    /* Either there is no pthread_cancel() or no need to intercept it.  */
#   define GC_NO_PTHREAD_CANCEL
# endif

#endif /* GC_PTHREADS */

#ifdef __cplusplus

#ifndef GC_ATTR_EXPLICIT
# if __cplusplus >= 201103L && !defined(__clang__) || _MSVC_LANG >= 201103L \
     || defined(CPPCHECK)
#   define GC_ATTR_EXPLICIT explicit
# else
#   define GC_ATTR_EXPLICIT /* empty */
# endif
#endif

#ifndef GC_NOEXCEPT
# if defined(__DMC__) || (defined(__BORLANDC__) \
        && (defined(_RWSTD_NO_EXCEPTIONS) || defined(_RWSTD_NO_EX_SPEC))) \
     || (defined(_MSC_VER) && defined(_HAS_EXCEPTIONS) && !_HAS_EXCEPTIONS) \
     || (defined(__WATCOMC__) && !defined(_CPPUNWIND))
#   define GC_NOEXCEPT /* empty */
#   ifndef GC_NEW_ABORTS_ON_OOM
#     define GC_NEW_ABORTS_ON_OOM
#   endif
# elif __cplusplus >= 201103L || _MSVC_LANG >= 201103L
#   define GC_NOEXCEPT noexcept
# else
#   define GC_NOEXCEPT throw()
# endif
#endif

#endif /* __cplusplus */

#endif
