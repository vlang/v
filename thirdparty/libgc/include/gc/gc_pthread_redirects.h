/*
 * Copyright (c) 1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1996 by Silicon Graphics.  All rights reserved.
 * Copyright (c) 1998 by Fergus Henderson.  All rights reserved.
 * Copyright (c) 2000-2010 by Hewlett-Packard Development Company.
 * All rights reserved.
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

/* Our pthread support normally needs to intercept a number of thread   */
/* calls.  We arrange to do that here, if appropriate.                  */

#ifndef GC_PTHREAD_REDIRECTS_H
#define GC_PTHREAD_REDIRECTS_H

/* Included from gc.h only.  Included only if GC_PTHREADS.              */
#if defined(GC_H) && defined(GC_PTHREADS)

/* We need to intercept calls to many of the threads primitives, so     */
/* that we can locate thread stacks and stop the world.                 */
/* Note also that the collector cannot always see thread specific data. */
/* Thread specific data should generally consist of pointers to         */
/* uncollectible objects (allocated with GC_malloc_uncollectable,       */
/* not the system malloc), which are deallocated using the destructor   */
/* facility in thr_keycreate.  Alternatively, keep a redundant pointer  */
/* to thread specific data on the thread stack.                         */

#ifndef GC_PTHREAD_REDIRECTS_ONLY

# include <pthread.h>
# ifndef GC_NO_DLOPEN
#   include <dlfcn.h>
# endif
# ifndef GC_NO_PTHREAD_SIGMASK
#   include <signal.h>  /* needed anyway for proper redirection */
# endif

# ifdef __cplusplus
    extern "C" {
# endif

# ifndef GC_SUSPEND_THREAD_ID
#   define GC_SUSPEND_THREAD_ID pthread_t
# endif

# ifndef GC_NO_DLOPEN
    GC_API void *GC_dlopen(const char * /* path */, int /* mode */);
# endif /* !GC_NO_DLOPEN */

# ifndef GC_NO_PTHREAD_SIGMASK
#   if defined(GC_PTHREAD_SIGMASK_NEEDED) \
        || defined(_BSD_SOURCE) || defined(_GNU_SOURCE) \
        || (_POSIX_C_SOURCE >= 199506L) || (_XOPEN_SOURCE >= 500)
      GC_API int GC_pthread_sigmask(int /* how */, const sigset_t *,
                                    sigset_t * /* oset */);
#   endif
# endif /* !GC_NO_PTHREAD_SIGMASK */

# ifndef GC_PTHREAD_CREATE_CONST
    /* This is used for pthread_create() only.    */
#   define GC_PTHREAD_CREATE_CONST const
# endif

  GC_API int GC_pthread_create(pthread_t *,
                               GC_PTHREAD_CREATE_CONST pthread_attr_t *,
                               void *(*)(void *), void * /* arg */);
  GC_API int GC_pthread_join(pthread_t, void ** /* retval */);
  GC_API int GC_pthread_detach(pthread_t);

# ifndef GC_NO_PTHREAD_CANCEL
    GC_API int GC_pthread_cancel(pthread_t);
# endif

# if defined(GC_HAVE_PTHREAD_EXIT) && !defined(GC_PTHREAD_EXIT_DECLARED)
#   define GC_PTHREAD_EXIT_DECLARED
    GC_API void GC_pthread_exit(void *) GC_PTHREAD_EXIT_ATTRIBUTE;
# endif

# ifdef __cplusplus
    } /* extern "C" */
# endif

#endif /* !GC_PTHREAD_REDIRECTS_ONLY */

#if !defined(GC_NO_THREAD_REDIRECTS) && !defined(GC_USE_LD_WRAP)
  /* Unless the compiler supports #pragma extern_prefix, the Tru64      */
  /* UNIX <pthread.h> redefines some POSIX thread functions to use      */
  /* mangled names.  Anyway, it's safe to undef them before redefining. */
# undef pthread_create
# undef pthread_join
# undef pthread_detach
# define pthread_create GC_pthread_create
# define pthread_join GC_pthread_join
# define pthread_detach GC_pthread_detach

# ifndef GC_NO_PTHREAD_SIGMASK
#   undef pthread_sigmask
#   define pthread_sigmask GC_pthread_sigmask
# endif
# ifndef GC_NO_DLOPEN
#   undef dlopen
#   define dlopen GC_dlopen
# endif
# ifndef GC_NO_PTHREAD_CANCEL
#   undef pthread_cancel
#   define pthread_cancel GC_pthread_cancel
# endif
# ifdef GC_HAVE_PTHREAD_EXIT
#   undef pthread_exit
#   define pthread_exit GC_pthread_exit
# endif
#endif /* !GC_NO_THREAD_REDIRECTS */

#endif /* GC_PTHREADS */

#endif /* GC_PTHREAD_REDIRECTS_H */
