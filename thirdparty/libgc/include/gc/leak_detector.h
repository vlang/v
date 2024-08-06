/*
 * Copyright (c) 2000-2011 by Hewlett-Packard Development Company.
 * All rights reserved.
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose, provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 */

#ifndef GC_LEAK_DETECTOR_H
#define GC_LEAK_DETECTOR_H

/* Include this header file (e.g., via gcc --include directive) */
/* to turn libgc into a leak detector.                          */

#ifndef GC_DEBUG
# define GC_DEBUG
#endif
#include "gc.h"

#ifndef GC_DONT_INCLUDE_STDLIB
  /* We ensure stdlib.h and string.h are included before        */
  /* redirecting malloc() and the accompanying functions.       */
# include <stdlib.h>
# include <string.h>
#endif

#undef malloc
#define malloc(n) GC_MALLOC(n)
#undef calloc
#define calloc(m,n) GC_MALLOC((m)*(n))
#undef free
#define free(p) GC_FREE(p)
#undef realloc
#define realloc(p,n) GC_REALLOC(p,n)
#undef reallocarray
#define reallocarray(p,m,n) GC_REALLOC(p,(m)*(n))

#undef strdup
#define strdup(s) GC_STRDUP(s)
#undef strndup
#define strndup(s,n) GC_STRNDUP(s,n)

#ifdef GC_REQUIRE_WCSDUP
  /* The collector should be built with GC_REQUIRE_WCSDUP       */
  /* defined as well to redirect wcsdup().                      */
# include <wchar.h>
# undef wcsdup
# define wcsdup(s) GC_WCSDUP(s)
#endif

/* The following routines for the aligned objects allocation    */
/* (aligned_alloc, valloc, etc.) do not have their debugging    */
/* counterparts.  Note that free() called for such objects      */
/* may output a warning that the pointer has no debugging info. */

#undef aligned_alloc
#define aligned_alloc(a,n) GC_memalign(a,n) /* identical to memalign */
#undef memalign
#define memalign(a,n) GC_memalign(a,n)
#undef posix_memalign
#define posix_memalign(p,a,n) GC_posix_memalign(p,a,n)

#undef _aligned_malloc
#define _aligned_malloc(n,a) GC_memalign(a,n) /* reverse args order */
#undef _aligned_free
#define _aligned_free(p) GC_free(p) /* non-debug */

#ifndef GC_NO_VALLOC
# undef valloc
# define valloc(n) GC_valloc(n)
# undef pvalloc
# define pvalloc(n) GC_pvalloc(n) /* obsolete */
#endif /* !GC_NO_VALLOC */

#undef malloc_usable_size /* available in glibc */
#define malloc_usable_size(p) GC_size(p)
#undef malloc_size /* available on Darwin */
#define malloc_size(p) GC_size(p)
#undef _msize /* available in Windows CRT */
#define _msize(p) GC_size(p)

#ifndef CHECK_LEAKS
# define CHECK_LEAKS() GC_gcollect()
  /* Note 1: CHECK_LEAKS does not have GC prefix (preserved for */
  /* backward compatibility).                                   */
  /* Note 2: GC_gcollect() is also called automatically in the  */
  /* leak-finding mode at program exit.                         */
#endif

#endif /* GC_LEAK_DETECTOR_H */
