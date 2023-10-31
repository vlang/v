/*
 * Copyright (c) 2000-2011 by Hewlett-Packard Development Company.
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

#ifndef GC_LEAK_DETECTOR_H
#define GC_LEAK_DETECTOR_H

/* Include leak_detector.h (e.g., via GCC --include directive)  */
/* to turn BoehmGC into a Leak Detector.                        */

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

#undef memalign
#define memalign(a,n) GC_memalign(a,n)
#undef posix_memalign
#define posix_memalign(p,a,n) GC_posix_memalign(p,a,n)

#ifndef CHECK_LEAKS
# define CHECK_LEAKS() GC_gcollect()
  /* Note 1: CHECK_LEAKS does not have GC prefix (preserved for */
  /* backward compatibility).                                   */
  /* Note 2: GC_gcollect() is also called automatically in the  */
  /* leak-finding mode at program exit.                         */
#endif

#endif /* GC_LEAK_DETECTOR_H */
