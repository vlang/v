/*
 * Copyright (c) 1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1996 by Silicon Graphics.  All rights reserved.
 * Copyright (c) 1998 by Fergus Henderson.  All rights reserved.
 * Copyright (c) 2000-2009 by Hewlett-Packard Development Company.
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

/* This should never be included directly; it is included only from gc.h. */
#if defined(GC_H)

/* The policy regarding version numbers: development code has odd       */
/* "minor" number (and "micro" part is 0); when development is finished */
/* and a release is prepared, "minor" number is incremented (keeping    */
/* "micro" number still zero), whenever a defect is fixed a new release */
/* is prepared incrementing "micro" part to odd value (the most stable  */
/* release has the biggest "micro" number).                             */

/* The version here should match that in configure/configure.ac */
/* Eventually this one may become unnecessary.  For now we need */
/* it to keep the old-style build process working.              */
#define GC_TMP_VERSION_MAJOR 8
#define GC_TMP_VERSION_MINOR 2
#define GC_TMP_VERSION_MICRO 0 /* 8.2.0 */

#ifdef GC_VERSION_MAJOR
# if GC_TMP_VERSION_MAJOR != GC_VERSION_MAJOR \
     || GC_TMP_VERSION_MINOR != GC_VERSION_MINOR \
     || GC_TMP_VERSION_MICRO != GC_VERSION_MICRO
#   error Inconsistent version info.  Check README.md, include/gc_version.h and configure.ac.
# endif
#else
# define GC_VERSION_MAJOR GC_TMP_VERSION_MAJOR
# define GC_VERSION_MINOR GC_TMP_VERSION_MINOR
# define GC_VERSION_MICRO GC_TMP_VERSION_MICRO
#endif /* !GC_VERSION_MAJOR */

#endif
