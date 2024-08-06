/*
 * Copyright 1988, 1989 Hans-J. Boehm, Alan J. Demers
 * Copyright (c) 1991-1995 by Xerox Corporation.  All rights reserved.
 * Copyright 1996-1999 by Silicon Graphics.  All rights reserved.
 * Copyright 1999 by Hewlett-Packard Company.  All rights reserved.
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

/* This file assumes the collector has been compiled with GC_GCJ_SUPPORT. */

/*
 * We allocate objects whose first word contains a pointer to a struct
 * describing the object type.  This struct contains a garbage collector mark
 * descriptor at offset MARK_DESCR_OFFSET.  Alternatively, the objects
 * may be marked by the mark procedure passed to GC_init_gcj_malloc_mp.
 */

#ifndef GC_GCJ_H
#define GC_GCJ_H

        /* Gcj keeps GC descriptor as second word of vtable.    This    */
        /* probably needs to be adjusted for other clients.             */
        /* We currently assume that this offset is such that:           */
        /*      - all objects of this kind are large enough to have     */
        /*        a value at that offset, and                           */
        /*      - it is not zero.                                       */
        /* These assumptions allow objects on the free list to be       */
        /* marked normally.                                             */

#ifndef GC_H
# include "gc.h"
#endif

#ifdef __cplusplus
  extern "C" {
#endif

/* This function must be called before the gcj allocators are invoked.  */
/* mp_index and mp are the index and mark proc (see gc_mark.h),         */
/* respectively, for the allocated objects.  mp will be used to build   */
/* the descriptor for objects allocated through the debugging           */
/* interface; it will be invoked on all such objects with an            */
/* "environment" value of 1.  The client may choose to use the same     */
/* mark proc for some of its generated mark descriptors.                */
/* In that case, it should use a different "environment" value to       */
/* detect the presence or absence of the debug header.                  */
/* mp is really of type GC_mark_proc, as defined in gc_mark.h; we do    */
/* not want to include that here for namespace pollution reasons.       */
/* Passing in mp_index here instead of having GC_init_gcj_malloc()      */
/* internally call GC_new_proc() is quite ugly, but in typical usage    */
/* scenarios a compiler also has to know about mp_index, so             */
/* generating it dynamically is not acceptable.  The mp_index will      */
/* typically be an integer less than RESERVED_MARK_PROCS, so that it    */
/* does not collide with indices allocated by GC_new_proc.  If the      */
/* application needs no other reserved indices, zero                    */
/* (GC_GCJ_RESERVED_MARK_PROC_INDEX in gc_mark.h) is an obvious choice. */
/* Deprecated, portable clients should include gc_mark.h and use        */
/* GC_init_gcj_malloc_mp() instead.                                     */
GC_API GC_ATTR_DEPRECATED void GC_CALL GC_init_gcj_malloc(int /* mp_index */,
                                                          void * /* mp */);

/* Allocate an object, clear it, and store the pointer to the type      */
/* structure (vtable in gcj).  This adds a byte at the end of the       */
/* object if GC_malloc() would.  In case of out of memory, GC_oom_fn()  */
/* is called and its result is returned.                                */
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
        GC_gcj_malloc(size_t /* lb */, const void * /* vtable_ptr */);

/* Similar to GC_gcj_malloc, but add the debug info.  This is allocated */
/* with GC_gcj_debug_kind.                                              */
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
        GC_debug_gcj_malloc(size_t /* lb */, const void * /* vtable_ptr */,
                            GC_EXTRA_PARAMS);

/* Similar to GC_gcj_malloc, but assumes that a pointer to near the     */
/* beginning (i.e. within the first heap block) of the allocated object */
/* is always maintained.                                                */
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
        GC_gcj_malloc_ignore_off_page(size_t /* lb */,
                                      const void * /* vtable_ptr */);

/* The kind numbers of normal and debug gcj objects.            */
/* Useful only for debug support, we hope.                      */
GC_API int GC_gcj_kind;

GC_API int GC_gcj_debug_kind;

#ifdef GC_DEBUG
# define GC_GCJ_MALLOC(s,d) GC_debug_gcj_malloc(s,d,GC_EXTRAS)
# define GC_GCJ_MALLOC_IGNORE_OFF_PAGE(s,d) GC_GCJ_MALLOC(s,d)
#else
# define GC_GCJ_MALLOC(s,d) GC_gcj_malloc(s,d)
# define GC_GCJ_MALLOC_IGNORE_OFF_PAGE(s,d) GC_gcj_malloc_ignore_off_page(s,d)
#endif

#ifdef __cplusplus
  } /* extern "C" */
#endif

#endif /* GC_GCJ_H */
