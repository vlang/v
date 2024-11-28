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
 * for any purpose, provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 */

/* This is a simple API to implement pointer back tracing, i.e. */
/* to answer questions such as "who is pointing to this" or     */
/* "why is this object being retained by the collector".        */
/* Most of these calls yield useful information on only after   */
/* a garbage collection.  Usually the client will first force   */
/* a full collection and then gather information, preferably    */
/* before much intervening allocation.                          */
/* The implementation of the interface is only about 99.9999%   */
/* correct.  It is intended to be good enough for profiling,    */
/* but is not intended to be used with production code.         */
/* Results are likely to be much more useful if all allocation  */
/* is accomplished through the debugging allocators.            */

/*
 * The implementation idea is due to A. Demers.
 */

#ifndef GC_BACKPTR_H
#define GC_BACKPTR_H

#ifndef GC_H
# include "gc.h"
#endif

#ifdef __cplusplus
  extern "C" {
#endif

typedef enum {
    GC_UNREFERENCED,    /* No reference info available.         */
    GC_NO_SPACE,        /* Dest not allocated with debug alloc. */
    GC_REFD_FROM_ROOT,  /* Referenced directly by root *base_p. */
    GC_REFD_FROM_REG,   /* Referenced from a register, i.e.     */
                        /* a root without an address.           */
    GC_REFD_FROM_HEAP,  /* Referenced from another heap obj.    */
    GC_FINALIZER_REFD   /* Finalizable and hence accessible.    */
} GC_ref_kind;

/* Store information about the object referencing dest in *base_p   */
/* and *offset_p.                                                   */
/* If multiple objects or roots point to dest, then the one         */
/* reported will be the last one used by the garbage collector to   */
/* trace the object.                                                */
/* If source is root, then *base_p = address and *offset_p = 0.     */
/* If source is heap object, then *base_p != 0, *offset_p = offset. */
/* Dest can be any address within a heap object.                    */
/* The allocator lock is not acquired by design (despite of the     */
/* possibility of a race); anyway the function should not be used   */
/* in production code.                                              */
GC_API GC_ref_kind GC_CALL GC_get_back_ptr_info(void * /* dest */,
                                void ** /* base_p */, size_t * /* offset_p */)
                                GC_ATTR_NONNULL(1);

/* Generate a random heap address.  The resulting address is    */
/* in the heap, but not necessarily inside a valid object.      */
/* The caller should hold the allocator lock.                   */
GC_API void * GC_CALL GC_generate_random_heap_address(void);

/* Generate a random address inside a valid marked heap object. */
/* The caller should hold the allocator lock.                   */
GC_API void * GC_CALL GC_generate_random_valid_address(void);

/* Force a garbage collection and generate a backtrace from a   */
/* random heap address.                                         */
/* This uses the GC logging mechanism (GC_printf) to produce    */
/* output.  It can often be called from a debugger.             */
GC_API void GC_CALL GC_generate_random_backtrace(void);

/* Print a backtrace from a specific address.  Used by the      */
/* above.  The client should call GC_gcollect() immediately     */
/* before invocation.                                           */
GC_API void GC_CALL GC_print_backtrace(void *) GC_ATTR_NONNULL(1);

#ifdef __cplusplus
  } /* extern "C" */
#endif

#endif /* GC_BACKPTR_H */
