/*
 * Copyright (c) 1999-2005 Hewlett-Packard Development Company, L.P.
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

#ifndef GC_TINY_FL_H
#define GC_TINY_FL_H

/* Constants and data structures for "tiny" free lists.  These are      */
/* used for thread-local allocation or in-lined allocators.             */
/* Each global free list also essentially starts with one of these.     */
/* However, global free lists are known to the GC.  "Tiny" free lists   */
/* are basically private to the client.  Their contents are viewed as   */
/* "in use" and marked accordingly by the core of the GC.               */
/* Note that inlined code might know about the layout of these and the  */
/* constants involved.  Thus any change here may invalidate clients,    */
/* and such changes should be avoided.  Hence we keep this as simple    */
/* as possible.                                                         */

/* We always set GC_GRANULE_BYTES to twice the length of a pointer.     */
/* This means that all allocation requests are rounded up to the next   */
/* multiple of 16 on 64-bit architectures or 8 on 32-bit architectures. */
/* This appears to be a reasonable compromise between fragmentation     */
/* overhead and space usage for mark bits (usually mark bytes).         */
/* On many 64-bit architectures some memory references require 16-byte  */
/* alignment, making this necessary anyway.  For a few 32-bit           */
/* architectures (e.g. i686), we may also need 16-byte alignment for    */
/* certain memory references.  But currently that does not seem to be   */
/* the default for all conventional malloc implementations, so we       */
/* ignore that problem.                                                 */
/* It would always be safe, and often useful, to be able to allocate    */
/* very small objects with smaller alignment.  But that would cost us   */
/* mark bit space, so we no longer do so.                               */
/* GC_GRANULE_BYTES should not be overridden in any instances of the GC */
/* library that may be shared between applications, since it affects    */
/* the binary interface to the library.                                 */
#if defined(CPPCHECK) && GC_GRANULE_BYTES == 1
# undef GC_GRANULE_BYTES
#endif
#ifdef GC_GRANULE_BYTES
# define GC_GRANULE_PTRS (GC_GRANULE_BYTES / GC_SIZEOF_PTR)
#else
# define GC_GRANULE_PTRS 2 /* in pointers */
# define GC_GRANULE_BYTES (GC_GRANULE_PTRS * GC_SIZEOF_PTR)
#endif /* !GC_GRANULE_BYTES */

/* Convert size in pointers to that in granules.        */
#define GC_PTRS_TO_GRANULES(n) ((n) / GC_GRANULE_PTRS)

/* Convert size in pointers to that in granules, but rounding up the    */
/* result.                                                              */
#define GC_PTRS_TO_WHOLE_GRANULES(n) \
                GC_PTRS_TO_GRANULES((n) + GC_GRANULE_PTRS - 1)

/* A "tiny" free-list header contains GC_TINY_FREELISTS pointers to     */
/* singly linked lists of objects of different sizes, the i-th one      */
/* containing objects i granules in size.  Note that there is a list    */
/* of size zero objects.                                                */
#ifndef GC_TINY_FREELISTS
# if GC_GRANULE_BYTES >= 16
#   define GC_TINY_FREELISTS 25
# else
#   define GC_TINY_FREELISTS 33 /* Up to and including 256 bytes */
# endif
#endif /* !GC_TINY_FREELISTS */

/* The i-th free list corresponds to size i*GC_GRANULE_BYTES    */
/* Internally to the collector, the index can be computed with  */
/* ALLOC_REQUEST_GRANS().  The later also depends on the        */
/* values returned by GC_get_dont_add_byte_at_end() and         */
/* GC_get_all_interior_pointers().                              */

/* Convert a free-list index to the actual size of objects      */
/* on that list, including extra space we added.  Not an        */
/* inverse of the above.                                        */
#define GC_RAW_BYTES_FROM_INDEX(i) ((i) * GC_GRANULE_BYTES)

/* Deprecated.  Use GC_GRANULE_PTRS instead.    */
#undef GC_GRANULE_WORDS
#define GC_GRANULE_WORDS GC_GRANULE_PTRS

/* Deprecated.  Use GC_PTRS_TO_GRANULES() instead.              */
#define GC_WORDS_TO_GRANULES(n) GC_PTRS_TO_GRANULES(n)

/* Deprecated.  */
#define GC_WORDS_TO_WHOLE_GRANULES(n) GC_PTRS_TO_WHOLE_GRANULES(n)

#endif /* GC_TINY_FL_H */
