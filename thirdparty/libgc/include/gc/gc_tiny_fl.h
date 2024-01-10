/*
 * Copyright (c) 1999-2005 Hewlett-Packard Development Company, L.P.
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

#ifndef GC_TINY_FL_H
#define GC_TINY_FL_H
/*
 * Constants and data structures for "tiny" free lists.
 * These are used for thread-local allocation or in-lined allocators.
 * Each global free list also essentially starts with one of these.
 * However, global free lists are known to the GC.  "Tiny" free lists
 * are basically private to the client.  Their contents are viewed as
 * "in use" and marked accordingly by the core of the GC.
 *
 * Note that inlined code might know about the layout of these and the constants
 * involved.  Thus any change here may invalidate clients, and such changes should
 * be avoided.  Hence we keep this as simple as possible.
 */

/*
 * We always set GC_GRANULE_BYTES to twice the length of a pointer.
 * This means that all allocation requests are rounded up to the next
 * multiple of 16 on 64-bit architectures or 8 on 32-bit architectures.
 * This appears to be a reasonable compromise between fragmentation overhead
 * and space usage for mark bits (usually mark bytes).
 * On many 64-bit architectures some memory references require 16-byte
 * alignment, making this necessary anyway.
 * For a few 32-bit architecture (e.g. x86), we may also need 16-byte alignment
 * for certain memory references.  But currently that does not seem to be the
 * default for all conventional malloc implementations, so we ignore that
 * problem.
 * It would always be safe, and often useful, to be able to allocate very
 * small objects with smaller alignment.  But that would cost us mark bit
 * space, so we no longer do so.
 */
#ifndef GC_GRANULE_BYTES
  /* GC_GRANULE_BYTES should not be overridden in any instances of the GC */
  /* library that may be shared between applications, since it affects    */
  /* the binary interface to the library.                                 */
# if defined(__LP64__) || defined (_LP64) || defined(_WIN64) \
        || defined(__s390x__) \
        || (defined(__x86_64__) && !defined(__ILP32__)) \
        || defined(__alpha__) || defined(__powerpc64__) \
        || defined(__arch64__)
#  define GC_GRANULE_BYTES 16
#  define GC_GRANULE_WORDS 2
# else
#  define GC_GRANULE_BYTES 8
#  define GC_GRANULE_WORDS 2
# endif
#endif /* !GC_GRANULE_BYTES */

#if GC_GRANULE_WORDS == 2
#  define GC_WORDS_TO_GRANULES(n) ((n)>>1)
#else
#  define GC_WORDS_TO_GRANULES(n) ((n)*sizeof(void *)/GC_GRANULE_BYTES)
#endif

/* A "tiny" free list header contains TINY_FREELISTS pointers to        */
/* singly linked lists of objects of different sizes, the ith one       */
/* containing objects i granules in size.  Note that there is a list    */
/* of size zero objects.                                                */
#ifndef GC_TINY_FREELISTS
# if GC_GRANULE_BYTES == 16
#   define GC_TINY_FREELISTS 25
# else
#   define GC_TINY_FREELISTS 33 /* Up to and including 256 bytes */
# endif
#endif /* !GC_TINY_FREELISTS */

/* The ith free list corresponds to size i*GC_GRANULE_BYTES     */
/* Internally to the collector, the index can be computed with  */
/* ROUNDED_UP_GRANULES.  Externally, we don't know whether      */
/* DONT_ADD_BYTE_AT_END is set, but the client should know.     */

/* Convert a free list index to the actual size of objects      */
/* on that list, including extra space we added.  Not an        */
/* inverse of the above.                                        */
#define GC_RAW_BYTES_FROM_INDEX(i) ((i) * GC_GRANULE_BYTES)

#endif /* GC_TINY_FL_H */
