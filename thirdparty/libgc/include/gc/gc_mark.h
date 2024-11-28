/*
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 2001 by Hewlett-Packard Company. All rights reserved.
 * Copyright (c) 2009-2022 Ivan Maidanski
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

/*
 * This contains interfaces to the GC marker that are likely to be useful to
 * clients that provide detailed heap layout information to the collector.
 * This interface should not be used by normal C or C++ clients.
 * It will be useful to runtimes for other languages.
 *
 * This is an experts-only interface!  There are many ways to break the
 * collector in subtle ways by using this functionality.
 */
#ifndef GC_MARK_H
#define GC_MARK_H

#ifndef GC_H
# include "gc.h"
#endif

#ifdef __cplusplus
  extern "C" {
#endif

#define GC_PROC_BYTES 100

#if defined(GC_BUILD) || defined(NOT_GCBUILD)
  struct GC_ms_entry;
  struct GC_hblk_s;
#else
  struct GC_ms_entry { void *opaque; };
  struct GC_hblk_s { void *opaque; };
#endif

/* A client supplied mark procedure.  Returns new mark stack pointer.   */
/* Primary effect should be to push new entries on the mark stack.      */
/* Mark stack pointer values are passed and returned explicitly.        */
/* Global variables describing mark stack are not necessarily valid.    */
/* (This usually saves a few cycles by keeping things in registers.)    */
/* Assumed to scan about GC_PROC_BYTES on average.  If it needs to do   */
/* much more work than that, it should do it in smaller pieces by       */
/* pushing itself back on the mark stack.                               */
/* Note that it should always do some work (defined as marking some     */
/* objects) before pushing more than one entry on the mark stack.       */
/* This is required to ensure termination in the event of mark stack    */
/* overflows.                                                           */
/* This procedure is always called with at least one empty entry on the */
/* mark stack.                                                          */
/* Currently we require that mark procedures look for pointers in a     */
/* subset of the places the conservative marker would.  It must be safe */
/* to invoke the normal mark procedure instead.                         */
/* WARNING: Such a mark procedure may be invoked on an unused object    */
/* residing on a free list.  Such objects are cleared, except for a     */
/* free-list link field (which is located at the beginning of each      */
/* object).  Thus mark procedures may not count on the presence of a    */
/* type descriptor, and must handle this case correctly somehow.  Also, */
/* a mark procedure should be prepared to be executed concurrently from */
/* the marker threads (the later ones are created only if the client    */
/* has called GC_start_mark_threads() or started a user thread          */
/* previously).  For the compatibility reason, addr is a pointer to     */
/* word, but it should be treated as a pointer to void pointer.         */
typedef struct GC_ms_entry * (GC_CALLBACK * GC_mark_proc)(GC_word * /* addr */,
                                struct GC_ms_entry * /* mark_stack_top */,
                                struct GC_ms_entry * /* mark_stack_limit */,
                                GC_word /* env */);

#define GC_LOG_MAX_MARK_PROCS 6
#define GC_MAX_MARK_PROCS (1 << GC_LOG_MAX_MARK_PROCS)

/* In a few cases it's necessary to assign statically known indices to  */
/* certain mark procs.  Thus we reserve a few for well known clients.   */
/* (This is necessary if mark descriptors are compiler generated.)      */
#define GC_RESERVED_MARK_PROCS 8
#define GC_GCJ_RESERVED_MARK_PROC_INDEX 0

/* Object descriptors on mark stack or in objects.  Low order two       */
/* bits are tags distinguishing among the following 4 possibilities     */
/* for the rest (high-order) bits.                                      */
#define GC_DS_TAG_BITS 2
#define GC_DS_TAGS   ((1U << GC_DS_TAG_BITS) - 1)
#define GC_DS_LENGTH 0  /* The entire word is a length in bytes that    */
                        /* must be a multiple of 4.                     */
#define GC_DS_BITMAP 1  /* The high-order bits are describing pointer   */
                        /* fields.  The most significant bit is set if  */
                        /* the first "pointer-sized" word is a pointer. */
                        /* (This unconventional ordering sometimes      */
                        /* makes the marker slightly faster.)           */
                        /* Zeroes indicate definite non-pointers; ones  */
                        /* indicate possible pointers.                  */
                        /* Only usable if pointers are aligned.         */
#define GC_DS_PROC   2
                        /* The objects referenced by this object can be */
                        /* pushed on the mark stack by invoking         */
                        /* PROC(descr).  ENV(descr) is passed as the    */
                        /* last argument.                               */
#define GC_MAKE_PROC(proc_index, env) \
            ((((((GC_word)(env)) << GC_LOG_MAX_MARK_PROCS) \
               | (unsigned)(proc_index)) << GC_DS_TAG_BITS) \
             | (GC_word)GC_DS_PROC)
#define GC_DS_PER_OBJECT 3
                        /* The real descriptor is at the byte           */
                        /* displacement from the beginning of the       */
                        /* object given by descr & ~GC_DS_TAGS.         */
                        /* If the descriptor is negative, the real      */
                        /* descriptor is at (*<object_start>) -         */
                        /* (descr&~GC_DS_TAGS) - GC_INDIR_PER_OBJ_BIAS. */
                        /* The latter alternative can be used if each   */
                        /* object contains a type descriptor at the     */
                        /* beginning of the object.  Note that in the   */
                        /* multi-threaded environments per-object       */
                        /* descriptors must be located in either the    */
                        /* first two or last two "pointer-sized" words  */
                        /* of the object, since only those are          */
                        /* guaranteed to be cleared while the allocator */
                        /* lock is held.                                */
#define GC_INDIR_PER_OBJ_BIAS 0x10

GC_API void * GC_least_plausible_heap_addr;
GC_API void * GC_greatest_plausible_heap_addr;
                        /* Bounds on the heap.  Guaranteed to be valid. */
                        /* Likely to include future heap expansion.     */
                        /* Hence usually includes not-yet-mapped        */
                        /* memory, or might overlap with other data     */
                        /* roots.  The address of any heap object is    */
                        /* larger than GC_least_plausible_heap_addr and */
                        /* less than GC_greatest_plausible_heap_addr.   */

/* Specify the pointer address mask.  Works only if the collector is    */
/* built with DYNAMIC_POINTER_MASK macro defined.  These primitives are */
/* normally needed only to support systems that use high-order pointer  */
/* tags.  The setter is expected to be called, if needed, before the GC */
/* initialization or, at least, before the first object is allocated.   */
/* Both the setter and the getter are unsynchronized.                   */
GC_API void GC_CALL GC_set_pointer_mask(GC_word);
GC_API GC_word GC_CALL GC_get_pointer_mask(void);

/* Similar to GC_set/get_pointer_mask but for the pointer address       */
/* shift.  The value should be less than the size of word, in bits.     */
/* Applied after the mask.                                              */
GC_API void GC_CALL GC_set_pointer_shift(unsigned);
GC_API unsigned GC_CALL GC_get_pointer_shift(void);

/* Handle nested references in a custom mark procedure.                 */
/* Check if obj is a valid object. If so, ensure that it is marked.     */
/* If it was not previously marked, push its contents onto the mark     */
/* stack for future scanning.  The object will then be scanned using    */
/* its mark descriptor.                                                 */
/* Returns the new mark stack pointer.                                  */
/* Handles mark stack overflows correctly.                              */
/* Since this marks first, it makes progress even if there are mark     */
/* stack overflows.                                                     */
/* Src is the address of the pointer to obj, which is used only         */
/* for back pointer-based heap debugging.                               */
/* It is strongly recommended that most objects be handled without mark */
/* procedures, e.g. with bitmap descriptors, and that mark procedures   */
/* be reserved for exceptional cases.  That will ensure that            */
/* performance of this call is not extremely performance critical.      */
/* (Otherwise we would need to inline GC_mark_and_push completely,      */
/* which would tie the client code to a fixed collector version.)       */
/* Note that mark procedures should explicitly call FIXUP_POINTER()     */
/* if required.                                                         */
GC_API struct GC_ms_entry * GC_CALL GC_mark_and_push(void * /* obj */,
                                struct GC_ms_entry * /* mark_stack_top */,
                                struct GC_ms_entry * /* mark_stack_limit */,
                                void ** /* src */);

#define GC_MARK_AND_PUSH(obj, msp, lim, src) \
    (GC_ADDR_LT((char *)GC_least_plausible_heap_addr, (char *)(obj)) \
     && GC_ADDR_LT((char *)(obj), (char *)GC_greatest_plausible_heap_addr) \
        ? GC_mark_and_push(obj, msp, lim, src) : (msp))

GC_API void GC_CALL GC_push_proc(GC_word /* descr */, void * /* obj */);

GC_API struct GC_ms_entry * GC_CALL GC_custom_push_proc(GC_word /* descr */,
                                void * /* obj */,
                                struct GC_ms_entry * /* mark_stack_top */,
                                struct GC_ms_entry * /* mark_stack_limit */);

GC_API struct GC_ms_entry * GC_CALL GC_custom_push_range(void * /* bottom */,
                                void * /* top */,
                                struct GC_ms_entry * /* mark_stack_top */,
                                struct GC_ms_entry * /* mark_stack_limit */);

/* The size of the header added to objects allocated through the        */
/* GC_debug routines.  Defined as a function so that client mark        */
/* procedures do not need to be recompiled for the collector library    */
/* version changes.                                                     */
GC_API GC_ATTR_CONST size_t GC_CALL GC_get_debug_header_size(void);
#define GC_USR_PTR_FROM_BASE(p) \
                ((void *)((char *)(p) + GC_get_debug_header_size()))

/* The same but defined as a variable.  Exists only for the backward    */
/* compatibility.  Some compilers do not accept "const" together with   */
/* deprecated or dllimport attributes, so the symbol is exported as     */
/* a non-constant one.                                                  */
GC_API GC_ATTR_DEPRECATED
# ifdef GC_BUILD
    const
# endif
  size_t GC_debug_header_size;

/* Return the heap block size.  Each heap block is devoted to a single  */
/* size and kind of object.                                             */
GC_API GC_ATTR_CONST size_t GC_CALL GC_get_hblk_size(void);

typedef void (GC_CALLBACK * GC_walk_hblk_fn)(struct GC_hblk_s *,
                                             void * /* client_data */);

/* Apply fn to each allocated heap block.  It is the responsibility     */
/* of the caller to avoid data race during the function execution (e.g. */
/* by acquiring the allocator lock at least in the reader mode).        */
GC_API void GC_CALL GC_apply_to_all_blocks(GC_walk_hblk_fn,
                                void * /* client_data */) GC_ATTR_NONNULL(1);

/* Same as GC_walk_hblk_fn but with index of the free list.             */
typedef void (GC_CALLBACK * GC_walk_free_blk_fn)(struct GC_hblk_s *,
                                                 int /* index */,
                                                 void * /* client_data */);

/* Apply fn to each completely empty heap block.  It is the             */
/* responsibility of the caller to avoid data race during the function  */
/* execution (e.g. by acquiring the allocator lock at least in the      */
/* reader mode).                                                        */
GC_API void GC_CALL GC_iterate_free_hblks(GC_walk_free_blk_fn,
                                void * /* client_data */) GC_ATTR_NONNULL(1);

/* If there are likely to be false references to a block starting at h  */
/* of the indicated length, then return the next plausible starting     */
/* location for h that might avoid these false references.  Otherwise   */
/* NULL is returned.  Assumes the allocator lock is held at least in    */
/* the reader mode but no assertion about it by design.                 */
GC_API struct GC_hblk_s *GC_CALL GC_is_black_listed(struct GC_hblk_s *,
                                                    size_t /* len */);

/* Return the number of set mark bits for the heap block where object   */
/* p is located.  Defined only if the library has been compiled         */
/* without NO_DEBUGGING.                                                */
GC_API unsigned GC_CALL GC_count_set_marks_in_hblk(const void * /* p */);

/* And some routines to support creation of new "kinds", e.g. with      */
/* custom mark procedures, by language runtimes.                        */
/* The _inner versions assume the caller holds the allocator lock.      */

/* Return a new free-list array.    */
GC_API void ** GC_CALL GC_new_free_list(void);
GC_API void ** GC_CALL GC_new_free_list_inner(void);

/* Return a new kind, as specified. */
GC_API unsigned GC_CALL GC_new_kind(void ** /* free_list */,
                            GC_word /* mark_descriptor_template */,
                            int /* add_size_to_descriptor */,
                            int /* clear_new_objects */) GC_ATTR_NONNULL(1);
                /* The last two parameters must be zero or one. */
GC_API unsigned GC_CALL GC_new_kind_inner(void ** /* free_list */,
                            GC_word /* mark_descriptor_template */,
                            int /* add_size_to_descriptor */,
                            int /* clear_new_objects */) GC_ATTR_NONNULL(1);

/* Return a new mark procedure identifier, suitable for use as  */
/* the first argument in GC_MAKE_PROC.                          */
GC_API unsigned GC_CALL GC_new_proc(GC_mark_proc);
GC_API unsigned GC_CALL GC_new_proc_inner(GC_mark_proc);

/* Similar to GC_init_gcj_malloc() described in gc_gcj.h but with the   */
/* proper types of the arguments.                                       */
/* Defined only if the library has been compiled with GC_GCJ_SUPPORT.   */
GC_API void GC_CALL GC_init_gcj_malloc_mp(unsigned /* mp_index */,
                                          GC_mark_proc /* mp */);

/* Allocate an object of a given kind.  By default, there are only      */
/* a few kinds: composite (pointerful), atomic, uncollectible, etc.     */
/* We claim it is possible for clever client code that understands the  */
/* GC internals to add more, e.g. to communicate object layout          */
/* information to the collector.  Note that in the multi-threaded       */
/* contexts, this is usually unsafe for kinds that have the descriptor  */
/* in the object itself, since there is otherwise a window in which     */
/* the descriptor is not correct.  Even in the single-threaded case,    */
/* we need to be sure that cleared objects on a free list don't         */
/* cause a GC crash if they are accidentally traced.                    */
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL GC_generic_malloc(
                                                            size_t /* lb */,
                                                            int /* knd */);

GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
                                        GC_generic_malloc_ignore_off_page(
                                            size_t /* lb */, int /* knd */);
                                /* As above, but pointers to past the   */
                                /* first hblk of the resulting object   */
                                /* are ignored.                         */

/* Generalized version of GC_malloc_[atomic_]uncollectable.     */
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
                                        GC_generic_malloc_uncollectable(
                                            size_t /* lb */, int /* knd */);

/* Same as above but primary for allocating an object of the same kind  */
/* as an existing one (kind obtained by GC_get_kind_and_size).          */
/* Not suitable for GCJ and typed-malloc kinds.                         */
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
                                        GC_generic_or_special_malloc(
                                            size_t /* size */, int /* knd */);
GC_API GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void * GC_CALL
                                        GC_debug_generic_or_special_malloc(
                                            size_t /* size */, int /* knd */,
                                            GC_EXTRA_PARAMS);

#ifdef GC_DEBUG
# define GC_GENERIC_OR_SPECIAL_MALLOC(sz, knd) \
                GC_debug_generic_or_special_malloc(sz, knd, GC_EXTRAS)
#else
# define GC_GENERIC_OR_SPECIAL_MALLOC(sz, knd) \
                GC_generic_or_special_malloc(sz, knd)
#endif /* !GC_DEBUG */

/* Similar to GC_size but returns object kind.  Size is returned too    */
/* if psize is not NULL.  The object pointer should not be NULL.        */
GC_API int GC_CALL GC_get_kind_and_size(const void *, size_t * /* psize */)
                                                        GC_ATTR_NONNULL(1);

typedef void (GC_CALLBACK * GC_describe_type_fn)(void * /* p */,
                                                 char * /* out_buf */);
                                /* A procedure which                    */
                                /* produces a human-readable            */
                                /* description of the "type" of object  */
                                /* p into the buffer out_buf of length  */
                                /* GC_TYPE_DESCR_LEN.  This is used by  */
                                /* the debug support when printing      */
                                /* objects.                             */
                                /* These functions should be as robust  */
                                /* as possible, though we do avoid      */
                                /* invoking them on objects on the      */
                                /* global free list.                    */
#define GC_TYPE_DESCR_LEN 40

GC_API void GC_CALL GC_register_describe_type_fn(int /* kind */,
                                                 GC_describe_type_fn);
                                /* Register a describe_type function    */
                                /* to be used when printing objects     */
                                /* of a particular kind.                */

/* Clear some of the inaccessible part of the stack.  Returns its       */
/* argument, so it can be used in a tail call position, hence clearing  */
/* another frame.  Argument may be NULL.                                */
GC_API void * GC_CALL GC_clear_stack(void *);

/* Set and get the client notifier on collections.  The client function */
/* is called at the start of every full GC (called with the allocator   */
/* lock held).  May be 0.  This is a really tricky interface to use     */
/* correctly.  Unless you really understand the collector internals,    */
/* the callback should not, directly or indirectly, make any GC_ or     */
/* potentially blocking calls.  In particular, it is not safe to        */
/* allocate memory using the garbage collector from within the callback */
/* function.  Both the setter and the getter acquire the allocator      */
/* lock (in the reader mode in case of the getter).                     */
typedef void (GC_CALLBACK * GC_start_callback_proc)(void);
GC_API void GC_CALL GC_set_start_callback(GC_start_callback_proc);
GC_API GC_start_callback_proc GC_CALL GC_get_start_callback(void);

/* Slow/general mark bit manipulation.  The caller should hold the      */
/* allocator lock (the reader mode is enough in case of GC_is_marked).  */
/* GC_is_marked returns 1 (true) or 0.  The argument should be the real */
/* address of an object (i.e. the address of the debug header if there  */
/* is one).                                                             */
GC_API int GC_CALL GC_is_marked(const void *) GC_ATTR_NONNULL(1);
GC_API void GC_CALL GC_clear_mark_bit(const void *) GC_ATTR_NONNULL(1);
GC_API void GC_CALL GC_set_mark_bit(const void *) GC_ATTR_NONNULL(1);

/* Push everything in the given range onto the mark stack.              */
/* (GC_push_conditional pushes either all or only dirty pages depending */
/* on the third argument.)  GC_push_all_eager also ensures that stack   */
/* is scanned immediately, not just scheduled for scanning.             */
GC_API void GC_CALL GC_push_all(void * /* bottom */, void * /* top */);
GC_API void GC_CALL GC_push_all_eager(void * /* bottom */, void * /* top */);
GC_API void GC_CALL GC_push_conditional(void * /* bottom */, void * /* top */,
                                        int /* bool all */);
GC_API void GC_CALL GC_push_finalizer_structures(void);

/* Set and get the client push-other-roots procedure.  A client         */
/* supplied procedure should also call the original procedure.          */
/* Note that both the setter and the getter require some external       */
/* synchronization to avoid data race.                                  */
typedef void (GC_CALLBACK * GC_push_other_roots_proc)(void);
GC_API void GC_CALL GC_set_push_other_roots(GC_push_other_roots_proc);
GC_API GC_push_other_roots_proc GC_CALL GC_get_push_other_roots(void);

/* Walk the GC heap visiting all reachable objects.  Assume the caller  */
/* holds the allocator lock at least in the reader mode.  Object base   */
/* pointer, object size and client custom data are passed to the        */
/* callback (holding the allocator lock in the same mode as the caller  */
/* does).                                                               */
typedef void (GC_CALLBACK * GC_reachable_object_proc)(void * /* obj */,
                                                size_t /* bytes */,
                                                void * /* client_data */);
GC_API void GC_CALL GC_enumerate_reachable_objects_inner(
                                GC_reachable_object_proc,
                                void * /* client_data */) GC_ATTR_NONNULL(1);

/* Is the given address in one of the temporary static root sections?   */
/* Acquires the allocator lock in the reader mode.                      */
GC_API int GC_CALL GC_is_tmp_root(void *);

GC_API void GC_CALL GC_print_trace(GC_word /* gc_no */);
GC_API void GC_CALL GC_print_trace_inner(GC_word /* gc_no */);

/* Set the client for when mark stack is empty.  A client can use       */
/* this callback to process (un)marked objects and push additional      */
/* work onto the stack.  Useful for implementing ephemerons.  Both the  */
/* setter and the getter acquire the allocator lock (in the reader mode */
/* in case of the getter).                                              */
typedef struct GC_ms_entry * (GC_CALLBACK * GC_on_mark_stack_empty_proc)(
                                struct GC_ms_entry * /* mark_stack_top */,
                                struct GC_ms_entry * /* mark_stack_limit */);
GC_API void GC_CALL GC_set_on_mark_stack_empty(GC_on_mark_stack_empty_proc);
GC_API GC_on_mark_stack_empty_proc GC_CALL GC_get_on_mark_stack_empty(void);

#ifdef __cplusplus
  } /* extern "C" */
#endif

#endif /* GC_MARK_H */
