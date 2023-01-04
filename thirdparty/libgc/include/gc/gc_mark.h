/*
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 2001 by Hewlett-Packard Company. All rights reserved.
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose,  provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 *
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
/* free list link field in the first word.  Thus mark procedures may    */
/* not count on the presence of a type descriptor, and must handle this */
/* case correctly somehow.                                              */
#define GC_PROC_BYTES 100

#if defined(GC_BUILD) || defined(NOT_GCBUILD)
  struct GC_ms_entry;
#else
  struct GC_ms_entry { void *opaque; };
#endif
typedef struct GC_ms_entry * (*GC_mark_proc)(GC_word * /* addr */,
                                struct GC_ms_entry * /* mark_stack_ptr */,
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
/* for the high order 30 bits.                                          */
#define GC_DS_TAG_BITS 2
#define GC_DS_TAGS   ((1 << GC_DS_TAG_BITS) - 1)
#define GC_DS_LENGTH 0  /* The entire word is a length in bytes that    */
                        /* must be a multiple of 4.                     */
#define GC_DS_BITMAP 1  /* 30 (62) bits are a bitmap describing pointer */
                        /* fields.  The msb is 1 if the first word      */
                        /* is a pointer.                                */
                        /* (This unconventional ordering sometimes      */
                        /* makes the marker slightly faster.)           */
                        /* Zeroes indicate definite nonpointers.  Ones  */
                        /* indicate possible pointers.                  */
                        /* Only usable if pointers are word aligned.    */
#define GC_DS_PROC   2
                        /* The objects referenced by this object can be */
                        /* pushed on the mark stack by invoking         */
                        /* PROC(descr).  ENV(descr) is passed as the    */
                        /* last argument.                               */
#define GC_MAKE_PROC(proc_index, env) \
            (((((env) << GC_LOG_MAX_MARK_PROCS) \
               | (proc_index)) << GC_DS_TAG_BITS) | GC_DS_PROC)
#define GC_DS_PER_OBJECT 3  /* The real descriptor is at the            */
                        /* byte displacement from the beginning of the  */
                        /* object given by descr & ~GC_DS_TAGS.         */
                        /* If the descriptor is negative, the real      */
                        /* descriptor is at (*<object_start>) -         */
                        /* (descr&~GC_DS_TAGS) - GC_INDIR_PER_OBJ_BIAS  */
                        /* The latter alternative can be used if each   */
                        /* object contains a type descriptor in the     */
                        /* first word.                                  */
                        /* Note that in the multi-threaded environments */
                        /* per-object descriptors must be located in    */
                        /* either the first two or last two words of    */
                        /* the object, since only those are guaranteed  */
                        /* to be cleared while the allocation lock is   */
                        /* held.                                        */
#define GC_INDIR_PER_OBJ_BIAS 0x10

GC_API void * GC_least_plausible_heap_addr;
GC_API void * GC_greatest_plausible_heap_addr;
                        /* Bounds on the heap.  Guaranteed valid        */
                        /* Likely to include future heap expansion.     */
                        /* Hence usually includes not-yet-mapped        */
                        /* memory.                                      */

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
                                struct GC_ms_entry * /* mark_stack_ptr */,
                                struct GC_ms_entry * /* mark_stack_limit */,
                                void ** /* src */);

#define GC_MARK_AND_PUSH(obj, msp, lim, src) \
          ((GC_word)(obj) >= (GC_word)GC_least_plausible_heap_addr && \
           (GC_word)(obj) <= (GC_word)GC_greatest_plausible_heap_addr ? \
           GC_mark_and_push(obj, msp, lim, src) : (msp))

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

/* And some routines to support creation of new "kinds", e.g. with      */
/* custom mark procedures, by language runtimes.                        */
/* The _inner versions assume the caller holds the allocation lock.     */

/* Return a new free list array.        */
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
                                /* first page of the resulting object   */
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
/* if psize is not NULL.                                                */
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
/* is called at the start of every full GC (called with the allocation  */
/* lock held).  May be 0.  This is a really tricky interface to use     */
/* correctly.  Unless you really understand the collector internals,    */
/* the callback should not, directly or indirectly, make any GC_ or     */
/* potentially blocking calls.  In particular, it is not safe to        */
/* allocate memory using the garbage collector from within the callback */
/* function.  Both the setter and getter acquire the GC lock.           */
typedef void (GC_CALLBACK * GC_start_callback_proc)(void);
GC_API void GC_CALL GC_set_start_callback(GC_start_callback_proc);
GC_API GC_start_callback_proc GC_CALL GC_get_start_callback(void);

/* Slow/general mark bit manipulation.  The caller must hold the        */
/* allocation lock.  GC_is_marked returns 1 (TRUE) or 0.                */
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
/* Note that both the setter and getter require some external           */
/* synchronization to avoid data race.                                  */
typedef void (GC_CALLBACK * GC_push_other_roots_proc)(void);
GC_API void GC_CALL GC_set_push_other_roots(GC_push_other_roots_proc);
GC_API GC_push_other_roots_proc GC_CALL GC_get_push_other_roots(void);

/* Walk the GC heap visiting all reachable objects.  Assume the caller  */
/* holds the allocation lock.  Object base pointer, object size and     */
/* client custom data are passed to the callback (holding the lock).    */
typedef void (GC_CALLBACK *GC_reachable_object_proc)(void * /* obj */,
                                                size_t /* bytes */,
                                                void * /* client_data */);
GC_API void GC_CALL GC_enumerate_reachable_objects_inner(
                                GC_reachable_object_proc,
                                void * /* client_data */) GC_ATTR_NONNULL(1);

GC_API int GC_CALL GC_is_tmp_root(void *);

GC_API void GC_CALL GC_print_trace(GC_word /* gc_no */);
GC_API void GC_CALL GC_print_trace_inner(GC_word /* gc_no */);

#ifdef __cplusplus
  } /* extern "C" */
#endif

#endif /* GC_MARK_H */
