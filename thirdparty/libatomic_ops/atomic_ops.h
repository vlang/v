/*
 * Copyright (c) 2003-2011 Hewlett-Packard Development Company, L.P.
 * Copyright (c) 2008-2021 Ivan Maidanski
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#ifndef AO_ATOMIC_OPS_H
#define AO_ATOMIC_OPS_H

#include "atomic_ops/ao_version.h"
                        /* Define version numbers here to allow         */
                        /* test on build machines for cross-builds.     */

#include <assert.h>
#include <stddef.h>

/* We define various atomic operations on memory in a           */
/* machine-specific way.  Unfortunately, this is complicated    */
/* by the fact that these may or may not be combined with       */
/* various memory barriers.  Thus the actual operations we      */
/* define have the form AO_<atomic-op>_<barrier>, for all       */
/* plausible combinations of <atomic-op> and <barrier>.         */
/* This of course results in a mild combinatorial explosion.    */
/* To deal with it, we try to generate derived                  */
/* definitions for as many of the combinations as we can, as    */
/* automatically as possible.                                   */
/*                                                              */
/* Our assumption throughout is that the programmer will        */
/* specify the least demanding operation and memory barrier     */
/* that will guarantee correctness for the implementation.      */
/* Our job is to find the least expensive way to implement it   */
/* on the applicable hardware.  In many cases that will         */
/* involve, for example, a stronger memory barrier, or a        */
/* combination of hardware primitives.                          */
/*                                                              */
/* Conventions:                                                 */
/* "plain" atomic operations are not guaranteed to include      */
/* a barrier.  The suffix in the name specifies the barrier     */
/* type.  Suffixes are:                                         */
/* _release: Earlier operations may not be delayed past it.     */
/* _acquire: Later operations may not move ahead of it.         */
/* _read: Subsequent reads must follow this operation and       */
/*        preceding reads.                                      */
/* _write: Earlier writes precede both this operation and       */
/*        later writes.                                         */
/* _full: Ordered with respect to both earlier and later memory */
/*        operations.                                           */
/* _release_write: Ordered with respect to earlier writes.      */
/* _acquire_read: Ordered with respect to later reads.          */
/*                                                              */
/* Currently we try to define the following atomic memory       */
/* operations, in combination with the above barriers:          */
/* AO_nop                                                       */
/* AO_load                                                      */
/* AO_store                                                     */
/* AO_test_and_set (binary)                                     */
/* AO_fetch_and_add                                             */
/* AO_fetch_and_add1                                            */
/* AO_fetch_and_sub1                                            */
/* AO_and                                                       */
/* AO_or                                                        */
/* AO_xor                                                       */
/* AO_compare_and_swap                                          */
/* AO_fetch_compare_and_swap                                    */
/*                                                              */
/* Note that atomicity guarantees are valid only if both        */
/* readers and writers use AO_ operations to access the         */
/* shared value, while ordering constraints are intended to     */
/* apply all memory operations.  If a location can potentially  */
/* be accessed simultaneously from multiple threads, and one of */
/* those accesses may be a write access, then all such          */
/* accesses to that location should be through AO_ primitives.  */
/* However if AO_ operations enforce sufficient ordering to     */
/* ensure that a location x cannot be accessed concurrently,    */
/* or can only be read concurrently, then x can be accessed     */
/* via ordinary references and assignments.                     */
/*                                                              */
/* AO_compare_and_swap takes an address and an expected old     */
/* value and a new value, and returns an int.  Non-zero result  */
/* indicates that it succeeded.                                 */
/* AO_fetch_compare_and_swap takes an address and an expected   */
/* old value and a new value, and returns the real old value.   */
/* The operation succeeded if and only if the expected old      */
/* value matches the old value returned.                        */
/*                                                              */
/* Test_and_set takes an address, atomically replaces it by     */
/* AO_TS_SET, and returns the prior value.                      */
/* An AO_TS_t location can be reset with the                    */
/* AO_CLEAR macro, which normally uses AO_store_release.        */
/* AO_fetch_and_add takes an address and an AO_t increment      */
/* value.  The AO_fetch_and_add1 and AO_fetch_and_sub1 variants */
/* are provided, since they allow faster implementations on     */
/* some hardware. AO_and, AO_or, AO_xor do atomically and, or,  */
/* xor (respectively) an AO_t value into a memory location,     */
/* but do not provide access to the original.                   */
/*                                                              */
/* We expect this list to grow slowly over time.                */
/*                                                              */
/* Note that AO_nop_full is a full memory barrier.              */
/*                                                              */
/* Note that if some data is initialized with                   */
/*      data.x = ...; data.y = ...; ...                         */
/*      AO_store_release_write(&data_is_initialized, 1)         */
/* then data is guaranteed to be initialized after the test     */
/*      if (AO_load_acquire_read(&data_is_initialized)) ...     */
/* succeeds.  Furthermore, this should generate near-optimal    */
/* code on all common platforms.                                */
/*                                                              */
/* All operations operate on unsigned AO_t, which               */
/* is the natural word size, and usually unsigned long.         */
/* It is possible to check whether a particular operation op    */
/* is available on a particular platform by checking whether    */
/* AO_HAVE_op is defined.  We make heavy use of these macros    */
/* internally.                                                  */

/* The rest of this file basically has three sections:          */
/*                                                              */
/* Some utility and default definitions.                        */
/*                                                              */
/* The architecture dependent section:                          */
/* This defines atomic operations that have direct hardware     */
/* support on a particular platform, mostly by including the    */
/* appropriate compiler- and hardware-dependent file.           */
/*                                                              */
/* The synthesis section:                                       */
/* This tries to define other atomic operations in terms of     */
/* those that are explicitly available on the platform.         */
/* This section is hardware independent.                        */
/* We make no attempt to synthesize operations in ways that     */
/* effectively introduce locks, except for the debugging/demo   */
/* pthread-based implementation at the beginning.  A more       */
/* realistic implementation that falls back to locks could be   */
/* added as a higher layer.  But that would sacrifice           */
/* usability from signal handlers.                              */
/* The synthesis section is implemented almost entirely in      */
/* atomic_ops/generalize.h.                                     */

/* Some common defaults.  Overridden for some architectures.    */
#define AO_t size_t

/* The test_and_set primitive returns an AO_TS_VAL_t value.     */
/* AO_TS_t is the type of an in-memory test-and-set location.   */

#define AO_TS_INITIALIZER ((AO_TS_t)AO_TS_CLEAR)

/* Convenient internal macro to test version of GCC.    */
#if defined(__GNUC__) && defined(__GNUC_MINOR__)
# define AO_GNUC_PREREQ(major, minor) \
            ((__GNUC__ << 16) + __GNUC_MINOR__ >= ((major) << 16) + (minor))
#else
# define AO_GNUC_PREREQ(major, minor) 0 /* false */
#endif

/* Convenient internal macro to test version of Clang.  */
#if defined(__clang__) && defined(__clang_major__)
# define AO_CLANG_PREREQ(major, minor) \
    ((__clang_major__ << 16) + __clang_minor__ >= ((major) << 16) + (minor))
#else
# define AO_CLANG_PREREQ(major, minor) 0 /* false */
#endif

/* Platform-dependent stuff:                                    */
#if (defined(__GNUC__) || defined(_MSC_VER) || defined(__INTEL_COMPILER) \
        || defined(__DMC__) || defined(__WATCOMC__)) && !defined(AO_NO_INLINE)
# define AO_INLINE static __inline
#elif defined(__sun) && !defined(AO_NO_INLINE)
# define AO_INLINE static inline
#else
# define AO_INLINE static
#endif

#if AO_GNUC_PREREQ(3, 0) && !defined(LINT2)
# define AO_EXPECT_FALSE(expr) __builtin_expect(expr, 0)
  /* Equivalent to (expr) but predict that usually (expr) == 0. */
#else
# define AO_EXPECT_FALSE(expr) (expr)
#endif /* !__GNUC__ */

#if defined(__has_feature)
  /* __has_feature() is supported.      */
# if __has_feature(address_sanitizer)
#   define AO_ADDRESS_SANITIZER
# endif
# if __has_feature(memory_sanitizer)
#   define AO_MEMORY_SANITIZER
# endif
# if __has_feature(thread_sanitizer)
#   define AO_THREAD_SANITIZER
# endif
#else
# ifdef __SANITIZE_ADDRESS__
    /* GCC v4.8+ */
#   define AO_ADDRESS_SANITIZER
# endif
#endif /* !__has_feature */

#ifndef AO_ATTR_NO_SANITIZE_MEMORY
# ifndef AO_MEMORY_SANITIZER
#   define AO_ATTR_NO_SANITIZE_MEMORY /* empty */
# elif AO_CLANG_PREREQ(3, 8)
#   define AO_ATTR_NO_SANITIZE_MEMORY __attribute__((no_sanitize("memory")))
# else
#   define AO_ATTR_NO_SANITIZE_MEMORY __attribute__((no_sanitize_memory))
# endif
#endif /* !AO_ATTR_NO_SANITIZE_MEMORY */

#ifndef AO_ATTR_NO_SANITIZE_THREAD
# ifndef AO_THREAD_SANITIZER
#   define AO_ATTR_NO_SANITIZE_THREAD /* empty */
# elif AO_CLANG_PREREQ(3, 8)
#   define AO_ATTR_NO_SANITIZE_THREAD __attribute__((no_sanitize("thread")))
# else
#   define AO_ATTR_NO_SANITIZE_THREAD __attribute__((no_sanitize_thread))
# endif
#endif /* !AO_ATTR_NO_SANITIZE_THREAD */

#if (AO_GNUC_PREREQ(7, 5) || __STDC_VERSION__ >= 201112L) && !defined(LINT2)
# define AO_ALIGNOF_SUPPORTED 1
#endif

#ifdef AO_ALIGNOF_SUPPORTED
# define AO_ASSERT_ADDR_ALIGNED(addr) \
    assert(((size_t)(addr) & (__alignof__(*(addr)) - 1)) == 0)
#else
# define AO_ASSERT_ADDR_ALIGNED(addr) \
    assert(((size_t)(addr) & (sizeof(*(addr)) - 1)) == 0)
#endif /* !AO_ALIGNOF_SUPPORTED */

#if defined(__GNUC__) && !defined(__INTEL_COMPILER)
# define AO_compiler_barrier() __asm__ __volatile__("" : : : "memory")
#elif defined(_MSC_VER) || defined(__DMC__) || defined(__BORLANDC__) \
        || defined(__WATCOMC__)
# if defined(_AMD64_) || defined(_M_X64) || _MSC_VER >= 1400
#   if defined(_WIN32_WCE)
/* #     include <cmnintrin.h> */
#   elif defined(_MSC_VER)
#     include <intrin.h>
#   endif
#   pragma intrinsic(_ReadWriteBarrier)
#   define AO_compiler_barrier() _ReadWriteBarrier()
        /* We assume this does not generate a fence instruction.        */
        /* The documentation is a bit unclear.                          */
# else
#   define AO_compiler_barrier() __asm { }
        /* The preceding implementation may be preferable here too.     */
        /* But the documentation warns about VC++ 2003 and earlier.     */
# endif
#elif defined(__INTEL_COMPILER)
# define AO_compiler_barrier() __memory_barrier()
                                        /* FIXME: Too strong? IA64-only? */
#elif defined(_HPUX_SOURCE)
# if defined(__ia64)
#   include <machine/sys/inline.h>
#   define AO_compiler_barrier() _Asm_sched_fence()
# else
    /* FIXME - We do not know how to do this.  This is a guess. */
    /* And probably a bad one.                                  */
    static volatile int AO_barrier_dummy;
#   define AO_compiler_barrier() (void)(AO_barrier_dummy = AO_barrier_dummy)
# endif
#else
  /* We conjecture that the following usually gives us the right        */
  /* semantics or an error.                                             */
# define AO_compiler_barrier() asm("")
#endif

#if defined(AO_USE_PTHREAD_DEFS)
# include "atomic_ops/sysdeps/generic_pthread.h"
#endif /* AO_USE_PTHREAD_DEFS */

#if (defined(__CC_ARM) || defined(__ARMCC__)) && !defined(__GNUC__) \
    && !defined(AO_USE_PTHREAD_DEFS)
# include "atomic_ops/sysdeps/armcc/arm_v6.h"
# define AO_GENERALIZE_TWICE
#endif

#if defined(__GNUC__) && !defined(AO_USE_PTHREAD_DEFS) \
    && !defined(__INTEL_COMPILER)
# if defined(__i386__)
    /* We don't define AO_USE_SYNC_CAS_BUILTIN for x86 here because     */
    /* it might require specifying additional options (like -march)     */
    /* or additional link libraries (if -march is not specified).       */
#   include "atomic_ops/sysdeps/gcc/x86.h"
# elif defined(__x86_64__)
#   if AO_GNUC_PREREQ(4, 2) && !defined(AO_USE_SYNC_CAS_BUILTIN)
      /* It is safe to use __sync CAS built-in on this architecture.    */
#     define AO_USE_SYNC_CAS_BUILTIN
#   endif
#   include "atomic_ops/sysdeps/gcc/x86.h"
# elif defined(__ia64__)
#   include "atomic_ops/sysdeps/gcc/ia64.h"
#   define AO_GENERALIZE_TWICE
# elif defined(__hppa__)
#   include "atomic_ops/sysdeps/gcc/hppa.h"
#   define AO_CAN_EMUL_CAS
# elif defined(__alpha__)
#   include "atomic_ops/sysdeps/gcc/alpha.h"
#   define AO_GENERALIZE_TWICE
# elif defined(__s390__)
#   include "atomic_ops/sysdeps/gcc/s390.h"
# elif defined(__sparc__)
#   include "atomic_ops/sysdeps/gcc/sparc.h"
#   define AO_CAN_EMUL_CAS
# elif defined(__m68k__)
#   include "atomic_ops/sysdeps/gcc/m68k.h"
# elif defined(__powerpc__) || defined(__ppc__) || defined(__PPC__) \
       || defined(__powerpc64__) || defined(__ppc64__) || defined(_ARCH_PPC)
#   include "atomic_ops/sysdeps/gcc/powerpc.h"
# elif defined(__aarch64__)
#   include "atomic_ops/sysdeps/gcc/aarch64.h"
#   define AO_CAN_EMUL_CAS
# elif defined(__arm__)
#   include "atomic_ops/sysdeps/gcc/arm.h"
#   define AO_CAN_EMUL_CAS
# elif defined(__cris__) || defined(CRIS)
#   include "atomic_ops/sysdeps/gcc/cris.h"
#   define AO_CAN_EMUL_CAS
#   define AO_GENERALIZE_TWICE
# elif defined(__mips__)
#   include "atomic_ops/sysdeps/gcc/mips.h"
# elif defined(__sh__) || defined(SH4)
#   include "atomic_ops/sysdeps/gcc/sh.h"
#   define AO_CAN_EMUL_CAS
# elif defined(__avr32__)
#   include "atomic_ops/sysdeps/gcc/avr32.h"
# elif defined(__hexagon__)
#   include "atomic_ops/sysdeps/gcc/hexagon.h"
# elif defined(__nios2__)
#   include "atomic_ops/sysdeps/gcc/generic.h"
#   define AO_CAN_EMUL_CAS
# elif defined(__riscv)
#   include "atomic_ops/sysdeps/gcc/riscv.h"
# elif defined(__tile__)
#   include "atomic_ops/sysdeps/gcc/tile.h"
# else /* etc. */
#   include "atomic_ops/sysdeps/gcc/generic.h"
# endif
#endif /* __GNUC__ && !AO_USE_PTHREAD_DEFS */

#if (defined(__IBMC__) || defined(__IBMCPP__)) && !defined(__GNUC__) \
    && !defined(AO_USE_PTHREAD_DEFS)
# if defined(__powerpc__) || defined(__powerpc) || defined(__ppc__) \
     || defined(__PPC__) || defined(_M_PPC) || defined(_ARCH_PPC) \
     || defined(_ARCH_PWR)
#   include "atomic_ops/sysdeps/ibmc/powerpc.h"
#   define AO_GENERALIZE_TWICE
# endif
#endif

#if defined(__INTEL_COMPILER) && !defined(AO_USE_PTHREAD_DEFS)
# if defined(__ia64__)
#   include "atomic_ops/sysdeps/icc/ia64.h"
#   define AO_GENERALIZE_TWICE
# endif
# if defined(__GNUC__)
    /* Intel Compiler in GCC compatible mode */
#   if defined(__i386__)
#     include "atomic_ops/sysdeps/gcc/x86.h"
#   endif /* __i386__ */
#   if defined(__x86_64__)
#     if (__INTEL_COMPILER > 1110) && !defined(AO_USE_SYNC_CAS_BUILTIN)
#       define AO_USE_SYNC_CAS_BUILTIN
#     endif
#     include "atomic_ops/sysdeps/gcc/x86.h"
#   endif /* __x86_64__ */
# endif
#endif

#if defined(_HPUX_SOURCE) && !defined(__GNUC__) && !defined(AO_USE_PTHREAD_DEFS)
# if defined(__ia64)
#   include "atomic_ops/sysdeps/hpc/ia64.h"
#   define AO_GENERALIZE_TWICE
# else
#   include "atomic_ops/sysdeps/hpc/hppa.h"
#   define AO_CAN_EMUL_CAS
# endif
#endif

#if defined(_MSC_VER) || defined(__DMC__) || defined(__BORLANDC__) \
        || (defined(__WATCOMC__) && defined(__NT__))
# if defined(_AMD64_) || defined(_M_X64) || defined(_M_ARM64)
#   include "atomic_ops/sysdeps/msftc/x86_64.h"
# elif defined(_M_IX86) || defined(x86)
#   include "atomic_ops/sysdeps/msftc/x86.h"
# elif defined(_M_ARM) || defined(ARM) || defined(_ARM_)
#   include "atomic_ops/sysdeps/msftc/arm.h"
#   define AO_GENERALIZE_TWICE
# endif
#endif

#if defined(__sun) && !defined(__GNUC__) && !defined(AO_USE_PTHREAD_DEFS)
  /* Note: use -DAO_USE_PTHREAD_DEFS if Sun CC does not handle inline asm. */
# if defined(__i386) || defined(__x86_64) || defined(__amd64)
#   include "atomic_ops/sysdeps/sunc/x86.h"
# endif
#endif

#if !defined(__GNUC__) && (defined(sparc) || defined(__sparc)) \
    && !defined(AO_USE_PTHREAD_DEFS)
# include "atomic_ops/sysdeps/sunc/sparc.h"
# define AO_CAN_EMUL_CAS
#endif

#if (defined(AO_REQUIRE_CAS) && !defined(AO_HAVE_compare_and_swap) \
    && !defined(AO_HAVE_fetch_compare_and_swap) \
    && !defined(AO_HAVE_compare_and_swap_full) \
    && !defined(AO_HAVE_fetch_compare_and_swap_full) \
    && !defined(AO_HAVE_compare_and_swap_acquire) \
    && !defined(AO_HAVE_fetch_compare_and_swap_acquire)) || defined(CPPCHECK)
# if defined(AO_CAN_EMUL_CAS)
#   include "atomic_ops/sysdeps/emul_cas.h"
# elif !defined(CPPCHECK)
#   error Cannot implement AO_compare_and_swap_full on this architecture.
# endif
#endif /* AO_REQUIRE_CAS && !AO_HAVE_compare_and_swap ... */

/* The most common way to clear a test-and-set location         */
/* at the end of a critical section.                            */
#if defined(AO_AO_TS_T) && !defined(AO_HAVE_CLEAR)
# define AO_CLEAR(addr) AO_store_release((AO_TS_t *)(addr), AO_TS_CLEAR)
# define AO_HAVE_CLEAR
#endif
#if defined(AO_CHAR_TS_T) && !defined(AO_HAVE_CLEAR)
# define AO_CLEAR(addr) AO_char_store_release((AO_TS_t *)(addr), AO_TS_CLEAR)
# define AO_HAVE_CLEAR
#endif

/* The generalization section.  */
#if !defined(AO_GENERALIZE_TWICE) && defined(AO_CAN_EMUL_CAS) \
    && !defined(AO_HAVE_compare_and_swap_full) \
    && !defined(AO_HAVE_fetch_compare_and_swap_full)
# define AO_GENERALIZE_TWICE
#endif

/* Theoretically we should repeatedly include atomic_ops/generalize.h.  */
/* In fact, we observe that this converges after a small fixed number   */
/* of iterations, usually one.                                          */
#include "atomic_ops/generalize.h"

#if !defined(AO_GENERALIZE_TWICE) \
    && defined(AO_HAVE_compare_double_and_swap_double) \
    && (!defined(AO_HAVE_double_load) || !defined(AO_HAVE_double_store))
# define AO_GENERALIZE_TWICE
#endif

#ifdef AO_T_IS_INT
  /* Included after the first generalization pass.      */
# include "atomic_ops/sysdeps/ao_t_is_int.h"
# ifndef AO_GENERALIZE_TWICE
    /* Always generalize again. */
#   define AO_GENERALIZE_TWICE
# endif
#endif /* AO_T_IS_INT */

#ifdef AO_GENERALIZE_TWICE
# include "atomic_ops/generalize.h"
#endif

/* For compatibility with version 0.4 and earlier       */
#define AO_TS_T AO_TS_t
#define AO_T AO_t
#define AO_TS_VAL AO_TS_VAL_t

#endif /* !AO_ATOMIC_OPS_H */
