/*
 * Copyright (c) 2003-2011 Hewlett-Packard Development Company, L.P.
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

/*
 * Initialized data and out-of-line functions to support atomic_ops.h
 * go here.  Currently this is needed only for pthread-based atomics
 * emulation, or for compare-and-swap emulation.
 * Pthreads emulation isn't useful on a native Windows platform, and
 * cas emulation is not needed.  Thus we skip this on Windows.
 */

#if defined(HAVE_CONFIG_H)
# include "config.h"
#endif

#if (defined(__hexagon__) || defined(__native_client__)) \
    && !defined(AO_USE_NO_SIGNALS) && !defined(AO_USE_NANOSLEEP)
  /* Hexagon QuRT does not have sigprocmask (but Hexagon does not need  */
  /* emulation, so it is OK not to bother about signals blocking).      */
  /* Since NaCl is not recognized by configure yet, we do it here.      */
# define AO_USE_NO_SIGNALS
# define AO_USE_NANOSLEEP
#endif

#if defined(AO_USE_WIN32_PTHREADS) && !defined(AO_USE_NO_SIGNALS)
# define AO_USE_NO_SIGNALS
#endif

#if (defined(__linux__) || defined(__GLIBC__) || defined(__GNU__)) \
    && !defined(AO_USE_NO_SIGNALS) && !defined(_GNU_SOURCE)
# define _GNU_SOURCE 1
#endif

#undef AO_REQUIRE_CAS
#include "atomic_ops.h" /* Without cas emulation! */

#if !defined(_MSC_VER) && !defined(__MINGW32__) && !defined(__BORLANDC__) \
    || defined(AO_USE_NO_SIGNALS)

#ifndef AO_NO_PTHREADS
# include <pthread.h>
#endif

#ifndef AO_USE_NO_SIGNALS
# include <signal.h>
#endif

#ifdef AO_USE_NANOSLEEP
  /* This requires _POSIX_TIMERS feature. */
# include <sys/time.h>
# include <time.h>
#elif defined(AO_USE_WIN32_PTHREADS)
# include <windows.h> /* for Sleep() */
#elif defined(_HPUX_SOURCE)
# include <sys/time.h>
#else
# include <sys/select.h>
#endif

#ifndef AO_HAVE_double_t
# include "atomic_ops/sysdeps/standard_ao_double_t.h"
#endif

/* Lock for pthreads-based implementation.      */
#ifndef AO_NO_PTHREADS
  pthread_mutex_t AO_pt_lock = PTHREAD_MUTEX_INITIALIZER;
#endif

/*
 * Out of line compare-and-swap emulation based on test and set.
 *
 * We use a small table of locks for different compare_and_swap locations.
 * Before we update perform a compare-and-swap, we grab the corresponding
 * lock.  Different locations may hash to the same lock, but since we
 * never acquire more than one lock at a time, this can't deadlock.
 * We explicitly disable signals while we perform this operation.
 *
 * TODO: Probably also support emulation based on Lamport
 * locks, since we may not have test_and_set either.
 */
#define AO_HASH_SIZE 16

#define AO_HASH(x) (((unsigned long)(x) >> 12) & (AO_HASH_SIZE-1))

static AO_TS_t AO_locks[AO_HASH_SIZE] = {
  AO_TS_INITIALIZER, AO_TS_INITIALIZER, AO_TS_INITIALIZER, AO_TS_INITIALIZER,
  AO_TS_INITIALIZER, AO_TS_INITIALIZER, AO_TS_INITIALIZER, AO_TS_INITIALIZER,
  AO_TS_INITIALIZER, AO_TS_INITIALIZER, AO_TS_INITIALIZER, AO_TS_INITIALIZER,
  AO_TS_INITIALIZER, AO_TS_INITIALIZER, AO_TS_INITIALIZER, AO_TS_INITIALIZER,
};

void AO_pause(int); /* defined below */

static void lock_ool(volatile AO_TS_t *l)
{
  int i = 0;

  while (AO_test_and_set_acquire(l) == AO_TS_SET)
    AO_pause(++i);
}

AO_INLINE void lock(volatile AO_TS_t *l)
{
  if (AO_EXPECT_FALSE(AO_test_and_set_acquire(l) == AO_TS_SET))
    lock_ool(l);
}

AO_INLINE void unlock(volatile AO_TS_t *l)
{
  AO_CLEAR(l);
}

#ifndef AO_USE_NO_SIGNALS
  static sigset_t all_sigs;
  static volatile AO_t initialized = 0;
  static volatile AO_TS_t init_lock = AO_TS_INITIALIZER;

  AO_INLINE void block_all_signals(sigset_t *old_sigs_ptr)
  {
    if (AO_EXPECT_FALSE(!AO_load_acquire(&initialized)))
    {
      lock(&init_lock);
      if (!initialized)
        sigfillset(&all_sigs);
      unlock(&init_lock);
      AO_store_release(&initialized, 1);
    }
    sigprocmask(SIG_BLOCK, &all_sigs, old_sigs_ptr);
        /* Neither sigprocmask nor pthread_sigmask is 100%      */
        /* guaranteed to work here.  Sigprocmask is not         */
        /* guaranteed be thread safe, and pthread_sigmask       */
        /* is not async-signal-safe.  Under linuxthreads,       */
        /* sigprocmask may block some pthreads-internal         */
        /* signals.  So long as we do that for short periods,   */
        /* we should be OK.                                     */
  }
#endif /* !AO_USE_NO_SIGNALS */

AO_t AO_fetch_compare_and_swap_emulation(volatile AO_t *addr, AO_t old_val,
                                         AO_t new_val)
{
  AO_TS_t *my_lock = AO_locks + AO_HASH(addr);
  AO_t fetched_val;

# ifndef AO_USE_NO_SIGNALS
    sigset_t old_sigs;
    block_all_signals(&old_sigs);
# endif
  lock(my_lock);
  fetched_val = *addr;
  if (fetched_val == old_val)
    *addr = new_val;
  unlock(my_lock);
# ifndef AO_USE_NO_SIGNALS
    sigprocmask(SIG_SETMASK, &old_sigs, NULL);
# endif
  return fetched_val;
}

int AO_compare_double_and_swap_double_emulation(volatile AO_double_t *addr,
                                                AO_t old_val1, AO_t old_val2,
                                                AO_t new_val1, AO_t new_val2)
{
  AO_TS_t *my_lock = AO_locks + AO_HASH(addr);
  int result;

# ifndef AO_USE_NO_SIGNALS
    sigset_t old_sigs;
    block_all_signals(&old_sigs);
# endif
  lock(my_lock);
  if (addr -> AO_val1 == old_val1 && addr -> AO_val2 == old_val2)
    {
      addr -> AO_val1 = new_val1;
      addr -> AO_val2 = new_val2;
      result = 1;
    }
  else
    result = 0;
  unlock(my_lock);
# ifndef AO_USE_NO_SIGNALS
    sigprocmask(SIG_SETMASK, &old_sigs, NULL);
# endif
  return result;
}

void AO_store_full_emulation(volatile AO_t *addr, AO_t val)
{
  AO_TS_t *my_lock = AO_locks + AO_HASH(addr);
  lock(my_lock);
  *addr = val;
  unlock(my_lock);
}

#else /* Non-posix platform */

# include <windows.h>

# define AO_USE_WIN32_PTHREADS
                /* define to use Sleep() */

  extern int AO_non_posix_implementation_is_entirely_in_headers;

#endif

static AO_t spin_dummy = 1;

/* Spin for 2**n units. */
static void AO_spin(int n)
{
  AO_t j = AO_load(&spin_dummy);
  int i = 2 << n;

  while (i-- > 0)
    j += (j - 1) << 2;
  /* Given 'spin_dummy' is initialized to 1, j is 1 after the loop.     */
  AO_store(&spin_dummy, j);
}

void AO_pause(int n)
{
  if (n < 12)
    AO_spin(n);
  else
    {
#     ifdef AO_USE_NANOSLEEP
        struct timespec ts;
        ts.tv_sec = 0;
        ts.tv_nsec = n > 28 ? 100000L * 1000 : 1L << (n - 2);
        nanosleep(&ts, 0);
#     elif defined(AO_USE_WIN32_PTHREADS)
        Sleep(n > 28 ? 100 /* millis */
                     : n < 22 ? 1 : (DWORD)1 << (n - 22));
#     else
        struct timeval tv;
        /* Short async-signal-safe sleep. */
        int usec = n > 28 ? 100000 : 1 << (n - 12);
                /* Use an intermediate variable (of int type) to avoid  */
                /* "shift followed by widening conversion" warning.     */

        tv.tv_sec = 0;
        tv.tv_usec = usec;
        (void)select(0, 0, 0, 0, &tv);
#     endif
    }
}
