// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module sync

import time

// There's no additional linking (-lpthread) needed for Android.
// See https://stackoverflow.com/a/31277163/1904615
$if !android {
	#flag -lpthread
}

#include <semaphore.h>

@[trusted]
fn C.pthread_mutex_init(voidptr, voidptr) int
fn C.pthread_mutex_lock(voidptr) int
fn C.pthread_mutex_trylock(voidptr) int
fn C.pthread_mutex_unlock(voidptr) int
fn C.pthread_mutex_destroy(voidptr) int
fn C.pthread_rwlockattr_init(voidptr) int
fn C.pthread_rwlockattr_setkind_np(voidptr, int) int
fn C.pthread_rwlockattr_destroy(voidptr) int
fn C.pthread_rwlock_init(voidptr, voidptr) int
fn C.pthread_rwlock_rdlock(voidptr) int
fn C.pthread_rwlock_wrlock(voidptr) int
fn C.pthread_rwlock_tryrdlock(voidptr) int
fn C.pthread_rwlock_trywrlock(voidptr) int
fn C.pthread_rwlock_unlock(voidptr) int
fn C.pthread_rwlock_destroy(voidptr) int
fn C.sem_init(voidptr, int, u32) int
fn C.sem_post(voidptr) int
fn C.sem_wait(voidptr) int
fn C.sem_trywait(voidptr) int
fn C.sem_timedwait(voidptr, voidptr) int
fn C.sem_destroy(voidptr) int

pub struct C.pthread_mutex {}

pub struct C.pthread_rwlock {}

@[typedef]
pub struct C.pthread_rwlockattr_t {}

@[typedef]
pub struct C.sem_t {}

// [init_with=new_mutex] // TODO: implement support for this struct attribute, and disallow Mutex{} from outside the sync.new_mutex() function.
@[heap]
pub struct Mutex {
	mutex &C.pthread_mutex = unsafe { nil }
}

@[heap]
pub struct RwMutex {
	mutex &C.pthread_rwlock = unsafe { nil }
}

struct RwMutexAttr {
	attr C.pthread_rwlockattr_t
}

@[heap]
pub struct Semaphore {
	sem C.sem_t
}

// new_mutex creates and initialises a new mutex instance on the heap, then returns a pointer to it.
pub fn new_mutex() &Mutex {
	mut m := &Mutex{}
	m.init()
	return m
}

// init initialises the mutex. It should be called once before the mutex is used,
// since it creates the associated resources needed for the mutex to work properly.
@[inline]
pub fn (mut m Mutex) init() {
	C.pthread_mutex_init(&m.mutex, C.NULL)
}

// new_rwmutex creates a new read/write mutex instance on the heap, and returns a pointer to it.
pub fn new_rwmutex() &RwMutex {
	mut m := &RwMutex{}
	m.init()
	return m
}

// init initialises the RwMutex instance. It should be called once before the rw mutex is used,
// since it creates the associated resources needed for the mutex to work properly.
pub fn (mut m RwMutex) init() {
	a := RwMutexAttr{}
	C.pthread_rwlockattr_init(&a.attr)
	// Give writer priority over readers
	C.pthread_rwlockattr_setkind_np(&a.attr, C.PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP)
	C.pthread_rwlock_init(&m.mutex, &a.attr)
	C.pthread_rwlockattr_destroy(&a.attr) // destroy the attr when done
}

// lock locks the mutex instance (`lock` is a keyword).
// If the mutex was already locked, it will block, till it is unlocked.
@[inline]
pub fn (mut m Mutex) lock() {
	C.pthread_mutex_lock(&m.mutex)
}

// try_lock try to lock the mutex instance and return immediately.
// If the mutex was already locked, it will return false.
@[inline]
pub fn (mut m Mutex) try_lock() bool {
	return C.pthread_mutex_trylock(&m.mutex) == 0
}

// unlock unlocks the mutex instance. The mutex is released, and one of
// the other threads, that were blocked, because they called lock can continue.
@[inline]
pub fn (mut m Mutex) unlock() {
	C.pthread_mutex_unlock(&m.mutex)
}

// destroy frees the resources associated with the mutex instance.
// Note: the mutex itself is not freed.
pub fn (mut m Mutex) destroy() {
	should_be_zero(C.pthread_mutex_destroy(&m.mutex))
}

// rlock locks the given RwMutex instance for reading.
// If the mutex was already locked, it will block, and will try to get the lock,
// once the lock is released by another thread calling unlock.
// Once it succeds, it returns.
// Note: there may be several threads that are waiting for the same lock.
// Note: RwMutex has separate read and write locks.
@[inline]
pub fn (mut m RwMutex) rlock() {
	C.pthread_rwlock_rdlock(&m.mutex)
}

// lock locks the given RwMutex instance for writing.
// If the mutex was already locked, it will block, till it is unlocked,
// then it will try to get the lock, and if it can, it will return, otherwise
// it will continue waiting for the mutex to become unlocked.
// Note: there may be several threads that are waiting for the same lock.
// Note: RwMutex has separate read and write locks.
@[inline]
pub fn (mut m RwMutex) lock() {
	C.pthread_rwlock_wrlock(&m.mutex)
}

// try_rlock try to lock the given RwMutex instance for reading and return immediately.
// If the mutex was already locked, it will return false.
@[inline]
pub fn (mut m RwMutex) try_rlock() bool {
	return C.pthread_rwlock_tryrdlock(&m.mutex) == 0
}

// try_wlock try to lock the given RwMutex instance for writing and return immediately.
// If the mutex was already locked, it will return false.
@[inline]
pub fn (mut m RwMutex) try_wlock() bool {
	return C.pthread_rwlock_trywrlock(&m.mutex) == 0
}

// destroy frees the resources associated with the rwmutex instance.
// Note: the mutex itself is not freed.
pub fn (mut m RwMutex) destroy() {
	should_be_zero(C.pthread_rwlock_destroy(&m.mutex))
}

// runlock unlocks the RwMutex instance, locked for reading.
// Note: Windows SRWLocks have different function to unlocking.
// To have a common portable API, there are two methods for
// unlocking here as well, even though that they do the same
// on !windows platforms.
@[inline]
pub fn (mut m RwMutex) runlock() {
	C.pthread_rwlock_unlock(&m.mutex)
}

// unlock unlocks the RwMutex instance, locked for writing.
// Note: Windows SRWLocks have different function to unlocking.
// To have a common portable API, there are two methods for
// unlocking here as well, even though that they do the same
// on !windows platforms.
@[inline]
pub fn (mut m RwMutex) unlock() {
	C.pthread_rwlock_unlock(&m.mutex)
}

// new_semaphore creates a new initialised Semaphore instance on the heap, and returns a pointer to it.
// The initial counter value of the semaphore is 0.
@[inline]
pub fn new_semaphore() &Semaphore {
	return new_semaphore_init(0)
}

// new_semaphore_init creates a new initialised Semaphore instance on the heap, and returns a pointer to it.
// The `n` parameter can be used to set the initial counter value of the semaphore.
pub fn new_semaphore_init(n u32) &Semaphore {
	mut sem := &Semaphore{}
	sem.init(n)
	return sem
}

// init initialises the Semaphore instance with `n` as its initial counter value.
// It should be called once before the semaphore is used, since it creates the associated
// resources needed for the semaphore to work properly.
@[inline]
pub fn (mut sem Semaphore) init(n u32) {
	C.sem_init(&sem.sem, 0, n)
}

// post increases/unlocks the counter of the semaphore by 1.
// If the resulting counter value is > 0, and if there is another thread waiting
// on the semaphore, the waiting thread will decrement the counter by 1
// (locking the semaphore), and then will continue running. See also .wait() .
@[inline]
pub fn (mut sem Semaphore) post() {
	C.sem_post(&sem.sem)
}

// wait will just decrement the semaphore count, if it was positive.
// It it was not positive, it will waits for the semaphore count to reach a positive number.
// When that happens, it will decrease the semaphore count (lock the semaphore), and will return.
// In effect, it allows you to block threads, until the semaphore, is posted by another thread.
// See also .post() .
pub fn (mut sem Semaphore) wait() {
	for {
		if C.sem_wait(&sem.sem) == 0 {
			return
		}
		e := C.errno
		match e {
			C.EINTR {
				continue // interrupted by signal
			}
			else {
				cpanic_errno()
			}
		}
	}
}

// try_wait tries to decrease the semaphore count by 1, if it was positive.
// If it succeeds in that, it returns true, otherwise it returns false.
// try_wait should return as fast as possible so error handling is only
// done when debugging
pub fn (mut sem Semaphore) try_wait() bool {
	$if !debug {
		return C.sem_trywait(&sem.sem) == 0
	} $else {
		if C.sem_trywait(&sem.sem) != 0 {
			e := C.errno
			match e {
				C.EAGAIN {
					return false
				}
				else {
					cpanic_errno()
				}
			}
		}
		return true
	}
}

// timed_wait is similar to .wait(), but it also accepts a timeout duration,
// thus it can return false early, if the timeout passed before the semaphore was posted.
pub fn (mut sem Semaphore) timed_wait(timeout time.Duration) bool {
	$if macos {
		time.sleep(timeout)
		return true
	}
	t_spec := timeout.timespec()
	for {
		$if !macos {
			if C.sem_timedwait(&sem.sem, &t_spec) == 0 {
				return true
			}
		}
		e := C.errno
		match e {
			C.EINTR {
				continue // interrupted by signal
			}
			C.ETIMEDOUT {
				break
			}
			else {
				cpanic(e)
			}
		}
	}
	return false
}

// destroy frees the resources associated with the Semaphore instance.
// Note: the semaphore instance itself is not freed.
pub fn (mut sem Semaphore) destroy() {
	should_be_zero(C.sem_destroy(&sem.sem))
}
