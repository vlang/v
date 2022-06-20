// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
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

[trusted]
fn C.pthread_mutex_init(voidptr, voidptr) int
fn C.pthread_mutex_lock(voidptr) int
fn C.pthread_mutex_unlock(voidptr) int
fn C.pthread_mutex_destroy(voidptr) int
fn C.pthread_rwlockattr_init(voidptr) int
fn C.pthread_rwlockattr_setkind_np(voidptr, int) int
fn C.pthread_rwlockattr_setpshared(voidptr, int) int
fn C.pthread_rwlock_init(voidptr, voidptr) int
fn C.pthread_rwlock_rdlock(voidptr) int
fn C.pthread_rwlock_wrlock(voidptr) int
fn C.pthread_rwlock_unlock(voidptr) int
fn C.sem_init(voidptr, int, u32) int
fn C.sem_post(voidptr) int
fn C.sem_wait(voidptr) int
fn C.sem_trywait(voidptr) int
fn C.sem_timedwait(voidptr, voidptr) int
fn C.sem_destroy(voidptr) int

[typedef]
struct C.pthread_mutex_t {}

[typedef]
struct C.pthread_rwlock_t {}

[typedef]
struct C.pthread_rwlockattr_t {}

[typedef]
struct C.sem_t {}

// [init_with=new_mutex] // TODO: implement support for this struct attribute, and disallow Mutex{} from outside the sync.new_mutex() function.
[heap]
pub struct Mutex {
	mutex C.pthread_mutex_t
}

[heap]
pub struct RwMutex {
	mutex C.pthread_rwlock_t
}

struct RwMutexAttr {
	attr C.pthread_rwlockattr_t
}

[heap]
pub struct Semaphore {
	sem C.sem_t
}

pub fn new_mutex() &Mutex {
	mut m := &Mutex{}
	m.init()
	return m
}

pub fn (mut m Mutex) init() {
	C.pthread_mutex_init(&m.mutex, C.NULL)
}

pub fn new_rwmutex() &RwMutex {
	mut m := &RwMutex{}
	m.init()
	return m
}

pub fn (mut m RwMutex) init() {
	a := RwMutexAttr{}
	C.pthread_rwlockattr_init(&a.attr)
	// Give writer priority over readers
	C.pthread_rwlockattr_setkind_np(&a.attr, C.PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP)
	C.pthread_rwlockattr_setpshared(&a.attr, C.PTHREAD_PROCESS_PRIVATE)
	C.pthread_rwlock_init(&m.mutex, &a.attr)
}

// @lock(), for *manual* mutex handling, since `lock` is a keyword
pub fn (mut m Mutex) @lock() {
	C.pthread_mutex_lock(&m.mutex)
}

pub fn (mut m Mutex) unlock() {
	C.pthread_mutex_unlock(&m.mutex)
}

// RwMutex has separate read- and write locks
pub fn (mut m RwMutex) @rlock() {
	C.pthread_rwlock_rdlock(&m.mutex)
}

pub fn (mut m RwMutex) @lock() {
	C.pthread_rwlock_wrlock(&m.mutex)
}

// Windows SRWLocks have different function to unlock
// So provide two functions here, too, to have a common interface
pub fn (mut m RwMutex) runlock() {
	C.pthread_rwlock_unlock(&m.mutex)
}

pub fn (mut m RwMutex) unlock() {
	C.pthread_rwlock_unlock(&m.mutex)
}

[inline]
pub fn new_semaphore() &Semaphore {
	return new_semaphore_init(0)
}

pub fn new_semaphore_init(n u32) &Semaphore {
	mut sem := &Semaphore{}
	sem.init(n)
	return sem
}

pub fn (mut sem Semaphore) init(n u32) {
	C.sem_init(&sem.sem, 0, n)
}

pub fn (mut sem Semaphore) post() {
	C.sem_post(&sem.sem)
}

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
				panic(unsafe { tos_clone(&u8(C.strerror(C.errno))) })
			}
		}
	}
}

// `try_wait()` should return as fast as possible so error handling is only
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
					panic(unsafe { tos_clone(&u8(C.strerror(C.errno))) })
				}
			}
		}
		return true
	}
}

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
				panic(unsafe { tos_clone(&u8(C.strerror(e))) })
			}
		}
	}
	return false
}

pub fn (sem Semaphore) destroy() {
	res := C.sem_destroy(&sem.sem)
	if res == 0 {
		return
	}
	panic(unsafe { tos_clone(&u8(C.strerror(res))) })
}
