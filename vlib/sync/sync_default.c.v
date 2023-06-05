// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
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
fn C.pthread_rwlockattr_destroy(voidptr) int
fn C.pthread_rwlock_init(voidptr, voidptr) int
fn C.pthread_rwlock_rdlock(voidptr) int
fn C.pthread_rwlock_wrlock(voidptr) int
fn C.pthread_rwlock_unlock(voidptr) int
fn C.pthread_rwlock_destroy(voidptr) int
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

// `new_mutex` create and init a new mutex object. You should not call `init` again.
pub fn new_mutex() &Mutex {
	mut m := &Mutex{}
	m.init()
	return m
}

// `init` the mutex object.
pub fn (mut m Mutex) init() {
	C.pthread_mutex_init(&m.mutex, C.NULL)
}

// `new_rwmutex` create and init a new rwmutex object. You should not call `init` again.
pub fn new_rwmutex() &RwMutex {
	mut m := &RwMutex{}
	m.init()
	return m
}

// `init` the rwmutex object.
pub fn (mut m RwMutex) init() {
	a := RwMutexAttr{}
	C.pthread_rwlockattr_init(&a.attr)
	// Give writer priority over readers
	C.pthread_rwlockattr_setkind_np(&a.attr, C.PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP)
	C.pthread_rwlockattr_setpshared(&a.attr, C.PTHREAD_PROCESS_PRIVATE)
	C.pthread_rwlock_init(&m.mutex, &a.attr)
	C.pthread_rwlockattr_destroy(&a.attr) // destroy the attr when done
}

// `@lock` the mutex, wait and return after got the mutex lock.
pub fn (mut m Mutex) @lock() {
	C.pthread_mutex_lock(&m.mutex)
}

// `unlock` the mutex. The mutex is released.
pub fn (mut m Mutex) unlock() {
	C.pthread_mutex_unlock(&m.mutex)
}

// `destroy` the mutex object.
pub fn (mut m Mutex) destroy() {
	res := C.pthread_mutex_destroy(&m.mutex)
	if res == 0 {
		return
	}
	panic(unsafe { tos_clone(&u8(C.strerror(res))) })
}

// `@rlock` read-lock the rwmutex, wait and return after got read access.
pub fn (mut m RwMutex) @rlock() {
	C.pthread_rwlock_rdlock(&m.mutex)
}

// `@lock` read & write-lock the rwmutex, wait and return after got read & write access.
pub fn (mut m RwMutex) @lock() {
	C.pthread_rwlock_wrlock(&m.mutex)
}

// `destroy` the rwmutex object.
pub fn (mut m RwMutex) destroy() {
	res := C.pthread_rwlock_destroy(&m.mutex)
	if res == 0 {
		return
	}
	panic(unsafe { tos_clone(&u8(C.strerror(res))) })
}

// Windows SRWLocks have different function to unlock
// So provide two functions here, too, to have a common interface
pub fn (mut m RwMutex) runlock() {
	C.pthread_rwlock_unlock(&m.mutex)
}

// `unlock` the rwmutex object. The rwmutex is released.
pub fn (mut m RwMutex) unlock() {
	C.pthread_rwlock_unlock(&m.mutex)
}

// `new_semaphore` create a new semaphore, with zero initial value.
[inline]
pub fn new_semaphore() &Semaphore {
	return new_semaphore_init(0)
}

// `new_semaphore_init` create a semaphore, with `n` initial value.
pub fn new_semaphore_init(n u32) &Semaphore {
	mut sem := &Semaphore{}
	sem.init(n)
	return sem
}

// `init` the semaphore, with `n` initial value.
pub fn (mut sem Semaphore) init(n u32) {
	C.sem_init(&sem.sem, 0, n)
}

// `post` increase the semaphore's value by 1.
pub fn (mut sem Semaphore) post() {
	C.sem_post(&sem.sem)
}

// `wait` decrease the semaphore's value by 1. If semaphore's original value is zero, then wait.
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

// `timed_wait` decrease the semaphore's value by 1. If semaphore's original
// value is zero, then wait. If `timeout` return false.
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

// `destroy` the semaphore object.
pub fn (mut sem Semaphore) destroy() {
	res := C.sem_destroy(&sem.sem)
	if res == 0 {
		return
	}
	panic(unsafe { tos_clone(&u8(C.strerror(res))) })
}
