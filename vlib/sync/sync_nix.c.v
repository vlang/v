// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module sync

import time

#flag -lpthread
$if macos {
	#include <dispatch/dispatch.h>
}
#include <semaphore.h>

// [init_with=new_mutex] // TODO: implement support for this struct attribute, and disallow Mutex{} from outside the sync.new_mutex() function.
[ref_only]
pub struct Mutex {
	mutex C.pthread_mutex_t
}

[ref_only]
pub struct RwMutex {
	mutex C.pthread_rwlock_t
}

[ref_only]
struct RwMutexAttr {
	attr C.pthread_rwlockattr_t
}

/* MacOSX has no unnamed semaphores and no `timed_wait()` at all
   so we emulate the behaviour with other devices */
// [ref_only]
// struct MacOSX_Semaphore {
// 	sem C.dispatch_semaphore_t
// }

[ref_only]
struct PosixSemaphore {
	sem C.sem_t
}

pub struct Semaphore {
mut:
	sem voidptr
}

pub fn new_mutex() &Mutex {
	m := &Mutex{}
	C.pthread_mutex_init(&m.mutex, C.NULL)
	return m
}

pub fn new_rwmutex() &RwMutex {
	m := &RwMutex{}
	a := &RwMutexAttr{}
	C.pthread_rwlockattr_init(&a.attr)
	// Give writer priority over readers
	C.pthread_rwlockattr_setkind_np(&a.attr, C.PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP)
	C.pthread_rwlockattr_setpshared(&a.attr, C.PTHREAD_PROCESS_PRIVATE)
	C.pthread_rwlock_init(&m.mutex, &a.attr)
	return m
}

// m_lock(), for *manual* mutex handling, since `lock` is a keyword
pub fn (mut m Mutex) m_lock() {
	C.pthread_mutex_lock(&m.mutex)
}

pub fn (mut m Mutex) unlock() {
	C.pthread_mutex_unlock(&m.mutex)
}

// RwMutex has separate read- and write locks
pub fn (mut m RwMutex) r_lock() {
	C.pthread_rwlock_rdlock(&m.mutex)
}

pub fn (mut m RwMutex) w_lock() {
	C.pthread_rwlock_wrlock(&m.mutex)
}

// Windows SRWLocks have different function to unlock
// So provide two functions here, too, to have a common interface
pub fn (mut m RwMutex) r_unlock() {
	C.pthread_rwlock_unlock(&m.mutex)
}

pub fn (mut m RwMutex) w_unlock() {
	C.pthread_rwlock_unlock(&m.mutex)
}

[inline]
pub fn new_semaphore() Semaphore {
	return new_semaphore_init(0)
}

pub fn new_semaphore_init(n u32) Semaphore {
	$if macos {
		s := Semaphore{
			sem: C.dispatch_semaphore_create(n)
		}
		return s
	} $else {
		s := Semaphore{
			sem: &PosixSemaphore{}
		}
		unsafe { C.sem_init(&&PosixSemaphore(s.sem).sem, 0, n) }
		return s
	}
}

pub fn (s Semaphore) post() {
	$if macos {
		C.dispatch_semaphore_signal(s.sem)
	} $else {
		unsafe { C.sem_post(&&PosixSemaphore(s.sem).sem) }
	}
}

pub fn (s Semaphore) wait() {
	$if macos {
		C.dispatch_semaphore_wait(s.sem, C.DISPATCH_TIME_FOREVER)
	} $else {
		unsafe { C.sem_wait(&&PosixSemaphore(s.sem).sem) }
	}
}

pub fn (s Semaphore) try_wait() bool {
	$if macos {
		return C.dispatch_semaphore_wait(s.sem, C.DISPATCH_TIME_NOW) == 0
	} $else {
		return unsafe { C.sem_trywait(&&PosixSemaphore(s.sem).sem) == 0 }
	}
}

pub fn (s Semaphore) timed_wait(timeout time.Duration) bool {
	$if macos {
		return C.dispatch_semaphore_wait(s.sem, C.dispatch_time(C.DISPATCH_TIME_NOW, i64(timeout))) == 0
	} $else {
		t_spec := timeout.timespec()
		return unsafe { C.sem_timedwait(&&PosixSemaphore(s.sem).sem, &t_spec) == 0 }
	}
}

pub fn (s Semaphore) destroy() bool {
	$if macos {
		for s.try_wait() {}
		C.dispatch_release(s.sem)
		return true
	} $else {
		return unsafe { C.sem_destroy(&&PosixSemaphore(s.sem).sem) == 0 }
	}
}
