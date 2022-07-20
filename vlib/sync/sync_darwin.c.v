// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module sync

import time

#flag -lpthread
#include <semaphore.h>
#include <sys/errno.h>

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
fn C.pthread_condattr_init(voidptr) int
fn C.pthread_condattr_setpshared(voidptr, int) int
fn C.pthread_condattr_destroy(voidptr) int
fn C.pthread_cond_init(voidptr, voidptr) int
fn C.pthread_cond_signal(voidptr) int
fn C.pthread_cond_wait(voidptr, voidptr) int
fn C.pthread_cond_timedwait(voidptr, voidptr, voidptr) int
fn C.pthread_cond_destroy(voidptr) int

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

struct CondAttr {
	attr C.pthread_condattr_t
}

/*
MacOSX has no unnamed semaphores and no `timed_wait()` at all
   so we emulate the behaviour with other devices
*/
[heap]
pub struct Semaphore {
	mtx  C.pthread_mutex_t
	cond C.pthread_cond_t
mut:
	count u32
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
	C.atomic_store_u32(&sem.count, n)
	C.pthread_mutex_init(&sem.mtx, C.NULL)
	attr := CondAttr{}
	C.pthread_condattr_init(&attr.attr)
	C.pthread_condattr_setpshared(&attr.attr, C.PTHREAD_PROCESS_PRIVATE)
	C.pthread_cond_init(&sem.cond, &attr.attr)
	C.pthread_condattr_destroy(&attr.attr)
}

pub fn (mut sem Semaphore) post() {
	mut c := C.atomic_load_u32(&sem.count)
	for c > 1 {
		if C.atomic_compare_exchange_weak_u32(&sem.count, &c, c + 1) {
			return
		}
	}
	C.pthread_mutex_lock(&sem.mtx)
	c = C.atomic_fetch_add_u32(&sem.count, 1)
	if c == 0 {
		C.pthread_cond_signal(&sem.cond)
	}
	C.pthread_mutex_unlock(&sem.mtx)
}

pub fn (mut sem Semaphore) wait() {
	mut c := C.atomic_load_u32(&sem.count)
	for c > 0 {
		if C.atomic_compare_exchange_weak_u32(&sem.count, &c, c - 1) {
			return
		}
	}
	C.pthread_mutex_lock(&sem.mtx)
	c = C.atomic_load_u32(&sem.count)

	outer: for {
		if c == 0 {
			C.pthread_cond_wait(&sem.cond, &sem.mtx)
			c = C.atomic_load_u32(&sem.count)
		}
		for c > 0 {
			if C.atomic_compare_exchange_weak_u32(&sem.count, &c, c - 1) {
				if c > 1 {
					C.pthread_cond_signal(&sem.cond)
				}
				break outer
			}
		}
	}
	C.pthread_mutex_unlock(&sem.mtx)
}

pub fn (mut sem Semaphore) try_wait() bool {
	mut c := C.atomic_load_u32(&sem.count)
	for c > 0 {
		if C.atomic_compare_exchange_weak_u32(&sem.count, &c, c - 1) {
			return true
		}
	}
	return false
}

pub fn (mut sem Semaphore) timed_wait(timeout time.Duration) bool {
	mut c := C.atomic_load_u32(&sem.count)
	for c > 0 {
		if C.atomic_compare_exchange_weak_u32(&sem.count, &c, c - 1) {
			return true
		}
	}
	C.pthread_mutex_lock(&sem.mtx)
	t_spec := timeout.timespec()
	mut res := 0
	c = C.atomic_load_u32(&sem.count)

	outer: for {
		if c == 0 {
			res = C.pthread_cond_timedwait(&sem.cond, &sem.mtx, &t_spec)
			if res == C.ETIMEDOUT {
				break outer
			}
			c = C.atomic_load_u32(&sem.count)
		}
		for c > 0 {
			if C.atomic_compare_exchange_weak_u32(&sem.count, &c, c - 1) {
				if c > 1 {
					C.pthread_cond_signal(&sem.cond)
				}
				break outer
			}
		}
	}
	C.pthread_mutex_unlock(&sem.mtx)
	return res == 0
}

pub fn (mut sem Semaphore) destroy() {
	mut res := C.pthread_cond_destroy(&sem.cond)
	if res == 0 {
		res = C.pthread_mutex_destroy(&sem.mtx)
		if res == 0 {
			return
		}
	}
	panic(unsafe { tos_clone(&u8(C.strerror(res))) })
}
