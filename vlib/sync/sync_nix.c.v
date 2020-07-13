// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module sync

#flag -lpthread
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

[ref_only]
struct PosixSemaphore {
	sem C.sem_t
}

pub struct Semaphore {
	sem &PosixSemaphore
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

pub fn new_semaphore() Semaphore {
	s := Semaphore{
		sem: &PosixSemaphore{}
	}
	C.sem_init(&s.sem.sem, 0, 0)
	return s
}

pub fn (s Semaphore) post() {
	C.sem_post(&s.sem.sem)
}

pub fn (s Semaphore) wait() {
	C.sem_wait(&s.sem.sem)
}
