// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module sync

#include <pthread.h>
pub struct Mutex {
	mutex C.pthread_mutex_t
}

pub fn (m mut Mutex) lock() {
	C.pthread_mutex_lock(&m.mutex)
}

pub fn (m mut Mutex) unlock() {
	C.pthread_mutex_unlock(&m.mutex)
}

