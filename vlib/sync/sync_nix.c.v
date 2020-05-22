// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module sync

#flag -lpthread

// [init_with=new_mutex] // TODO: implement support for this struct attribute, and disallow Mutex{} from outside the sync.new_mutex() function.
[ref_only]
pub struct Mutex {
	mutex C.pthread_mutex_t
}

pub fn new_mutex() &Mutex {
	m := &Mutex{}
	C.pthread_mutex_init(&m.mutex, C.NULL)
	return m
}

pub fn (mut m Mutex) lock() {
	C.pthread_mutex_lock(&m.mutex)
}

pub fn (mut m Mutex) unlock() {
	C.pthread_mutex_unlock(&m.mutex)
}
