// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module sync

struct Mutex {
}

fn (m Mutex) lock() {
panic('not implemented') 
}

fn (m Mutex) unlock() {
panic('not implemented') 
}

