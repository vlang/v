// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module sync

struct WaitGroup {
mut:
    mu Mutex
    finished Mutex
    active int
}

pub fn (wg mut WaitGroup) add(delta int) {
    wg.mu.lock()
    if wg.active == 0 {
        wg.finished.lock()
    }
    wg.active += delta
    if wg.active < 0 {
        panic('Negative number of jobs in waitgroup')
    }
    if wg.active == 0 {
        wg.finished.unlock()
    }
    wg.mu.unlock()
}

pub fn (wg mut WaitGroup) done() {
    wg.add(-1)
}

pub fn (wg mut WaitGroup) wait() {
    wg.finished.lock()
    wg.finished.unlock()
}

