// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net
import sync

@[heap]
struct TlsIdleConnTracker {
	mu &sync.Mutex = sync.new_mutex()
mut:
	handles []int
	closing bool
}

fn (mut t TlsIdleConnTracker) mark_idle(handle int) bool {
	t.mu.lock()
	defer {
		t.mu.unlock()
	}
	if t.closing {
		return false
	}
	t.handles << handle
	return true
}

fn (mut t TlsIdleConnTracker) unmark_idle(handle int) {
	t.mu.lock()
	defer {
		t.mu.unlock()
	}
	idx := t.handles.index(handle)
	if idx >= 0 {
		t.handles.delete(idx)
	}
}

fn (mut t TlsIdleConnTracker) close_idle() {
	t.mu.lock()
	t.closing = true
	handles := t.handles.clone()
	t.handles.clear()
	t.mu.unlock()
	for handle in handles {
		net.shutdown(handle)
		$if windows {
			net.close(handle) or {}
		}
	}
}
