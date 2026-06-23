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
	defer {
		t.mu.unlock()
	}
	t.closing = true
	handles := t.handles.clone()
	t.handles.clear()
	// Shut down handles under the lock: a worker racing through
	// unmark_idle → conn.shutdown() → net.close(fd) could cause the OS
	// to reuse the fd before we call net.shutdown here, hitting an
	// unrelated socket. Holding the lock keeps unmark_idle serialized
	// with this loop, closing the window.
	// On Windows, net.shutdown(SD_BOTH) alone does not unblock a blocking
	// select() in another thread — only closesocket() does. We call
	// net.close here for that reason. The worker's deferred conn.shutdown()
	// will call closesocket again; that second call returns WSAENOTSOCK and
	// is swallowed by or {}. The double-close is safe: listen_and_serve_tls
	// calls ch.close() before close_idle(), so the accept loop has already
	// stopped; no new connection can claim the recycled handle before the
	// worker's conn.shutdown() runs.
	for handle in handles {
		net.shutdown(handle)
		$if windows {
			net.close(handle) or {}
		}
	}
}
