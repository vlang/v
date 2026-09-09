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
	// closed records handles that close_idle force-closed on Windows. The woken
	// worker consults was_force_closed and relinquishes socket ownership (so its
	// conn.shutdown frees the TLS resources without closing the fd again), keeping
	// the fd closed exactly once. Empty on non-Windows (close_idle only shuts the
	// fd down there, leaving the worker the sole closer).
	closed []int
}

fn (mut t TlsIdleConnTracker) mark_idle(handle int) bool {
	t.mu.lock()
	defer {
		t.mu.unlock()
	}
	if t.closing {
		return false
	}
	// Dedup: the same handle can be marked twice (e.g. a worker marks it for the
	// handshake window and again per keep-alive request, or the OS recycles the
	// fd value). A duplicate would make close_idle shut down / close the same fd
	// twice within its own loop, so only track each handle once.
	if t.handles.index(handle) < 0 {
		t.handles << handle
	}
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

// was_force_closed reports whether close_idle has already closed `handle`
// (Windows force-close to wake a worker blocked in select). The woken worker
// must then relinquish socket ownership before its own conn.shutdown, which
// then frees the TLS resources without closing the fd again: a second close
// would race process-wide SOCKET reuse — the value can be reused by ANY socket
// the process opens, not just this server's accept loop — and could close an
// unrelated socket. Always false on non-Windows, where close_idle only shuts
// the fd down and the worker remains the sole closer.
fn (mut t TlsIdleConnTracker) was_force_closed(handle int) bool {
	$if windows {
		t.mu.lock()
		defer {
			t.mu.unlock()
		}
		return t.closed.index(handle) >= 0
	} $else {
		return false
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
	$if windows {
		// On Windows, net.shutdown(SD_BOTH) does not unblock a worker blocked in
		// select() — only closesocket() (net.close, below) does — so we must
		// force-close idle handles to wake their workers. Record ownership of
		// those closes here, under the lock, so a worker that wakes (or exits for
		// another reason) sees it via was_force_closed and relinquishes socket
		// ownership instead of closing the fd again. Without that the fd would be
		// closed twice, and between
		// the two closes the SOCKET value can be reused by any socket the process
		// opens, so the second close could hit an unrelated socket.
		t.closed << handles
	}
	// Hold the lock across the loop. On non-Windows, was_force_closed is always
	// false, so the worker remains the sole closer (via conn.shutdown); holding
	// the lock blocks its unmark_idle until our net.shutdown below has run, so
	// the worker cannot close the fd — and let the OS recycle the value — before
	// we shut it down. (Releasing the lock here would reopen exactly that race.)
	// On Windows the woken worker skips its close regardless, so the held lock
	// only briefly delays it and is otherwise harmless.
	for handle in handles {
		net.shutdown(handle)
		$if windows {
			net.close(handle) or {}
		}
	}
}
