// AsyncHandler moves formatting and I/O to a worker thread.
//
// Records are queued and written by a single worker, so the caller never blocks
// on the underlying handler. This makes records lossy on abrupt process exit:
// call close() to flush the queue before shutting down.
module log

import context
import sync

// AsyncHandler queues records for a background writer.
pub struct AsyncHandler {
	inner    Handler
	queue    chan Record
	stop     chan int
	ctx      context.Context
	capacity int
mut:
	mu     &sync.Mutex = sync.new_mutex()
	wg     &sync.WaitGroup
	closed bool
}

// new_async_handler starts a worker that writes to `inner`.
pub fn new_async_handler(inner Handler, capacity int) &AsyncHandler {
	h := &AsyncHandler{
		inner:    inner
		queue:    chan Record{cap: capacity}
		stop:     chan int{}
		ctx:      context.background()
		capacity: capacity
		wg:       sync.new_waitgroup()
	}
	h.wg.add(1)
	spawn async_worker(h)
	return h
}

fn async_worker(h &AsyncHandler) {
	defer { h.wg.done() }
	for {
		select {
			r := <-h.queue {
				mut rr := r
				h.inner.handle(h.ctx, mut rr) or {}
			}
			_ := <-h.stop {
				for {
					select {
						last := <-h.queue {
							mut rr := last
							h.inner.handle(h.ctx, mut rr) or {}
						}
						else {
							return
						}
					}
				}
			}
		}
	}
}

pub fn (h &AsyncHandler) enabled(level Level) bool {
	return h.inner.enabled(level)
}

pub fn (h &AsyncHandler) handle(_ context.Context, mut r Record) ! {
	mut self := unsafe { h }
	self.mu.lock()
	closed := self.closed
	self.mu.unlock()
	if closed {
		return
	}
	h.queue <- r
}

// close stops the worker after draining the queue. It is safe to call more
// than once.
pub fn (h &AsyncHandler) close() {
	mut self := unsafe { h }
	self.mu.lock()
	defer { self.mu.unlock() }
	if self.closed {
		return
	}
	self.closed = true
	h.stop <- 1
	h.wg.wait()
}

pub fn (h &AsyncHandler) with_attrs(attrs []Attr) Handler {
	return new_async_handler(h.inner.with_attrs(attrs), h.capacity)
}

pub fn (h &AsyncHandler) with_group(name string) Handler {
	return new_async_handler(h.inner.with_group(name), h.capacity)
}

pub fn (h &AsyncHandler) set_level(level Level) {
	h.inner.set_level(level)
}
