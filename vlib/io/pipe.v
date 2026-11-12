module io

import sync

// PipeState is the shared state behind a PipeReader/PipeWriter pair.
struct PipeState {
mut:
	mu       &sync.Mutex = sync.new_mutex()
	write_mu &sync.Mutex = sync.new_mutex()
	cond     &sync.Cond  = unsafe { nil }
	buf      []u8
	rerr     ?IError
	werr     ?IError
}

// PipeReader is the read half of an in-memory pipe.
@[heap]
pub struct PipeReader {
mut:
	state &PipeState
}

// PipeWriter is the write half of an in-memory pipe.
@[heap]
pub struct PipeWriter {
mut:
	state &PipeState
}

// pipe creates a synchronous in-memory pipe.
// Each write blocks until one or more reads consume all of its bytes or either end closes.
// Concurrent reads, writes, and closes are safe. Separate writes never interleave.
pub fn pipe() (&PipeReader, &PipeWriter) {
	mut s := &PipeState{}
	s.cond = sync.new_cond(s.mu)
	r := &PipeReader{ state: s }
	w := &PipeWriter{ state: s }
	return r, w
}

// read waits for data or a close. A closed writer yields its first error, or Eof.
// A closed reader yields a pipe-closed error. Empty reads always return zero.
pub fn (mut r PipeReader) read(mut buf []u8) !int {
	if buf.len == 0 {
		return 0
	}
	r.state.mu.lock()
	defer { r.state.mu.unlock() }
	for r.state.buf.len == 0 && r.state.werr == none && r.state.rerr == none {
		r.state.cond.wait()
	}
	if r.state.rerr != none {
		return error('io: pipe closed')
	}
	if err := r.state.werr {
		return err
	}
	n := copy_bytes(mut buf, r.state.buf)
	r.state.buf = r.state.buf[n..]
	// Readers and the active writer share this condition. Wake all so that a
	// waiting reader cannot consume the writer's final notification.
	r.state.cond.broadcast()
	return n
}

// close closes the reader; subsequent writes return an error.
pub fn (mut r PipeReader) close() {
	r.close_with_error(error('io: pipe closed'))
}

// close_with_error closes the reader and wakes blocked operations.
// Writes receive err while the writer remains open. The first reader error is retained.
pub fn (mut r PipeReader) close_with_error(err IError) {
	r.state.mu.lock()
	defer { r.state.mu.unlock() }
	if r.state.rerr == none {
		r.state.rerr = err
	}
	r.state.cond.broadcast()
}

// write waits until readers consume every byte, returning the full count on success.
// A close may interrupt it after partial consumption; the error result has no byte count.
// Empty writes always return zero. The caller must not mutate buf until write returns.
pub fn (mut w PipeWriter) write(buf []u8) !int {
	if buf.len == 0 {
		return 0
	}
	w.state.write_mu.lock()
	defer { w.state.write_mu.unlock() }
	w.state.mu.lock()
	defer { w.state.mu.unlock() }
	if w.state.werr != none {
		return error('io: pipe closed')
	}
	if err := w.state.rerr {
		return err
	}
	// The write stays alive until this slice is consumed or discarded on close.
	w.state.buf = buf
	w.state.cond.broadcast()
	for w.state.buf.len > 0 && w.state.rerr == none && w.state.werr == none {
		w.state.cond.wait()
	}
	remaining := w.state.buf.len
	w.state.buf = []u8{}
	if remaining == 0 {
		return buf.len
	}
	if w.state.werr != none {
		return error('io: pipe closed')
	}
	if err := w.state.rerr {
		return err
	}
	return error('io: pipe closed')
}

// close closes the writer; subsequent reads return Eof.
pub fn (mut w PipeWriter) close() {
	w.close_with_error(Eof{})
}

// close_with_error closes the writer and wakes blocked operations.
// Reads receive err while the reader remains open. The first writer error is retained.
pub fn (mut w PipeWriter) close_with_error(err IError) {
	w.state.mu.lock()
	defer { w.state.mu.unlock() }
	if w.state.werr == none {
		w.state.werr = err
	}
	w.state.cond.broadcast()
}
