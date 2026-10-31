module io

import sync

// PipeState is the shared state behind a PipeReader/PipeWriter pair.
struct PipeState {
mut:
	mu   sync.Mutex
	cond &sync.Cond = unsafe { nil }
	buf  []u8
	rerr ?IError
	werr ?IError
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

// pipe creates a synchronous in-memory pipe, similar to Go's io.Pipe.
// Reads block until data is written; writes block until data is read.
pub fn pipe() (&PipeReader, &PipeWriter) {
	mut s := &PipeState{}
	s.cond = sync.new_cond(&s.mu)
	r := &PipeReader{ state: s }
	w := &PipeWriter{ state: s }
	return r, w
}

// read implements Reader for PipeReader.
pub fn (mut r PipeReader) read(mut buf []u8) !int {
	if buf.len == 0 {
		return 0
	}
	r.state.mu.lock()
	for r.state.buf.len == 0 && r.state.werr == none && r.state.rerr == none {
		r.state.cond.wait()
	}
	if r.state.buf.len > 0 {
		n := if buf.len < r.state.buf.len { buf.len } else { r.state.buf.len }
		for i in 0 .. n {
			buf[i] = r.state.buf[i]
		}
		r.state.buf = r.state.buf[n..]
		r.state.cond.signal()
		r.state.mu.unlock()
		return n
	}
	if r.state.rerr != none {
		err := r.state.rerr
		r.state.mu.unlock()
		return err
	}
	if r.state.werr != none {
		err := r.state.werr
		r.state.mu.unlock()
		return err
	}
	r.state.mu.unlock()
	return Eof{}
}

// close closes the reader; subsequent writes return an error.
pub fn (mut r PipeReader) close() {
	r.state.mu.lock()
	r.state.rerr = error('io: pipe closed')
	r.state.cond.broadcast()
	r.state.mu.unlock()
}

// close_with_error closes the reader with err.
pub fn (mut r PipeReader) close_with_error(err IError) {
	r.state.mu.lock()
	r.state.rerr = err
	r.state.cond.broadcast()
	r.state.mu.unlock()
}

// write implements Writer for PipeWriter.
pub fn (mut w PipeWriter) write(buf []u8) !int {
	if buf.len == 0 {
		return 0
	}
	w.state.mu.lock()
	if w.state.rerr != none {
		w.state.mu.unlock()
		return error('io: pipe closed')
	}
	if w.state.werr != none {
		w.state.mu.unlock()
		return error('io: pipe closed')
	}
	w.state.buf << buf
	w.state.cond.broadcast()
	w.state.mu.unlock()
	return buf.len
}

// close closes the writer; subsequent reads return Eof.
pub fn (mut w PipeWriter) close() {
	w.state.mu.lock()
	w.state.werr = Eof{}
	w.state.cond.broadcast()
	w.state.mu.unlock()
}

// close_with_error closes the writer, causing reads to return err.
pub fn (mut w PipeWriter) close_with_error(err IError) {
	w.state.mu.lock()
	w.state.werr = err
	w.state.cond.broadcast()
	w.state.mu.unlock()
}
