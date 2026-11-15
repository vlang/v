module io

import time

fn test_pipe_roundtrip() {
	r, w := pipe()
	spawn fn (w &PipeWriter) {
		w.write('hello'.bytes()) or {}
		w.close()
	}(w)
	data := read_all(ReadAllConfig{ reader: r, read_to_end_of_stream: true }) or { panic(err) }
	assert data.bytestr() == 'hello'
	r.close()
}

fn test_pipe_write_after_reader_close() {
	r, w := pipe()
	r.close()
	if _ := w.write('x'.bytes()) {
		assert false
	} else {
		assert true
	}
}

fn test_pipe_close_with_error() {
	r, w := pipe()
	spawn fn (w &PipeWriter) {
		w.close_with_error(error('boom'))
	}(w)
	mut buf := []u8{len: 10}
	if _ := r.read(mut buf) {
		assert false
	} else {
		assert err.msg() == 'boom'
	}
}

struct PipeWriteResult {
	count   int
	message string
}

fn pipe_test_write(w &PipeWriter, data []u8, started chan bool, done chan PipeWriteResult) {
	started <- true
	n := w.write(data) or {
		done <- PipeWriteResult{ message: err.msg() }
		return
	}
	done <- PipeWriteResult{ count: n }
}

fn pipe_test_wait_write(done chan PipeWriteResult) PipeWriteResult {
	select {
		result := <-done {
			return result
		}
		5 * time.second {
			panic('pipe writer did not finish')
		}
	}
	return PipeWriteResult{}
}

fn pipe_test_expect_waiting(done chan PipeWriteResult) {
	select {
		result := <-done {
			assert false, 'write returned before its bytes were read: ${result}'
		}
		20 * time.millisecond {
		}
	}
}

fn test_pipe_write_waits_for_every_byte() {
	r, w := pipe()
	defer { r.close() }
	started := chan bool{cap: 1}
	done := chan PipeWriteResult{cap: 1}
	spawn pipe_test_write(w, 'abcdef'.bytes(), started, done)
	_ = <-started
	pipe_test_expect_waiting(done)
	mut prefix := []u8{len: 2}
	assert r.read(mut prefix)! == 2
	assert prefix.bytestr() == 'ab'
	pipe_test_expect_waiting(done)
	mut suffix := []u8{len: 8}
	assert r.read(mut suffix)! == 4
	assert suffix[..4].bytestr() == 'cdef'
	assert pipe_test_wait_write(done) == PipeWriteResult{ count: 6 }
}

fn test_pipe_reader_first_close_error_reaches_writers() {
	r, w := pipe()
	r.close_with_error(error('first reader error'))
	r.close_with_error(error('second reader error'))
	r.close()
	if _ := w.write('x'.bytes()) {
		assert false
	} else {
		assert err.msg() == 'first reader error'
	}
}

fn test_pipe_writer_first_close_error_reaches_readers() {
	r, w := pipe()
	w.close_with_error(error('first writer error'))
	w.close_with_error(error('second writer error'))
	w.close()
	mut buf := []u8{len: 1}
	if _ := r.read(mut buf) {
		assert false
	} else {
		assert err.msg() == 'first writer error'
	}
}

fn test_pipe_writer_close_preserves_eof() {
	r, w := pipe()
	w.close()
	w.close_with_error(error('later'))
	mut buf := []u8{len: 1}
	if _ := r.read(mut buf) {
		assert false
	} else {
		assert err is Eof
	}
}

fn test_pipe_reader_close_releases_active_and_queued_writers() {
	r, w := pipe()
	started := chan bool{cap: 2}
	done := chan PipeWriteResult{cap: 2}
	spawn pipe_test_write(w, 'abcd'.bytes(), started, done)
	_ = <-started
	mut prefix := []u8{len: 1}
	assert r.read(mut prefix)! == 1
	assert prefix[0] == `a`
	spawn pipe_test_write(w, 'ef'.bytes(), started, done)
	_ = <-started
	r.close_with_error(error('reader stopped'))
	for _ in 0 .. 2 {
		assert pipe_test_wait_write(done).message == 'reader stopped'
	}
	if _ := r.read(mut prefix) {
		assert false, 'closed reader returned buffered data'
	} else {
		assert err.msg() == 'io: pipe closed'
	}
}

fn test_pipe_writer_close_releases_partial_write() {
	r, w := pipe()
	started := chan bool{cap: 1}
	done := chan PipeWriteResult{cap: 1}
	spawn pipe_test_write(w, 'abcd'.bytes(), started, done)
	_ = <-started
	mut prefix := []u8{len: 1}
	assert r.read(mut prefix)! == 1
	w.close_with_error(error('writer stopped'))
	assert pipe_test_wait_write(done).message == 'io: pipe closed'
	if _ := r.read(mut prefix) {
		assert false
	} else {
		assert err.msg() == 'writer stopped'
	}
}

fn pipe_test_read_until_close(r &PipeReader, started chan bool, done chan string) {
	started <- true
	mut buf := []u8{len: 1}
	if _ := r.read(mut buf) {
		done <- 'unexpected data'
	} else {
		done <- err.msg()
	}
}

fn test_pipe_close_wakes_all_readers() {
	for close_reader in [false, true] {
		r, w := pipe()
		started := chan bool{cap: 2}
		done := chan string{cap: 2}
		for _ in 0 .. 2 {
			spawn pipe_test_read_until_close(r, started, done)
		}
		for _ in 0 .. 2 {
			_ = <-started
		}
		if close_reader {
			r.close()
		} else {
			w.close_with_error(error('writer stopped'))
		}
		for _ in 0 .. 2 {
			select {
				message := <-done {
					assert message == if close_reader {
						'io: pipe closed'
					} else {
						'writer stopped'
					}
				}
				5 * time.second {
					panic('pipe reader did not finish')
				}
			}
		}
	}
}

fn test_pipe_parallel_writes_do_not_interleave() {
	r, w := pipe()
	defer { r.close() }
	started := chan bool{cap: 2}
	done := chan PipeWriteResult{cap: 2}
	spawn pipe_test_write(w, 'ABCD'.bytes(), started, done)
	spawn pipe_test_write(w, 'wxyz'.bytes(), started, done)
	mut all := []u8{}
	mut byte := []u8{len: 1}
	for _ in 0 .. 8 {
		assert r.read(mut byte)! == 1
		all << byte[0]
	}
	assert all.bytestr() in ['ABCDwxyz', 'wxyzABCD']
	for _ in 0 .. 2 {
		assert pipe_test_wait_write(done) == PipeWriteResult{ count: 4 }
	}
}

fn test_pipe_empty_operations_are_noops() {
	r, w := pipe()
	mut empty := []u8{}
	assert r.read(mut empty)! == 0
	assert w.write(empty)! == 0
	r.close()
	w.close()
	assert r.read(mut empty)! == 0
	assert w.write(empty)! == 0
}
