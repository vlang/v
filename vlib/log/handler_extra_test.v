module log

import io
import time

struct ExtraCapture {
mut:
	buf []u8
}

fn (mut c ExtraCapture) write(data []u8) !int {
	c.buf << data
	return data.len
}

fn iattr(k string, v i64) Attr {
	return Attr{ key: k, value: v }
}

fn test_sampled_handler_every_third() {
	mut cap := &ExtraCapture{}
	inner := new_text_handler(io.Writer(cap), HandlerOptions{ color: false })
	sampled := new_sampled_handler(inner, 3)
	l := new_logger(sampled)
	for i in 0 .. 9 {
		l.info('tick')
	}
	lines := cap.buf.bytestr().trim_space().split('\n')
	assert lines.len == 3
}

fn test_async_handler_flushes_on_close() {
	mut cap := &ExtraCapture{}
	inner := new_text_handler(io.Writer(cap), HandlerOptions{ color: false })
	async_h := new_async_handler(inner, 64)
	l := new_logger(async_h)
	for i in 0 .. 50 {
		l.info('async', iattr('i', i))
	}
	async_h.close()
	lines := cap.buf.bytestr().trim_space().split('\n')
	assert lines.len == 50
	// close is idempotent
	async_h.close()
}

fn test_async_handler_close_is_quick() {
	mut cap := &ExtraCapture{}
	async_h := new_async_handler(new_discard_handler(), 8)
	l := new_logger(async_h)
	for i in 0 .. 20 {
		l.info('x')
	}
	start := time.now()
	async_h.close()
	assert time.since(start) < 2 * time.second
	assert cap.buf.len == 0
}
