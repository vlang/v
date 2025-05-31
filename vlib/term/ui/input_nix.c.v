// Copyright (c) 2020-2024 Raúl Hernández. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
@[has_globals]
module ui

struct ExtraContext {
mut:
	read_buf []u8
	// read_all_bytes causes all the raw bytes to be read as one event unit.
	// This is cruicial for UTF-8 support since Unicode codepoints can span several bytes.
	read_all_bytes bool = true
}

__global ctx_ptr = &Context(unsafe { nil })

// init initializes the terminal console with Config `cfg`.
pub fn init(cfg Config) &Context {
	mut ctx := &Context{
		cfg: cfg
	}
	ctx.read_buf = []u8{cap: cfg.buffer_size}

	ctx_ptr = ctx
	return ctx
}

@[inline]
fn save_title() {
	// restore the previously saved terminal title
	print('\x1b[22;0t')
	flush_stdout()
}

@[inline]
fn load_title() {
	// restore the previously saved terminal title
	print('\x1b[23;0t')
	flush_stdout()
}

// run sets up and starts the terminal.
pub fn (mut ctx Context) run() ! {
	if ctx.cfg.use_x11 {
		ctx.fail('error: x11 backend not implemented yet')
		exit(1)
	} else {
		ctx.termios_setup() or { panic(err) }
		ctx.termios_loop()
	}
}

// shifts the array left, to remove any data that was just read, and updates its len
// TODO: remove
@[inline]
fn (mut ctx Context) shift(len int) {
	unsafe {
		C.memmove(ctx.read_buf.data, &u8(ctx.read_buf.data) + len, ctx.read_buf.cap - len)
		ctx.resize_arr(ctx.read_buf.len - len)
	}
}

// TODO: don't actually do this, lmao
@[inline]
fn (mut ctx Context) resize_arr(size int) {
	mut l := unsafe { &ctx.read_buf.len }
	unsafe {
		*l = size
		_ = l
	}
}
