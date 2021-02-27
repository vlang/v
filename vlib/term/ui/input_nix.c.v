// Copyright (c) 2020-2021 Raúl Hernández. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ui


struct ExtraContext {
mut:
	read_buf []byte
}

const (
	ctx_ptr = &Context(0)
)

pub fn init(cfg Config) &Context {
	mut ctx := &Context{
		cfg: cfg,
	}
	ctx.read_buf = []byte{ cap: cfg.buffer_size }

	// lmao
	unsafe {
		x := &ctx_ptr
		*x = ctx
	}

	return ctx
}

[inline]
fn save_title() {
    // restore the previously saved terminal title
    print('\x1b[22;0t')
}

[inline]
fn load_title() {
    // restore the previously saved terminal title
    print('\x1b[23;0t')
}

pub fn (mut ctx Context) run() ? {
	if ctx.cfg.use_x11 {
		ctx.fail('error: x11 backend not implemented yet')
		exit(1)
	} else {
		ctx.termios_setup()?
		ctx.termios_loop()
	}
}

[inline]
// shifts the array left, to remove any data that was just read, and updates its len
// TODO: remove
fn (mut ctx Context) shift(len int) {
	unsafe {
		C.memmove(ctx.read_buf.data, byteptr(ctx.read_buf.data) + len, ctx.read_buf.cap - len)
		ctx.resize_arr(ctx.read_buf.len - len)
	}
}

// TODO: don't actually do this, lmao
[inline]
fn (mut ctx Context) resize_arr(size int) {
	mut l := &ctx.read_buf.len
	unsafe { *l = size }
}
