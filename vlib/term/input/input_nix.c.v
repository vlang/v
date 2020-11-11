module input

const (
	ctx_ptr = &Context(0)
)

pub fn init(cfg Config) &Context {
	mut ctx := &Context{
		cfg: cfg,
		read_buf: []byte{ cap: cfg.buffer_size }
	}

	if cfg.hide_cursor {
		s := '\x1b[?25l'
		C.write(C.STDOUT_FILENO, s.str, s.len)
	}

	unsafe {
		x := &ctx_ptr
		*x = ctx
	}

	return ctx
}

pub fn (mut ctx Context) run() {
	if ctx.cfg.use_x11 {
		ctx.fail('error: x11 backend not implemented yet')
		exit(1)
	} else {
		ctx.termios_setup()
		ctx.termios_loop()
	}
}

[inline]
// shifts the array left, to remove any data that was just read, and updates its len
// TODO: remove
fn (mut ctx Context) shift(len int) {
	unsafe {
		C.memmove(ctx.read_buf.data, ctx.read_buf.data + len, ctx.read_buf.cap - len)
		ctx.resize_arr(ctx.read_buf.len - len)
	}
}

// TODO: don't actually do this, lmao
[inline]
fn (mut ctx Context) resize_arr(size int) {
	mut l := &ctx.read_buf.len
	unsafe { *l = size }
}

[inline]
fn (ctx &Context) init() {
	if ctx.cfg.init_fn != voidptr(0) {
		ctx.cfg.init_fn(ctx.cfg.user_data)
	}
}

[inline]
fn (ctx &Context) frame() {
	if ctx.cfg.frame_fn != voidptr(0) {
		ctx.cfg.frame_fn(ctx.cfg.user_data)
	}
}

[inline]
fn (ctx &Context) cleanup() {
	if ctx.cfg.cleanup_fn != voidptr(0) {
		ctx.cfg.cleanup_fn(ctx.cfg.user_data)
	}
}

[inline]
fn (ctx &Context) fail(error string) {
	if ctx.cfg.fail_fn != voidptr(0) {
		ctx.cfg.fail_fn(error)
	}
}

[inline]
fn (ctx &Context) event(event &Event) {
	if ctx.cfg.event_fn != voidptr(0) {
		ctx.cfg.event_fn(event, ctx.cfg.user_data)
	}
}
