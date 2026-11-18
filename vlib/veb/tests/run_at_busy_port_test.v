// vtest build: !windows // SO_REUSEADDR lets two listeners share a port on windows, so the bind error cannot be triggered there
import net
import veb

pub struct Context {
	veb.Context
}

pub struct App {}

pub fn (mut app App) index(mut ctx Context) veb.Result {
	return ctx.text('hi')
}

// test_run_at_returns_error_when_port_is_busy ensures a busy port is reported
// as an error from run_at, instead of panicking the application, so callers
// can report it themselves or try another port.
// See https://github.com/vlang/v/issues/32337
fn test_run_at_returns_error_when_port_is_busy() {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0') or { panic(err) }
	defer {
		listener.close() or {}
	}
	port := int(listener.addr()!.port()!)
	mut app := App{}
	veb.run_at[App, Context](mut app,
		host:                 '127.0.0.1'
		port:                 port
		family:               .ip
		show_startup_message: false
	) or {
		assert err.msg().contains('bind'), err.msg()
		return
	}
	assert false, 'veb.run_at should have returned an error for busy port ${port}'
}
