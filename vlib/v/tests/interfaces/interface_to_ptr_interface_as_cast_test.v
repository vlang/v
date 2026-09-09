// regression test for https://github.com/vlang/v/issues/25785
// `iface as &OtherIface` must produce a valid pointer to the converted interface.

pub interface Poolable {
mut:
	validate() !bool
	close() !
	reset() !
}

pub interface DbConn {
mut:
	query(q string) ![]string
	close() !
}

pub struct PgConn {
pub mut:
	tag string
}

pub fn (mut c PgConn) query(q string) ![]string {
	return [c.tag, q]
}

pub fn (mut c PgConn) close() ! {}

pub fn (mut c PgConn) validate() !bool {
	return true
}

pub fn (mut c PgConn) reset() ! {}

fn take_ptr(p &Poolable) string {
	mut conn := unsafe { p }
	return if conn.validate() or { false } { 'ok' } else { 'bad' }
}

fn iface_to_ptr_iface(c DbConn) string {
	return take_ptr(c as &Poolable)
}

fn test_iface_as_ptr_iface() {
	c := DbConn(PgConn{
		tag: 'pg'
	})
	assert iface_to_ptr_iface(c) == 'ok'
}
