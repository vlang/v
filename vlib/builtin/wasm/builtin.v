module builtin

fn JS.__panic_abort(&u8, int)

pub fn panic(s string) {
	JS.__panic_abort(s.str, s.len)
}