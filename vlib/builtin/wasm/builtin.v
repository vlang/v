module builtin

fn JS.__panic_abort(&u8, int)
fn JS.__writeln(&u8, int)

pub fn panic(s string) {
	JS.__panic_abort(s.str, s.len)
}

pub fn println(s string) {
	JS.__writeln(s.str, s.len)
}