module builtin

fn JS.__panic_abort(&u8, int)
fn JS.__writeln(&u8, int)

// panic calls the `__panic_abort` JS panic handler.
@[noreturn]
pub fn panic(s string) {
	JS.__panic_abort(s.str, s.len)
	for {}
}

// println prints a message with a line end, to stdout. stdout is flushed.
pub fn println(s string) {
	JS.__writeln(s.str, s.len)
}
