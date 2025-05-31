// vtest build: !(windows && gcc) // it declares its own `main` function, but on windows && gcc, it needs to be `wWinMain`
module no_main

// Compile with:
// v -cc clang -prod -gc none -cflags '-s -fuse-ld=mold' -o hw examples/minimal_c_like_program_using_puts.v && ll hw && sstrip -z hw && ll hw
// With clang-10, on Ubuntu 20.04, it produces an executable with a size of 2331 bytes, after stripping.
// Note: the above uses sstrip 2.1 (from https://git.sr.ht/~breadbox/ELFkickers) and mold 2.32.0 (from https://github.com/rui314/mold).

fn C.puts(const_msg &char) int

@[export: 'main']
fn hello() int {
	C.puts(c'Hello world')
	return 0
}
