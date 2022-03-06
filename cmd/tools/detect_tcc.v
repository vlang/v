fn main() {
	$if tinyc {
		println('Your `tcc` is working. Good - it is much faster at compiling C source code.')
		exit(0)
	}

	println('
Note: `tcc` was not used, so unless you install it yourself, your backend
C compiler will be `cc`, which is usually either `clang`, `gcc` or `msvc`.

These C compilers, are several times slower at compiling C source code,
compared to `tcc`. They do produce more optimised executables, but that
is done at the cost of compilation speed.
')
}
