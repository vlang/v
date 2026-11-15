// vtest build: gcc
// vtest vflags: -cc gcc -cstrict -cflags -Werror=pointer-sign -no-retry-compilation

fn test_wide_map_strings_compile_with_strict_pointer_signedness() {
	unsigned := {
		u128(1) << 100: i128(-7)
	}
	signed := {
		i128(-1) << 100: u128(1) << 100
	}
	assert unsigned.str() == '{1267650600228229401496703205376: -7}'
	assert signed.str() == '{-1267650600228229401496703205376: 1267650600228229401496703205376}'
}
