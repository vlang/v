module builtin

// input_character gives back a single character, read from the standard input.
// It returns -1 on error (when the input is finished (EOF), on a broken pipe etc).
pub fn input_character() int {
	mut ch := 0
	$if freestanding {
		// TODO
		return -1
	} $else $if vinix {
		// TODO
		return -1
	} $else {
		ch = C.getchar()
		if ch == C.EOF {
			return -1
		}
	}
	return ch
}

// print_character writes the single character `ch` to the standard output.
// It returns -1 on error (when the output is closed, on a broken pipe, etc).
// Note: this function does not allocate memory, unlike `print(ch.ascii_str())`
// which does, and is thus cheaper to call, which is important, if you have
// to output many characters one by one. If you instead want to print entire
// strings at once, use `print(your_string)`.
pub fn print_character(ch u8) int {
	$if android && !termux {
		C.android_print(C.stdout, c'%.*s', 1, voidptr(&ch))
	} $else $if freestanding {
		bare_print(voidptr(&ch), u64(1))
	} $else $if vinix {
		// TODO
		return 0
	} $else {
		x := C.putchar(ch)
		if x == C.EOF {
			return -1
		}
	}
	return ch
}
