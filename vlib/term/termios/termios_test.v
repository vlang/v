module termios

fn test_portable() {
	assert 123 == int(flag(123))
	o := Termios{
		c_lflag: flag(0xFFFF)
	} // assume c_lflag exists everywhere
	// dump( o.c_lflag )
	mut n := o
	n.c_lflag &= invert(1)
	// dump( n.c_lflag )
	assert n.c_lflag != o.c_lflag
	n.disable_echo() // just assume it exists, and can be called everywhere
	assert true
}

@[if !windows]
fn test_termios() {
	mut original_term := Termios{}
	tcgetattr(0, mut original_term)
	println(original_term)

	mut silent_term := original_term
	silent_term.c_lflag &= invert(C.ECHO)
	tcsetattr(0, C.TCSANOW, mut silent_term)

	mut check_term := Termios{}
	tcgetattr(0, mut check_term)
	assert check_term.c_lflag == silent_term.c_lflag

	tcsetattr(0, C.TCSANOW, mut original_term)

	tcgetattr(0, mut check_term)
	assert check_term.c_lflag == original_term.c_lflag
}
