module termios

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
