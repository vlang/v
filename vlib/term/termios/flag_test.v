module termios

fn test_flag_returns_its_argument_as_a_termios_flag() {
	assert flag(0) == 0
	assert flag(1) == 1
	assert flag(123) == TcFlag(123)
	assert flag(0xFFFF) == TcFlag(0xFFFF)
}

fn test_invert_clears_the_bits_it_is_and_ed_with() {
	mut flags := flag(0xFF)
	flags &= invert(flag(0x0F))
	assert flags == flag(0xF0)
}

fn test_invert_is_its_own_inverse() {
	for v in [flag(0), flag(1), flag(0xFF), flag(0xFFFF)] {
		assert invert(invert(v)) == v
	}
}

fn test_termios_keeps_the_flag_fields() {
	t := Termios{
		c_iflag:  flag(1)
		c_oflag:  flag(2)
		c_cflag:  flag(4)
		c_lflag:  flag(8)
		c_line:   Cc(3)
		c_ispeed: Speed(9600)
		c_ospeed: Speed(9600)
	}
	assert t.c_iflag == flag(1)
	assert t.c_lflag == flag(8)
	assert t.c_line == Cc(3)
	assert t.c_ispeed == Speed(9600)
}

// NOTE: termios_windows.c.v is a stub, so nothing is implemented there.
$if windows {
	fn test_the_windows_stub_reports_that_termios_is_unimplemented() {
		mut t := Termios{}
		assert tcgetattr(0, mut t) == -1
		assert tcsetattr(0, 0, mut t) == -1
		assert ioctl(0, 0, unsafe { nil }) == -1
		assert set_state(0, t) == -1
	}

	fn test_disable_echo_is_a_no_op_on_windows() {
		mut t := Termios{}
		t.c_lflag = flag(0xFF)
		t.disable_echo()
		assert t.c_lflag == flag(0xFF)
	}
} $else {
	fn test_disable_echo_only_clears_bits() {
		mut t := Termios{}
		t.c_lflag = flag(0xFF)
		t.disable_echo()
		assert t.c_lflag != flag(0xFF)
		assert invert(t.c_lflag) & flag(0xFF) != 0
	}
}
