fn f(x u8) u8 {
	eprintln('> f: $x')
	if x == 0 {
		return 0
	}
	mut local := [131072]u8{}
	local[local.len - 1] = x + f(x - 1)
	return local[0] + local[local.len - 1]
}

fn abc(depth u8) u8 {
	return f(depth)
}

fn test_default_stack_depth() {
	$if tinyc && windows {
		exit(0) // skip for now testing on windows-tcc
	}
	// Note: 10 levels of recursing f, requires a little over 1.4MB,
	// and would have failed on macos, where the default thread size
	// is just 512KB, if V was not changed to have a default for
	// `-thread-stack-size` of 8MB.
	t := spawn abc(10)
	res := t.wait()
	assert res == 55
}
