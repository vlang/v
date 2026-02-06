import flag

struct CliOptions {
	show_help bool @[long: 'help'; short: h]
}

struct CliOptions2 {
	show_help bool @[long: 'help'; short: h]
	long      string
}

fn test_v_style_short_tail_bool() {
	cli_options, unmatched := flag.to_struct[CliOptions](['some.exe', '-h'],
		skip:  1
		style: .v
		mode:  .relaxed
	)!

	if unmatched.len > 0 {
		assert false
	}
	if cli_options.show_help {
		assert true
	} else {
		assert false
	}
}

fn test_v_style_long_tail_bool() {
	cli_options, unmatched := flag.to_struct[CliOptions](['some.exe', '-help'],
		skip:  1
		style: .v
		mode:  .relaxed
	)!

	if unmatched.len > 0 {
		assert false
	}
	if cli_options.show_help {
		assert true
	} else {
		assert false
	}
}

fn test_v_style_short_bool() {
	cli_options, unmatched := flag.to_struct[CliOptions2](['some.exe', '-h', '-long', 'val'],
		skip:  1
		style: .v
		mode:  .relaxed
	)!

	if unmatched.len > 0 {
		assert false
	}
	if cli_options.show_help {
		assert true
	} else {
		assert false
	}
	assert cli_options.long == 'val'
}

fn test_v_style_long_bool() {
	cli_options, unmatched := flag.to_struct[CliOptions2](['some.exe', '-help', '-long', 'val'],
		skip:  1
		style: .v
		mode:  .relaxed
	)!

	if unmatched.len > 0 {
		assert false
	}
	if cli_options.show_help {
		assert true
	} else {
		assert false
	}
	assert cli_options.long == 'val'
}
