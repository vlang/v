import flag

const args_bool_short_tail = ['some.exe', '-h']
const args_bool_long_tail = ['some.exe', '-help']

const args_bool_short_mixed = ['some.exe', '-h', '-long', 'val']
const args_bool_long_mixed = ['some.exe', '-help', '-long', 'val']

struct CliOptions {
	show_help bool @[long: 'help'; short: h]
}

struct CliOptions2 {
	show_help bool @[long: 'help'; short: h]
	long      string
}

fn test_v_style_short_tail_bool() {
	cli_options, unmatched := flag.to_struct[CliOptions](args_bool_short_tail,
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
	cli_options, unmatched := flag.to_struct[CliOptions](args_bool_long_tail,
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
	cli_options, unmatched := flag.to_struct[CliOptions2](args_bool_short_mixed,
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
	cli_options, unmatched := flag.to_struct[CliOptions2](args_bool_long_mixed,
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
