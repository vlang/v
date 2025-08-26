import flag

const args_bool_short = ['some.exe', '-h']
const args_bool_long = ['some.exe', '-help']

struct CliOptions {
	show_help bool @[long: 'help'; short: h]
}

fn test_short_tail_bool() {
	cli_options, unmatched := flag.to_struct[CliOptions](args_bool_short,
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

fn test_long_tail_bool() {
	cli_options, unmatched := flag.to_struct[CliOptions](args_bool_long,
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
