// Test that long flags accept their value both as `--name=value` and as the next argument `--name value`,
// like GNU's `getopt_long()` and GO's `flag` module do.
import flag

struct Options {
	sort_by   string @[long: sort; short: s]
	count     int
	verbose   bool     @[short: v]
	files     []string @[long: file]
	verbosity int      @[repeats; short: l]
	paths     []string @[tail]
}

const long_styles = [flag.Style.short_long, .long, .go_flag]

fn test_long_flag_with_value_as_next_arg() {
	for style in long_styles {
		opts, no_matches := flag.to_struct[Options](['--sort', 'name', '--count', '3', '--verbose',
			'--file', 'a', '--file=b', 'x', 'y'],
			style: style
		)!
		assert opts.sort_by == 'name', '${style}'
		assert opts.count == 3, '${style}'
		assert opts.verbose, '${style}'
		assert opts.files == ['a', 'b'], '${style}'
		assert opts.paths == ['x', 'y'], '${style}'
		assert no_matches == [], '${style}'
	}
}

fn test_long_flag_with_assigned_value() {
	for style in long_styles {
		opts, no_matches := flag.to_struct[Options](['--sort=name', '--count=3', 'x'],
			style: style
		)!
		assert opts.sort_by == 'name', '${style}'
		assert opts.count == 3, '${style}'
		assert opts.paths == ['x'], '${style}'
		assert no_matches == [], '${style}'
	}
}

fn test_long_flag_value_starting_with_delimiter() {
	for style in long_styles {
		opts, no_matches := flag.to_struct[Options](['--sort', '-name', '--count', '-3', '--verbose'],
			style: style
		)!
		assert opts.sort_by == '-name', '${style}'
		assert opts.count == -3, '${style}'
		assert opts.verbose, '${style}'
		assert no_matches == [], '${style}'
	}
}

fn test_long_flag_without_value() {
	for style in long_styles {
		if _, _ := flag.to_struct[Options](['--verbose', '--sort'], style: style) {
			assert false, 'flags should not have reached this assert'
		} else {
			assert err.msg() == 'flag `--sort` mapping to `sort_by` expects an argument. E.g.: `--sort value` or `--sort=value`'
		}
	}
}

fn test_long_flag_repeats_needs_assignment() {
	for style in long_styles {
		if _, _ := flag.to_struct[Options](['--verbosity', '3'], style: style) {
			assert false, 'flags should not have reached this assert'
		} else {
			assert err.msg() == 'field `verbosity` has @[repeats], only POSIX short style allows repeating'
		}
		opts, _ := flag.to_struct[Options](['--verbosity=3'], style: style)!
		assert opts.verbosity == 3
	}
}

fn test_short_long_single_dash_long_name() {
	// GO `flag` (and GNU `getopt_long_only()`) style: `-sort name` = `--sort name`, *not* `-s ort name`
	opts, no_matches := flag.to_struct[Options](['-sort', 'name', '-count=3', '-verbose', '-file',
		'a', '-lll'])!
	assert opts.sort_by == 'name'
	assert opts.count == 3
	assert opts.verbose
	assert opts.files == ['a']
	assert opts.verbosity == 3
	assert no_matches == []

	// POSIX short with sticky argument still works
	opts2, _ := flag.to_struct[Options](['-sname', '-vlll'])!
	assert opts2.sort_by == 'name'
	assert opts2.verbose
	assert opts2.verbosity == 3

	if _, _ := flag.to_struct[Options](['-sort', 'a', '-sort', 'b']) {
		assert false, 'flags should not have reached this assert'
	} else {
		assert err.msg() == 'flag `-sort b` is already mapped to field `sort_by` via `-sort a`'
	}
}

fn test_go_flag_single_dash_assignment() {
	opts, no_matches := flag.to_struct[Options](['-sort=name', '-count=3', '-file=a', '-file',
		'b', 'x'],
		style: .go_flag
	)!
	assert opts.sort_by == 'name'
	assert opts.count == 3
	assert opts.files == ['a', 'b']
	assert opts.paths == ['x']
	assert no_matches == []
}

fn test_short_cluster_with_unknown_flag() {
	if _, _ := flag.to_struct[Options](['-vxl']) {
		assert false, 'flags should not have reached this assert'
	} else {
		assert err.msg() == 'unknown flag `-x` in short flag cluster `-vxl`'
	}
	opts, no_matches := flag.to_struct[Options](['-vxl', '-l'], mode: .relaxed)!
	assert !opts.verbose
	assert opts.verbosity == 1
	assert no_matches == ['-vxl']
}
