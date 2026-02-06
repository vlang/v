import flag

const all_style_enums = [flag.Style.short, .short_long, .long, .v, .v_flag_parser]
const posix_gnu_style_enums = [flag.Style.short, .short_long, .long]
const mixed_args = ['/path/to/exe', '-vv', 'vvv', '-version', '--mix', '--mix-all=all', '-ldflags',
	'-m', '2', '-fgh', '["test", "test"]', '-m', '{map: 2, ml-q:"hello"}']

const posix_and_gnu_args = ['-vv', 'vvv', '-mwindows', '-d', 'one', '--device=two', '--amount=8',
	'-d', 'three']

const posix_and_gnu_args_with_subcmd = ['/path/to/exe', 'subcmd', '-vv', 'vvv', '-mwindows', '-d',
	'one', '--device=two', '--amount=8', '-d', 'three']

const posix_and_gnu_args_with_paths = ['/path/to/exe', '-vv', 'vvv', '-mwindows', '-d', 'one',
	'--device=two', '--amount=8', '-d', 'three', '/path/to/a', '/path/to/b']

const posix_args_error = ['/path/to/exe', '-vv', 'vvv', '-mwindows', '-m', 'gnu']
const gnu_args = ['--f=10.2', '--mix', '--test=test', '--amount=5', '--version', 'other']
const gnu_args_error = ['--f=10.2', '--mix', '--test=test', '--amount=5', '--version', 'other',
	'oo']
const ignore_args_error = ['--show-version', '--some-test=ouch', '--amount=5', 'end']

struct Config {
	mix           bool
	linker_option string   @[only: m]
	mix_hard      bool     @[json: muh] // Test that no other attributes get picked up
	def_test      string = 'def'   @[long: test; short: t]
	device        []string @[short: d]
	paths         []string @[tail]
	amount        int = 1
	verbosity     int  @[repeats; short: v]
	show_version  bool @[long: version]
	no_long_beef  bool @[only: n]
}

struct LongConfig {
	f            f32
	mix          bool
	some_test    string = 'abc' @[long: test]
	path         string @[tail]
	amount       int = 1
	show_version bool @[long: version]
}

struct IgnoreConfig {
	some_test    string = 'abc' @[ignore]
	path         string @[tail]
	amount       int = 1
	show_version bool
}

fn test_flags() {
	// Test .short_long parse style
	config1, _ := flag.to_struct[Config](posix_and_gnu_args_with_subcmd, skip: 1)!
	assert config1.mix == false
	assert config1.verbosity == 5
	assert config1.amount == 8
	assert config1.def_test == 'def'
	assert 'one' in config1.device
	assert 'two' in config1.device
	assert 'three' in config1.device
	assert config1.linker_option == 'windows'

	config2, _ := flag.to_struct[Config](posix_and_gnu_args)!
	assert config2.mix == false
	assert config2.verbosity == 5
	assert config2.amount == 8
	assert config2.def_test == 'def'
	assert 'one' in config2.device
	assert 'two' in config2.device
	assert 'three' in config2.device
	assert config2.device.len == 3
	assert config2.linker_option == 'windows'

	mut posix_and_gnu_args_plus_test := posix_and_gnu_args.clone()
	posix_and_gnu_args_plus_test << ['--test=ok', '-d', 'four']
	config3, _ := flag.to_struct[Config](posix_and_gnu_args_plus_test)!
	assert config3.mix == false
	assert config3.verbosity == 5
	assert config3.amount == 8
	assert config3.def_test == 'ok'
	assert config3.device.len == 4
	assert 'one' == config3.device[0]
	assert 'two' == config3.device[1]
	assert 'three' == config3.device[2]
	assert 'four' == config3.device[3]
	assert config3.linker_option == 'windows'

	config4, _ := flag.to_struct[Config](posix_and_gnu_args_with_paths, skip: 1)!
	assert config4.verbosity == 5
	assert config4.amount == 8
	assert config4.def_test == 'def'
	assert config4.device.len == 3
	assert 'one' == config4.device[0]
	assert 'two' == config4.device[1]
	assert 'three' == config4.device[2]
	assert config4.linker_option == 'windows'
	assert config4.paths.len == 2
	assert config4.paths[0] == '/path/to/a'
	assert config4.paths[1] == '/path/to/b'
}

fn test_long_flags() {
	// Test .long parse style
	lc1, _ := flag.to_struct[LongConfig](gnu_args, style: .long)!
	assert lc1.f == 10.2
	assert lc1.mix == true
	assert lc1.some_test == 'test'
	assert lc1.path == 'other'
	assert lc1.amount == 5
	assert lc1.show_version == true
}

fn test_flag_error_messages() {
	// Test error for GNU long flag in .short (Posix) mode
	if _, _ := flag.to_struct[Config](posix_and_gnu_args_with_subcmd,
		skip:  1
		style: .short
	)
	{
		assert false, 'flags should not have reached this assert'
	} else {
		assert err.msg() == 'long delimiter `--` encountered in flag `--device=two` in short (POSIX) style parsing mode'
	}

	// Test double mapping of flags
	if _, _ := flag.to_struct[Config](posix_args_error, skip: 1) {
		assert false, 'flags should not have reached this assert'
	} else {
		assert err.msg() == 'flag `-m gnu` is already mapped to field `linker_option` via `-m windows`'
	}

	for e_num in all_style_enums {
		// Test no match for non-flag as first arg (usually the `/path/to/executable`) - which must be skipped with `.skip`
		if _, no_matches := flag.to_struct[Config](posix_and_gnu_args_with_subcmd,
			style: e_num
		)
		{
			assert no_matches == ['/path/to/exe', 'subcmd'] // index 0 = executable, index 1 = subcmd
		}
	}

	for e_num in posix_gnu_style_enums {
		if _, _ := flag.to_struct[Config](mixed_args, skip: 1, style: e_num) {
			assert false, 'flags should not have reached this assert'
		} else {
			if e_num == .short {
				assert err.msg() == 'long delimiter `--` encountered in flag `--mix` in short (POSIX) style parsing mode'
			} else if e_num == .long {
				assert err.msg() == 'short delimiter `-` encountered in flag `-vv` in long (GNU) style parsing mode'
			} else {
				assert err.msg() == 'flag `-m {map: 2, ml-q:"hello"}` is already mapped to field `linker_option` via `-m 2`'
			}
			assert true
		}
	}
	if _, no_matches := flag.to_struct[LongConfig](gnu_args_error, style: .long) {
		assert no_matches == ['oo']
	} else {
		assert false, 'flags should not have reached this assert'
	}

	if _, _ := flag.to_struct[LongConfig](['--version=1.2.3'], style: .long) {
		assert false, 'flags should not have reached this assert'
	} else {
		assert err.msg() == 'flag `--version=1.2.3` can not be assigned to bool field "show_version"'
	}
	if _, no_matches := flag.to_struct[IgnoreConfig](ignore_args_error, style: .long) {
		assert no_matches == ['--some-test=ouch']
	}
}

fn test_flag_no_error_messages_when_relaxed() {
	// Test that there's no errors in `mode: .relaxed`
	_, no_matches1 := flag.to_struct[Config](posix_and_gnu_args_with_subcmd,
		skip:  1
		style: .short
		mode:  .relaxed
	)!
	assert no_matches1 == ['subcmd', '--device=two', '--amount=8']

	_, no_matches2 := flag.to_struct[LongConfig](gnu_args_error,
		style: .long
		mode:  .relaxed
	)!
	assert no_matches2 == ['oo']

	if _, _ := flag.to_struct[LongConfig](['--version=1.2.3'],
		style: .long
		mode:  .relaxed
	)
	{
		assert false, 'flags should not have reached this assert'
	} else {
		// Type errors should not be ignored, only non-matching flags/args
		assert err.msg() == 'flag `--version=1.2.3` can not be assigned to bool field "show_version"'
	}

	_, no_matches3 := flag.to_struct[IgnoreConfig](ignore_args_error,
		style: .long
		mode:  .relaxed
	)!
	assert no_matches3 == ['--some-test=ouch']
}
