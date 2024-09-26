// Test .short (POSIX) parse style
import flag

const exe_and_posix_args = ['/path/to/exe', '-vv', 'vvv', '-mwindows', '-t', 'abc', '-done', '-d',
	'two', '-dthree', '-xyz', '2']

const exe_and_posix_args_with_tail = ['/path/to/exe', '-vvv', 'vvv', '-t', 'abc', '-done', '-d',
	'two', '-dthree', '/path/to/x', '/path/to/y', '/path/to/z']

const posix_multi_short_args_1 = ['-vv', 'vvv', '-done', '-d', 'two', '-yxz2']
const posix_multi_short_args_2 = ['-vv', 'vvv', '-done', '-d', 'two', '-yxz', '2']
const posix_multi_short_args_3 = ['-vv', 'vvv', '-xyz', '2', '-done', '-d', 'two']
const posix_multi_short_args_4 = ['-yxz2', '-vv', 'vvv', '-done', '-d', 'two']
const posix_multi_short_args_1_err = ['-zxy']

struct Config {
	linker_option string   @[short: m]
	test          string = 'def'   @[short: t]
	device        []string @[short: d]
	paths         []string @[tail]
	verbosity     int      @[repeats; short: v]
	not_mapped    string = 'not changed'
	x             bool
	b             bool @[only: y]
	u             int  @[short: z]
}

fn test_pure_posix_short() {
	config, _ := flag.to_struct[Config](exe_and_posix_args, skip: 1, style: .short)!
	assert config.verbosity == 5
	assert config.test == 'abc'
	assert 'one' in config.device
	assert 'two' in config.device
	assert 'three' in config.device
	assert config.linker_option == 'windows'
	assert config.not_mapped == 'not changed'
	assert config.paths.len == 0
	assert config.x
	assert config.b
	assert config.u == 2
}

fn test_pure_posix_multi_short_1() {
	config, _ := flag.to_struct[Config](posix_multi_short_args_1, style: .short)!
	assert config.verbosity == 5
	assert config.test == 'def'
	assert 'one' in config.device
	assert 'two' in config.device
	assert config.linker_option == ''
	assert config.not_mapped == 'not changed'
	assert config.paths.len == 0
	assert config.x
	assert config.b
	assert config.u == 2
}

fn test_pure_posix_multi_short_2() {
	config, _ := flag.to_struct[Config](posix_multi_short_args_2, style: .short)!
	assert config.verbosity == 5
	assert config.test == 'def'
	assert 'one' in config.device
	assert 'two' in config.device
	assert config.linker_option == ''
	assert config.not_mapped == 'not changed'
	assert config.paths.len == 0
	assert config.x
	assert config.b
	assert config.u == 2
}

fn test_pure_posix_multi_short_3() {
	config, _ := flag.to_struct[Config](posix_multi_short_args_3, style: .short)!
	assert config.verbosity == 5
	assert config.test == 'def'
	assert 'one' in config.device
	assert 'two' in config.device
	assert config.linker_option == ''
	assert config.not_mapped == 'not changed'
	assert config.paths.len == 0
	assert config.x
	assert config.b
	assert config.u == 2
}

fn test_pure_posix_multi_short_4() {
	config, _ := flag.to_struct[Config](posix_multi_short_args_4, style: .short)!
	assert config.verbosity == 5
	assert config.test == 'def'
	assert 'one' in config.device
	assert 'two' in config.device
	assert config.linker_option == ''
	assert config.not_mapped == 'not changed'
	assert config.paths.len == 0
	assert config.x
	assert config.b
	assert config.u == 2
}

fn test_pure_posix_multi_short_err_1() {
	if _, _ := flag.to_struct[Config](posix_multi_short_args_1_err, style: .short) {
		assert false, 'flags should not have reached this assert'
	} else {
		assert err.msg() == 'can not assign non-integer value `xy` from flag `-zxy` to `Config.u`'
	}
}

fn test_pure_posix_short_no_exe() {
	config, _ := flag.to_struct[Config](exe_and_posix_args[1..], style: .short)!
	assert config.verbosity == 5
	assert config.test == 'abc'
	assert 'one' in config.device
	assert 'two' in config.device
	assert 'three' in config.device
	assert config.linker_option == 'windows'
	assert config.not_mapped == 'not changed'
	assert config.paths.len == 0
}

fn test_pure_posix_short_with_tail() {
	config, _ := flag.to_struct[Config](exe_and_posix_args_with_tail, skip: 1, style: .short)!
	assert config.verbosity == 6
	assert config.test == 'abc'
	assert 'one' in config.device
	assert 'two' in config.device
	assert 'three' in config.device
	assert config.linker_option == ''
	assert config.not_mapped == 'not changed'
	assert config.paths.len == 3
	assert config.paths[0] == '/path/to/x'
	assert config.paths[1] == '/path/to/y'
	assert config.paths[2] == '/path/to/z'
}

fn test_pure_posix_short_with_tail_no_exe() {
	config, _ := flag.to_struct[Config](exe_and_posix_args_with_tail[1..], style: .short)!
	assert config.verbosity == 6
	assert config.test == 'abc'
	assert 'one' in config.device
	assert 'two' in config.device
	assert 'three' in config.device
	assert config.linker_option == ''
	assert config.not_mapped == 'not changed'
	assert config.paths.len == 3
	assert config.paths[0] == '/path/to/x'
	assert config.paths[1] == '/path/to/y'
	assert config.paths[2] == '/path/to/z'
}
