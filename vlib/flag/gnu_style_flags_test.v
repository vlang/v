// Test .long (GNU) parse style
import flag

const exe_and_gnu_args = ['/path/to/exe', '--f=10.2', '--mix', '--f2=2', '--test=test', '--amount=5',
	'--version=1.2.3']
const exe_and_gnu_args_with_tail = ['/path/to/exe', '--f2=2', '--f=10.2', '--mix', '--test=test',
	'--amount=6', '--version=1.2.3', '/path/to/x', '/path/to/y', '/path/to/z']

struct Config {
	f           f32
	f2          f64
	mix         bool
	some_test   string = 'abc' @[long: test]
	path        string @[tail]
	amount      int = 1
	version_str string @[long: version]
}

fn test_pure_gnu_long() {
	config, _ := flag.to_struct[Config](exe_and_gnu_args, skip: 1, style: .long)!
	assert config.f == 10.2
	assert config.f2 == 2.0
	assert config.mix == true
	assert config.some_test == 'test'
	assert config.path == ''
	assert config.amount == 5
	assert config.version_str == '1.2.3'
}

fn test_pure_gnu_long_no_exe() {
	config, _ := flag.to_struct[Config](exe_and_gnu_args[1..], style: .long)!
	assert config.f == 10.2
	assert config.f2 == 2.0
	assert config.mix == true
	assert config.some_test == 'test'
	assert config.path == ''
	assert config.amount == 5
	assert config.version_str == '1.2.3'
}

fn test_pure_gnu_long_with_tail() {
	config, no_matches := flag.to_struct[Config](exe_and_gnu_args_with_tail, skip: 1, style: .long)!
	assert config.path == '/path/to/x' // path is of type `string`, not `[]string`
	assert no_matches[0] == '/path/to/y'
	assert no_matches[1] == '/path/to/z'

	assert config.amount == 6
}

fn test_pure_gnu_long_with_tail_no_exe() {
	a := exe_and_gnu_args_with_tail[1..]
	config, no_matches := flag.to_struct[Config](a, style: .long)!
	assert config.path == '/path/to/x'
	assert no_matches[0] == '/path/to/y'
	assert no_matches[1] == '/path/to/z'

	assert config.amount == 6
}
