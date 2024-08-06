// Test .short (POSIX) parse style
import flag

const cmd_exe_args = ['/a', 'C:\\', '/B', 'xyz', '/a', 'D:\\', '/d', '/e', '/c', '"xyz"']

const cmd_exe_mixed_args = ['/a', 'C:\\', '/Big', 'hij', '/a', 'D:\\', '/Dev', '/e', '/c', '"xyz"']

const cmd_exe_args_with_tail = ['/a', 'C:\\', '/b', '/B', 'xyz', '/a', 'D:\\', '/d', '/e', '/c',
	'"xyz"', '"/path/to/x"', '"/path/to/y"', '"/path/to/z"']

struct Config {
	big_b      string = 'def'   @[long: Big; short: B]
	small_b    bool     @[short: b]
	a_device   []string @[short: a]
	paths      []string @[tail]
	not_mapped string = 'not changed'
	e          bool
	b          bool   @[long: Dev; only: d]
	u          string @[short: c]
}

fn test_cmd_exe() {
	config, _ := flag.to_struct[Config](cmd_exe_args, style: .cmd_exe, delimiter: '/')!
	assert config.big_b == 'xyz'
	assert config.small_b == false
	assert config.a_device.len == 2
	assert config.a_device[0] == 'C:\\'
	assert config.a_device[1] == 'D:\\'
	assert config.paths.len == 0
	assert config.not_mapped == 'not changed'
	assert config.e
	assert config.b
	assert config.u == '"xyz"'
}

fn test_cmd_exe_mixed() {
	config, _ := flag.to_struct[Config](cmd_exe_mixed_args, style: .cmd_exe, delimiter: '/')!
	assert config.big_b == 'hij'
	assert config.small_b == false
	assert config.a_device.len == 2
	assert config.a_device[0] == 'C:\\'
	assert config.a_device[1] == 'D:\\'
	assert config.paths.len == 0
	assert config.not_mapped == 'not changed'
	assert config.e
	assert config.b
	assert config.u == '"xyz"'
}

fn test_cmd_exe_with_tail() {
	config, _ := flag.to_struct[Config](cmd_exe_args_with_tail, style: .cmd_exe, delimiter: '/')!
	assert config.big_b == 'xyz'
	assert config.small_b
	assert config.a_device.len == 2
	assert config.a_device[0] == 'C:\\'
	assert config.a_device[1] == 'D:\\'
	assert config.paths.len == 3
	assert config.paths[0] == '"/path/to/x"'
	assert config.paths[1] == '"/path/to/y"'
	assert config.paths[2] == '"/path/to/z"'
	assert config.not_mapped == 'not changed'
	assert config.e
	assert config.b
	assert config.u == '"xyz"'
}
