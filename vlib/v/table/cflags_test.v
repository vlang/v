import table
import v.cflag

const (
	module_name = 'main'
	cdefines    = []string{}
	no_name     = ''
	no_flag     = ''
	no_os       = ''
)

fn test_parse_valid_cflags() {
	t := table.new_table()
	expected_flags := [
		make_flag('freebsd', '-I', '/usr/local/include/freetype2'),
		make_flag('linux', '-l', 'glfw'),
		make_flag('mingw', no_name, '-mwindows'),
		make_flag('solaris', '-L', '/opt/local/lib'),
		make_flag('darwin', '-framework', 'Cocoa'),
		make_flag('windows', '-l', 'gdi32'),
		make_flag(no_os, '-l', 'mysqlclient'),
		make_flag(no_os, no_name, '-test')
	]
	parse_valid_flag(t, '-lmysqlclient')
	parse_valid_flag(t, '-test')
	parse_valid_flag(t, 'darwin -framework Cocoa')
	parse_valid_flag(t, 'freebsd -I/usr/local/include/freetype2')
	parse_valid_flag(t, 'linux -lglfw')
	parse_valid_flag(t, 'mingw -mwindows')
	parse_valid_flag(t, 'solaris -L/opt/local/lib')
	parse_valid_flag(t, 'windows -lgdi32')
	assert t.cflags.len == expected_flags.len
	for f in expected_flags {
		assert t.has_cflag(f)
	}
}

fn test_parse_invalid_cflags() {
	t := table.new_table()
	// -I, -L, -l must have values
	assert_parse_invalid_flag(t, 'windows -l')
	assert_parse_invalid_flag(t, '-I')
	assert_parse_invalid_flag(t, '-L')
	// OS/compiler name only is not allowed
	assert_parse_invalid_flag(t, 'darwin')
	assert_parse_invalid_flag(t, 'freebsd')
	assert_parse_invalid_flag(t, 'linux')
	assert_parse_invalid_flag(t, 'mingw')
	assert_parse_invalid_flag(t, 'solaris')
	assert_parse_invalid_flag(t, 'windows')
	// Empty flag is not allowed
	assert_parse_invalid_flag(t, no_flag)
	assert t.cflags.len == 0
}

fn parse_valid_flag(t &table.Table, flag string) {
	t.parse_cflag(flag, module_name, cdefines) or {
	}
}

fn assert_parse_invalid_flag(t &table.Table, flag string) {
	t.parse_cflag(flag, module_name, cdefines) or {
		return
	}
	assert false
}

fn make_flag(os, name, value string) cflag.CFlag {
	return cflag.CFlag{
		mod: module_name
		os: os
		name: name
		value: value
	}
}
