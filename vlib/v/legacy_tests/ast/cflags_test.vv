import v.ast
import v.cflag

const module_name = 'main'
const cdefines = []string{}
const no_name = ''
const no_flag = ''
const no_os = ''

fn test_parse_valid_cflags() {
	mut t := ast.new_table()
	expected_flags := [
		make_flag('freebsd', '-I', '/usr/local/include/freetype2'),
		make_flag('linux', '-l', 'glfw'),
		make_flag('mingw', no_name, '-mwindows'),
		make_flag('solaris', '-L', '/opt/local/lib'),
		make_flag('darwin', '-framework', 'Cocoa'),
		make_flag('mac', '-l', 'openal'),
		make_flag('windows', '-l', 'gdi32'),
		make_flag(no_os, '-l', 'mysqlclient'),
		make_flag(no_os, no_name, '-test'),
		make_flag('linux', '-I', '/usr/include/SDL2'),
		make_flag('linux', '-D', '_REENTRANT'),
		make_flag('linux', '-L', '/usr/lib/x86_64-linux-gnu'),
		make_flag('linux', '-l', 'SDL2'),
		make_flag(no_os, '-I', '/usr/include/mysql'),
		make_flag(no_os, no_name, '-m64'),
		make_flag(no_os, '-I', '/usr/include'),
		make_flag(no_os, no_name, '/v/thirdparty/tcc/lib/libgc.a'),
		make_flag(no_os, '-I', '/usr/include/你好 my , @с интервали'),
	]
	parse_valid_flag(mut t, '-lmysqlclient')
	parse_valid_flag(mut t, '-test')
	parse_valid_flag(mut t, 'darwin -framework Cocoa')
	parse_valid_flag(mut t, 'mac -lopenal')
	parse_valid_flag(mut t, 'freebsd -I/usr/local/include/freetype2')
	parse_valid_flag(mut t, 'linux -lglfw')
	parse_valid_flag(mut t, 'mingw -mwindows')
	parse_valid_flag(mut t, 'solaris -L/opt/local/lib')
	parse_valid_flag(mut t, 'windows -lgdi32')
	parse_valid_flag(mut t,
		'linux -I/usr/include/SDL2 -D_REENTRANT -L/usr/lib/x86_64-linux-gnu -lSDL2')
	parse_valid_flag(mut t, '-I/usr/include/mysql -m64 -I/usr/include')
	parse_valid_flag(mut t, '/v/thirdparty/tcc/lib/libgc.a')
	parse_valid_flag(mut t, '-I/usr/include/你好 my , @с интервали')
	assert t.cflags.len == expected_flags.len
	for f in expected_flags {
		assert t.has_cflag(f)
	}
}

fn test_parse_invalid_cflags() {
	mut t := ast.new_table()
	// -I, -L, -l must have values
	assert_parse_invalid_flag(mut t, 'windows -l')
	assert_parse_invalid_flag(mut t, '-I')
	assert_parse_invalid_flag(mut t, '-L')
	assert_parse_invalid_flag(mut t, 'darwin `sdl2-config --cflags --libs` -lSDL2')
	// OS/compiler name only is not allowed
	assert_parse_invalid_flag(mut t, 'darwin')
	assert_parse_invalid_flag(mut t, 'mac')
	assert_parse_invalid_flag(mut t, 'freebsd')
	assert_parse_invalid_flag(mut t, 'linux')
	assert_parse_invalid_flag(mut t, 'mingw')
	assert_parse_invalid_flag(mut t, 'solaris')
	assert_parse_invalid_flag(mut t, 'windows')
	// Empty flag is not allowed
	assert_parse_invalid_flag(mut t, no_flag)
	assert t.cflags.len == 0
}

fn test_parse_cflag_with_link_segment_preserves_occurrences_and_global_uniqueness() {
	mut t := ast.new_table()
	controls := '-Wl,--whole-archive -Wl,--no-whole-archive'
	expected := [
		make_flag(no_os, '-Wl', ',--whole-archive'),
		make_flag(no_os, '-Wl', ',--no-whole-archive'),
	]
	t.parse_cflag_with_link_segment(controls, module_name, cdefines) or { panic(err) }
	t.parse_cflag_with_link_segment(controls, module_name, cdefines) or { panic(err) }

	assert t.cflags == expected
	assert t.link_flag_segments.len == 2
	assert t.link_flag_segments.all(!it.is_pkgconfig && it.flags == expected)

	mut legacy := ast.new_table()
	legacy.parse_cflag(controls, module_name, cdefines) or { panic(err) }
	legacy.parse_cflag(controls, module_name, cdefines) or { panic(err) }
	assert legacy.cflags == expected
	assert legacy.link_flag_segments == []
}

fn test_parse_pkgconfig_link_flags_preserves_structured_boundaries_and_repetition() {
	mut t := ast.new_table()
	fragments := ['-L/issue74/structured-boundary', '/issue74/structured-boundary/libissue74_b.a',
		'/issue74/structured-boundary/libissue74_a.a', '/issue74/structured-boundary/libissue74_b.a']
	t.parse_pkgconfig_link_flags(fragments, module_name, cdefines) or { panic(err) }
	assert t.cflags == []
	assert t.link_flag_segments.len == 1
	segment := t.link_flag_segments[0]
	assert segment.is_pkgconfig
	assert segment.flags == [
		make_flag(no_os, '-L', '/issue74/structured-boundary'),
		make_flag(no_os, no_name, '/issue74/structured-boundary/libissue74_b.a'),
		make_flag(no_os, no_name, '/issue74/structured-boundary/libissue74_a.a'),
		make_flag(no_os, no_name, '/issue74/structured-boundary/libissue74_b.a'),
	]
}

fn test_parse_pkgconfig_link_flags_preserves_quoted_hyphenated_paths() {
	mut t := ast.new_table()
	t.parse_pkgconfig_link_flags(['-L"/tmp/search - libs"', '"/tmp/direct - libs/lib x.a"'],
		module_name, cdefines) or { panic(err) }
	assert t.cflags == []
	assert t.link_flag_segments.len == 1
	segment := t.link_flag_segments[0]
	assert segment.is_pkgconfig
	assert segment.flags == [
		make_flag(no_os, '-L', '"/tmp/search - libs"'),
		make_flag(no_os, no_name, '"/tmp/direct - libs/lib x.a"'),
	]
}

fn test_parse_pkgconfig_link_flags_consumes_split_library_operands() {
	mut t := ast.new_table()
	t.parse_pkgconfig_link_flags(['-Wl,--start-group', '-L', '"/tmp/search - libs"', '-l',
		'"issue74 - static"', '-Wl,--end-group'], module_name, cdefines) or { panic(err) }
	assert t.cflags == []
	assert t.link_flag_segments.len == 1
	segment := t.link_flag_segments[0]
	assert segment.is_pkgconfig
	assert segment.flags == [
		make_flag(no_os, '-Wl', ',--start-group'),
		make_flag(no_os, '-L', '"/tmp/search - libs"'),
		make_flag(no_os, '-l', '"issue74 - static"'),
		make_flag(no_os, '-Wl', ',--end-group'),
	]
}

fn test_parse_pkgconfig_link_flags_rejects_missing_split_library_operands() {
	for fragment in ['-L', '-l'] {
		mut t := ast.new_table()
		t.parse_pkgconfig_link_flags([fragment], module_name, cdefines) or {
			assert err.msg().contains('missing')
			assert t.link_flag_segments == []
			continue
		}
		assert false
	}
}

fn parse_valid_flag(mut t ast.Table, flag string) {
	t.parse_cflag(flag, module_name, cdefines) or {}
}

fn assert_parse_invalid_flag(mut t ast.Table, flag string) {
	t.parse_cflag(flag, module_name, cdefines) or { return }
	assert false
}

fn make_flag(os string, name string, value string) cflag.CFlag {
	return cflag.CFlag{
		mod:   module_name
		os:    os
		name:  name
		value: value
	}
}
