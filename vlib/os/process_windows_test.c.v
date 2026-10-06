module os

#include <shellapi.h>
#flag -lshell32

fn C.CommandLineToArgvW(command_line &u16, argc &i32) &&u16

fn test_requote_args_matches_windows_argument_parser() {
	mut arguments := ['echo', '/c', 'hello', '&|<>()^;!=', '', 'one argument', '%PATH%',
		'quote " and punctuation ;', r'C:\trailing\', r'C:\two trailing\\', r'C:\space directory\',
		r'\\server\share\', 'tab\targument', 'héllo 世界']
	for count in 0 .. 6 {
		arguments << 'before' + '\\'.repeat(count) + '"after'
		arguments << 'trailing' + '\\'.repeat(count)
	}
	// argv[0] has separate parsing rules; use a normal executable name there.
	command_line := ('"argv.exe" ' + requote_args(arguments)).to_wide()
	defer { unsafe { free(command_line) } }
	mut argc := i32(0)
	argv := C.CommandLineToArgvW(command_line, &argc)
	assert !isnil(argv)
	defer { C.LocalFree(argv) }
	assert int(argc) == arguments.len + 1
	for i, argument in arguments {
		actual := unsafe { string_from_wide(argv[i + 1]) }
		assert actual == argument, '${i}: expected `${argument}`, got `${actual}`'
	}
}

fn test_exec_windows_cmd_builtins() {
	echo := exec(['cmd', '/d', '/c', 'echo', 'hello'])
	assert echo.exit_code == 0, echo.output
	assert echo.output == 'hello\r\n', echo.output
	exited := exec(['cmd', '/d', '/c', 'exit', '/b', '23'])
	assert exited.exit_code == 23, exited.output
	env_name := 'V_EXEC_BUILTIN_${getpid()}'
	setenv(env_name, 'builtin-value', true)
	defer { unsetenv(env_name) }
	set := exec(['cmd', '/d', '/c', 'set', env_name])
	assert set.exit_code == 0, set.output
	assert set.output == '${env_name}=builtin-value\r\n', set.output
	root := join_path(vtmp_dir(), 'cmd builtin args ${getpid()}')
	mkdir_all(root)!
	defer { rmdir_all(root) or {} }
	marker := join_path(root, 'marker.txt')
	write_file(marker, 'marker')!
	listed := exec(['cmd', '/d', '/c', 'dir', '/b', marker])
	assert listed.exit_code == 0, listed.output
	assert listed.output == 'marker.txt\r\n', listed.output
}
