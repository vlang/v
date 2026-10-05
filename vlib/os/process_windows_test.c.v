module os

#include <shellapi.h>
#flag -lshell32

fn C.CommandLineToArgvW(command_line &u16, argc &i32) &&u16

fn test_requote_args_matches_windows_argument_parser() {
	mut arguments := ['', 'one argument', '%PATH%', 'quote " and punctuation ;', r'C:\trailing\',
		r'C:\two trailing\\', r'\\server\share\', 'tab\targument', 'héllo 世界']
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
