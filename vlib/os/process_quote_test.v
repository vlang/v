module os

fn test_requote_args_leaves_windows_builtins_and_switches_unquoted() {
	assert requote_args(['cmd', '/c', 'echo', 'hello']) == 'cmd /c echo hello'
	assert requote_args(['/d', '/c', 'exit', '/b', '23']) == '/d /c exit /b 23'
	assert requote_args(['/d', '/c', 'set', 'V_EXEC_BUILTIN']) == '/d /c set V_EXEC_BUILTIN'
	assert requote_args(['/d', '/c', 'dir', '/b', 'file with spaces.txt']) ==
		'/d /c dir /b "file with spaces.txt"'
}

fn test_requote_arg_preserves_literal_windows_arguments() {
	cases := [
		['hello', 'hello'],
		['héllo世界', 'héllo世界'],
		['%PATH%', '%PATH%'],
		['&|<>()^;!=', '&|<>()^;!='],
		[r'C:\trailing\', r'C:\trailing\'],
		[r'\\server\share\\', r'\\server\share\\'],
		['', '""'],
		['two words', '"two words"'],
		['tab\targument', '"tab\targument"'],
		['héllo 世界', '"héllo 世界"'],
		['a"b', r'"a\"b"'],
		[r'before\"after', r'"before\\\"after"'],
		[r'C:\space directory\', r'"C:\space directory\\"'],
		[r'\\server\space share\\', r'"\\server\space share\\\\"'],
	]
	for case in cases {
		assert requote_arg(case[0]) == case[1], 'argument: ${case[0]}'
	}
}
