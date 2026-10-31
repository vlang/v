module main

import os

fn test_shell_shim_content_forwards_to_v_exe() {
	assert shell_shim_content(r'C:\Users\MR\v\v.exe') == '#!/bin/sh\nexec \'C:/Users/MR/v/v.exe\' "$@"\n'
}

fn test_shell_shim_content_quotes_literal_windows_path() {
	path := r"D:\tools\v lang$dollar`backtick's\v.exe"
	assert shell_shim_content(path) == '#!/bin/sh\nexec \'D:/tools/v lang$dollar`backtick\'"\'"\'s/v.exe\' "$@"\n'
}

fn test_shell_shim_executes_literal_paths_and_preserves_arguments() {
	$if windows {
		// Content tests run on Windows; execution needs a POSIX shell and filesystem.
		return
	} $else {
		root := os.join_path(os.vtmp_dir(), 'vsymlink shell ${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		shim := os.join_path(root, 'v')
		arguments := ['', 'argument with spaces', '$literal', '`printf expanded`', "apostrophe's",
			'quote "', '* ? [a-z]', 'line\nbreak', 'tab\targument', r'trailing\']
		mut expected := '${arguments.len}\n'
		for argument in arguments {
			expected += '<${argument}>\n'
		}
		for name in ['compiler with spaces', r'compiler$dollar', 'compiler`printf expanded`',
			"compiler's", "compiler with spaces$dollar`printf expanded`'quotes'"] {
			dir := os.join_path(root, name)
			os.mkdir_all(dir)!
			executable := os.join_path(dir, 'v.exe')
			os.write_file(executable, '#!/bin/sh\nprintf \'%s\\n\' "$#"\nfor arg do\n  printf \'<%s>\\n\' "$arg"\ndone\nexit 23\n')!
			os.chmod(executable, 0o700)!
			os.write_file(shim, shell_shim_content(executable))!
			os.chmod(shim, 0o700)!
			mut command := [shim]
			command << arguments
			result := os.exec(command)
			assert result.exit_code == 23, '${name}: ${result.output}'
			assert result.output == expected, '${name}: ${result.output}'
			// "$@" must also preserve an empty argument list.
			empty_result := os.exec([shim])
			assert empty_result.exit_code == 23, '${name}: ${empty_result.output}'
			assert empty_result.output == '0\n', '${name}: ${empty_result.output}'
		}
	}
}
