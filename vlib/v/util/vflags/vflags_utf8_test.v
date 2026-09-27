module vflags

import os

fn test_tokenize_to_args_preserves_multibyte_utf8() {
	for token in ['café', '日本語', '🧪', 'résumé=naïve', 'é漢😀'] {
		assert tokenize_to_args('-d ${token}') == ['-d', token]
		assert tokenize_to_args('-d "${token} path"') == ['-d', '${token} path']
		assert tokenize_to_args("-d '${token} path'") == ['-d', '${token} path']
	}
}

fn test_tokenize_to_args_preserves_unicode_paths_and_escaped_quotes() {
	assert tokenize_to_args('-cc "/tmp/café compiler/clang" -o "build/日本語 app"') ==
		['-cc', '/tmp/café compiler/clang', '-o', 'build/日本語 app']
	assert tokenize_to_args(r'-o C:\Users\René\app.exe') == ['-o', r'C:\Users\René\app.exe']
	assert tokenize_to_args(r'-d "salutation=hé \"世界\""') == ['-d', 'salutation=hé "世界"']
}

fn test_environment_arguments_keep_their_utf8_bytes() {
	old_vflags := os.getenv_opt('VFLAGS')
	old_vosargs := os.getenv_opt('VOSARGS')
	defer {
		if value := old_vflags {
			os.setenv('VFLAGS', value, true)
		} else {
			os.unsetenv('VFLAGS')
		}
		if value := old_vosargs {
			os.setenv('VOSARGS', value, true)
		} else {
			os.unsetenv('VOSARGS')
		}
	}
	os.unsetenv('VOSARGS')
	os.setenv('VFLAGS', '-o "build/日本語 app" -d café', true)
	mut expected := [os.args[0], '-o', 'build/日本語 app', '-d', 'café']
	expected << os.args#[1..]
	assert join_env_vflags_and_os_args() == expected

	// VOSARGS replaces the complete vector, rather than appending VFLAGS.
	os.setenv('VOSARGS', '"outil français" "source/🧪 test.v"', true)
	assert join_env_vflags_and_os_args() == ['outil français', 'source/🧪 test.v']
}
