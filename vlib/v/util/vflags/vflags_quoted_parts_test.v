module vflags

import os

fn test_tokenize_to_args_keeps_partially_quoted_paths_together() {
	for quote in ['"', "'"] {
		assert tokenize_to_args('-cc ${quote}compiler dir${quote}/gcc') == ['-cc',
			'compiler dir/gcc']
		assert tokenize_to_args('-o build/${quote}nested dir${quote}/app') == ['-o',
			'build/nested dir/app']
		assert tokenize_to_args('-I${quote}include dir${quote}/headers') == ['-Iinclude dir/headers']
		assert tokenize_to_args('-Dname=${quote}hello world${quote}!') == ['-Dname=hello world!']
		assert tokenize_to_args('-o ${quote}build dir${quote}/app -showcc') == ['-o',
			'build dir/app', '-showcc']
	}
	assert tokenize_to_args(r'-cc "C:\Program Files"\LLVM\bin\clang.exe') == ['-cc',
		r'C:\Program Files\LLVM\bin\clang.exe']
}

fn test_tokenize_to_args_concatenates_adjacent_quoted_parts() {
	for first_quote in ['"', "'"] {
		for second_quote in ['"', "'"] {
			input := 'pre${first_quote}middle ${first_quote}${second_quote}tail${second_quote}post'
			assert tokenize_to_args(input) == ['premiddle tailpost'], input
		}
	}
	assert tokenize_to_args(r'"one""two"three') == ['onetwothree']
	assert tokenize_to_args("'one''two'three") == ['onetwothree']
}

fn test_tokenize_to_args_distinguishes_empty_arguments_from_empty_parts() {
	for quote in ['"', "'"] {
		empty_part := quote + quote
		assert tokenize_to_args('${empty_part}suffix') == ['suffix']
		assert tokenize_to_args('prefix${empty_part}') == ['prefix']
		assert tokenize_to_args('prefix${empty_part}suffix') == ['prefixsuffix']
		assert tokenize_to_args(empty_part + empty_part) == ['']
		assert tokenize_to_args('${empty_part} ${empty_part}') == ['', '']
		assert tokenize_to_args('-ldflags ${empty_part} -showcc') == ['-ldflags', '', '-showcc']
		assert tokenize_to_args('first${empty_part} second${empty_part}') == ['first', 'second']
	}
	assert tokenize_to_args('') == []string{}
	assert tokenize_to_args(' \t\r\n ') == []string{}
	assert tokenize_to_args('""' + "''") == ['']
}

fn test_tokenize_to_args_only_unquoted_whitespace_separates_parts() {
	for separator in [' ', '\t', '\n', '\r\n', ' \t '] {
		input := '"a b"suffix${separator}prefix"c d"${separator}""${separator}'
		assert tokenize_to_args(input) == ['a bsuffix', 'prefixc d', ''], input
	}
	assert tokenize_to_args('"line\nbreak"suffix') == ['line\nbreaksuffix']
}

fn test_tokenize_to_args_preserves_utf8_and_escaped_quotes_in_parts() {
	for quote in ['"', "'"] {
		assert tokenize_to_args('${quote}café dir${quote}/日本語') == ['café dir/日本語']
		assert tokenize_to_args('é${quote}漢 🧪${quote}😀') == ['é漢 🧪😀']
	}
	assert tokenize_to_args(r'"a \"quote\""suffix') == ['a "quote"suffix']
	assert tokenize_to_args(r'-Dtext="say \"hello\""!') == ['-Dtext=say "hello"!']
}

fn test_environment_flags_keep_quoted_parts_and_argument_precedence() {
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
	os.setenv('VFLAGS', '-cc "compiler dir"/gcc -cflags "-I"/opt/"include dir" -ldflags ""', true)
	mut expected := [os.args[0], '-cc', 'compiler dir/gcc', '-cflags', '-I/opt/include dir',
		'-ldflags', '']
	expected << os.args#[1..]
	assert join_env_vflags_and_os_args() == expected

	// VOSARGS replaces the whole vector, including argv[0], even with VFLAGS set.
	os.setenv('VOSARGS', '"tool dir"/v ""suffix -o "build dir"/app', true)
	assert join_env_vflags_and_os_args() == ['tool dir/v', 'suffix', '-o', 'build dir/app']
}
