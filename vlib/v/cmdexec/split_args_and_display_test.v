module cmdexec

import os

fn test_split_args_splits_a_command_line_into_argv() {
	assert split_args('-c "foo bar"') or { panic(err) } == ['-c', 'foo bar']
	assert split_args('a b c') or { panic(err) } == ['a', 'b', 'c']
	assert split_args('  a   b  ') or { panic(err) } == ['a', 'b']
	assert split_args('a') or { panic(err) } == ['a']
	assert split_args('\ta\nb') or { panic(err) } == ['a', 'b']
}

// No shell is involved, so quotes are consumed but no expansion happens.
fn test_split_args_preserves_quoted_text_without_expanding_it() {
	assert split_args('-D "A=B C"') or { panic(err) } == ['-D', 'A=B C']
	assert split_args("-D 'A B'") or { panic(err) } == ['-D', 'A B']
	assert split_args('-D "$HOME/x"') or { panic(err) } == ['-D', '$HOME/x']
	assert split_args('') or { panic(err) } == []string{}
	assert split_args('   ') or { panic(err) } == []string{}
}

fn test_split_args_reports_an_unbalanced_quote() {
	if _ := split_args('-c "unfinished') {
		assert false, 'an unbalanced quote should be an error'
	} else {
		assert err.msg().len > 0
	}
	if _ := split_args("a 'b") {
		assert false, 'an unbalanced quote should be an error'
	} else {
		assert err.msg().len > 0
	}
}

// The result only ever round-trips back through display, so what display writes
// must be readable as the same argv by a shell-free parser.
fn test_display_round_trips_through_split_args() {
	for command in [['cc', '-c', 'x.c', '-o', 'x.o'], ['pkg-config', '--cflags', '--libs', 'zlib']] {
		text := display(command[0], command[1..])
		assert split_args(text) or { panic(err) } == command
	}
}

fn test_display_leaves_plain_arguments_unquoted() {
	assert display('cc', ['-c', 'a.c']) == 'cc -c a.c'
	assert display('cc', ['-o', 'out']) == 'cc -o out'
	assert display('/path/to/cc', ['-I/home/user/include']) == '/path/to/cc -I/home/user/include'
	assert display('cc', []) == 'cc'
	// A `.` in an argument is plain enough not to need quoting.
	assert display('cc', ['a.c']) == 'cc a.c'
}

fn test_display_quotes_only_an_argument_that_needs_it() {
	quoted := os.quoted_path('a b')
	assert display('cc', ['a b']) == 'cc ${quoted}'
	assert display('cc', ['a"b']) != display('cc', ['a b'])
	assert display('cc', ['a b']) != display('cc', ['a  b'])
}
