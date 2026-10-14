module util

import os

fn test_tokenize_c_flag_splits_on_unquoted_whitespace() {
	assert tokenize_c_flag('-DFOO=1 -O2') == ['-DFOO=1', '-O2']
	assert tokenize_c_flag('-I /usr/include') == ['-I', '/usr/include']
	assert tokenize_c_flag('-include a.c') == ['-include', 'a.c']
	// Leading and trailing blanks never produce an empty token.
	assert tokenize_c_flag('  -O2  ') == ['-O2']
	assert tokenize_c_flag('\t-O2\n-O3') == ['-O2', '-O3']
}

fn test_tokenize_c_flag_returns_nothing_for_blank_input() {
	assert tokenize_c_flag('') == []string{}
	assert tokenize_c_flag('   ') == []string{}
	assert tokenize_c_flag('\t\n ') == []string{}
}

// The quotes are part of the token, not delimiters that are consumed: what
// tokenize_c_flag guarantees is only that a blank inside them does not split.
fn test_tokenize_c_flag_keeps_quote_marks_in_the_token() {
	assert tokenize_c_flag('-D "A B"') == ['-D', '"A B"']
	assert tokenize_c_flag("-D 'A B'") == ['-D', "'A B'"]
	assert tokenize_c_flag('-D "A B" -D C') == ['-D', '"A B"', '-D', 'C']
	// An unmatched quote stays that way; the split still stops at it.
	assert tokenize_c_flag('-D "unfinished') == ['-D', '"unfinished']
}

// A backslash hides the next byte from the quote and blank handling, and both
// bytes stay in the token.
fn test_tokenize_c_flag_honours_a_backslash_escape() {
	assert tokenize_c_flag('-include a\\ b.c') == ['-include', 'a\\ b.c']
	assert tokenize_c_flag('-D \\"') == ['-D', '\\"']
	assert tokenize_c_flag('-D "a\\"b"') == ['-D', '"a\\"b"']
}

fn test_quote_path_and_args_quote_paths() {
	$if windows {
		assert quote_path('plain.v') == '"plain.v"'
		assert quote_path('with space.v') == '"with space.v"'
		assert args_quote_paths(['a.v', 'b c.v']) == '"a.v" "b c.v"'
	} $else {
		assert quote_path('plain.v') == "'plain.v'"
		assert quote_path('with space.v') == "'with space.v'"
		assert args_quote_paths(['a.v', 'b c.v']) == "'a.v' 'b c.v'"
	}
	assert args_quote_paths([]string{}) == ''
}

fn test_find_all_v_files_walks_a_directory_for_v_and_vsh_sources() {
	root := os.join_path(os.vtmp_dir(), 'util_find_all_${os.getpid()}')
	os.rmdir_all(root) or {}
	src := os.join_path(root, 'src')
	os.mkdir_all(src)!
	os.write_file(os.join_path(src, 'a.v'), 'module a\n')!
	os.write_file(os.join_path(src, 'b.vsh'), 'echo b\n')!
	os.write_file(os.join_path(src, 'notes.txt'), 'x\n')!
	defer {
		os.rmdir_all(root) or {}
	}
	mut found := find_all_v_files([src])!
	found.sort()
	assert found == [os.join_path(src, 'a.v'), os.join_path(src, 'b.vsh')]
	// A directory nested below a root is walked too.
	os.mkdir_all(os.join_path(src, 'deep'))!
	os.write_file(os.join_path(src, 'deep', 'c.v'), 'module c\n')!
	found = find_all_v_files([src])!
	found.sort()
	assert found == [os.join_path(src, 'a.v'), os.join_path(src, 'b.vsh'), os.join_path(src, 'deep',
		'c.v')]
}

fn test_find_all_v_files_keeps_explicit_roots_in_order() {
	root := os.join_path(os.vtmp_dir(), 'util_find_all_explicit_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	a := os.join_path(root, 'a.v')
	b := os.join_path(root, 'b.vsh')
	os.write_file(a, 'module a\n')!
	os.write_file(b, 'echo b\n')!
	defer {
		os.rmdir_all(root) or {}
	}
	assert find_all_v_files([a, b])! == [a, b]
	// A root that is a directory is expanded in place, so the two forms mix.
	assert find_all_v_files([root, a])! == [a, b, a]
}

fn test_find_all_v_files_rejects_other_extensions_and_missing_files() {
	root := os.join_path(os.vtmp_dir(), 'util_find_all_rejects_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	text := os.join_path(root, 'a.txt')
	os.write_file(text, 'x\n')!
	missing := os.join_path(root, 'missing.v')
	if _ := find_all_v_files([text]) {
		assert false, 'a .txt root should be rejected'
	} else {
		assert err.msg().contains('v fmt can only be used on .v files.')
		assert err.msg().contains(text)
	}
	if _ := find_all_v_files([missing]) {
		assert false, 'a missing root should be rejected'
	} else {
		assert err.msg() == '"${missing}" does not exist'
	}
}
