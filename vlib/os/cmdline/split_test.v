import os.cmdline

fn test_split_basic() {
	assert cmdline.split('v run "my prog.v" --flag=value')! == ['v', 'run', 'my prog.v', '--flag=value']
	assert cmdline.split('echo foo')! == ['echo', 'foo']
}

fn test_split_quotes() {
	assert cmdline.split("echo 'a b' c")! == ['echo', 'a b', 'c']
	assert cmdline.split('echo "a b" c')! == ['echo', 'a b', 'c']
	assert cmdline.split("a'b c'd")! == ['ab cd']
	assert cmdline.split("''")! == ['']
	assert cmdline.split('""')! == ['']
}

fn test_split_escapes() {
	assert cmdline.split('a\\ b')! == ['a b']
	assert cmdline.split('a\\\\b')! == ['a\\b']
	assert cmdline.split('"a\\"b"')! == ['a"b']
	assert cmdline.split('"a\$b"')! == ['a\$b']
	assert cmdline.split('"a\\nb"')! == ['a\\nb']
	assert cmdline.split('a\\\nb')! == ['ab']
	assert cmdline.split('"a\\`b"')! == ['a`b']
}

fn test_split_unicode_passthrough() {
	assert cmdline.split("'héllo wörld' x")! == ['héllo wörld', 'x']
}

fn test_split_empty_and_errors() {
	assert cmdline.split('')!.len == 0
	assert cmdline.split('   \t\n')!.len == 0
	cmdline.split("'abc") or {
		assert err.msg() == 'cmdline.split: unbalanced quote'
		return
	}
	assert false, 'unbalanced single quote must error'
}

fn test_split_unbalanced_double_quote_error() {
	cmdline.split('"abc') or {
		assert err.msg() == 'cmdline.split: unbalanced quote'
		return
	}
	assert false, 'unbalanced double quote must error'
}

fn test_split_trailing_backslash_error() {
	cmdline.split('abc\\') or {
		assert err.msg() == 'cmdline.split: trailing backslash'
		return
	}
	assert false, 'trailing backslash must error'
}
