module shlex

fn test_split_basic() {
	assert split('v run "my prog.v" --flag=value')! == ['v', 'run', 'my prog.v', '--flag=value']
	assert split('echo foo')! == ['echo', 'foo']
}

fn test_split_quotes() {
	assert split("echo 'a b' c")! == ['echo', 'a b', 'c']
	assert split('echo "a b" c')! == ['echo', 'a b', 'c']
	assert split("a'b c'd")! == ['ab cd']
	assert split("''")! == ['']
	assert split('""')! == ['']
}

fn test_split_escapes() {
	assert split('a\\ b')! == ['a b']
	assert split('a\\\\b')! == ['a\\b']
	assert split('"a\\"b"')! == ['a"b']
	assert split('"a\$b"')! == ['a\$b']
	assert split('"a\\nb"')! == ['a\\nb']
	assert split('a\\\nb')! == ['ab']
}

fn test_split_unicode_passthrough() {
	assert split("'héllo wörld' x")! == ['héllo wörld', 'x']
}

fn test_split_empty_and_errors() {
	assert split('')! == []
	assert split('   \t\n')! == []
	split("'abc") or {
		assert err.msg() == 'shlex.split: unbalanced quote'
		return
	}
	assert false, 'unbalanced single quote must error'
	split('"abc') or {
		assert err.msg() == 'shlex.split: unbalanced quote'
		return
	}
	assert false, 'unbalanced double quote must error'
	split('abc\\') or {
		assert err.msg() == 'shlex.split: trailing backslash'
		return
	}
	assert false, 'trailing backslash must error'
}
