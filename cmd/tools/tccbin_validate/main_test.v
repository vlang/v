module main

const test_paths = CommandPaths{
	tcc:    '/tmp/physical-tcc/tcc.exe'
	libgc:  '/tmp/physical-tcc/lib/libgc.dylib'
	rpath:  '/tmp/physical-tcc/lib'
	binary: '/tmp/tcc_boehm_gc_smoke'
}

fn good_tokens() []string {
	return [test_paths.tcc, '-Wl,-rpath,${test_paths.rpath}', test_paths.libgc, '-o',
		test_paths.binary, '/tmp/showcc-parser-probe.c']
}

fn probe(tokens []string) string {
	return command_prefix + tokens.join(' ')
}

fn assert_rejected(showcc string, kind string) {
	validate_showcc(showcc, test_paths) or {
		assert err.msg().starts_with('${kind}:'), '${showcc}: ${err}'
		return
	}
	assert false, 'invalid command was accepted: ${showcc}'
}

fn test_valid_commands_preserve_posix_quote_tokenization() ! {
	tokens := good_tokens()
	assert validate_showcc(probe(tokens), test_paths)! == tokens
	realistic := command_prefix + '"${test_paths.tcc}"  -O2 -D GC_THREADS=1\t' +
		'-I"${test_paths.rpath}" -Wl,-rpath,${test_paths.rpath} "${test_paths.libgc}" ' +
		'-o "${test_paths.binary}" "/tmp/showcc-parser.123.tmp.c"'
	parsed := validate_showcc(realistic, test_paths)!
	assert parsed[0] == test_paths.tcc
	assert parsed[4] == '-I${test_paths.rpath}'
	assert parsed[8] == test_paths.binary
	quoted := command_prefix + tokens.map("'${it}'").join(' ')
	assert validate_showcc(quoted, test_paths)! == tokens
	assert tokenize_command('ab"cd"\'ef\' "" \'\'')! == ['abcdef', '', '']
}

fn test_raw_character_allowlist_applies_inside_quotes() {
	good := probe(good_tokens())
	// These strings are parsed only; none of the probes is executed.
	for forbidden in [';', '&', '|', '<', '>', '`', '$', '#', '@', '?', '[', ']', '*', '{', '}',
		'~', '\\', '\n', '\r', 'é', '\x00', '(', ')', '!'] {
		assert_rejected(good + " '${forbidden}'", 'raw-character')
	}
}

fn test_command_and_quote_arity() {
	good := probe(good_tokens())
	assert_rejected(' ' + good, 'command-count')
	assert_rejected(good + command_prefix, 'command-count')
	assert_rejected(good + ' "unfinished', 'tokenization')
	assert_rejected(good + " 'unfinished", 'tokenization')
}

fn test_output_options_require_one_exact_adjacent_pair() {
	tokens := good_tokens()
	for extra in ['-o/tmp/other', '-oother', '-o=/tmp/other', '-oformat=binary'] {
		assert_rejected(probe([tokens[0], extra, ...tokens[1..]]), 'output-joined')
		assert_rejected(probe([...tokens, extra]), 'output-joined')
	}
	assert_rejected(probe([...tokens, '-o', '/tmp/other']), 'output-arity')
	assert_rejected(probe([tokens[0], tokens[1], tokens[2], tokens[5]]), 'output-arity')
	assert_rejected(probe([tokens[0], tokens[1], tokens[2], '-o']), 'output-pair')
	assert_rejected(probe([tokens[0], tokens[1], tokens[2], '-o', test_paths.binary + '.suffix',
		tokens[5]]), 'output-pair')
}

fn test_exact_compiler_link_paths_and_static_gc_rejection() {
	tokens := good_tokens()
	assert_rejected(probe([test_paths.tcc + '.suffix', ...tokens[1..]]), 'compiler-arity')
	assert_rejected(probe([tokens[0], tokens[0], ...tokens[1..]]), 'compiler-arity')
	assert_rejected(probe(['-O2', ...tokens]), 'compiler-position')
	for extra in [test_paths.tcc + '.suffix', test_paths.libgc + '.suffix', tokens[1] + '-suffix',
		test_paths.libgc, tokens[1], test_paths.rpath + '/libgc.a'] {
		assert_rejected(probe([...tokens, extra]), 'link-contract')
	}
	assert_rejected(probe([tokens[0], tokens[1], test_paths.libgc + '.suffix', ...tokens[3..]]),
		'link-contract')
	assert_rejected(probe([tokens[0], tokens[1] + '-suffix', ...tokens[2..]]), 'link-contract')
	assert_rejected(probe(['/tmp/logical-tcc/tcc.exe', '-Wl,-rpath,/tmp/logical-tcc/lib',
		'/tmp/logical-tcc/lib/libgc.dylib', ...tokens[3..]]), 'compiler-arity')
	assert_rejected(probe([tokens[0], '-Wl,-rpath,/tmp/logical-tcc/lib',
		'/tmp/logical-tcc/lib/libgc.dylib', ...tokens[3..]]), 'link-contract')
	assert_rejected(probe(['/tmp/arbitrary-tcc-root/tcc.exe', ...tokens[1..]]), 'compiler-arity')
}
