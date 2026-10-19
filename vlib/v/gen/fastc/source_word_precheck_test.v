module fastc

import os
import v.cmdexec
import v.flat
import v.pref
import v.scanner
import v.token

fn test_fastc_source_word_candidates_respect_identifier_boundaries() {
	for word in ['chan', 'spawn'] {
		for source in [word, '${word}\tint', '(${word})', '\n${word}', '${word}/* comment */'] {
			assert fastc_source_contains_word(source, word), source
		}
		for source in ['', '_${word}', '${word}_', '${word}2', '2${word}', 'pre_${word}', '${word}ed',
			'${word}er'] {
			assert !fastc_source_contains_word(source, word), source
		}
		assert fastc_source_contains_word('${word}er ${word}', word)
		assert fastc_source_contains_word('// ${word}', word)
		assert fastc_source_contains_word('"${word}"', word)
	}
	assert !fastc_source_contains_word('changed channel', 'chan')
	assert !fastc_source_contains_word('anything', '')
}

fn test_fastc_channel_runtime_candidates_are_checked_lexically() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	header := FastcSourceHeader{ module_name: 'main' }
	for body in ['channel := 1\n changed := 2', '// chan int\n value := 1', '/* chan int */ value := 1',
		'text := "chan int"'] {
		source := 'module main\nfn main() {\n${body}\n}\n'
		result := fastc_header_with_channel_runtime(header, source, prefs)
		assert '__fastc_channels' !in result.imports, body
		assert 'sync' !in result.import_order, body
	}
	for body in ['items := chan int{}', 'items := chan\tint{}', 'items := chan/* element */int{}'] {
		source := 'module main\nfn main() {\n${body}\n}\n'
		result := fastc_header_with_channel_runtime(header, source, prefs)
		assert result.imports['__fastc_channels'] == 'sync', body
		assert result.import_order == ['sync'], body
	}
}

fn test_fastc_spawn_parameter_candidates_are_checked_lexically() {
	prefs := pref.new_preferences()
	for body in ['spawner(&state)', '// spawn read(&state)\n read(&state)',
		'/* spawn read(&state) */ read(&state)', 'println("spawn read(&state)")'] {
		source := '{\n${body}\n}'
		mut scan := scanner.new_scanner(prefs, .normal)
		scan.init(token.File.unindexed('spawn_candidates.v', source.len), source)
		assert scan.scan() == .lcbr
		tok := scan.scan()
		g := Parser{
			prefs:  prefs
			s:      scan
			tok:    tok
			locals: {
				'state': FastcLocal{ typ: flat.source_type_id('State') }
			}
		}
		assert g.spawn_escaping_parameters() == [], body
	}
	for body in ['spawn read(&state)', 'spawn/* worker */read(&state)', 'if true { spawn state.read() }'] {
		source := '{\n${body}\n}'
		mut scan := scanner.new_scanner(prefs, .normal)
		scan.init(token.File.unindexed('spawn_candidates.v', source.len), source)
		assert scan.scan() == .lcbr
		tok := scan.scan()
		g := Parser{
			prefs:  prefs
			s:      scan
			tok:    tok
			locals: {
				'state': FastcLocal{ typ: flat.source_type_id('State') }
			}
		}
		assert g.spawn_escaping_parameters() == ['state'], body
	}
}

fn test_fastc_spawn_precheck_uses_the_current_synthetic_scanner_source() {
	prefs := pref.new_preferences()
	mut g := Parser{
		prefs:  prefs
		path:   'original.v'
		s:      scanner.new_scanner(prefs, .normal)
		locals: {
			'state': FastcLocal{ typ: flat.source_type_id('State') }
		}
	}
	original := '{ /*no*/read(&state) }'
	synthetic := '{ spawn read(&state) }'
	assert original.len == synthetic.len
	g.s.init(token.File.unindexed('original.v', original.len), original)
	assert g.s.scan() == .lcbr
	g.tok = g.s.scan()
	original_scan := g.s
	original_tok := g.tok
	assert !g.spawn_source_checked
	assert g.spawn_escaping_parameters() == []
	assert g.spawn_escaping_parameters() == []
	assert g.spawn_source_checked
	assert !g.spawn_source_has_word
	g.s.init(token.File.unindexed('synthetic.v', synthetic.len), synthetic)
	assert g.s.scan() == .lcbr
	g.tok = g.s.scan()
	assert g.spawn_escaping_parameters() == ['state']
	assert g.spawn_escaping_parameters() == ['state']
	assert g.spawn_source_has_word
	g.s = original_scan
	g.tok = original_tok
	assert g.spawn_escaping_parameters() == []
	assert !g.spawn_source_has_word
}

fn test_fastc_spawn_precheck_distinguishes_source_views_with_the_same_pointer() {
	prefs := pref.new_preferences()
	mut g := Parser{
		prefs:  prefs
		s:      scanner.new_scanner(prefs, .normal)
		locals: {
			'state': FastcLocal{ typ: flat.source_type_id('State') }
		}
	}
	first_body := '{ read(&state) }'
	source := first_body + ' { spawn read(&state) }'
	// Keep the backing bytes fixed while giving the scanner a shorter view.
	prefix := unsafe { source.substr_unsafe(0, first_body.len) }
	assert prefix.str == source.str
	g.s.init(token.File.unindexed('prefix.v', prefix.len), prefix)
	assert g.s.scan() == .lcbr
	g.tok = g.s.scan()
	assert g.spawn_escaping_parameters() == []
	assert !g.spawn_source_has_word
	g.s.init(token.File.unindexed('full.v', source.len), source)
	g.tok = g.s.scan()
	for g.tok != .key_spawn && g.tok != .eof {
		g.tok = g.s.scan()
	}
	assert g.tok == .key_spawn
	assert g.spawn_escaping_parameters() == ['state']
	assert g.spawn_source_has_word
}

fn test_fastc_capture_candidates_keep_comment_and_newline_separated_tokens() {
	for expression in ['fn [value] () {}', 'fn\t[mut value] () {}', 'fn\n[value] () {}',
		'fn/* outer /* nested */ tail */[value] () {}', 'fn// capture\n[value] () {}',
		'fn /* before */\n/* after */ [value] () {}', 'fn /* /*/ [value] () {}',
		'fn /* //*/ [value] () {}'] {
		assert fastc_source_has_capture_syntax(expression), expression
		assert fastc_source_uses_captures(expression, pref.new_preferences()), expression
	}
	for source in ['', 'fn ordinary(values []int) []int { return values }',
		'fn generic[T](values []T) []T { return values }', '_fn [value]', 'fn_ [value]', 'fn2 [value]',
		'2fn [value]', 'fn /* no bracket */ ordinary() {}'] {
		assert !fastc_source_has_capture_syntax(source), source
	}
	// An escaped name and bytes outside ASCII identifiers stay conservative candidates.
	for source in ['@fn [value]', 'éfn [value]', '// fn [value]', '"fn [value]"'] {
		assert fastc_source_has_capture_syntax(source), source
	}
}

fn test_fastc_capture_candidates_preserve_lexical_import_detection() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	for body in ['@fn := [1]\n println(@fn [0])', '// fn [value] () {}', '/* fn [value] () {} */',
		'text := "fn [value] () {}"'] {
		source := 'module main\nfn main() {\n${body}\n}\n'
		assert fastc_source_has_capture_syntax(source), body
		assert !fastc_source_uses_captures(source, prefs), body
		header := fastc_scan_source_header(source, 'capture_candidate.v', prefs) or { panic(err) }
		assert '__fastc_closure' !in header.imports, body
	}
	for between in [' ', '\t', '\n', '/* receiver */', '// receiver\n', '/* outer /* nested */ tail */\n',
		'/* /*/', '/* //*/'] {
		source := 'module main\nfn main() {\n value := 1\n callback := fn${between}[value] () int { return value }\n _ = callback()\n}\n'
		header := fastc_scan_source_header(source, 'capture_candidate.v', prefs) or { panic(err) }
		assert header.imports['__fastc_closure'] == 'builtin.closure', between
		assert header.import_order == ['builtin.closure'], between
	}
}

fn test_fastc_source_flags_accept_block_comment_closing_idioms() {
	for between in ['/* /*/', '/* //*/'] {
		conditional := '$' + between + 'if true { fn selected() {} }'
		assert fastc_source_scan_flags(conditional).has_comptime_if, conditional
		for source in ['fn ${between}pick[T](value T) T { return value }',
			'fn pick${between}[T](value T) T { return value }',
			'fn (state${between}State) pick[T](value T) T { return value }',
			'fn (state State)${between}pick[T](value T) T { return value }'] {
			assert fastc_source_scan_flags(source).has_generic_fn_syntax, source
		}
	}
}

fn test_fastc_capture_comment_closing_idioms_compile_and_run() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_capture_comment_closers_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
fn main() {
	value := 21
	first := fn /* /*/ [value] () int { return value + 1 }
	second := fn /* //*/ [value] () int { return value * 2 }
	assert first() == 22
	assert second() == 42
	println("capture comment closures work")
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = os.real_path(os.join_path(@DIR, '../../../..'))
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	c_source := generate_files([root], prefs) or { panic(err) }
	c_path := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_path, c_source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-pthread', '-o', program, c_path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'capture comment closures work', run.output
}

fn test_fastc_sql_candidates_preserve_lexical_import_detection() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	for body in ['sql_stmt := 1', 'sql_connection := 1', 'text := "sql db {}"',
		'// sql db {}\n value := 1', '/* sql db {} */ value := 1', 'connection.sql()'] {
		source := 'module main\nfn main() {\n${body}\n}\n'
		assert !fastc_source_uses_sql(source, prefs), body
		header := fastc_scan_source_header(source, 'sql_candidate.v', prefs) or { panic(err) }
		assert 'orm' !in header.imports, body
	}
	for body in ['sql db { select from User }', 'sql\tdb { select from User }',
		'sql/* connection */db { select from User }'] {
		source := 'module main\nfn main() {\n${body}\n}\n'
		assert fastc_source_uses_sql(source, prefs), body
		header := fastc_scan_source_header(source, 'sql_candidate.v', prefs) or { panic(err) }
		assert header.imports['orm'] == 'orm', body
		assert header.import_order == ['orm'], body
	}
}
