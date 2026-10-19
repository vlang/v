module parser

import os
import v.pref

fn test_parse_text_owns_caller_source_storage() {
	expected := "module main\nfn main() { println('hello') }\n"
	mut backing := expected.bytes()
	// Model a caller-owned source buffer reused after parsing.
	borrowed := unsafe { tos(backing.data, backing.len) }
	mut prefs := pref.new_preferences()
	prefs.is_fmt = true
	mut p := Parser.new(prefs)
	a := p.parse_text('memory.v', borrowed)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	assert a.source_buffers[0].str != borrowed.str
	for i in 0 .. backing.len {
		backing[i] = `x`
	}
	assert a.source_buffers[0] == expected
	assert a.formatter_file_sources[1] == expected
	assert a.nodes.any(it.kind == .fn_decl && it.value == 'main')
}

fn test_parse_text_appends_files_with_independent_state_and_spans() {
	mut prefs := pref.new_preferences()
	prefs.is_fmt = true
	mut p := Parser.new(prefs)
	first := 'module first\n@[inline]\nfn one() {}\n'
	a := p.parse_text('first.v', first)
	second := 'module second\nfn two() {}\n'
	b := p.parse_text('second.vh', second)
	assert a == b
	assert p.diagnostics.len == 0, p.diagnostics.str()
	assert p.parsed_v_file_paths == ['first.v']
	assert p.parsed_v_header_file_paths == ['second.vh']
	assert a.source_buffers == [first, second]
	assert a.file_node_ids.len == 4
	assert !a.file_index_incomplete
	functions := a.nodes.filter(it.kind == .fn_decl)
	assert functions.len == 2
	assert functions[0].value == 'one'
	assert functions[1].value == 'two'
	assert functions[0].pos.id == 1
	assert functions[1].pos.id == 2
	first_file := a.source_files[1] or { panic('missing first source file') }
	second_file := a.source_files[2] or { panic('missing second source file') }
	assert first_file.name == 'first.v'
	assert second_file.name == 'second.vh'
	assert a.formatter_file_sources[1] == first
	assert a.formatter_file_sources[2] == second
	// The first file's pending attributes must not leak to its successor.
	assert a.nodes.count(it.kind == .directive) == 1
}

fn test_parse_text_preserves_diagnostics_and_recovers_for_next_file() {
	mut p := Parser.new(pref.new_preferences())
	p.parse_text('broken.v', 'fn broken( {')
	assert p.diagnostics.len > 0
	first_diagnostics := p.diagnostics.clone()
	a := p.parse_text('valid.v', 'module valid\nfn recovered() {}\n')
	assert p.diagnostics == first_diagnostics
	assert p.diagnostics.all(it.file == 'broken.v')
	assert a.file_node_ids.len == 4
	assert !a.file_index_incomplete
	assert a.nodes.any(it.kind == .fn_decl && it.value == 'recovered' && it.pos.id == 2)
}

fn test_parse_text_after_failed_file_read_keeps_incomplete_index() {
	missing := os.join_path(os.vtmp_dir(), 'parse_text_missing_${os.getpid()}.v')
	assert !os.exists(missing)
	mut p := Parser.new(pref.new_preferences())
	p.parse_file(missing)
	assert p.diagnostics.len == 1, p.diagnostics.str()
	assert p.diagnostics[0].message.starts_with('error reading source:')
	assert p.diagnostics[0].file == missing
	assert p.a.file_index_incomplete
	assert p.a.file_node_ids.len == 1
	a := p.parse_text('valid.v', 'fn recovered() {}\n')
	assert p.diagnostics.len == 1
	assert a.file_index_incomplete
	assert a.file_node_ids.len == 3
	assert a.source_buffers.len == 1
	assert a.nodes.any(it.kind == .fn_decl && it.value == 'recovered' && it.pos.id == 2)
}

fn test_parse_text_script_name_preserves_script_mode() {
	source := "println('hello')\n"
	path := os.join_path(os.vtmp_dir(), 'parse_text_script_${os.getpid()}.vsh')
	os.write_file(path, source) or { panic(err) }
	defer {
		os.rm(path) or {}
	}
	mut file_parser := Parser.new(pref.new_preferences())
	file_ast := file_parser.parse_file(path)
	assert file_parser.diagnostics.len == 0, file_parser.diagnostics.str()
	mut p := Parser.new(pref.new_preferences())
	a := p.parse_text(path, source)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	assert a.has_vsh_source
	assert a.has_vsh_source == file_ast.has_vsh_source
	assert a.nodes == file_ast.nodes
	assert a.file_node_ids == file_ast.file_node_ids
	assert a.source_buffers == file_ast.source_buffers
	assert a.nodes.any(it.kind == .import_decl && it.value == 'os')
	assert a.nodes.any(it.kind == .expr_stmt && it.pos.id == 1)
}
