module parser

import os
import strings
import v.pref
import v.workers

fn source_texts_test_root(name string) string {
	root := os.join_path(os.vtmp_dir(), 'v3_parser_source_texts_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	return root
}

fn fn_decl_names(p &Parser) []string {
	mut names := []string{}
	for node in p.a.nodes {
		if node.kind == .fn_decl {
			names << node.value
		}
	}
	return names
}

// A parser that is asked to keep its sources records the text of each file it
// parsed under the id of that file; by default it records none.
fn test_keep_source_texts_records_the_text_of_each_parsed_file() {
	root := source_texts_test_root('keep')
	defer {
		os.rmdir_all(root) or {}
	}
	first := os.join_path(root, 'first.v')
	second := os.join_path(root, 'second.v')
	first_text := 'module main\n\nfn first() {}\n'
	second_text := 'module main\n\nfn second() {}\n'
	os.write_file(first, first_text)!
	os.write_file(second, second_text)!
	mut p := Parser.new(pref.new_preferences())
	p.keep_source_texts = true
	p.parse_files_with_starts([first, second])
	assert p.a.source_texts.len == 2
	for file_id, file in p.a.source_files {
		expected := if file.name == first { first_text } else { second_text }
		assert p.a.source_texts[file_id] == expected, file.name
	}
	// The texts outlive the buffers that parsing drops.
	p.release_source_storage()
	assert p.a.source_texts.len == 2
	mut plain := Parser.new(pref.new_preferences())
	plain.parse_files_with_starts([first, second])
	assert plain.a.source_texts.len == 0
}

// A preloaded source is parsed instead of the contents of the file, so the
// file is not read: here it does not even exist.
fn test_preload_source_is_parsed_in_place_of_the_file() {
	root := source_texts_test_root('preload')
	defer {
		os.rmdir_all(root) or {}
	}
	on_disk := os.join_path(root, 'on_disk.v')
	os.write_file(on_disk, 'module main\n\nfn from_disk() {}\n')!
	missing := os.join_path(root, 'missing.v')
	mut p := Parser.new(pref.new_preferences())
	p.keep_source_texts = true
	p.preload_source(missing, 'module main\n\nfn preloaded() {}\n')
	p.parse_files_with_starts([missing, on_disk])
	assert p.diagnostics.len == 0
	assert fn_decl_names(p) == ['preloaded', 'from_disk']
	mut kept := []string{}
	for file_id, file in p.a.source_files {
		kept << '${os.file_name(file.name)}:${p.a.source_texts[file_id].len}'
	}
	kept.sort()
	assert kept == ['missing.v:31', 'on_disk.v:31']
	// Once the preloaded texts are dropped, the path is read from disk again.
	p.clear_preloaded_sources()
	assert p.preloaded_sources.len == 0
	mut again := Parser.new(pref.new_preferences())
	again.parse_files_with_starts([missing])
	assert again.diagnostics.len == 1
	assert again.diagnostics[0].message.starts_with('error reading source')
}

// declared_texts lists the declarations and string literals of a parsed
// program, the part of it that the selected compile-time branches decide.
fn declared_texts(p &Parser) []string {
	mut texts := []string{}
	for node in p.a.nodes {
		if node.kind in [.fn_decl, .const_field, .string_literal] {
			texts << '${node.kind}:${node.value}'
		}
	}
	return texts
}

// A template in an early chunk of a parallel parse takes file ids of its own,
// so the ids of the files in the later chunks move. The kept texts have to move
// with them: each one stays the text of the file that has its id.
fn test_parallel_parse_keeps_each_text_under_the_id_of_its_file() {
	root := source_texts_test_root('parallel_ids')
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'worker.txt'), 'first\nsecond\n')!
	mut files := []string{cap: 4}
	mut texts := map[string]string{}
	for file_index in 0 .. 4 {
		mut src := strings.new_builder(48_000)
		src.writeln('module main')
		src.writeln('')
		if file_index == 0 {
			src.writeln('fn templated_action() string {')
			src.writeln("\treturn \$tmpl('worker.txt')")
			src.writeln('}')
			src.writeln('')
		}
		for i in 0 .. 800 {
			src.writeln('fn kept_text_padding_${file_index}_${i}() int { return ${i} }')
		}
		path := os.join_path(root, '${file_index}.v')
		text := src.str()
		os.write_file(path, text)!
		files << path
		texts[path] = text
	}
	mut p := Parser.new(pref.new_preferences())
	p.keep_source_texts = true
	p.a.worker_pool = workers.new(3)
	_, was_parallel := p.parse_files_dispatch(files, true)
	assert was_parallel
	// The template took ids between those of the first file and of the others.
	assert p.a.source_files.len > files.len
	mut kept := []string{}
	for file_id, text in p.a.source_texts {
		file := p.a.source_files[file_id] or { panic('the kept text ${file_id} has no file') }
		assert text == texts[file.name], file.name
		kept << file.name
	}
	kept.sort()
	assert kept == files
}

// The parallel const prepasses scan the sources before the workers parse them.
// They have to scan the text that is parsed, the preloaded one, or the workers
// start from constants that the parsed program does not declare. The file on
// the disk declares another value here, as it would after an edit.
fn test_parallel_const_prepasses_scan_the_preloaded_sources() {
	root := source_texts_test_root('parallel_prepass')
	defer {
		os.rmdir_all(root) or {}
	}
	contents := [
		"const route_has_get_method = 'GET /users'.starts_with('GET')\n",
		"\$if route_has_get_method { const chosen = 'yes' } \$else { const chosen = 'no' }\n",
		"\$if chosen == 'yes' { fn selected() string { return 'yes' } } \$else { fn selected() string { return 'no' } }\n",
		'fn main() { println(selected()) }\n',
	]
	on_disk := "const chosen = 'no'\n"
	mut paths := []string{}
	mut sources := []string{}
	for i, content in contents {
		path := os.join_path(root, '${i}.v')
		module_content := 'module main\n' + content
		source := module_content + '\n'.repeat(40000 - module_content.len)
		disk_content := if i == 1 { 'module main\n' + on_disk } else { module_content }
		os.write_file(path, disk_content + '\n'.repeat(40000 - disk_content.len))!
		paths << path
		sources << source
	}
	mut parsed := [][]string{}
	for parallel in [false, true] {
		mut p := Parser.new(pref.new_preferences())
		for i, path in paths {
			p.preload_source(path, sources[i])
		}
		if parallel {
			p.a.worker_pool = workers.new(3)
		}
		_, used_parallel := p.parse_files_dispatch(paths, parallel)
		assert used_parallel == parallel
		assert p.diagnostics.len == 0, p.diagnostics.str()
		p.resolve_comptime_string_declarations()
		assert p.comptime_const_values[comptime_const_value_key('main', 'chosen')] == "'yes'", p.comptime_const_values.str()
		assert p.a.nodes.filter(it.kind == .fn_decl && it.value == 'selected').len == 1
		parsed << declared_texts(p)
	}
	// Both parses select the branches of the preloaded text, not of the file.
	assert parsed[0] == ['string_literal:GET /users', 'string_literal:GET',
		'const_field:route_has_get_method', 'string_literal:yes', 'const_field:chosen',
		'string_literal:yes', 'fn_decl:selected', 'fn_decl:main']
	assert parsed[1] == parsed[0]
}
