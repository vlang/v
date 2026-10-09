module parser

import os
import v.pref

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
