module types

import os
import v.parser
import v.pref

// The checker indexes the text of every parsed source. When the parser kept
// the text it read, the checker takes that one and does not read the file: it
// is gone from the disk here, and its text and import lines are still known.
fn test_collect_takes_the_source_texts_the_parser_kept() {
	root := os.join_path(os.vtmp_dir(), 'v3_types_kept_source_texts_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	path := os.join_path(root, 'main.v')
	source := 'module main\n\nimport os, strings\n\nfn main() {}\n'
	os.write_file(path, source)!
	mut p := parser.Parser.new(pref.new_preferences())
	p.keep_source_texts = true
	p.parse_files_with_starts([path])
	p.release_source_storage()
	os.rm(path)!
	mut file_id := -1
	for id, file in p.a.source_files {
		if file.name == path {
			file_id = id
		}
	}
	assert file_id >= 0
	mut tc := TypeChecker.new(p.a)
	tc.index_multiple_module_import_lines(p.a)
	assert tc.source_texts_by_file[path] == source
	assert tc.multiple_module_import_lines[multiple_module_import_line_key(file_id, 3)]
	assert tc.multiple_module_import_lines.len == 1
}

// A file whose text the parser did not keep is read from the disk as before.
fn test_collect_reads_the_sources_the_parser_did_not_keep() {
	root := os.join_path(os.vtmp_dir(), 'v3_types_unkept_source_texts_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	path := os.join_path(root, 'main.v')
	source := 'module main\n\nimport os, strings\n'
	os.write_file(path, source)!
	mut p := parser.Parser.new(pref.new_preferences())
	p.parse_files_with_starts([path])
	p.release_source_storage()
	assert p.a.source_texts.len == 0
	mut tc := TypeChecker.new(p.a)
	tc.index_multiple_module_import_lines(p.a)
	assert tc.source_texts_by_file[path] == source
	assert tc.multiple_module_import_lines.len == 1
}
