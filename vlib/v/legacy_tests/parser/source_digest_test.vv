module parser

import os
import crypto.sha256
import v.ast
import v.pref

fn test_parse_file_captures_exact_retry_source_digest() {
	root := os.join_path(os.vtmp_dir(), 'v_parser_source_digest_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	path := os.join_path(root, 'main.v')
	parsed_source := 'module main\nfn main() { println(42) }\n'
	os.write_file(path, parsed_source)!
	mut prefs := pref.new_preferences()
	prefs.capture_source_digests = true
	mut table := ast.new_table()
	file := parse_file(path, mut table, .skip_comments, prefs)
	os.write_file(path, parsed_source.replace('42', 'changed'))!
	assert file.source_digest == sha256.hexhash(parsed_source)
}
