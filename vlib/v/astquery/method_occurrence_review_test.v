module main

import os
import v.astquery

fn test_method_occurrences_point_to_actual_name_tokens() {
	source := "module main\nstruct Host {}\nfn (h Host) hello() int { return 42 }\nfn main() {\n\thello := Host{}\n\tprintln(hello.hello())\n\tbound := hello.hello\n\tprintln(bound())\n\tprintln('hello') // hello\n}\n"
	dir := os.join_path(os.vtmp_dir(), 'astquery_method_positions_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	path := os.join_path(dir, 'main.v')
	os.write_file(path, source)!
	mentions := astquery.references(astquery.parse(path), 'hello')
	assert mentions.len == 6, '${mentions}'
	lines := source.split_into_lines()
	mut positions := map[string]bool{}
	for mention in mentions {
		start := mention.column - 1
		assert lines[mention.line - 1][start..mention.end_column - 1] == 'hello', '${mention}'
		positions['${mention.line}:${mention.column}'] = true
		assert mention.line != 9, '${mention}'
	}
	assert positions['3:13']
	assert positions['6:10'] && positions['6:16']
	assert positions['7:11'] && positions['7:17']
}

fn test_method_occurrences_skip_substrings_of_receiver_identifiers() {
	source := 'module main\nstruct Host {}\nfn (h Host) hello() int { return 42 }\nfn main() {\n\tmyhello := Host{}\n\tprintln(myhello.hello())\n\tbound := myhello.hello\n\tprintln(bound())\n}\n'
	dir := os.join_path(os.vtmp_dir(), 'astquery_method_substrings_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	path := os.join_path(dir, 'main.v')
	os.write_file(path, source)!
	mentions := astquery.references(astquery.parse(path), 'hello')
	assert mentions.len == 3, '${mentions}'
	mut positions := map[string]bool{}
	for mention in mentions {
		positions['${mention.line}:${mention.column}'] = true
	}
	assert positions['3:13']
	assert positions['6:18']
	assert positions['7:19']
}
