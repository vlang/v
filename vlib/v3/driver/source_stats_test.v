module driver

import os
import v3.token

fn test_source_line_counts_use_parsed_buffers_and_preserve_final_newline_rules() {
	mut set := token.FileSet.new()
	mut files := map[int]&token.File{}
	mut paths := []string{}
	for i, source in ['', 'one', 'one\n', '\n', 'one\ntwo', 'one\ntwo\n', 'one\n\n'] {
		path := 'parsed_source_${i}.v'
		mut file := set.add_file(path, source.len)
		file.index_lines(source)
		files[i] = file
		paths << path
	}
	assert source_file_line_count(paths, files) == 9
	assert source_file_line_count([paths[1], paths[1]], files) == 2
	assert source_file_line_count([]string{}, files) == 0
}

fn test_source_line_counts_read_files_without_a_parser_index() {
	path := os.join_path(os.temp_dir(), 'v3_source_stats_${os.getpid()}.v')
	defer {
		os.rm(path) or {}
	}
	os.write_file(path, 'one\ntwo\n') or { panic(err) }
	assert source_file_line_count([path], map[int]&token.File{}) == 2
	files := {
		1: token.File.unindexed(path, 8)
	}
	assert source_file_line_count([path], files) == 2
}
