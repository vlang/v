module main

import io
import os
import rand
import v3.parser
import v3.pref

const max_fuzz_input_size = 16 * 1024 * 1024

fn main() {
	if os.args.len > 2 {
		eprintln('usage: scanner_parser [input-file]; without a file, input is read from stdin')
		exit(2)
	}
	data := if os.args.len == 2 {
		os.read_bytes(os.args[1]) or { exit(0) }
	} else {
		io.read_all(reader: os.stdin()) or { exit(0) }
	}
	if data.len > max_fuzz_input_size {
		return
	}
	path := os.join_path(os.temp_dir(), 'v3_scanner_parser_fuzz_${os.getpid()}_${rand.ulid()}.v')
	os.write_file_array(path, data) or { return }
	defer {
		os.rm(path) or {}
	}
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	p.parse_into(path)
	p.release_source_storage()
}
