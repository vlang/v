module main

import os
import v.scanner
import v.token
import flag

fn main() {
	mut fp := flag.new_flag_parser(os.args#[2..])
	fp.application('v scan')
	fp.version('0.0.1')
	fp.description('\nScan .v source files, and print the V tokens contained in them.')
	fp.arguments_description('PATH [PATH]...')
	fp.limit_free_args_to_at_least(1)!
	mut all_paths := fp.remaining_parameters()
	for path in all_paths {
		content := os.read_file(path) or {
			eprintln('> could not read: ${path}, skipping; err: ${err}')
			continue
		}
		mut scanner_ := scanner.new_silent_scanner()
		scanner_.prepare_for_new_text(content)
		scanner_.is_fmt = false
		scanner_.pref.output_mode = .stdout
		scanner_.comments_mode = .skip_comments
		mut tok := token.Token{}
		for tok.kind != .eof {
			tok = scanner_.text_scan()
			pos := tok.pos()
			location := '${path}:${pos.line_nr + 1}:${pos.col + 1}:'
			println('${location:-32} | pos: ${pos.pos:-5} | ${tok.debug()}')
		}
	}
}
