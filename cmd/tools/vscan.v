module main

import os
import v.scanner
import v.pref
import v.token
import flag

fn main() {
	mut fp := flag.new_flag_parser(os.args#[2..])
	fp.application('v scan')
	fp.version('0.0.1')
	fp.description('\nScan .v source files, and print the V tokens contained in them.')
	fp.arguments_description('PATH [PATH]...')
	fp.limit_free_args_to_at_least(1)?
	pref := pref.new_preferences()
	mut all_paths := fp.remaining_parameters()
	for path in all_paths {
		mut scanner := scanner.new_scanner_file(path, .parse_comments, pref)?
		mut tok := token.Token{}
		for tok.kind != .eof {
			tok = scanner.scan()
			pos := tok.pos()
			location := '$path:${pos.line_nr + 1}:${pos.col + 1}:'
			println('${location:-32} | pos: ${pos.pos:-5} | $tok.debug()')
		}
	}
}
