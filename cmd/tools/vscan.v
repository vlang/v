module main

import os
import flag
import v.pref
import v.scanner
import v.token

fn main() {
	mut fp := flag.new_flag_parser(os.args#[2..])
	fp.application('v scan')
	fp.version('0.0.1')
	fp.description('\nScan .v source files, and print the V tokens contained in them.')
	fp.arguments_description('PATH [PATH]...')
	fp.limit_free_args_to_at_least(1)!
	all_paths := fp.remaining_parameters()
	for path in all_paths {
		content := os.read_file(path) or {
			eprintln('> could not read: ${path}, skipping; err: ${err}')
			continue
		}
		mut fs := token.FileSet.new()
		mut file := fs.add_file(path, content.len)
		file.index_lines(content)
		prefs := pref.new_preferences()
		mut scanner_ := scanner.new_scanner(prefs, .normal)
		scanner_.init(file, content)
		for {
			kind := scanner_.scan()
			pos := file.position_at(scanner_.pos)
			location := '${path}:${pos.line}:${pos.column}:'
			literal := if scanner_.lit == '' { kind.str() } else { scanner_.lit }
			println('${location:-32} | pos: ${scanner_.pos:-5} | tok: .${kind:-12} | lit: `${literal}`')
			if kind == .eof {
				break
			}
		}
	}
}
