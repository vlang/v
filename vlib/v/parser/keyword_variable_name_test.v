module parser

import os
import v.pref

fn test_statement_keywords_cannot_be_variable_names() {
	path := os.join_path(os.vtmp_dir(), 'keyword_variable_name_${os.getpid()}.v')
	defer {
		os.rm(path) or {}
	}
	for name in ['go', 'spawn', 'match', 'fn'] {
		for use_name in [false, true] {
			argument := if use_name { name } else { "'done'" }
			source := 'fn main() {\n\t${name} := 1\n\tprintln(${argument})\n}\n'
			os.write_file(path, source) or { panic(err) }
			for is_fmt in [false, true] {
				mut prefs := pref.new_preferences()
				prefs.is_fmt = is_fmt
				mut p := Parser.new(prefs)
				p.parse_file(path)
				assert p.diagnostics.len > 0
				first := p.diagnostics[0]
				assert first.message == 'cannot use keyword `${name}` as a variable name'
				assert first.line == 2
				assert source[first.pos.offset..first.pos.end] == name
				if !use_name {
					assert p.diagnostics.len == 1, p.diagnostics.str()
				}
			}
		}
	}
}
