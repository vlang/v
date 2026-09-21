module parser

import os
import v.pref

fn test_c_aggregate_uppercase_array_fields() {
	path := os.join_path(os.vtmp_dir(), 'c_array_fields_${os.getpid()}.c.v')
	defer { os.rm(path) or {} }
	for kind in ['struct', 'union'] {
		for gap in [' ', ''] {
			source := 'const size = 8
${kind} C.ArrayFields {
mut:
	Data4${gap}[8]u8
	Named${gap}[size]u8
	Computed${gap}[size + 1]u8
	Matrix${gap}[2][4]u8
	Dynamic${gap}[]u8
	@[tag]
	Tagged${gap}[3]u8
	Last int
}
'
			os.write_file(path, source)!
			for is_fmt in [false, true] {
				mut prefs := pref.new_preferences()
				prefs.is_fmt = is_fmt
				mut p := Parser.new(prefs)
				a := p.parse_file(path)
				assert p.diagnostics.len == 0, '${kind}, gap=${gap.len}, fmt=${is_fmt}: ${p.diagnostics}'
				fields := a.nodes.filter(it.kind == .field_decl)
				assert fields.map(it.value) == ['Data4', 'Named', 'Computed', 'Matrix', 'Dynamic',
					'Tagged', 'Last']
				assert fields.map(it.typ) == ['[8]u8', '[size]u8', '[size + 1]u8', '[2][4]u8',
					'[]u8', '[3]u8', 'int']
				for i, field in fields {
					assert field.generic_params() == if i == 5 { ['m', 'tag'] } else { ['m'] }
				}
			}
		}
	}
}
