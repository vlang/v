module parser

import v.pref

fn test_cached_header_preserves_exported_function_names() {
	for extension in ['v', 'vh'] {
		body := if extension == 'vh' { '' } else { ' { return value }' }
		mut p := Parser.new(pref.new_preferences())
		a := p.parse_text('cached.${extension}', 'module cached
@[export: "native_callback"]
fn callback(value int) int${body}
fn ordinary(value int) int${body}
')
		assert p.diagnostics.len == 0, p.diagnostics.str()
		assert a.export_fn_names == {
			'cached.callback': 'native_callback'
		}
		assert p.export_records.len == 1
		assert p.export_records[0].qname == 'cached.callback'
		assert p.export_records[0].value == 'native_callback'
	}
}
