import v.ast
import v.parser
import v.pref

fn test_typed_map_init_is_rejected_in_fmt_mode() {
	source_text := 'module main

const c_cap = 10

fn main() {
	_ := map[string][]u8{cap: c_cap}
}
'
	mut table := ast.new_table()
	prefs := &pref.Preferences{
		is_fmt:      true
		output_mode: .silent
	}
	prog := parser.parse_text(source_text, '', mut table, .skip_comments, prefs)
	assert prog.errors.any(it.message == '`}` expected; explicit `map` initialization does not support parameters')
}
