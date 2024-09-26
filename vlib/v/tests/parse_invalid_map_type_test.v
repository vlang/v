import v.ast
import v.parser
import v.pref

fn test_parser_map_type() {
	mut table := ast.new_table()
	pref_ := pref.Preferences{
		output_mode: .silent
		is_fmt:      true
	}
	result := parser.parse_text('a := map[*Node]bool', '', mut table, .parse_comments,
		pref_)
	println(result)
	assert true
}
