import v.ast
import v.parser
import v.pref

fn test_parser_map_type() {
	result := parser.parse_text('a := map[*Node]bool', '', ast.new_table(), .parse_comments,
		&pref.Preferences{
		output_mode: .silent
		is_fmt: true
	})
	println(result)
	assert true
}
