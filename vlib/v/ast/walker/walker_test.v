import v.ast
import v.ast.walker
import v.parser
import v.table
import v.pref

fn parse_text(text string) ast.File {
	return parser.parse_text(text, '', table.new_table(), .skip_comments, pref.new_preferences(), &ast.Scope{ parent: 0 })
}

// fn test_walk() {}

fn test_inspect() {
	source := '
module main
fn main() {}
	'

	file := parse_text(source)
	walker.inspect(file, 0, fn (node ast.Node, data voidptr) bool {
		if node is ast.Stmt {
			if node is ast.Module {
				eprintln('node is ast module')
				return false
			}
		}
		return true
	})
}
