import v.parser
import v.table
import v.ast
import v.pref
import os
import flag

fn main() {
	mut fp := flag.new_flag_parser(os.args)
	path := fp.string('path', `p`, '', 'path to the file which will be parsed')
	cut_index := fp.int('index', 0, 1, 'index of the source file where the rest will be cut away')
	table := table.new_table()
	scope := &ast.Scope{
		parent: 0
	}
	mut pref := &pref.Preferences{
		output_mode: .silent
	}
	mut source := os.read_file(path)?
	source = source[..cut_index]
	_ := parser.parse_text(source, path, table, .skip_comments, pref, scope)
}