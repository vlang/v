module parser

import os
import v.pref

fn test_shared_address_argument_reports_one_actionable_diagnostic() {
	path := os.join_path(os.vtmp_dir(), 'shared_address_argument_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	source := 'fn worker(shared values &map[string]int) {}
fn main() {
 shared values := map[string]int{}
 task := spawn worker(ARGUMENT)
 task.wait()
}
'
	for argument in ['shared &values', 'shared & values'] {
		os.write_file(path, source.replace('ARGUMENT', argument))!
		mut p := Parser.new(pref.new_preferences())
		p.parse_file(path)
		assert p.diagnostics.len == 1, p.diagnostics.str()
		assert p.diagnostics[0].message == 'shared address arguments are not supported; remove `&` from the shared parameter type and argument'
		mut prefs := pref.new_preferences()
		prefs.is_fmt = true
		mut formatter := Parser.new(prefs)
		ast := formatter.parse_file(path)
		assert formatter.diagnostics.len == 0, formatter.diagnostics.str()
		mut preserved_address := false
		for node in ast.nodes {
			if node.kind == .prefix && node.value == 'shared' && node.children_count == 1 {
				child := ast.child_node(&node, 0)
				preserved_address = child.kind == .prefix && child.op == .amp
			}
		}
		assert preserved_address
	}
}

fn test_shared_identifier_bitwise_arguments_still_parse() {
	path := os.join_path(os.vtmp_dir(), 'shared_identifier_bitwise_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, 'fn consume(value int) {}
fn main() {
 shared := 3
 mask := 1
 consume(shared & mask)
 consume(shared & 1)
 consume(shared &mask)
}
')!
	mut p := Parser.new(pref.new_preferences())
	ast := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	assert ast.nodes.filter(it.kind == .infix && it.op == .amp).len == 3
}
