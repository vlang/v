module types

import os

fn test_interface_context_preserves_mutability_checks() {
	root := os.join_path(os.vtmp_dir(), 'v3_interface_context_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	prefix := 'interface Layout { mut: id int }
interface Widget { mut: id int }
struct Item { mut: id int }
fn (mut w Widget) change() { w.id = 42 }
'
	for i, body in [
		'l := Layout(Item{id: 1}); mut w := mut l as Widget; w.id = 2',
		'l := Layout(Item{id: 1}); if l is Item { Widget(l).change() }',
		'mut l := Layout(Item{id: 1}); if l is Item { println(l.id) }',
		'mut l := Layout(Item{id: 1}); if l is Item && l.id > 0 { println(l.id) }',
		'mut l := Layout(Item{id: 1}); if !(l is Item) || false {} else { println(l.id) }',
		'mut l := Layout(Item{id: 1}); if l !is Item {} else { println(l.id) }',
	] {
		path := os.join_path(root, 'invalid_${i}.v')
		os.write_file(path, prefix + 'fn main() { ${body} }
')!
		result := os.execute('${os.quoted_path(@VEXE)} -check ${os.quoted_path(path)}')
		assert result.exit_code != 0, result.output
		assert result.output.contains('immutable') || result.output.contains('smart casting'), result.output
	}
}
