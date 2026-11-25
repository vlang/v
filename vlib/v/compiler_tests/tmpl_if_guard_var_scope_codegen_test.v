import os
import v.tempname

// A template lowered into an IIFE (here because `$tmpl()` is a subexpression of `+`) that opens
// an `@if found := find(items)` guard, references `@{found}` inside the guarded block, and then
// references the outer `@{item}` AFTER the guard must keep the guard binding local to the block
// and capture the OUTER `item`.
//
// The guard binding used to be named `item` too, shadowing the outer name inside the block.
// 1fa660cb8d made that a `redefinition` error (the V1 rule), so the shadowing form now has to be
// rejected by the parser with a diagnostic in the template, never reach C.
fn test_template_if_guard_var_does_not_shadow_outer_capture_after_guard() {
	root := os.join_path(os.vtmp_dir(), 'v3_tmpl_if_guard_var_scope_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'figs' }\n") or { panic(err) }
	// Guard-bind `found` to the first element, print it inside the block, then print the OUTER
	// `item` after the guard closes.
	os.write_file(os.join_path(root, 't.html'),
		'@if found := find(items)\n@{found}\n@end\n|@{item}\n') or { panic(err) }
	source := "module main\n\nfn find(items []string) ?string {\n\tif items.len > 0 {\n\t\treturn items[0]\n\t}\n\treturn none\n}\n\nfn build(item string, items []string) string {\n\treturn ('[' + \$tmpl('t.html') + ']').replace('\\n', '')\n}\n\nfn main() {\n\tprintln(build('OUTER', ['a', 'b']))\n}\n"
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	bin := os.join_path(root, 'app')
	compile := os.exec([@VEXE, os.join_path(root, 'main.v'), '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	// Guarded block printed the first element, then the OUTER `item` printed after the guard.
	assert run.output.trim_space() == '[a|OUTER]', run.output

	// A guard binding that shadows the outer `item` is a V error reported in the template.
	os.write_file(os.join_path(root, 't.html'),
		'@if item := find(items)\n@{item}\n@end\n|@{item}\n') or { panic(err) }
	shadow := os.exec([@VEXE, os.join_path(root, 'main.v'), '-b', 'c', '-o', bin])
	assert shadow.exit_code != 0, shadow.output
	assert shadow.output.contains('t.html:1:5: error: redefinition of `item`'), shadow.output
	assert !shadow.output.contains('C compilation failed'), shadow.output
}
