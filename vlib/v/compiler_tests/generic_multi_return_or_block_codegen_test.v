import os
import v.tempname

// A specialized generic call returning `!(&T, map[K]V)` used with `or {}` or `!`
// must keep its whole multi-return payload type. Treating that text as a generic
// application truncated it at the member map's `]`, so codegen declared the
// fallback temporary with a `multi_return_..._int` type that was never emitted.
fn test_generic_multi_return_or_block_keeps_map_member_type() {
	dir := os.join_path(os.vtmp_dir(), 'v3_generic_multi_return_or_block_codegen_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer { os.rmdir_all(dir) or {} }
	os.write_file(os.join_path(dir, 'main.v'), "module main\n\nstruct Node {\n\tname string\n}\n\nfn evaluate[T](root &Node, model T) !(&Node, map[string]int) {\n\tif root.name == '' {\n\t\treturn error('empty')\n\t}\n\tmut events := map[string]int{}\n\tevents['seen'] = model.len\n\treturn root, events\n}\n\nfn with_or(root &Node) string {\n\tresolved, events := evaluate(root, 'ab') or { return 'fallback' }\n\treturn resolved.name + ':' + events['seen'].str()\n}\n\nfn with_propagation(root &Node) !string {\n\tresolved, events := evaluate(root, 'abc')!\n\treturn resolved.name + ':' + events['seen'].str()\n}\n\nfn main() {\n\tassert with_or(&Node{ name: 'root' }) == 'root:2'\n\tassert with_or(&Node{ name: '' }) == 'fallback'\n\tassert with_propagation(&Node{ name: 'root' })! == 'root:3'\n\tif _ := with_propagation(&Node{ name: '' }) {\n\t\tassert false\n\t}\n}\n") or { panic(err) }
	program := os.join_path(dir, 'program')
	result := os.exec([@VEXE, '-b', 'c', '-o', program, os.join_path(dir, 'main.v')])
	assert result.exit_code == 0, result.output
	run := os.exec([program])
	assert run.exit_code == 0, run.output
}
