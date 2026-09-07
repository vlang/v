import os

// A specialized generic call returning `!(&T, map[K]V)` used with `or {}` or `!`
// must keep its whole multi-return payload type. Treating that text as a generic
// application truncated it at the member map's `]`, so codegen declared the
// fallback temporary with a `multi_return_..._int` type that was never emitted.
fn test_generic_multi_return_or_block_keeps_map_member_type() {
	dir := os.join_path(os.vtmp_dir(), 'v3_generic_multi_return_or_block_codegen')
	v3_bin := os.join_path(dir, 'v3')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	v3_dir := os.dir(os.dir(@FILE))
	vlib_dir := os.dir(v3_dir)
	build := os.execute('${os.quoted_path(@VEXE)} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(v3_bin)} ${os.quoted_path(os.join_path(v3_dir, 'v3.v'))}')
	assert build.exit_code == 0, build.output
	os.write_file(os.join_path(dir, 'main.v'), "module main\n\nstruct Node {\n\tname string\n}\n\nfn evaluate[T](root &Node, model T) !(&Node, map[string]int) {\n\tif root.name == '' {\n\t\treturn error('empty')\n\t}\n\tmut events := map[string]int{}\n\tevents['seen'] = model.len\n\treturn root, events\n}\n\nfn with_or(root &Node) string {\n\tresolved, events := evaluate(root, 'ab') or { return 'fallback' }\n\treturn resolved.name + ':' + events['seen'].str()\n}\n\nfn with_propagation(root &Node) !string {\n\tresolved, events := evaluate(root, 'abc')!\n\treturn resolved.name + ':' + events['seen'].str()\n}\n\nfn main() {\n\tassert with_or(&Node{ name: 'root' }) == 'root:2'\n\tassert with_or(&Node{ name: '' }) == 'fallback'\n\tassert with_propagation(&Node{ name: 'root' })! == 'root:3'\n\tif _ := with_propagation(&Node{ name: '' }) {\n\t\tassert false\n\t}\n}\n") or { panic(err) }
	program := os.join_path(dir, 'program')
	result := os.execute('${os.quoted_path(v3_bin)} -b c -o ${os.quoted_path(program)} ${os.quoted_path(os.join_path(dir, 'main.v'))}')
	assert result.exit_code == 0, result.output
	run := os.execute(program)
	assert run.exit_code == 0, run.output
	os.rmdir_all(dir) or {}
}
