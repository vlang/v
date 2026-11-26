import os
import v.tempname

fn test_generic_heap_pointer_can_be_stored_safely() {
	dir := os.join_path(os.vtmp_dir(), 'v3_generic_heap_pointer_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'v3_generic_heap_pointer_input.v')
	os.write_file(source,
		"@[heap]\nstruct Cell[T] {\n\tvalue T\n}\n\nstruct Holder[T] {\n\tcell &Cell[T]\n}\n\nfn hold[T](cell &Cell[T]) int {\n\tholder := Holder[T]{\n\t\tcell: cell\n\t}\n\treturn holder.cell.value\n}\n\nfn main() {\n\tcell := &Cell[int]{\n\t\tvalue: 41\n\t}\n\tassert hold[int](cell) == 41\n\tprintln('ok')\n}\n")!
	out := os.exec([@VEXE, '-no-parallel', 'run', source])
	assert out.exit_code == 0, out.output
	// `run` prints no C compiler lines before the program output any more.
	assert out.output.split_into_lines().any(it == 'ok'), out.output
}
