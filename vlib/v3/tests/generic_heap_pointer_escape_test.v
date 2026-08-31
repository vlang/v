import os

const generic_heap_vexe = @VEXE
const generic_heap_tests_dir = os.dir(@FILE)
const generic_heap_v3_dir = os.dir(generic_heap_tests_dir)
const generic_heap_vlib_dir = os.dir(generic_heap_v3_dir)
const generic_heap_v3_src = os.join_path(generic_heap_v3_dir, 'v3.v')

fn test_generic_heap_pointer_can_be_stored_safely() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_generic_heap_pointer_test')
	build :=
		os.execute('${generic_heap_vexe} -gc none -path "${generic_heap_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${generic_heap_v3_src}')
	assert build.exit_code == 0, build.output

	source := os.join_path(os.temp_dir(), 'v3_generic_heap_pointer_input.v')
	os.write_file(source,
		"@[heap]\nstruct Cell[T] {\n\tvalue T\n}\n\nstruct Holder[T] {\n\tcell &Cell[T]\n}\n\nfn hold[T](cell &Cell[T]) int {\n\tholder := Holder[T]{\n\t\tcell: cell\n\t}\n\treturn holder.cell.value\n}\n\nfn main() {\n\tcell := &Cell[int]{\n\t\tvalue: 41\n\t}\n\tassert hold[int](cell) == 41\n\tprintln('ok')\n}\n")!
	out := os.execute('${v3_bin} -no-parallel run ${source}')
	assert out.exit_code == 0, out.output
	assert out.output.contains('\nok\n'), out.output
}
