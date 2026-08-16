import os

const borrowed_array_clone_vexe = @VEXE
const borrowed_array_clone_tests_dir = os.dir(@FILE)
const borrowed_array_clone_v3_dir = os.dir(borrowed_array_clone_tests_dir)
const borrowed_array_clone_vlib_dir = os.dir(borrowed_array_clone_v3_dir)
const borrowed_array_clone_v3_src = os.join_path(borrowed_array_clone_v3_dir, 'v3.v')

fn test_clone_of_array_returned_by_borrow_does_not_address_pointer_rvalue() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_borrowed_array_clone_test')
	build :=
		os.execute('${borrowed_array_clone_vexe} -gc none -d ownership -path "${borrowed_array_clone_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${borrowed_array_clone_v3_src}')
	assert build.exit_code == 0, build.output

	source := os.join_path(os.temp_dir(), 'v3_borrowed_array_clone_input.v')
	os.write_file(source,
		"import sync.arc\n\nstruct Holder {\n\tvalues []string\n}\n\nfn (holder &^a Holder) get[^a]() &^a []string {\n\treturn &holder.values\n}\n\nfn main() {\n\tholder := Holder{values: ['one', 'two']}\n\tcloned := holder.get().clone()\n\tassert cloned == ['one', 'two']\n\towner := arc.new(['three', 'four'])\n\tshared_clone := owner.get().clone()\n\tassert shared_clone == ['three', 'four']\n\tprintln('ok')\n}\n")!
	out := os.execute('${v3_bin} -ownership -d ownership -no-parallel run ${source}')
	assert out.exit_code == 0, out.output
	assert out.output.contains('\nok\n'), out.output
}
