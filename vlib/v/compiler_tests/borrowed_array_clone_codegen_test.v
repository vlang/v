// vtest ownership
import os
import v.cmdexec
import v.tempname

fn test_clone_of_array_returned_by_borrow_does_not_address_pointer_rvalue() {
	shared := os.getenv('V3_TEST_UNIT_SHARED_V3_OWNERSHIP')
	v3_src := os.join_path(os.dir(@FILE), '../v.v')
	compiler := if os.is_file(shared) && os.is_executable(shared)
		&& os.real_path(os.getenv('V3_TEST_UNIT_V3_SRC')) == os.real_path(v3_src) {
		os.real_path(shared)
	} else {
		@VEXE
	}
	root := os.join_path(os.vtmp_dir(), 'v3_borrowed_array_clone_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'v3_borrowed_array_clone_input.v')
	os.write_file(source,
		"import sync.arc\n\nstruct Holder {\n\tvalues []string\n}\n\nfn (holder &^a Holder) get[^a]() &^a []string {\n\treturn &holder.values\n}\n\nfn main() {\n\tholder := Holder{values: ['one', 'two']}\n\tcloned := holder.get().clone()\n\tassert cloned == ['one', 'two']\n\towner := arc.new(['three', 'four'])\n\tshared_clone := owner.get().clone()\n\tassert shared_clone == ['three', 'four']\n\tprintln('ok')\n}\n")!
	out := cmdexec.run_in_merged_with_env(compiler, ['-ownership', '-d', 'ownership', '-no-parallel',
		'run', source], '', {
		'VFLAGS':               ''
		'VTOOLS_CACHE_VERBOSE': ''
	})
	assert out.exit_code == 0, out.output
	assert out.output.trim_space() == 'ok', out.output
}
