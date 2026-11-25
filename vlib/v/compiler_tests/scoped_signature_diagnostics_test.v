import os
import v.cmdexec
import v.tempname

// A parallel check runs the top-level signature checks on a fork beside the body
// workers. Besides adding diagnostics they can replace earlier ones: the unknown
// types of a nongeneric declaration give way to its missing generic parameters.
fn test_parallel_signature_checks_merge_the_diagnostics_they_replace() {
	root := os.join_path(os.vtmp_dir(), 'v3_scoped_signature_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	mut source := 'module main\n\nstruct Box {\n\ta T = T{}\n\tb T = T{}\n}\n'
	// Enough functions for the checker to use the worker pool.
	for i in 0 .. 64 {
		source += '\nfn pad_${i}() int {\n\treturn ${i}\n}\n'
	}
	source += '\nfn main() {}\n'
	source_path := os.join_path(root, 'signature.v')
	os.write_file(source_path, source)!
	output_path := os.join_path(root, 'signature.c')
	res := cmdexec.run_in_merged_with_env(@VEXE, ['-nocache', '-o', output_path, source_path], '', {
		'VJOBS': '4'
	})
	assert res.exit_code != 0, res.output
	assert !res.output.contains('V panic'), res.output
	errors := res.output.split_into_lines().filter(it.contains(': error: ')).map(it.all_after(': error: '))
	assert errors == ['cannot initialize field `a` with `void`; expected `T`',
		'cannot initialize field `b` with `void`; expected `T`',
		'generic struct `Box` declaration must specify the generic type names, e.g. Box[T]'], res.output
}
