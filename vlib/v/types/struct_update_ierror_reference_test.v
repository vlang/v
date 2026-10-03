module types

import os

fn test_struct_update_cannot_implicitly_box_nested_ierror_reference() {
	root := os.join_path(os.vtmp_dir(), 'struct_update_ierror_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'nested_ierror.v')
	os.write_file(path, 'module main\nstruct UpdateError { value int }\nfn (e UpdateError) msg() string { return "error" }\nfn (e UpdateError) code() int { return e.value }\nfn take(value &&IError) {}\nfn main() { base := UpdateError{value: 1}; take(UpdateError{...base, value: 2}) }\n')!
	result := os.exec([@VEXE, '-check', path])
	assert result.exit_code != 0, result.output
	assert result.output.contains('cannot use `UpdateError` as `&&IError`'), result.output
}
