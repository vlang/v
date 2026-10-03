module types

import os

fn test_ordinary_alias_parameters_keep_their_value_type() {
	root := os.join_path(os.vtmp_dir(), 'translated_pointer_params_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'translated.v'), '@[translated]\nmodule main\nfn translated() {}\n')!
	os.write_file(os.join_path(root, 'main.v'), 'module main
type IntPtr = &int
fn read(mut pointer IntPtr) int { return **pointer }
fn main() {
	value := 42
	mut pointer := &value
	_ = read(mut pointer)
	translated()
}
')!
	for flags in ['', '-no-parallel -nocache'] {
		result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), '-check', root])
		assert result.exit_code != 0, result.output
		assert result.output.contains('invalid indirect of `int`'), result.output
	}
}
