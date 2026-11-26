import os
import v.tempname

// Under `-d no_main` the entry `main` is emitted as the ordinary symbol
// `main__main`. A function calling it must use that name and see a prototype;
// previously the call kept the bare `main` and `main__main` was never
// forward-declared, so the C compiler saw an implicit declaration of `main`
// emitted before its definition.
fn test_no_main_renamed_main_is_declared_and_called_by_symbol() {
	root := os.join_path(os.vtmp_dir(), 'v3_no_main_renamed_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'v3_no_main_prog.v')
	out_c := os.join_path(root, 'v3_no_main_prog.c')
	obj := os.join_path(root, 'v3_no_main_prog.o')
	os.write_file(src, "@[export: 'invoke']
fn invoke() {
	main()
}

fn main() {
	println('in main')
}
") or {
		panic(err)
	}

	compile := os.exec([@VEXE, '${src}', '-d', 'no_main', '-b', 'c', '-o', '${out_c}'])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('cannot be called'), compile.output

	c_source := os.read_file(out_c) or { panic(err) }
	// The renamed entry is forward-declared and defined as `main__main`.
	assert c_source.contains('void main__main(void);'), c_source
	assert c_source.contains('void main__main(void) {'), c_source
	// The caller uses the renamed symbol, not the bare `main`.
	assert c_source.contains('\tmain__main();'), c_source
	assert !c_source.contains('\tmain();'), c_source

	// The renamed function has a prototype, so strict C compilation of the unit
	// succeeds (no implicit declaration / missing prototype).
	cc := os.getenv_opt('CC') or { 'cc' }
	strict := os.exec([cc, '-c', '-std=gnu11', '-w', '-Wmissing-prototypes', '-Werror', '${out_c}',
		'-o', '${obj}'])
	assert strict.exit_code == 0, strict.output
}
