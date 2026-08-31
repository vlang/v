import os

const no_main_vexe = @VEXE
const no_main_tests_dir = os.dir(@FILE)
const no_main_v3_dir = os.dir(no_main_tests_dir)
const no_main_vlib_dir = os.dir(no_main_v3_dir)
const no_main_v3_src = os.join_path(no_main_v3_dir, 'v3.v')

fn no_main_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_no_main_renamed_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${no_main_vexe} -gc none -path "${no_main_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${no_main_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

// Under `-d no_main` the entry `main` is emitted as the ordinary symbol
// `main__main`. A function calling it must use that name and see a prototype;
// previously the call kept the bare `main` and `main__main` was never
// forward-declared, so the C compiler saw an implicit declaration of `main`
// emitted before its definition.
fn test_no_main_renamed_main_is_declared_and_called_by_symbol() {
	v3_bin := no_main_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_no_main_prog_${os.getpid()}.v')
	out_c := os.join_path(os.temp_dir(), 'v3_no_main_prog_${os.getpid()}.c')
	obj := os.join_path(os.temp_dir(), 'v3_no_main_prog_${os.getpid()}.o')
	defer {
		os.rm(v3_bin) or {}
		os.rm(src) or {}
		os.rm(out_c) or {}
		os.rm(obj) or {}
	}
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

	compile := os.execute('${v3_bin} ${src} -d no_main -b c -o ${out_c}')
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
	strict := os.execute('${cc} -c -std=gnu11 -w -Wmissing-prototypes -Werror ${out_c} -o ${obj}')
	assert strict.exit_code == 0, strict.output
}
