import os

const implicit_closure_vexe = @VEXE

fn implicit_closure_program(name string, source string) string {
	root := os.join_path(os.vtmp_dir(), 'v3_implicit_closure_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	main_file := os.join_path(root, 'main.v')
	os.write_file(main_file, source) or { panic(err) }
	return main_file
}

// closure_runtime_files_of returns the sources of `builtin.closure` among the
// files that the program in `main_file` is made of.
fn closure_runtime_files_of(main_file string) []string {
	listed := os.exec([implicit_closure_vexe, '-nocache', '-silent', '-print-v-files', main_file])
	assert listed.exit_code == 0, listed.output
	return listed.output.split_into_lines().filter(it.replace('\\', '/').contains('/builtin/closure/'))
}

// builtin and the modules that it imports are part of every program: a value in
// them that the import scan cannot tell from a bound method loads the closure
// runtime everywhere.
fn test_a_program_without_closures_does_not_load_the_closure_runtime() {
	main_file := implicit_closure_program('hello', "fn main() {
	println('hello')
}
")
	defer {
		os.rmdir_all(os.dir(main_file)) or {}
	}
	assert closure_runtime_files_of(main_file) == []
}

// A name that a branch of a function declares with several others, and another
// branch with a value of its own, is of the type of neither.
fn test_a_program_with_a_bound_method_of_a_name_declared_twice_loads_the_closure_runtime() {
	main_file := implicit_closure_program('bound_method', "struct Item {}

fn (item Item) len() int {
	return 7
}

fn pair() (string, int) {
	return 'text', 1
}

fn use(flag bool, items map[string]Item) int {
	if flag {
		value, _ := pair()
		return value.len
	} else {
		value := items['a']
		callback := value.len
		return callback()
	}
}

fn main() {
	println(use(true, {}) + use(false, {
		'a': Item{}
	}))
}
")
	defer {
		os.rmdir_all(os.dir(main_file)) or {}
	}
	assert closure_runtime_files_of(main_file).len > 0
	ran := os.exec([implicit_closure_vexe, '-nocache', 'run', main_file])
	assert ran.exit_code == 0, ran.output
	assert ran.output.trim_space() == '11', ran.output
}
