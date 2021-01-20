import os

const (
	test_dir   = os.join_path('vlib', 'v', 'gen', 'js', 'tests')
	output_dir = '_js_tests/'
	v_options  = '-b js -w'
)

fn testsuite_end() {
	os.rmdir_all(output_dir)
}

fn test_example_compilation() {
	vexe := os.getenv('VEXE')
	os.chdir(os.dir(vexe))
	os.mkdir_all(output_dir)
	files := find_test_files()
	for file in files {
		path := os.join_path(test_dir, file)
		println('Testing $file')
		v_code := os.system('$vexe $v_options -o ${output_dir}${file}.js $path')
		if v_code != 0 { assert false } // Compilation failed
		js_code := os.system('node ${output_dir}${file}.js')
		if js_code != 0 { assert false } // Running failed
	}
}

fn find_test_files() []string {
	files := os.ls(test_dir) or { panic(err) }
	// The life example never exits, so tests would hang with it, skip
	mut tests := files.filter(it.ends_with('.v')).filter(it != 'life.v')
	tests.sort()
	return tests
}
