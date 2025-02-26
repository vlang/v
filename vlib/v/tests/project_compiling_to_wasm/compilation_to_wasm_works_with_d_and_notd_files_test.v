import os

const vexe = os.quoted_path(@VEXE)
const project_folder = os.dir(@FILE)
const output_path = os.join_path(os.vtmp_dir(), 'check_wasm_works')

fn testsuite_begin() {
	os.mkdir_all(output_path) or {}
	os.chdir(output_path)!
	dump(os.getwd())
}

fn testsuite_end() {
	os.system('ls -la .')
	os.chdir(os.home_dir()) or {}
	os.rmdir_all(output_path) or {}
}

fn test_normal() {
	if os.user_os() == 'windows' {
		return
	}
	defer { println('done ${@FN}') }
	dump(vexe)
	res := os.system('${os.quoted_path(vexe)} -o normal.exe ${os.quoted_path(project_folder)}')
	assert res == 0
	dump(res)
	assert os.exists('normal.exe')
	content := os.read_file('normal.exe')!
	assert content.contains('This is abc_notd_wasm32_emscripten.c.v')
}

fn test_emcc() {
	if os.user_os() == 'windows' {
		return
	}
	defer { println('done ${@FN}') }
	emcc := os.find_abs_path_of_executable('emcc') or {
		println('skipping ${@FN} since `emcc` is not found')
		return
	}
	dump(emcc)
	res := os.system('${os.quoted_path(vexe)} -os wasm32_emscripten -o wasm_check.html ${os.quoted_path(project_folder)}')
	assert res == 0
	dump(res)
	assert os.exists('wasm_check.html')
	assert os.exists('wasm_check.js')
	assert os.exists('wasm_check.wasm')
	content := os.read_file('wasm_check.wasm')!
	assert content.contains('This is abc_d_wasm32_emscripten.c.v')
}
