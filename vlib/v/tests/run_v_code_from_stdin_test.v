import os

const (
	vexe = os.getenv('VEXE')
)

fn test_vexe_is_set() {
	assert vexe != ''
}

fn pipe_to_v_run() ? {
	cat_cmd := if os.user_os() == 'windows' { 'cmd /c type' } else { 'cat' }
	tmp_v_file := os.join_path(os.real_path(os.temp_dir()), 'generated_piped_program.v')
	// eprintln('>>> tmp_v_file: $tmp_v_file')
	os.write_file(tmp_v_file, 'println(1 + 3)\nprintln("hello")\n') ?
	assert os.is_file(tmp_v_file)
	cmd := '$cat_cmd "$tmp_v_file" | "$vexe" run -'
	res := os.exec(cmd) ?
	// eprintln('>> cmd: $cmd | res: $res')
	assert res.exit_code == 0
	assert res.output.replace('\r', '').trim_space().split('\n') == ['4', 'hello']
	os.rm(tmp_v_file)
	assert !os.exists(tmp_v_file)
}

fn test_pipe_to_v_run() {
	pipe_to_v_run() or {
		panic(err)
	}
}
