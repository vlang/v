import os

fn test_all_samples_can_be_compiled() {
	vexe := @VEXE
	vroot := os.dir(vexe)
	samples := os.walk_ext('${vroot}/vlib/gg/testdata', '.vv')
	mut fails := []string{}
	for program_source in samples {
		compile_cmd := '${os.quoted_path(vexe)} ${os.quoted_path(program_source)}'
		res := os.execute(compile_cmd)
		if res.exit_code != 0 {
			eprintln('>>> FAIL ${compile_cmd}')
			eprintln(res.output)
			fails << compile_cmd
		}
		println('OK ${compile_cmd}')
	}
	if fails.len > 0 {
		eprintln('> Failed summary:')
		for f in fails {
			eprintln('   failed cmd: ${f}')
		}
		assert false
	}
}
