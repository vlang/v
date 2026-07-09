// vtest build: !musl? && !sanitized_job?
import os

fn test_all_samples_can_be_compiled() {
	vexe := @VEXE
	vroot := os.dir(vexe)
	cache_dir := os.join_path(os.vtmp_dir(), 'draw_fns_api_vcache_${os.getpid()}')
	os.rmdir_all(cache_dir) or {}
	os.mkdir_all(cache_dir) or { panic(err) }
	old_vcache := os.getenv_opt('VCACHE')
	os.setenv('VCACHE', cache_dir, true)
	defer {
		if vcache := old_vcache {
			os.setenv('VCACHE', vcache, true)
		} else {
			os.unsetenv('VCACHE')
		}
		os.rmdir_all(cache_dir) or {}
	}
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
