import os
import v3.cmdexec

const fastc_backend_v3_dir = os.dir(os.dir(@FILE))
const fastc_backend_vlib_dir = os.dir(fastc_backend_v3_dir)
const fastc_backend_v3_source = os.join_path(fastc_backend_v3_dir, 'v3.v')

fn test_fastc_backend_and_checked_fallback() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_backend_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := os.join_path(root, 'v3')
	build := cmdexec.run(@VEXE, ['-gc', 'none', '-path', '${fastc_backend_vlib_dir}|@vlib|@vmodules',
		'-o', v3_bin, fastc_backend_v3_source])
	assert build.exit_code == 0, build.output

	valid_source := os.join_path(root, 'valid.v')
	os.write_file(valid_source, 'module main

fn twice(value int) int {
	return value * 2
}

fn main() {
	value := twice(21)
	println(value)
}
') or {
		panic(err)
	}
	valid_binary := os.join_path(root, 'valid')
	valid_compile := cmdexec.run(v3_bin, ['-macos-v3-compat-c99', '-b', 'fastc', '-o', valid_binary,
		valid_source])
	assert valid_compile.exit_code == 0, valid_compile.output
	assert valid_compile.output.contains('fastc')
	assert !valid_compile.output.contains('  parse '), valid_compile.output
	retained_c := os.read_file(valid_binary + '.c') or { panic(err) }
	assert retained_c.contains('__typeof__((twice(21))) value = (twice(21));')
	assert !retained_c.contains('builtin__builtin_init')
	valid_run := cmdexec.run(valid_binary, [])
	assert valid_run.exit_code == 0, valid_run.output
	assert valid_run.output.trim_space() == '42'

	invalid_source := os.join_path(root, 'invalid.v')
	os.write_file(invalid_source, 'module main

fn main() {
	value := missing_name
	println(value)
}
') or {
		panic(err)
	}
	invalid_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o',
		os.join_path(root, 'invalid'), invalid_source])
	assert invalid_compile.exit_code != 0
	assert invalid_compile.output.contains('undefined variable: `missing_name`'), invalid_compile.output
	assert !invalid_compile.output.to_lower().contains('tcc:'), invalid_compile.output

	old_vjobs := os.getenv('VJOBS')
	os.setenv('VJOBS', '4', true)
	mut selfhosted_v3 := v3_bin
	for level in 1 .. 6 {
		next_v3 := os.join_path(root, 'v3_selfhosted_${level}')
		selfhost := cmdexec.run(selfhosted_v3, ['-silent', '-nocache', '-no-memory-limit',
			'-selfhost', '-b', 'fastc', '-o', next_v3, fastc_backend_v3_source])
		assert selfhost.exit_code == 0, 'fastc self-host level ${level}: ${selfhost.output}'
		assert os.is_executable(next_v3)
		selfhosted_v3 = next_v3
	}
	os.setenv('VJOBS', old_vjobs, true)

	selfhosted_binary := os.join_path(root, 'selfhosted_valid')
	selfhosted_compile := cmdexec.run(selfhosted_v3, ['-silent', '-b', 'fastc', '-o',
		selfhosted_binary, valid_source])
	assert selfhosted_compile.exit_code == 0, selfhosted_compile.output
	selfhosted_run := cmdexec.run(selfhosted_binary, [])
	assert selfhosted_run.exit_code == 0, selfhosted_run.output
	assert selfhosted_run.output.trim_space() == '42'
}
