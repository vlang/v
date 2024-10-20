import os
import rand

const vexe = @VEXE

fn testsuite_begin() {
	$if windows {
		skip_test('windows does not support envbang lines')
	}
	// Make sure, that we will be using the local v, instead of the globally installed one,
	// for the rest of the tests here:
	local_v_path := os.dir(vexe)
	old_path := os.getenv('PATH')
	os.setenv('PATH', '${local_v_path}:${old_path}', true)
	println('Changed PATH: ${os.getenv('PATH')}')
}

fn test_envbang_script_runs() {
	env_location := '/usr/bin/env'
	if !os.exists(env_location) {
		skip_test('${env_location} does not exist')
	}
	if !os.is_executable(env_location) {
		skip_test('${env_location} is not executable')
	}
	os.find_abs_path_of_executable('v') or { skip_test('v is not in PATH') }
	rndname := rand.ulid()
	rnd_vsh_script_path := os.real_path(os.join_path(os.cache_dir(), '${rndname}.vsh'))
	os.write_file(rnd_vsh_script_path, "#!${env_location} v
import os
println('hello')
println('@VEXE: ${vexe}')
println(os.args)
")!
	os.chmod(rnd_vsh_script_path, 0o700)!
	res := os.execute('${os.quoted_path(rnd_vsh_script_path)} abc 123 -option')
	assert res.exit_code == 0
	lines := res.output.split_into_lines()
	dump(lines)
	assert lines[0] == 'hello'
	assert lines[1].starts_with('@VEXE: ')
	assert os.real_path(lines[1].all_after('@VEXE: ')) == os.real_path(vexe)
	assert lines[2].ends_with(", 'abc', '123', '-option']")
	os.rm(rnd_vsh_script_path)!
}

@[noreturn]
fn skip_test(reason string) {
	println('skipping test, because ${reason} .')
	exit(0)
}
