import os
import time

// max_path is the longest TMP value that GetTempPathW reads without looping.
const max_path = 260

const helper_source = "import os

fn main() {
	os.setenv('TMP', os.args[1], true)
	println('exec=' + os.exec(['hostname']).exit_code.str())
	println('execute=' + os.execute('hostname').exit_code.str())
	println('system=' + os.system('hostname >nul').str())
	println('tmp=' + os.getenv('TMP').len.str())
}
"

fn long_paths_enabled() bool {
	res := os.exec(['reg', 'query', 'HKLM\\SYSTEM\\CurrentControlSet\\Control\\FileSystem', '/v',
		'LongPathsEnabled'])
	return res.exit_code == 0 && res.output.contains('0x1')
}

// test_child_launches_survive_a_tmp_past_max_path runs a longPathAware program that sets its
// own TMP past MAX_PATH and then starts programs in each of the ways `os` offers. Without
// the guard, GetTempPathW inside CreateProcessW loops forever there, so the program gets a
// deadline instead of being waited for.
fn test_child_launches_survive_a_tmp_past_max_path() ! {
	if !long_paths_enabled() {
		eprintln('skipped: LongPathsEnabled is off, so no process can reach the loop')
		return
	}
	root := os.join_path(os.vtmp_dir(), 'long_tmp_child_${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	long_tmp := os.join_path(root, 'b'.repeat(120), 'f'.repeat(150))
	os.mkdir_all(long_tmp)!
	assert long_tmp.len > max_path
	src := os.join_path(root, 'helper.v')
	exe := os.join_path(root, 'helper.exe')
	os.write_file(src, helper_source)!
	// `-w`: the helper calls the deprecated os.execute and os.system on purpose.
	build := os.exec([@VEXE, '-w', '-o', exe, src])
	assert build.exit_code == 0, build.output
	manifest := os.read_file('${exe}.manifest') or { '' }
	if !os.read_file(exe)!.contains('longPathAware') && !manifest.contains('longPathAware') {
		eprintln('skipped: the helper is not longPathAware, so it cannot loop')
		return
	}
	mut p := os.new_process(exe)
	p.set_args([long_tmp])
	p.set_redirect_stdio()
	p.run()
	deadline := time.now().add(30 * time.second)
	for p.is_alive() && time.now() < deadline {
		time.sleep(100 * time.millisecond)
	}
	if p.is_alive() {
		p.signal_kill()
		p.wait()
		p.close()
		assert false, 'the helper hung starting a program with a ${long_tmp.len} character TMP'
	}
	output := p.stdout_slurp()
	p.wait()
	p.close()
	assert output.contains('exec=0'), output
	assert output.contains('execute=0'), output
	assert output.contains('system=0'), output
	tmp_text := output.all_after('tmp=').trim_space()
	assert tmp_text != '' && tmp_text.bytes().all(it.is_digit()), output
	tmp_len := tmp_text.int()
	assert tmp_len > 0 && tmp_len <= max_path, output
}
