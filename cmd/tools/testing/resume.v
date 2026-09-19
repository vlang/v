module testing

import crypto.sha256
import os
import rand

// A resume is a continuation of a CI run, not a cache valid across arbitrary
// source changes. The caller owns this directory and clears it on reset/success.
struct TestResume {
	path     string
	contents string
	passed   bool
}

// Claim the opt-in directory for this process. Test programs inherit the owner,
// so nested `v test` invocations cannot consume their parent's success records.
fn test_resume_dir() string {
	dir := os.getenv('VTEST_RESUME_DIR')
	if dir == '' {
		return ''
	}
	owner := os.getenv('VTEST_RESUME_OWNER')
	pid := os.getpid().str()
	if owner != '' && owner != pid {
		return ''
	}
	os.setenv('VTEST_RESUME_OWNER', pid, true)
	return os.abs_path(dir)
}

fn (ts &TestSession) test_resume(file string) !TestResume {
	if ts.resume_dir == '' || !ts.will_compile || ts.exec_mode != .compile_and_run
		|| !(file.ends_with('_test.v') || file.ends_with('_test.c.v')
		|| file.ends_with('_test.js.v')) {
		return TestResume{}
	}
	// Separate files and compiler options, including function filters. Hash the
	// test source too, so editing a previously successful test makes it pending.
	context := [
		os.real_path(ts.vexe),
		ts.vargs,
		ts.custom_defines.str(),
		os.getenv('VFLAGS'),
		os.getenv('VTEST_ONLY_FN'),
		file,
	].str()
	path := os.join_path(ts.resume_dir, sha256.hexhash(context) + '.ok')
	contents := 'vtest-resume-v1\n${file}\n${sha256.hexhash(os.read_file(file)!)}\n'
	mut passed := false
	if os.exists(path) {
		passed = os.read_file(path)! == contents
	}
	return TestResume{
		path:     path
		contents: contents
		passed:   passed
	}
}

fn (r TestResume) save() ! {
	if r.path == '' {
		return
	}
	os.mkdir_all(os.dir(r.path))!
	// Each worker owns a different temporary directory. Rename only after the
	// complete success record is written, keeping interrupted writes harmless.
	tmp_dir := '${r.path}.${os.getpid()}.${rand.ulid()}.tmp'
	os.mkdir(tmp_dir, mode: 0o700)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	tmp_path := os.join_path(tmp_dir, 'progress')
	os.write_file(tmp_path, r.contents)!
	os.rename(tmp_path, r.path)!
}

fn (mut ts TestSession) fail_test_resume(err IError, cmd string, mtc MessageThreadContext) {
	ts.benchmark_fail()
	ts.add_failed_cmd(cmd)
	ts.append_message(.fail, 'Could not update test progress for ${mtc.file}: ${err}', mtc)
}
