module testing

import os
import rand

fn test_resume_records_only_matching_files_and_options() ! {
	root := os.join_path(os.temp_dir(), 'vtest-resume-${rand.ulid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	mut ts := TestSession{
		vexe:         os.executable()
		will_compile: true
		exec_mode:    .compile_and_run
		resume_dir:   os.join_path(root, 'progress')
	}
	for name in ['a_test.v', 'b_test.c.v', 'c_test.js.v'] {
		file := os.join_path(root, name)
		os.write_file(file, 'fn test_ok() {}\n')!
		pending := ts.test_resume(file)!
		assert !pending.passed
		pending.save()!
		passed := ts.test_resume(file)!
		assert passed.passed
		assert os.read_file(passed.path)!.contains(file)
		os.write_file(file, 'fn test_changed() {}\n')!
		changed := ts.test_resume(file)!
		assert !changed.passed
		changed.save()!
		ts.vargs = '-d other_configuration'
		other_options := ts.test_resume(file)!
		assert !other_options.passed
		assert other_options.path != changed.path
		ts.vargs = ''
	}
	file := os.join_path(root, 'a_test.v')
	ts.exec_mode = .compile
	compile_only := ts.test_resume(file)!
	assert compile_only.path == ''
	ts.exec_mode = .compile_and_run
	ts.resume_dir = ''
	disabled := ts.test_resume(file)!
	assert disabled.path == ''
}

fn test_resume_invalid_records_and_write_errors() ! {
	root := os.join_path(os.temp_dir(), 'vtest-resume-${rand.ulid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	file := os.join_path(root, 'a_test.v')
	os.write_file(file, 'fn test_ok() {}\n')!
	ts := TestSession{
		vexe:         os.executable()
		will_compile: true
		exec_mode:    .compile_and_run
		resume_dir:   root
	}
	pending := ts.test_resume(file)!
	os.write_file(pending.path, 'vtest-resume-v1\n')!
	invalid := ts.test_resume(file)!
	assert !invalid.passed
	invalid.save()!
	passed := ts.test_resume(file)!
	assert passed.passed
	os.rm(pending.path)!
	// A directory at the marker path must be an error, not a successful test.
	os.mkdir(pending.path)!
	if _ := ts.test_resume(file) {
		assert false, 'checkpoint read errors must propagate'
	}
	bad := TestResume{
		path:     os.join_path(file, 'cannot-write.ok')
		contents: 'success'
	}
	bad.save() or { return }
	assert false, 'checkpoint write errors must propagate'
}

fn test_resume_owner_excludes_nested_test_runners() {
	old_dir := os.getenv_opt('VTEST_RESUME_DIR')
	old_owner := os.getenv_opt('VTEST_RESUME_OWNER')
	defer {
		if value := old_dir {
			os.setenv('VTEST_RESUME_DIR', value, true)
		} else {
			os.unsetenv('VTEST_RESUME_DIR')
		}
		if value := old_owner {
			os.setenv('VTEST_RESUME_OWNER', value, true)
		} else {
			os.unsetenv('VTEST_RESUME_OWNER')
		}
	}
	os.setenv('VTEST_RESUME_DIR', os.temp_dir(), true)
	os.setenv('VTEST_RESUME_OWNER', 'another-process', true)
	assert test_resume_dir() == ''
	os.unsetenv('VTEST_RESUME_OWNER')
	assert test_resume_dir() == os.abs_path(os.temp_dir())
	assert os.getenv('VTEST_RESUME_OWNER') == os.getpid().str()
	// A second session in the same process may use the same directory.
	assert test_resume_dir() == os.abs_path(os.temp_dir())
}
