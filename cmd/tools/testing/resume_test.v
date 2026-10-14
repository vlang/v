module testing

import os
import rand
import time

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

fn test_resume_records_vet_and_fmt_source_checks() ! {
	root := os.join_path(os.temp_dir(), 'vtest-resume-source-${rand.ulid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	file := os.join_path(root, 'source.v')
	os.write_file(file, 'fn main() {}\n')!
	mut ts := TestSession{
		vexe:       os.executable()
		resume_dir: os.join_path(root, 'progress')
		vargs:      'vet'
	}
	vet := ts.test_resume(file)!
	assert vet.path != ''
	assert !vet.passed
	vet.save()!
	assert ts.test_resume(file)!.passed

	ts.vargs = 'fmt -inprocess -verify'
	fmt := ts.test_resume(file)!
	assert fmt.path != vet.path
	assert !fmt.passed
	fmt.save()!
	assert ts.test_resume(file)!.passed

	os.write_file(file, 'fn changed() {}\n')!
	assert !ts.test_resume(file)!.passed
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

// hold_record_open opens `path` as a reader does, reports on `opened` whether it could,
// and releases the record after `delay`. The handle is owned by this thread alone.
fn hold_record_open(path string, delay time.Duration, opened chan bool) {
	mut held := os.open(path) or {
		opened <- false
		return
	}
	opened <- true
	time.sleep(delay)
	held.close()
}

// A reader that holds the record open only briefly must not fail the save on Windows.
fn test_resume_replacement_waits_out_a_brief_reader() ! {
	$if windows {
		root := os.join_path(os.temp_dir(), 'vtest-resume-brief-${rand.ulid()}')
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
			resume_dir:   os.join_path(root, 'progress')
		}
		first := ts.test_resume(file)!
		first.save()!
		os.write_file(file, 'fn test_changed() {}\n')!
		changed := ts.test_resume(file)!
		opened := chan bool{cap: 1}
		reader := spawn hold_record_open(first.path, 100 * time.millisecond, opened)
		assert <-opened, 'the brief reader could not open the record'
		changed.save()!
		reader.wait()
		assert os.read_file(first.path)! == changed.contents
	}
}

// On Windows a record that another process holds open without delete sharing cannot be
// replaced. The save must fail and leave the previous record exactly as it was.
fn test_resume_refused_replacement_keeps_the_previous_record() ! {
	$if windows {
		root := os.join_path(os.temp_dir(), 'vtest-resume-replace-${rand.ulid()}')
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
			resume_dir:   os.join_path(root, 'progress')
		}
		first := ts.test_resume(file)!
		first.save()!
		previous := os.read_file(first.path)!
		os.write_file(file, 'fn test_changed() {}\n')!
		changed := ts.test_resume(file)!
		mut held := os.open(first.path)!
		mut refused := false
		changed.save() or { refused = true }
		held.close()
		assert refused, 'a replacement of a record held open must be refused'
		assert os.read_file(first.path)! == previous
		changed.save()!
		assert os.read_file(first.path)! == changed.contents
	}
}
