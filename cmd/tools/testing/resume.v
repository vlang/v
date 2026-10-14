module testing

import crypto.sha256
import os
import rand
import time

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
	is_test_file := ts.will_compile && ts.exec_mode == .compile_and_run
		&& (file.ends_with('_test.v') || file.ends_with('_test.c.v')
			|| file.ends_with('_test.js.v'))
	is_source_check := ts.vargs.split(' ').any(it in ['vet', 'fmt'])
	if ts.resume_dir == '' || (!is_test_file && !is_source_check) {
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
		if !os.is_file(path) {
			return error('test progress path is not a file: ${path}')
		}
		passed = os.read_file(path)! == contents
	}
	return TestResume{
		path:     path
		contents: contents
		passed:   passed
	}
}

// A replacement refused for a transient reason (see replace_record) is retried this many
// times, this far apart. The constants are defined on every platform, so the time import
// stays used everywhere.
const replace_retry_attempts = 20
const replace_retry_delay = 25 * time.millisecond

$if windows {
	fn C.MoveFileExW(lp_existing_file_name &u16, lp_new_file_name &u16, flags u32) int
	fn C.GetLastError() u32
}

// replace_record moves `src` onto `dst`, replacing `dst` if it exists. On Windows a
// single MoveFileExW call does the replacement, and when it fails the previous `dst`
// is left in place. os.mv cannot do that there: it deletes `dst` before copying.
// A replacement refused for a transient reason, such as a reader holding `dst` open, is
// retried for about half a second before the error is returned.
fn replace_record(src string, dst string) ! {
	$if windows {
		// Convert separators as os.rename does. to_wide allocates, so the buffers are
		// released by hand once the call is done.
		src_wide := src.replace('/', '\\').to_wide()
		dst_wide := dst.replace('/', '\\').to_wide()
		defer {
			unsafe {
				free(src_wide)
				free(dst_wide)
			}
		}
		// MOVEFILE_REPLACE_EXISTING is 0x1. A reader that holds the record open for a moment
		// makes the call fail with access denied or a sharing violation, so those failures
		// are retried for a short while. The error code is read right after the failed
		// call, before anything else can change it.
		mut attempt := 1
		for C.MoveFileExW(src_wide, dst_wide, u32(0x1)) == 0 {
			code := C.GetLastError()
			if attempt >= replace_retry_attempts || code !in [u32(5), u32(32), u32(33)] {
				return error('failed to replace `${dst}` (Windows error ${code})')
			}
			attempt++
			time.sleep(replace_retry_delay)
		}
	} $else {
		os.mv(src, dst, overwrite: true)!
	}
}

fn (r TestResume) save() ! {
	if r.path == '' {
		return
	}
	os.mkdir_all(os.dir(r.path))!
	// Each worker owns a different temporary directory. Replace the record only after
	// the complete success record is written, keeping interrupted writes harmless.
	tmp_dir := '${r.path}.${os.getpid()}.${rand.ulid()}.tmp'
	os.mkdir(tmp_dir, mode: 0o700)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	tmp_path := os.join_path(tmp_dir, 'progress')
	os.write_file(tmp_path, r.contents)!
	// An edited test rewrites its record at the same path. A refused replacement leaves
	// the previous record in place, so a failed save never removes a record.
	replace_record(tmp_path, r.path)!
}

fn (mut ts TestSession) fail_test_resume(err IError, cmd string, mtc MessageThreadContext) {
	ts.benchmark_fail()
	ts.add_failed_cmd(cmd)
	ts.append_message(.fail, 'Could not update test progress for ${mtc.file}: ${err}', mtc)
}
