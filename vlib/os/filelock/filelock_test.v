import os
import os.filelock
import time

const test_dir = os.join_path(os.vtmp_dir(), 'filelock_tests')

const helper_mode_env = 'V_FILELOCK_HELPER_MODE'
const helper_path_env = 'V_FILELOCK_HELPER_PATH'
const helper_lock_mode_env = 'V_FILELOCK_LOCK_MODE'
const helper_start_env = 'V_FILELOCK_START'
const helper_len_env = 'V_FILELOCK_LEN'

fn testsuite_begin() {
	if os.getenv(helper_mode_env) != '' {
		run_helper_process()
	}
	os.mkdir_all(test_dir) or {}
}

fn testsuite_end() {
	os.rmdir_all(test_dir) or {}
}

fn run_helper_process() {
	path := os.getenv(helper_path_env)
	lock_mode := if os.getenv(helper_lock_mode_env) == 'shared' {
		filelock.LockMode.shared
	} else {
		filelock.LockMode.exclusive
	}
	start := os.getenv(helper_start_env).u64()
	len := os.getenv(helper_len_env).u64()
	mut file_lock := filelock.new_file(path, filelock.LockOptions{
		mode:  lock_mode
		start: start
		len:   len
	})
	match os.getenv(helper_mode_env) {
		'hold' {
			file_lock.acquire() or {
				eprintln(err.msg())
				exit(2)
			}
			println('locked')
			os.flush()
			time.sleep(300 * time.millisecond)
			file_lock.release()
			exit(0)
		}
		'try' {
			exit(if file_lock.try_acquire() {
				file_lock.release()
				0
			} else {
				1
			})
		}
		else {
			exit(3)
		}
	}
}

fn helper_path(name string) string {
	return os.join_path(test_dir, name)
}

fn new_helper_process(path string, lock_mode filelock.LockMode, start u64, len u64, mode string) &os.Process {
	mut p := os.new_process(os.executable())
	mut env := os.environ()
	env[helper_mode_env] = mode
	env[helper_path_env] = path
	env[helper_lock_mode_env] = lock_mode.str()
	env[helper_start_env] = start.str()
	env[helper_len_env] = len.str()
	p.set_environment(env)
	p.set_redirect_stdio()
	return p
}

fn wait_for_lock(mut p os.Process) {
	mut output := ''
	for _ in 0 .. 50 {
		if p.is_pending(.stdout) {
			output += p.stdout_read()
			if output.contains('locked') {
				return
			}
		}
		if !p.is_alive() {
			break
		}
		time.sleep(10 * time.millisecond)
	}
	p.wait()
	stderr := p.stderr_slurp().trim_space()
	assert false, 'helper failed to acquire lock; exit=${p.code} stdout="${output.trim_space()}" stderr="${stderr}"'
}

fn try_lock_from_helper(path string, lock_mode filelock.LockMode, start u64, len u64) int {
	mut p := new_helper_process(path, lock_mode, start, len, 'try')
	p.run()
	p.wait()
	stderr := p.stderr_slurp().trim_space()
	code := p.code
	p.close()
	assert stderr == ''
	return code
}

fn test_flock() {
	lockfile := helper_path('test.lock')
	os.rm(lockfile) or {}
	mut l := filelock.new(lockfile)
	assert !os.exists(lockfile)
	l.acquire() or { panic(err) }
	assert os.exists(lockfile)
	l.release()
	assert !os.exists(lockfile)
}

fn test_flock_try() {
	lockfile := helper_path('test-try.lock')
	os.rm(lockfile) or {}
	mut l := filelock.new(lockfile)
	assert l.try_acquire()
	l.release()
	assert !os.exists(lockfile)
	assert l.try_acquire()
	assert os.exists(lockfile)
	l.release()
	assert l.try_acquire()
	l.release()
	assert !os.exists(lockfile)
}

fn test_existing_file_lock_range() {
	target := helper_path('existing-range.txt')
	os.write_file(target, '0123456789abcdef')!
	mut holder := new_helper_process(target, .exclusive, 0, 5, 'hold')
	holder.run()
	defer {
		if holder.status == .running || holder.is_alive() {
			holder.wait()
		}
		holder.close()
	}
	wait_for_lock(mut holder)
	assert try_lock_from_helper(target, .exclusive, 0, 5) == 1
	assert try_lock_from_helper(target, .exclusive, 6, 5) == 0
	holder.wait()
	assert holder.code == 0
	assert os.exists(target)
	assert os.read_file(target)! == '0123456789abcdef'
}

fn test_existing_file_lock_shared_mode() {
	target := helper_path('existing-shared.txt')
	os.write_file(target, '0123456789abcdef')!
	mut holder := new_helper_process(target, .shared, 0, 5, 'hold')
	holder.run()
	defer {
		if holder.status == .running || holder.is_alive() {
			holder.wait()
		}
		holder.close()
	}
	wait_for_lock(mut holder)
	assert try_lock_from_helper(target, .shared, 0, 5) == 0
	assert try_lock_from_helper(target, .exclusive, 0, 5) == 1
	holder.wait()
	assert holder.code == 0
	assert os.exists(target)
}
