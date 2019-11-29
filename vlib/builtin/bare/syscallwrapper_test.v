import os

fn test_syscallwrappers() {
	dot_checks := "vlib/builtin/bare/.checks"
	assert os.dir_exists(dot_checks)
	os.chdir(dot_checks)
	checks_v := "checks.v"
	assert os.file_exists(checks_v)
	rc := os.exec("v run $checks_v") or { panic(err) }
	assert rc.exit_code == 0
	assert !rc.output.contains("V panic: An assertion failed.")
	assert !rc.output.contains("failed")
}
