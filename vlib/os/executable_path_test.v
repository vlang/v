import os

fn test_executable_returns_complete_process_path() {
	executable := os.executable()
	assert executable.len > 0
	assert !executable.contains('\x00')
	assert os.is_file(executable)
	assert os.real_path(executable) == os.real_path(os.args[0])
}
