import os

fn test_long_paths_status_is_readable() {
	// Read-only: safe on every machine, including CI.
	status := os.long_paths_status()
	assert status in [os.LongPathsStatus.enabled, os.LongPathsStatus.disabled,
		os.LongPathsStatus.unknown]
}

fn test_long_paths_manual_command() {
	assert os.long_paths_manual_command() == 'reg add "HKLM\\SYSTEM\\CurrentControlSet\\Control\\FileSystem" /v LongPathsEnabled /t REG_DWORD /d 1 /f'
}
