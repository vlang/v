module main

fn test_shell_shim_content_forwards_to_v_exe() {
	assert shell_shim_content(r'C:\Users\MR\v\v.exe') == '#!/bin/sh\nexec \'C:/Users/MR/v/v.exe\' "$@"\n'
}

fn test_shell_shim_content_quotes_literal_windows_path() {
	path := r"D:\tools\v lang$dollar`backtick's\v.exe"
	assert shell_shim_content(path) == '#!/bin/sh\nexec \'D:/tools/v lang$dollar`backtick\'"\'"\'s/v.exe\' "$@"\n'
}
