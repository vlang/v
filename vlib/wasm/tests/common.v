module main

import os

const pid = os.getpid()

const wasm_validate_exe = find_wasm_validate() or {
	println('>>> Skipping test, since wasm-validate could not be found, error: ${err}')
	exit(0)
}

fn find_wasm_validate() !string {
	// Prefer to find our own version first, if it was installed already
	// through install_wabt.vsh, since it is more likely to be known, recent, and stable:
	thirdpart_wasm_validate_folder := os.join_path(@VROOT, 'thirdparty', 'wabt', 'bin')
	extension := $if windows { '.exe' } $else { '' }
	wasm_validate_executable := os.join_path(thirdpart_wasm_validate_folder, 'wasm-validate${extension}')
	if os.exists(wasm_validate_executable) {
		return wasm_validate_executable
	}
	if path := os.find_abs_path_of_executable('wasm-validate') {
		return path
	}
	return error('could not find wasm-validate executable in thirdparty/ as well, try first `v run cmd/tools/install_wabt.vsh`')
}

// validate validates the given wasm code using `wasm-validate` executable.
pub fn validate(code []u8) ! {
	println('validating using: ${wasm_validate_exe}')
	outfile := os.join_path(os.temp_dir(), 'code_${pid}.wasm')
	os.write_file(outfile, code.bytestr())!
	validation_cmd := '${os.quoted_path(wasm_validate_exe)} ${os.quoted_path(outfile)}'
	res := os.execute(validation_cmd)
	if res.exit_code != 0 {
		eprintln('failed exit code: ${res.exit_code} | command:\n${validation_cmd}')
		return error('wasm-validate exited with a non zero exit code: ${res.exit_code}')
	}
	os.rm(outfile)!
}
