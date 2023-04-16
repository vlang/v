module main

import os

const wasm_validate_exe = os.find_abs_path_of_executable('wasm-validate') or {
	println('skipping test, since wasm-validate could not be found')
	exit(0)
}

const pid = os.getpid()

pub fn validate(code []u8) ! {
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
