import os

const vexe = @VEXE

const vroot = os.dir(vexe)

const testdata_folder = 'vlib/v/tests/known_errors/testdata'

fn test_known_errors_testdata_folder_exists() ? {
	os.chdir(vroot)?
	assert os.is_dir(testdata_folder)
}

fn test_known_failures_are_still_failures() ? {
	mut oks := []string{}
	mut files := os.walk_ext(testdata_folder, '.v')
	files << os.walk_ext(testdata_folder, '.vv')
	for f in files {
		cmd := '${os.quoted_path(vexe)} ${os.quoted_path(f)}'
		println('known compilation failure: $cmd')
		res := os.execute(cmd)
		if res.exit_code == 0 {
			oks << cmd
			println('    unexpectedly COMPILED: $cmd')
		} else {
			assert true
		}
	}
	println('Summary: $files.len files, $oks.len unexpectedly succeeded.')
	if oks.len != 0 {
		for cmd in oks {
			println('    expected to fail, but SUCCEEDED command: $cmd')
		}
		assert false
	}
}
