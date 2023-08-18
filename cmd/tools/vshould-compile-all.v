import os

fn main() {
	mut files := []string{}
	args := os.args#[2..]
	for a in args {
		if os.is_file(a) {
			files << a
			continue
		}
		if os.is_dir(a) {
			files << os.walk_ext(a, '.v')
			continue
		}
	}
	files.sort()
	if files.len == 0 {
		println('0 .v files found.\n')
		println('Usage:')
		println('  v should-compile-all examples/ some/deep/file.v another/')
		println('... will try to compile all .v files found in the given folders and files, one by one.')
		println('If every single one of them compiles, the command will exit with an error code of 0.')
		println('If *any* of them *fail* to compile, the command will exit with an error code of 1.')
		println('')
		println('Note: this command is intended to be used in CI pipelines for v modules, like this:')
		println(' cd module/ ; v should-compile-all examples/ \n')
		exit(1)
	}
	mut failed_commands := []string{}
	for idx, example in files {
		folder_of_example := os.dir(example)
		if os.is_file(os.join_path_single(folder_of_example, '.skip_should_compile_all')) {
			println('>>> skipping file: ${example}, because a `.skip_should_compile_all` file is present next to it.')
			continue
		}
		cmd := '${os.quoted_path(@VEXE)} ${os.quoted_path(example)}'
		println('> compiling ${idx + 1:4}/${files.len:-4}: ${cmd}')
		if 0 != os.system(cmd) {
			failed_commands << cmd
		}
	}
	if failed_commands.len > 0 {
		for idx, fcmd in failed_commands {
			eprintln('>>> FAILED command ${idx + 1:4}/${failed_commands.len:-4}: ${fcmd}')
		}
		println('Summary: ${failed_commands.len:4}/${files.len:-4} file(s) failed to compile.')
		exit(1)
	}
	println('Summary: all ${files.len} file(s) compiled successfully.')
}
