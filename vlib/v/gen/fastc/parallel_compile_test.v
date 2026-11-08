module fastc

import os

fn test_separate_c_files_compile_concurrently_and_link() ! {
	$if !macos && !linux {
		return
	}
	tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	assert os.is_executable(tcc)
	root := os.join_path(os.vtmp_dir(), 'fastc parallel compile ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	first := os.join_path(root, 'first.c')
	second := os.join_path(root, 'second.c')
	os.write_file(first, 'int answer(void); int main(void) { return answer() != 42; }\n')!
	os.write_file(second, 'int answer(void) { return 42; }\n')!
	// Neither compiler can proceed until both files have started compiling.
	// Sequential dispatch fails the bounded barrier instead of passing on timing.
	wrapper := os.join_path(root, 'tcc')
	script := '#!/bin/sh\nfor arg in "$@"; do\n  case "$arg" in *.c) : > "$arg.started";; esac\ndone\n' +
		'i=0\nwhile [ ! -f ' + os.quoted_path(first + '.started') + ' ] || [ ! -f ' + os.quoted_path(second + '.started') + ' ]; do\n' +
		'  i=$((i + 1))\n  if [ "$i" -ge 500 ]; then echo "C files were not compiled concurrently"; exit 1; fi\n  sleep 0.01\ndone\nexec ' + os.quoted_path(tcc) + ' "$@"\n'
	os.write_file(wrapper, script)!
	os.chmod(wrapper, 0o755)!
	flags := ['-B' + os.dir(tcc), '-std=gnu11']
	paths := [first, second]
	prepared := fastc_prepare_c_units(wrapper, flags, paths, false)
	objects := fastc_compile_c_units(wrapper, flags, paths, prepared)!
	assert objects.len == 2
	// TinyCC's external macOS codesign command needs an output without spaces.
	program := os.join_path(os.vtmp_dir(), 'fastc_parallel_link_${os.getpid()}')
	defer { os.rm(program) or {} }
	mut args := flags.clone()
	args << ['-o', program]
	args << objects
	linked := fastc_run_command(tcc, args)
	assert linked.exit_code == 0, linked.output
	ran := fastc_run_command(program, [])
	assert ran.exit_code == 0, ran.output
}
