import os

fn test_ownership_flags_select_target_branches_and_files() {
	root := os.join_path(os.temp_dir(), 'ownership_target_define_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main

import sync.arc

fn main() {
	$if ownership ? {
		assert selected_mode() == "ownership"
		value := arc.new(7)
		cloned := value.clone()
		assert *cloned.get() == 7
		assert value.strong_count() == 2
		println("ok")
	} $else {
		$compile_error("ownership flag did not define the target option")
	}
}
')!
	os.write_file(os.join_path(root, 'selected_d_ownership.v'), 'module main
fn selected_mode() string { return "ownership" }
')!
	os.write_file(os.join_path(root, 'selected_notd_ownership.v'), 'module main
$compile_error("ownership flag selected the non-ownership file")
')!
	for flag in ['-ownership', '--ownership', '-autofree'] {
		for mode in ['-no-parallel', ''] {
			out := os.execute('${os.quoted_path(@VEXE)} -new-compiler -no-memory-limit -nocache -cc clang ${flag} ${mode} run ${os.quoted_path(root)}')
			assert out.exit_code == 0, '${flag} ${mode}: ${out.output}'
			assert out.output.trim_space() == 'ok', out.output
		}
	}
}
