import os

fn test_wasm_browser_target_allows_empty_main() {
	vexe := os.quoted_path(@VEXE)
	wrkdir := os.join_path(os.vtmp_dir(), 'wasm_browser_tests')
	os.mkdir_all(wrkdir)!
	defer {
		os.rmdir_all(wrkdir) or {}
	}

	source_path := os.join_path(wrkdir, 'empty_main.wasm.v')
	os.write_file(source_path, 'pub fn main() {}\n')!

	flags_sets := [
		'-no-bounds-checking -b wasm -os browser',
		'-no-bounds-checking -enable-globals -b wasm -os browser',
	]

	for idx, flags in flags_sets {
		output_path := os.join_path(wrkdir, 'empty_main_${idx}.wasm')
		res :=
			os.execute('${vexe} ${flags} -o ${os.quoted_path(output_path)} ${os.quoted_path(source_path)}')
		assert res.exit_code == 0, 'compilation failed for `${flags}`: ${res.output}'
		assert os.exists(output_path), 'missing output for `${flags}`'
	}
}
