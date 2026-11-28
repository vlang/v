import os

fn test_ownership_map_clear_pointer() {
	root := os.join_path(os.vtmp_dir(), 'ownership_map_clear_pointer_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	shared := "type Value = int | string

fn clear_map[F](field &F) {
	mut target := unsafe { field }
	target.clear()
}

fn free_map[F](field &F) {
	mut target := unsafe { field }
	target.free()
}

fn check_string_clear() {
	mut items := map[string]Value{}
	items['key'] = Value('value')
	clear_map(&items)
	print('clear string map ')
	println(items.len)
}

fn check_string_free() {
	mut items := map[string]Value{}
	items['key'] = Value('value')
	free_map(&items)
	print('free string map ')
	println(items.len)
}
"
	for gc_mode in ['vgc', 'none'] {
		mut fixture := shared
		mut calls := ['\tcheck_string_clear()', '\tcheck_string_free()']
		mut expected := ['clear string map 0', 'free string map 0']
		if gc_mode == 'none' {
			fixture += "
fn check_int_clear() {
	mut items := map[int]Value{}
	items[1] = Value('value')
	clear_map(&items)
	print('clear integer map ')
	println(items.len)
}
"
			calls << '\tcheck_int_clear()'
			expected << 'clear integer map 0'
		}
		os.write_file(source, fixture + '\nfn main() {\n${calls.join('\n')}\n}\n')!
		for mode in ['', '-no-parallel'] {
			output := os.join_path(root, 'app')
			mut args := [@VEXE, '-nocache', '-ownership', '-d', 'ownership', '-autofree', '-gc',
				gc_mode, '-no-prealloc', '-o', output]
			if mode != '' {
				args << mode
			}
			args << source
			result := os.exec(args)
			assert result.exit_code == 0, result.output
			run := os.exec([output])
			assert run.exit_code == 0, run.output
			assert run.output.trim_space().split_into_lines() == expected, run.output
		}
	}
}
