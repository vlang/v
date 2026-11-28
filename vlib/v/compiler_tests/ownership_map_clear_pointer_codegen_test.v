import os

fn test_ownership_map_clear_and_string_key_lifetime() {
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
	os.write_file(os.join_path(root, 'key_lifetime.v'), "fn check_string_key_lifetime() {
	mut values := map[string]int{}
	for i in 0 .. 64 {
		key := 'stored-key-\${i}'
		values[key] = i + 1
	}
	for i in 0 .. 64 {
		assert values['stored-key-\${i}'] == i + 1
	}
	keys := values.keys()
	values.clear()
	assert values.len == 0
	assert keys.len == 64
	for key in keys {
		assert key.starts_with('stored-key-')
	}
	for i in 0 .. 64 {
		values['next-key-\${i}'] = i + 1
	}
	for i in 0 .. 64 {
		assert values['next-key-\${i}'] == i + 1
	}
	unsafe { values.free() }
	assert values.len == 0
	println('ok')
}
")!
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
		calls << '\tcheck_string_key_lifetime()'
		expected << 'ok'
		os.write_file(source, fixture + '\nfn main() {\n${calls.join('\n')}\n}\n')!
		for mode in ['', '-no-parallel'] {
			output := os.join_path(root, 'app')
			mut args := [@VEXE, '-nocache', '-ownership', '-d', 'ownership', '-autofree', '-gc',
				gc_mode, '-no-prealloc', '-o', output]
			if mode != '' {
				args << mode
			}
			args << root
			result := os.exec(args)
			assert result.exit_code == 0, result.output
			run := os.exec([output])
			assert run.exit_code == 0, run.output
			assert run.output.trim_space().split_into_lines() == expected, run.output
		}
	}
}
