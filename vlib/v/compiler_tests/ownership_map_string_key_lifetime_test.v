import os

fn test_ownership_map_clones_string_key_storage_before_releasing_the_source() {
	root := os.join_path(os.vtmp_dir(), 'ownership_map_key_lifetime_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, "fn main() {
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
	for mode in ['', '-no-parallel'] {
		for gc_mode in ['vgc', 'none'] {
			mut args := [@VEXE, '-new-compiler', '-nocache', '-ownership', '-d', 'ownership',
				'-autofree', '-gc', gc_mode, '-no-prealloc']
			if mode != '' { args << mode }
			args << ['run', source]
			result := os.exec(args)
			assert result.exit_code == 0, result.output
			assert result.output.trim_space() == 'ok', result.output
		}
	}
}
