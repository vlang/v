import os

fn test_ownership_map_clear_pointer() {
	root := os.join_path(os.vtmp_dir(), 'ownership_map_clear_pointer_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, "type Value = int | string

fn clear_map[F](field &F) {
	mut target := unsafe { field }
	target.clear()
}

fn main() {
	mut items := map[string]Value{}
	items['key'] = Value('value')
	clear_map(&items)
	println(items.len)
}
")!
	for mode in ['', '-no-parallel'] {
		for gc_mode in ['vgc', 'none'] {
			output := os.join_path(root, 'app')
			mut args := [@VEXE, '-new-compiler', '-nocache', '-ownership', '-d', 'ownership',
				'-autofree', '-gc', gc_mode, '-no-prealloc', '-o', output]
			if mode != '' {
				args << mode
			}
			args << source
			result := os.exec(args)
			assert result.exit_code == 0, result.output
			clear_source := os.read_file(source)!
			os.write_file(source, clear_source.replace('target.clear()', 'target.free()'))!
			free_build := os.exec(args)
			assert free_build.exit_code == 0, free_build.output
			os.write_file(source, clear_source)!
			if gc_mode == 'none' {
				// Integer keys isolate value cleanup from string-key callback ownership.
				stored := os.read_file(source)!
				os.write_file(source, stored.replace('map[string]Value', 'map[int]Value').replace("items['key']", 'items[1]'))!
				runtime_build := os.exec(args)
				assert runtime_build.exit_code == 0, runtime_build.output
				run := os.exec([output])
				assert run.exit_code == 0, run.output
				assert run.output.trim_space() == '0', run.output
				os.write_file(source, stored)!
			}
		}
	}
}
