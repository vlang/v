import os

fn test_heap_map_nested_struct_assignment_in_mut_receiver() {
	root := os.join_path(os.vtmp_dir(), 'heap_map_nested_assignment_${os.getpid()}')
	os.mkdir_all(root)!
	previous_cache := os.getenv_opt('V3CACHE')
	defer {
		if previous := previous_cache {
			os.setenv('V3CACHE', previous, true)
		} else {
			os.unsetenv('V3CACHE')
		}
		os.rmdir_all(root) or {}
	}
	os.setenv('V3CACHE', os.join_path(root, 'cache'), true)
	source := os.join_path(root, 'main.v')
	os.write_file(source, "struct Task {
	id string
}

@[heap]
struct Item {
mut:
	task Task
}

@[heap]
struct Box {
mut:
	items []Item
}

fn (mut m map[string]Box) keep_tasks(key string, old []Item) {
	for i, it in old {
		m[key].items[i].task = it.task
	}
}

fn main() {
	mut m := map[string]Box{}
	m['a'] = Box{
		items: [Item{}]
	}
	m.keep_tasks('a', [Item{
		task: Task{'t1'}
	}])
	println(m['a'].items[0].task.id)
}
")!
	for mode in ['', '-nocache', '-no-parallel'] {
		output := os.join_path(root, 'app')
		mut args := [@VEXE, '-new-compiler', '-o', output]
		if mode != '' {
			args << mode
		}
		args << source
		result := os.exec(args)
		assert result.exit_code == 0, result.output
		run := os.exec([output])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == 't1', run.output
	}
}
