module fastc

import os
import v.cmdexec

fn test_fastc_owned_pointers_and_map_keys_can_be_freed_without_gc() ! {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_manual_memory_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
import json2

struct Plain {
	value int
}

interface Value {
	value int
}

type Number = int | string

struct Defaulted {
	value int = 42
}

struct Page {
	values [64]int
}

fn local_pointer() &Plain {
	value := Plain{value: 43}
	return &value
}

fn updated_pointer(base &Plain) &Plain {
	return &Plain{...base, value: 44}
}

fn main() {
	for _ in 0 .. 32 {
		plain := &Plain{value: 42}
		local := local_pointer()
		updated := updated_pointer(plain)
		defaulted := &Defaulted{}
		page := &Page{values: [64]int{init: 7}}
		positional := &Plain{45}
		interface_pointer := &Value(Plain{value: 46})
		sum_pointer := &Number(47)
		map_pointer := &map[string]int{}
		assert plain.value == 42
		assert local.value == 43
		assert updated.value == 44
		assert defaulted.value == 42
		assert page.values[63] == 7
		assert positional.value == 45
		assert interface_pointer.value == 46
		assert *sum_pointer == Number(47)
		assert map_pointer.len == 0
		unsafe {
			free(plain)
			free(local)
			free(updated)
			free(defaulted)
			free(page)
			free(positional)
			free(interface_pointer)
			free(sum_pointer)
			map_pointer.free()
			free(map_pointer)
		}
		mut numbers := map[u64]int{}
		numbers[1] = 42
		assert numbers[1] == 42
		unsafe { numbers.free() }
		mut values := {"answer": 42}
		assert json2.encode(values) == "{\"answer\":42}"
		assert json2.encode(values) == "{\"answer\":42}"
		assert values["answer"] == 42
		unsafe { values.free() }
	}
}
')!
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-gc', 'none', '-no-prealloc', '-nocache', '-o',
		program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
