module main

import arena

fn worker(value i64) i64 {
	if arena.active() { return -1 }
	return value * 3 + 1
}

@[export: 'v_thread_library_probe']
fn library_probe(value i64) i64 {
	if arena.active() { return -2 }
	mut region := arena.new()
	region.push()
	task := spawn worker(value)
	result := task.wait()
	valid := region.is_current()
	region.pop()
	region.free()
	return if valid { result } else { -3 }
}
