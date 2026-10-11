import os

struct WalkCounter {
mut:
	count int
}

fn (mut c WalkCounter) visit(_ string, _ os.WalkDirEntry) os.WalkDirAction {
	c.count++
	return .stop
}

fn count_with_mut_parameter(root string, mut c WalkCounter) {
	os.walk_dir(root, c.visit) or { panic(err) }
}

fn test_walk_dir_callback_updates_mutable_local_receiver() {
	root := os.join_path(os.temp_dir(), 'v_walk_dir_callback_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'entry'), '') or { panic(err) }
	mut c := WalkCounter{}
	os.walk_dir(root, c.visit) or { panic(err) }
	assert c.count == 1
	count_with_mut_parameter(root, mut c)
	assert c.count == 2
}

fn test_walk_dir_closure_updates_referenced_state() {
	root := os.join_path(os.temp_dir(), 'v_walk_dir_closure_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'entry'), '') or { panic(err) }
	mut c := &WalkCounter{}
	os.walk_dir(root, fn [mut c] (_ string, _ os.WalkDirEntry) os.WalkDirAction {
		c.count++
		return .stop
	}) or { panic(err) }
	assert c.count == 1
}
