import os

// A local whose address escapes is moved to the heap and stored as a pointer: `mut &buf`
// is then that pointer, as `&buf` and `mut buf` are, not the address of it.
fn test_mut_address_arg_passes_the_storage_of_a_heap_local() {
	root := os.join_path(os.vtmp_dir(), 'v3_mut_address_arg_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_path := os.join_path(root, 'main.v')
	source := 'module main

struct Entry {
mut:
	ino  u64
	name [16]u8
}

fn fill_and_keep(mut e Entry) (u64, &Entry) {
	e.ino = 9
	return 0, unsafe { e }
}

fn pointer() (u64, &Entry) {
	mut on_heap := Entry{}
	ret, kept := fill_and_keep(mut &on_heap)
	return ret, kept
}

fn main() {
	_, kept := pointer()
	println(kept.ino)
}
'
	os.write_file(main_path, source) or { panic(err) }
	out_path := os.join_path(root, 'out.c')
	result := os.execute('${os.quoted_path(@VEXE)} -new-compiler -gc none -nocache -warn-about-allocs -o ${os.quoted_path(out_path)} ${os.quoted_path(main_path)}')
	assert result.exit_code == 0, result.output
	moved := 'allocation (local moved to the heap: its address escapes)'
	assert result.output.count(moved) == 1, result.output
	on_heap_line := source.all_before('mut on_heap := ').count('\n') + 1
	assert result.output.contains('main.v:${on_heap_line}:6: warning: ${moved}'), result.output
	c_code := os.read_file(out_path) or { panic(err) }
	assert c_code.contains('main__Entry* on_heap = ')
	assert c_code.contains('fill_and_keep(on_heap)')
	assert !c_code.contains('fill_and_keep(&on_heap)')
}
