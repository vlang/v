import os

// A separate process is needed because other arena tests have installed the hooks.
const first_use_source = r'
import arena
import time

fn create_first_arena() {
	time.sleep(time.millisecond)
	mut a := arena.new()
	a.push()
	text := "testing arena".repeat(10)
	assert text.len == 130
	a.pop()
	a.free()
}

fn main() {
	worker := spawn create_first_arena()
	for _ in 0 .. 1_000_000 {
		// Exercise the allocator entry points while another thread publishes the hooks.
		unsafe {
			p := malloc(32)
			free(p)
		}
	}
	worker.wait()
}
'

fn test_first_arena_publication_during_heap_allocations() ! {
	work_dir := os.join_path(os.vtmp_dir(), 'arena_first_use_${os.getpid()}')
	os.mkdir_all(work_dir)!
	defer {
		os.rmdir_all(work_dir) or {}
	}
	source := os.join_path(work_dir, 'first_use.v')
	binary := os.join_path(work_dir, 'first_use' + $if windows { '.exe' } $else { '' })
	os.write_file(source, first_use_source)!
	compile := os.exec([@VEXE, '-gc', 'none', '-o', binary, source])
	assert compile.exit_code == 0, compile.output
	for _ in 0 .. 3 {
		result := os.exec([binary])
		assert result.exit_code == 0, result.output
	}
}
