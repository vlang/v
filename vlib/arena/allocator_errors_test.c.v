import os

const allocator_errors_source = r'
import arena
import os

fn allocate_on_other_thread(allocator arena.Allocator) {
	_ = unsafe { allocator.calloc(16) }
}

fn main() {
	mut storage := arena.new()
	match os.args[1] {
		"negative_heap" { _ = unsafe { arena.heap_allocator().calloc(-1) } }
		"negative_arena" { _ = unsafe { storage.allocator().calloc(-1) } }
		"freed_arena" {
			storage.free()
			_ = storage.allocator()
		}
		"active_elsewhere" {
			allocator := storage.allocator()
			storage.push()
			worker := spawn allocate_on_other_thread(allocator)
			worker.wait()
			storage.pop()
		}
		else { panic("unknown test case") }
	}
}
'

fn test_explicit_allocator_rejects_invalid_uses() ! {
	work_dir := os.join_path(os.vtmp_dir(), 'arena_allocator_errors_${os.getpid()}')
	os.mkdir_all(work_dir)!
	defer { os.rmdir_all(work_dir) or {} }
	source := os.join_path(work_dir, 'errors.v')
	binary := os.join_path(work_dir, 'errors' + $if windows { '.exe' } $else { '' })
	os.write_file(source, allocator_errors_source)!
	compile := os.exec([@VEXE, '-gc', 'none', '-o', binary, source])
	assert compile.exit_code == 0, compile.output
	for case_name, message in {
		'negative_heap':    'negative allocation size'
		'negative_arena':   'negative allocation size'
		'freed_arena':      'allocator() of a freed arena'
		'active_elsewhere': 'allocation in an arena active on another thread or being modified'
	} {
		result := os.exec([binary, case_name])
		assert result.exit_code != 0, '${case_name}: ${result.output}'
		assert result.output.contains('arena: ${message}'), '${case_name}: ${result.output}'
	}
}
