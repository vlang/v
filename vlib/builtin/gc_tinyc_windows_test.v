import os
import v.cmdexec

fn test_windows_tinyc_precise_gc_build_uses_selected_object_and_counts_allocations() ! {
	compiler := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	if !os.is_file(compiler) {
		return
	}
	dir := os.join_path(os.vtmp_dir(), 'gc_windows_tinyc_runtime_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'main.c.v')
	os.write_file(source, '#include <gc.h>
fn C.GC_get_total_bytes() uint
fn main() {
 mut retained := [][]u8{cap: 128}
 for i in 0 .. 128 {
  before := C.GC_get_total_bytes()
  mut data := []u8{len: 16}
  data[0] = u8(i)
  after := C.GC_get_total_bytes()
  assert after >= before + 16
  retained << data
 }
 for i, data in retained {
  assert data[0] == u8(i)
 }
}
')!
	flags_file := os.join_path(dir, 'flags.txt')
	program := os.join_path(dir, 'precise.exe')
	compiled := cmdexec.run(@VEXE, ['-new-compiler', '-no-retry-compilation', '-cc', compiler,
		'-gc', 'boehm', '-d', 'no_gc_thread_local_alloc', '-dump-c-flags', flags_file, '-o', program,
		source])
	assert compiled.exit_code == 0, compiled.output
	flags := os.read_file(flags_file)!
	assert flags.contains('gc_tinyc_windows_o_'), flags
	assert !flags.contains('libgc.a'), flags
	assert !flags.contains('THREAD_LOCAL_ALLOC'), flags
	assert !flags.contains('GC_BUILTIN_ATOMIC'), flags
	ran := cmdexec.run(program, [])
	assert ran.exit_code == 0, ran.output
}
