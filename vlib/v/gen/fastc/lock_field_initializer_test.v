module fastc

import os
import v.cmdexec

fn test_fastc_lock_named_fields_preserve_explicit_initializers() {
	$if !macos && !linux {
		return
	}
	vroot := os.real_path(os.join_path(@DIR, '../../../..'))
	root := os.join_path(os.vtmp_dir(), 'fastc_lock_fields_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
import sync
type ValueId = u32
struct Holder {
mut:
	lock &sync.RwMutex = unsafe { nil }
	values map[string]int
	ids map[u64]ValueId
}
fn new_holder() &Holder {
	return &Holder{
		lock: sync.new_rwmutex()
		values: {"answer": 42}
		ids: map[u64]ValueId{}
	}
}
fn check(mut holder Holder) {
	assert !isnil(holder.lock)
	holder.lock.lock()
	assert holder.values["answer"] == 42
	holder.ids[7] = ValueId(23)
	assert holder.ids[7] == ValueId(23)
	holder.lock.unlock()
}
fn has_values(holder &Holder) bool {
	return !isnil(holder.lock) && holder.values["answer"] == 42
}
fn main() {
	mut holder := new_holder()
	check(mut holder)
	assert has_values(&Holder{
		lock: sync.new_rwmutex()
		values: {"answer": 42}
		ids: map[u64]ValueId{}
	})
	println("lock fields work")
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-nocache', '-gc', 'none', '-o',
		program, root], vroot, {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'lock fields work', run.output
}
