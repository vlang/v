module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_spawned_parameter_addresses_keep_one_live_storage_cell() {
	$if !macos && !linux { return }
	root := os.join_path(os.vtmp_dir(), 'fastc_spawn_parameter_escape_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'gate.h'), 'void clobber_stack(void);
void set_state_value(void *state, long long value);
void wait_gate(void);
void release_gate(void);
void reset_gate(void);
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), r'module main
#include "@DIR/gate.h"
struct State {
mut:
	value i64
	values []i64
}
fn C.clobber_stack()
fn C.set_state_value(state voidptr, value i64)
fn C.wait_gate()
fn C.release_gate()
fn C.reset_gate()
fn read_after(state &State) i64 {
	C.wait_gate()
	return state.value + state.values[0]
}
fn (state &State) read_method() i64 {
	C.wait_gate()
	return state.value + state.values[0]
}
fn start(state State) thread i64 {
	return spawn read_after(&state)
}
fn make_worker(value i64) thread i64 {
	state := State{value: value, values: [i64(1)]}
	return start(state)
}
fn start_method(state State) thread i64 {
	return spawn state.read_method()
}
fn start_shared(state State) []thread i64 {
	mut workers := []thread i64{}
	workers << spawn read_after(&state)
	workers << spawn read_after(&state)
	C.set_state_value(voidptr(&state), 99)
	return workers
}
fn start_borrowed(state &State) thread i64 {
	return spawn read_after(state)
}
fn main() {
	first := make_worker(42)
	second := make_worker(99)
	C.clobber_stack()
	C.release_gate()
	assert first.wait() == 43
	assert second.wait() == 100
	C.reset_gate()
	original := State{value: 42, values: [i64(1)]}
	shared := start_shared(original)
	C.clobber_stack()
	C.release_gate()
	assert shared.wait() == [i64(100), 100]
	assert original.value == 42
	C.reset_gate()
	mut borrowed := State{value: 5, values: [i64(1)]}
	worker := start_borrowed(&borrowed)
	borrowed.value = 7
	C.release_gate()
	assert worker.wait() == 8
	C.reset_gate()
	method := start_method(State{value: 70, values: [i64(2)]})
	C.clobber_stack()
	C.release_gate()
	assert method.wait() == 72
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	c_source := generate_files([root], prefs) or { panic(err) }
	c_path := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_path, c_source + r'
static pthread_mutex_t gate_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t gate_cond = PTHREAD_COND_INITIALIZER;
static bool gate_ready;
void wait_gate(void) {
	pthread_mutex_lock(&gate_mutex);
	while (!gate_ready) pthread_cond_wait(&gate_cond, &gate_mutex);
	pthread_mutex_unlock(&gate_mutex);
}
void release_gate(void) {
	pthread_mutex_lock(&gate_mutex);
	gate_ready = true;
	pthread_cond_broadcast(&gate_cond);
	pthread_mutex_unlock(&gate_mutex);
}
void reset_gate(void) {
	pthread_mutex_lock(&gate_mutex);
	gate_ready = false;
	pthread_mutex_unlock(&gate_mutex);
}
void clobber_stack(void) {
	volatile u64 storage[8192];
	for (int i = 0; i < 8192; i++) storage[i] = 0x1111222233334444;
}
void set_state_value(void *state, i64 value) { *(i64 *)state = value; }
') or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-pthread', '-o', program, c_path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
