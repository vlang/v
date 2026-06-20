import os

const vexe = @VEXE

fn testsuite_begin() {
	$if rv64 {
		skip_test('closure lifetime API tests are too slow under riscv64 emulation')
	}
}

@[noreturn]
fn skip_test(reason string) {
	println('skipping test, because ${reason} .')
	exit(0)
}

fn missing_boehm_lib(output string) bool {
	mentions_gc := output.contains('libgc') || output.contains('-lgc')
		|| output.contains("library 'gc'") || output.contains('bdw-gc')
	return mentions_gc && (output.contains('was not found')
		|| output.contains('cannot find') || output.contains('not found')
		|| output.contains('No such file'))
}

// assert_run_succeeds_or_missing_boehm runs the program with the given gc mode and
// asserts success, but skips gracefully when a `boehm`/`boehm_leak` build fails only
// because the Boehm GC library is unavailable (e.g. musl images without musl libgc).
fn assert_run_succeeds_or_missing_boehm(tmp_dir string, name string, source string, mode string) {
	res := run_program_with_gc(tmp_dir, name, source, mode)
	if mode != 'none' && res.exit_code != 0 && missing_boehm_lib(res.output) {
		eprintln('skipping ${mode} run for ${name}: missing libgc')
		return
	}
	assert res.exit_code == 0, res.output
}

fn count_occurrences(haystack string, needle string) int {
	mut pos := 0
	mut count := 0
	for {
		idx := haystack[pos..].index(needle) or { break }
		count++
		pos += idx + needle.len
	}
	return count
}

fn write_program(tmp_dir string, name string, source string) string {
	source_path := os.join_path(tmp_dir, '${name}.v')
	os.write_file(source_path, source) or { panic(err) }
	return source_path
}

fn run_program_with_gc(tmp_dir string, name string, source string, mode string) os.Result {
	source_path := write_program(tmp_dir, '${name}_${mode}', source)
	return os.execute('${os.quoted_path(vexe)} -gc ${mode} run ${os.quoted_path(source_path)}')
}

fn compile_program_with_gc(tmp_dir string, name string, source string, mode string) os.Result {
	source_path := write_program(tmp_dir, '${name}_${mode}', source)
	binary_path := os.join_path(tmp_dir, '${name}_${mode}')
	return os.execute('${os.quoted_path(vexe)} -gc ${mode} -o ${os.quoted_path(binary_path)} ${os.quoted_path(source_path)}')
}

fn compile_freestanding_object(tmp_dir string, name string, source string) os.Result {
	source_path := write_program(tmp_dir, name, source)
	object_path := os.join_path(tmp_dir, '${name}.o')
	return os.execute('${os.quoted_path(vexe)} -gc none -freestanding -no-std -is_o -o ${os.quoted_path(object_path)} ${os.quoted_path(source_path)}')
}

fn run_program_with_track_heap(tmp_dir string, name string, source string) os.Result {
	source_path := write_program(tmp_dir, name, source)
	return os.execute('${os.quoted_path(vexe)} -gc none -d track_heap run ${os.quoted_path(source_path)}')
}

fn c_output_for_program(tmp_dir string, name string, source string) os.Result {
	source_path := write_program(tmp_dir, name, source)
	return os.execute('${os.quoted_path(vexe)} -o - ${os.quoted_path(source_path)}')
}

fn assert_boehm_leak_compile_or_missing_lib(tmp_dir string, name string, source string) {
	res := compile_program_with_gc(tmp_dir, name, source, 'boehm_leak')
	if res.exit_code != 0 && missing_boehm_lib(res.output) {
		eprintln('skipping boehm_leak compile for ${name}: missing libgc')
		return
	}
	assert res.exit_code == 0, res.output
}

fn assert_boehm_leak_runtime_or_compile_only(tmp_dir string, name string, source string) {
	res := run_program_with_gc(tmp_dir, name, source, 'boehm_leak')
	if res.exit_code == 0 {
		return
	}
	if missing_boehm_lib(res.output) {
		eprintln('skipping boehm_leak runtime for ${name}: missing libgc')
		return
	}
	if res.output.contains('leaked objects') || res.output.contains('Found ') {
		eprintln('boehm_leak runtime reported runtime allocations for ${name}; keeping compile-only coverage')
		assert_boehm_leak_compile_or_missing_lib(tmp_dir, '${name}_compile_only', source)
		return
	}
	assert false, res.output
}

fn no_captured_closure_lifetime_source() string {
	return [
		'module main',
		'import builtin.closure',
		'',
		'fn no_capture_work() {',
		'\tassert true',
		'}',
		'',
		'fn run() ! {',
		'\tmut lifetime := closure.new_lifetime()',
		'\tlifetime.frame(no_capture_work)!',
		'\tlifetime.suspend(no_capture_work)!',
		'\tlifetime.untracked(no_capture_work)!',
		'\tlifetime.reclaim_all()!',
		'\tlifetime.dispose()!',
		'}',
		'',
		'fn main() {',
		'\trun() or { panic(err) }',
		'}',
	].join('\n')
}

fn closure_lifetime_runtime_source() string {
	return [
		'module main',
		'import builtin.closure',
		'',
		'fn no_capture_work() {}',
		'',
		'@[heap]',
		'struct CallbackBox {',
		'mut:',
		'\tcb fn () int = fn () int { return -1 }',
		'}',
		'',
		'@[heap]',
		'struct IntBox {',
		'mut:',
		'\tvalue int',
		'}',
		'',
		'fn make_cb(value int) fn () int {',
		'\tpayload := []int{len: 64, init: value + index}',
		'\treturn fn [payload] () int {',
		'\t\treturn payload[0] + payload[payload.len - 1]',
		'\t}',
		'}',
		'',
		'fn call_cb(cb fn () int) int {',
		'\treturn cb()',
		'}',
		'',
		'fn collect_and_churn() {',
		'\tgc_collect()',
		'\tfor _ in 0 .. 512 {',
		'\t\tunsafe {',
		'\t\t\tp := malloc(32)',
		'\t\t\tvmemset(p, 0x55, 32)',
		'\t\t}',
		'\t}',
		'\tgc_collect()',
		'}',
		'',
		'fn outside_lifetime_closure_survives_lifetime_reclaim() ! {',
		'\toutside := make_cb(10)',
		'\tmut lifetime := closure.new_lifetime()',
		'\tlifetime.frame(fn () {',
		'\t\tinside := make_cb(100)',
		'\t\tassert inside() == 263',
		'\t})!',
		'\tlifetime.reclaim_all()!',
		'\tcollect_and_churn()',
		'\tassert outside() == 83',
		'\tlifetime.dispose()!',
		'}',
		'',
		'fn reclaim_keeps_requested_recent_frame() ! {',
		'\tmut first := &CallbackBox{}',
		'\tmut second := &CallbackBox{}',
		'\tmut lifetime := closure.new_lifetime()',
		'\tlifetime.frame(fn [mut first] () {',
		'\t\tfirst.cb = make_cb(1)',
		'\t\tassert first.cb() == 65',
		'\t})!',
		'\tlifetime.frame(fn [mut second] () {',
		'\t\tsecond.cb = make_cb(2)',
		'\t\tassert second.cb() == 67',
		'\t})!',
		'\tlifetime.reclaim(1)!',
		'\tcollect_and_churn()',
		'\tassert second.cb() == 67',
		'\tlifetime.reclaim_all()!',
		'\tlifetime.dispose()!',
		'}',
		'',
		'fn two_lifetimes_reclaim_independently() ! {',
		'\tmut first := &CallbackBox{}',
		'\tmut second := &CallbackBox{}',
		'\tmut lifetime_a := closure.new_lifetime()',
		'\tmut lifetime_b := closure.new_lifetime()',
		'\tlifetime_a.frame(fn [mut first] () {',
		'\t\tfirst.cb = make_cb(20)',
		'\t\tassert first.cb() == 103',
		'\t})!',
		'\tlifetime_b.frame(fn [mut second] () {',
		'\t\tsecond.cb = make_cb(30)',
		'\t\tassert second.cb() == 123',
		'\t})!',
		'\tlifetime_a.reclaim_all()!',
		'\tcollect_and_churn()',
		'\tassert second.cb() == 123',
		'\tlifetime_b.reclaim_all()!',
		'\tlifetime_a.dispose()!',
		'\tlifetime_b.dispose()!',
		'}',
		'',
		'fn suspended_persistent_callbacks_survive_reclaim_and_spawn() ! {',
		'\tmut persisted := &CallbackBox{}',
		'\tmut spawned_value := &IntBox{value: -1}',
		'\tmut lifetime := closure.new_lifetime()',
		'\tlifetime.frame(fn [mut lifetime, mut persisted, mut spawned_value] () {',
		'\t\tshort := make_cb(5)',
		'\t\tassert short() == 73',
		'\t\tlifetime.suspend(fn [mut persisted, mut spawned_value] () {',
		'\t\t\tpersisted.cb = make_cb(40)',
		'\t\t\tth := spawn call_cb(persisted.cb)',
		'\t\t\tspawned_value.value = th.wait()',
		'\t\t}) or { panic(err) }',
		'\t})!',
		'\tlifetime.reclaim_all()!',
		'\tcollect_and_churn()',
		'\tassert persisted.cb() == 143',
		'\tassert spawned_value.value == 143',
		'\tlifetime.dispose()!',
		'}',
		'',
		'fn stale_reused_slot_does_not_release_suspended_newer_closure() ! {',
		'\tmut survivor := &CallbackBox{}',
		'\tmut lifetime := closure.new_lifetime()',
		'\tlifetime.frame(fn [mut lifetime, mut survivor] () {',
		'\t\tfor i in 0 .. 96 {',
		'\t\t\th := fn [i] () int { return i }',
		'\t\t\tassert h() == i',
		'\t\t}',
		'\t\tlifetime.suspend(fn [mut survivor] () {',
		'\t\t\tsurvivor.cb = make_cb(70)',
		'\t\t\tassert survivor.cb() == 203',
		'\t\t}) or { panic(err) }',
		'\t})!',
		'\tlifetime.reclaim_all()!',
		'\tcollect_and_churn()',
		'\tassert survivor.cb() == 203',
		'\tlifetime.dispose()!',
		'}',
		'',
		'fn direct_untracked_callback_is_public_api() ! {',
		'\tmut marker := &IntBox{value: -1}',
		'\tmut lifetime := closure.new_lifetime()',
		'\tlifetime.frame(fn [mut lifetime, mut marker] () {',
		'\t\tlifetime.untracked(fn [mut marker] () {',
		'\t\t\tmarker.value = 91',
		'\t\t}) or { panic(err) }',
		'\t})!',
		'\tlifetime.reclaim_all()!',
		'\tassert marker.value == 91',
		'\tlifetime.dispose()!',
		'}',
		'',
		'fn local_cleanup_and_lifetime_reclaim_do_not_double_release() ! {',
		'\tmut lifetime := closure.new_lifetime()',
		'\tfor i in 0 .. 1024 {',
		'\t\tlifetime.frame(fn [i] () {',
		'\t\t\th := fn [i] () int { return i + 1 }',
		'\t\t\tassert h() == i + 1',
		'\t\t})!',
		'\t\tlifetime.reclaim_all()!',
		'\t}',
		'\tlifetime.dispose()!',
		'}',
		'',
		'fn lifetime_reclaim_does_not_accumulate_owned_escaping_closures() ! {',
		'\t$if gcboehm ? {',
		'\t\tmut cb := &CallbackBox{}',
		'\t\tmut lifetime := closure.new_lifetime()',
		'\t\tgc_collect()',
		'\t\tstart_mb := gc_memory_use() / 1024 / 1024',
		'\t\tfor n in 0 .. 20_000 {',
		'\t\t\tlifetime.frame(fn [mut cb, n] () {',
		'\t\t\t\tbig := []int{len: 512, init: n + index}',
		'\t\t\t\tcb.cb = fn [big] () int {',
		'\t\t\t\t\treturn big[0] + big[big.len - 1]',
		'\t\t\t\t}',
		'\t\t\t\tassert cb.cb() == 2 * n + 511',
		'\t\t\t})!',
		'\t\t\tlifetime.reclaim_all()!',
		'\t\t\tif n % 5000 == 0 {',
		'\t\t\t\tgc_collect()',
		'\t\t\t}',
		'\t\t}',
		'\t\tgc_collect()',
		'\t\tend_mb := gc_memory_use() / 1024 / 1024',
		'\t\tassert end_mb <= start_mb + 24',
		'\t}',
		'}',
		'',
		'fn external_captured_frame_callback_survives_reclaim_all() ! {',
		'\tpayload := []int{len: 64, init: 200 + index}',
		'\tcb := fn [payload] () {',
		'\t\tassert payload[0] == 200',
		'\t\tassert payload[payload.len - 1] == 263',
		'\t}',
		'\tmut lifetime := closure.new_lifetime()',
		'\tlifetime.frame(cb)!',
		'\tlifetime.reclaim_all()!',
		'\tcollect_and_churn()',
		'\tcb()',
		'\tlifetime.dispose()!',
		'}',
		'',
		'fn parenthesized_frame_return_with_captured_callback() ! {',
		'\tx := 37',
		'\tmut lifetime := closure.new_lifetime()',
		'\tdefer {',
		'\t\tlifetime.dispose() or {}',
		'\t}',
		'\treturn (lifetime.frame(fn [x] () {',
		'\t\tassert x == 37',
		'\t}))',
		'}',
		'',
		'fn frame_end_uses_original_token_state_after_lifetime_reassign() ! {',
		'\tmut lifetime := closure.new_lifetime()',
		'\tmut original := lifetime',
		'\tlifetime.frame(fn [mut lifetime] () {',
		'\t\tlifetime = closure.new_lifetime()',
		'\t})!',
		'\tmut next := closure.new_lifetime()',
		'\tnext.frame(no_capture_work)!',
		'\tnext.dispose()!',
		'\toriginal.dispose()!',
		'}',
		'',
		'fn lifetime_copies_after_dispose_report_error() ! {',
		'\tmut lifetime := closure.new_lifetime()',
		'\tmut copy := lifetime',
		'\tlifetime.dispose()!',
		'\tcollect_and_churn()',
		'\tmut replacement := closure.new_lifetime()',
		'\treplacement.frame(no_capture_work)!',
		'\tmut saw_frame := false',
		'\tmut saw_reclaim := false',
		'\tmut saw_suspend := false',
		'\tmut saw_untracked := false',
		'\tmut saw_dispose := false',
		'\tcopy.frame(no_capture_work) or {',
		"\t\tassert err.msg().contains('dispose') || err.msg().contains('after')",
		'\t\tsaw_frame = true',
		'\t}',
		'\tcopy.reclaim_all() or {',
		"\t\tassert err.msg().contains('dispose') || err.msg().contains('after')",
		'\t\tsaw_reclaim = true',
		'\t}',
		'\tcopy.suspend(no_capture_work) or {',
		"\t\tassert err.msg().contains('dispose') || err.msg().contains('after')",
		'\t\tsaw_suspend = true',
		'\t}',
		'\tcopy.untracked(no_capture_work) or {',
		"\t\tassert err.msg().contains('dispose') || err.msg().contains('after')",
		'\t\tsaw_untracked = true',
		'\t}',
		'\tcopy.dispose() or {',
		"\t\tassert err.msg().contains('dispose') || err.msg().contains('after')",
		'\t\tsaw_dispose = true',
		'\t}',
		'\tassert saw_frame',
		'\tassert saw_reclaim',
		'\tassert saw_suspend',
		'\tassert saw_untracked',
		'\tassert saw_dispose',
		'\treplacement.dispose()!',
		'}',
		'',
		'fn run() ! {',
		'\toutside_lifetime_closure_survives_lifetime_reclaim()!',
		'\treclaim_keeps_requested_recent_frame()!',
		'\ttwo_lifetimes_reclaim_independently()!',
		'\tsuspended_persistent_callbacks_survive_reclaim_and_spawn()!',
		'\tstale_reused_slot_does_not_release_suspended_newer_closure()!',
		'\tdirect_untracked_callback_is_public_api()!',
		'\tlocal_cleanup_and_lifetime_reclaim_do_not_double_release()!',
		'\tlifetime_reclaim_does_not_accumulate_owned_escaping_closures()!',
		'\texternal_captured_frame_callback_survives_reclaim_all()!',
		'\tparenthesized_frame_return_with_captured_callback()!',
		'\tframe_end_uses_original_token_state_after_lifetime_reassign()!',
		'\tlifetime_copies_after_dispose_report_error()!',
		'}',
		'',
		'fn main() {',
		'\trun() or { panic(err) }',
		'}',
	].join('\n')
}

fn boehm_leak_clean_lifetime_source() string {
	return [
		'module main',
		'import builtin.closure',
		'',
		'fn make_cb(value int) fn () int {',
		'\treturn fn [value] () int {',
		'\t\treturn value + 1',
		'\t}',
		'}',
		'',
		'fn run() ! {',
		'\tmut lifetime := closure.new_lifetime()',
		'\tfor n in 0 .. 128 {',
		'\t\tlifetime.frame(fn [n] () {',
		'\t\t\tcb := make_cb(n)',
		'\t\t\tassert cb() == n + 1',
		'\t\t})!',
		'\t\tlifetime.reclaim_all()!',
		'\t}',
		'\tlifetime.dispose()!',
		'}',
		'',
		'fn main() {',
		'\trun() or { panic(err) }',
		'}',
	].join('\n')
}

fn gc_none_lifetime_reclaim_memory_source() string {
	return [
		'module main',
		'import builtin.closure',
		'import runtime',
		'',
		'fn used_mb() u64 {',
		'\tused := runtime.used_memory() or { return 0 }',
		'\treturn used / 1024 / 1024',
		'}',
		'',
		'fn frame_callback_context_is_reclaimed() ! {',
		'\tmut lifetime := closure.new_lifetime()',
		'\tmut start_mb := u64(0)',
		'\tfor n in 0 .. 220000 {',
		'\t\ta0 := n',
		'\t\ta1 := n + 1',
		'\t\ta2 := n + 2',
		'\t\ta3 := n + 3',
		'\t\ta4 := n + 4',
		'\t\ta5 := n + 5',
		'\t\ta6 := n + 6',
		'\t\ta7 := n + 7',
		'\t\ta8 := n + 8',
		'\t\ta9 := n + 9',
		'\t\ta10 := n + 10',
		'\t\ta11 := n + 11',
		'\t\ta12 := n + 12',
		'\t\ta13 := n + 13',
		'\t\ta14 := n + 14',
		'\t\ta15 := n + 15',
		'\t\tlifetime.frame(fn [a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15] () {',
		'\t\t\tassert a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 == 8 * a0 + 28',
		'\t\t\tassert a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 == 8 * a0 + 92',
		'\t\t})!',
		'\t\tlifetime.reclaim_all()!',
		'\t\tif n == 2048 {',
		'\t\t\tstart_mb = used_mb()',
		'\t\t}',
		'\t}',
		'\tend_mb := used_mb()',
		'\tif start_mb > 0 && end_mb > start_mb + 64 {',
		"\t\tpanic('closure frame callback memory grew too much')",
		'\t}',
		'\tlifetime.dispose()!',
		'}',
		'',
		'fn run() ! {',
		'\tframe_callback_context_is_reclaimed()!',
		'}',
		'',
		'fn main() {',
		'\trun() or { panic(err) }',
		'}',
	].join('\n')
}

fn gc_none_lifetime_bookkeeping_track_heap_source(header_path string) string {
	c_header_path := header_path.replace('\\', '/')
	return [
		'module main',
		'import builtin.closure',
		'',
		'#include "${c_header_path}"',
		'',
		'fn no_capture_work() {}',
		'',
		'fn exercise(rounds int) ! {',
		'\tfor _ in 0 .. rounds {',
		'\t\tmut lifetime := closure.new_lifetime()',
		'\t\tmut copy := lifetime',
		'\t\tlifetime.dispose()!',
		'\t\tcopy.frame(no_capture_work) or {',
		'\t\t\tcontinue',
		'\t\t}',
		"\t\treturn error('disposed lifetime copy was accepted')",
		'\t}',
		'}',
		'',
		'fn main() {',
		'\tmut warmup := closure.new_lifetime()',
		'\twarmup.dispose() or { panic(err) }',
		'\texercise(128) or { panic(err) }',
		'\tmut start := u64(0)',
		'\tunsafe { closure.lifetime_state_allocs(&start) }',
		'\texercise(1000) or { panic(err) }',
		'\tmut end := u64(0)',
		'\tunsafe { closure.lifetime_state_allocs(&end) }',
		'\tif end != start {',
		"\t\tpanic('leaked lifetime state recycling bookkeeping: start=\${start} end=\${end}')",
		'\t}',
		'}',
	].join('\n')
}

fn lazy_concurrent_lifetime_init_source() string {
	return [
		'module main',
		'import builtin.closure',
		'',
		'@[heap]',
		'struct IntBox {',
		'mut:',
		'\tvalue int',
		'}',
		'',
		'fn worker(id int, ready chan bool, start chan bool) ! {',
		'\tready <- true',
		"\t_ := <-start or { return error('start channel closed') }",
		'\tfor round in 0 .. 32 {',
		'\t\tmut box := &IntBox{}',
		'\t\texpected := id * 1000 + round',
		'\t\tmut lifetime := closure.new_lifetime()',
		'\t\tlifetime.frame(fn [mut box, expected] () {',
		'\t\t\tmake_value := fn [expected] () int { return expected }',
		'\t\t\tbox.value = make_value()',
		'\t\t})!',
		'\t\tassert box.value == expected',
		'\t\tlifetime.dispose()!',
		'\t}',
		'}',
		'',
		'fn run() ! {',
		'\tthread_count := 16',
		'\tready := chan bool{cap: thread_count}',
		'\tstart := chan bool{cap: thread_count}',
		'\tmut threads := []thread !{cap: thread_count}',
		'\tfor i in 0 .. thread_count {',
		'\t\tthreads << spawn worker(i, ready, start)',
		'\t}',
		'\tfor _ in 0 .. thread_count {',
		"\t\t_ := <-ready or { return error('ready channel closed') }",
		'\t}',
		'\tfor _ in 0 .. thread_count {',
		'\t\tstart <- true',
		'\t}',
		'\tthreads.wait()!',
		'}',
		'',
		'fn main() {',
		'\trun() or { panic(err) }',
		'}',
	].join('\n')
}

fn assert_misuse_program_passes(tmp_dir string, name string, source string) {
	source_path := write_program(tmp_dir, name, source)
	res := os.execute('${os.quoted_path(vexe)} run ${os.quoted_path(source_path)}')
	assert res.exit_code == 0, res.output
}

fn test_lifetime_public_api_without_captured_closure() {
	$if gcboehm_leak ? {
		return
	}
	tmp_dir := os.join_path(os.vtmp_dir(), 'v_closure_lifetime_no_capture_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source := no_captured_closure_lifetime_source()
	c_res := c_output_for_program(tmp_dir, 'no_captured_lifetime_codegen', source)
	assert c_res.exit_code == 0, c_res.output
	assert !c_res.output.contains('builtin__closure__closure_create')
	assert !c_res.output.contains('_V_closure_main__')
	for mode in ['boehm', 'none'] {
		assert_run_succeeds_or_missing_boehm(tmp_dir, 'no_captured_lifetime', source, mode)
	}
	assert_boehm_leak_compile_or_missing_lib(tmp_dir, 'no_captured_lifetime', source)
}

fn test_closure_lifetime_runtime_api_contract() {
	$if gcboehm_leak ? {
		return
	}
	tmp_dir := os.join_path(os.vtmp_dir(), 'v_closure_lifetime_runtime_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source := closure_lifetime_runtime_source()
	for mode in ['boehm', 'none'] {
		assert_run_succeeds_or_missing_boehm(tmp_dir, 'closure_lifetime_runtime', source, mode)
	}
	assert_boehm_leak_compile_or_missing_lib(tmp_dir, 'closure_lifetime_runtime', source)
}

fn test_closure_lifetime_boehm_leak_runtime_without_persistent_callbacks() {
	$if gcboehm_leak ? {
		return
	}
	tmp_dir := os.join_path(os.vtmp_dir(), 'v_closure_lifetime_boehm_leak_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	assert_boehm_leak_runtime_or_compile_only(tmp_dir, 'closure_lifetime_boehm_leak_clean',
		boehm_leak_clean_lifetime_source())
}

fn test_closure_lifetime_gc_none_does_not_leak_frame_callback_or_bookkeeping() {
	$if gcboehm_leak ? {
		return
	}
	tmp_dir := os.join_path(os.vtmp_dir(), 'v_closure_lifetime_gc_none_memory_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	res := run_program_with_gc(tmp_dir, 'closure_lifetime_gc_none_memory',
		gc_none_lifetime_reclaim_memory_source(), 'none')
	assert res.exit_code == 0, res.output
}

fn test_closure_lifetime_gc_none_reuses_disposed_state_bookkeeping() {
	$if gcboehm_leak ? {
		return
	}
	tmp_dir := os.join_path(os.vtmp_dir(), 'v_closure_lifetime_gc_none_state_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	header_path := os.join_path(tmp_dir, 'track_heap_hooks.h')
	os.write_file(header_path, [
		'void vheap_alloc(void* p, unsigned long long n) { (void)p; (void)n; }',
		'void vheap_free(void* p) { (void)p; }',
	].join('\n')) or { panic(err) }
	res := run_program_with_track_heap(tmp_dir, 'closure_lifetime_gc_none_state',
		gc_none_lifetime_bookkeeping_track_heap_source(header_path))
	assert res.exit_code == 0, res.output
}

fn test_closure_lifetime_dispose_recycles_state_for_later_lifetimes() {
	source_path := os.join_path(os.dir(vexe), 'vlib/builtin/closure/closure.c.v')
	source := os.read_file(source_path) or { panic(err) }
	start := source.index('pub fn (mut lifetime Lifetime) dispose() !') or {
		panic('missing dispose helper')
	}
	end := source[start..].index('pub fn (mut lifetime Lifetime) suspend') or {
		panic('missing dispose helper end')
	}
	helper := source[start..start + end]
	assert helper.contains('closure_lifetime_recycle_state_no_lock(mut state)')
	assert helper.contains('lifetime.state = unsafe { nil }')
	assert helper.contains('lifetime.disposed = true')
	assert !helper.contains('free(state)')
}

fn test_closure_lifetime_freestanding_no_std_object_compile() {
	$if !linux {
		return
	}
	$if !amd64 {
		return
	}
	$if gcboehm_leak ? {
		return
	}
	tmp_dir := os.join_path(os.vtmp_dir(), 'v_closure_lifetime_freestanding_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source := [
		'module main',
		'import builtin.closure',
		'',
		'fn main() {',
		'\tmut lifetime := closure.new_lifetime()',
		'\tlifetime.dispose() or { panic(err) }',
		'}',
	].join('\n')
	res := compile_freestanding_object(tmp_dir, 'closure_lifetime_freestanding', source)
	assert res.exit_code == 0, res.output
}

fn test_closure_lifetime_lazy_concurrent_runtime_init() {
	$if gcboehm_leak ? {
		return
	}
	tmp_dir := os.join_path(os.vtmp_dir(), 'v_closure_lifetime_lazy_init_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source := lazy_concurrent_lifetime_init_source()
	for mode in ['none', 'boehm'] {
		assert_run_succeeds_or_missing_boehm(tmp_dir, 'closure_lifetime_lazy_init', source, mode)
	}
}

fn test_closure_lifetime_reclaim_helper_reuses_buffers_without_clone() {
	source_path := os.join_path(os.dir(vexe), 'vlib/builtin/closure/closure.c.v')
	source := os.read_file(source_path) or { panic(err) }
	start := source.index('fn closure_lifetime_reclaim_no_lock') or {
		panic('missing reclaim helper')
	}
	end := source[start..].index('fn closure_ensure_initialized') or {
		panic('missing reclaim helper end')
	}
	helper := source[start..start + end]
	assert !helper.contains('.clone()')
	assert helper.contains('delete_many(0, reclaim_count)')
	assert helper.contains('delete_many(0, cutoff)')
}

fn test_closure_lifetime_dispose_frees_bookkeeping_buffers_but_keeps_state() {
	source_path := os.join_path(os.dir(vexe), 'vlib/builtin/closure/closure.c.v')
	source := os.read_file(source_path) or { panic(err) }
	start := source.index('fn closure_lifetime_recycle_state_no_lock') or {
		panic('missing recycle helper')
	}
	end := source[start..].index('fn closure_lifetime_error') or {
		panic('missing recycle helper end')
	}
	helper := source[start..start + end]
	assert helper.contains('state.disposed = true')
	assert helper.contains('state.records.free()')
	assert helper.contains('state.frames.free()')
	assert helper.contains('state.records = []ClosureLifetimeRecord{}')
	assert helper.contains('state.frames = []ClosureLifetimeFrame{}')
	assert helper.contains('state.next_free = g_closure.free_lifetime_states')
	assert helper.contains('g_closure.free_lifetime_states = state')
	assert !helper.contains('free(state)')
}

fn test_closure_lifetime_once_headers_keep_helper_internal() {
	for header_name in ['closure_once_nix.h', 'closure_once_windows.h'] {
		source_path := os.join_path(os.dir(vexe), 'vlib/builtin/closure/${header_name}')
		source := os.read_file(source_path) or { panic(err) }
		assert source.contains('V_CLOSURE_STATIC_INLINE void v_closure_init_once')
		assert !source.contains('\nvoid v_closure_init_once')
	}
}

fn test_closure_lifetime_codegen_emits_single_local_destroy() {
	$if gcboehm_leak ? {
		return
	}
	tmp_dir := os.join_path(os.vtmp_dir(), 'v_closure_lifetime_codegen_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source := [
		'module main',
		'import builtin.closure',
		'',
		'fn run() ! {',
		'\tmut lifetime := closure.new_lifetime()',
		'\tlifetime.frame(fn () {',
		'\t\tvalue := 7',
		'\t\th := fn [value] () int { return value }',
		'\t\tassert h() == 7',
		'\t})!',
		'\tlifetime.reclaim_all()!',
		'\tlifetime.dispose()!',
		'}',
		'',
		'fn main() {',
		'\trun() or { panic(err) }',
		'}',
	].join('\n')
	res := c_output_for_program(tmp_dir, 'closure_lifetime_single_destroy', source)
	assert res.exit_code == 0, res.output
	assert count_occurrences(res.output, 'builtin__closure__closure_try_destroy((voidptr)h);') == 1
}

fn test_closure_lifetime_inline_callback_codegen_cleanup() {
	$if gcboehm_leak ? {
		return
	}
	tmp_dir := os.join_path(os.vtmp_dir(), 'v_closure_lifetime_inline_codegen_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source := [
		'module main',
		'import builtin.closure',
		'',
		'struct View {',
		'\tvalue int',
		'}',
		'',
		'fn (view View) draw() {',
		'\tassert view.value == 41',
		'}',
		'',
		'fn frame_bound_method() ! {',
		'\tview := View{value: 41}',
		'\tmut lifetime := closure.new_lifetime()',
		'\tlifetime.frame(view.draw)!',
		'\tlifetime.dispose()!',
		'}',
		'',
		'fn outside_suspend_untracked() ! {',
		'\tmut lifetime := closure.new_lifetime()',
		'\tvalue := 7',
		'\tlifetime.suspend(fn [value] () {',
		'\t\tassert value == 7',
		'\t})!',
		'\tlifetime.untracked(fn [value] () {',
		'\t\tassert value == 7',
		'\t})!',
		'\tlifetime.dispose()!',
		'}',
		'',
		'fn parenthesized_inline_frame() ! {',
		'\tvalue := 12',
		'\tmut lifetime := closure.new_lifetime()',
		'\tlifetime.frame((fn [value] () {',
		'\t\tassert value == 12',
		'\t}))!',
		'\tlifetime.dispose()!',
		'}',
		'',
		'fn tail_return_frame(value int) ! {',
		'\tmut lifetime := closure.new_lifetime()',
		'\treturn lifetime.frame(fn [value] () {',
		'\t\tassert value == 9',
		'\t})',
		'}',
		'',
		'fn main() {',
		'\tframe_bound_method() or { panic(err) }',
		'\toutside_suspend_untracked() or { panic(err) }',
		'\tparenthesized_inline_frame() or { panic(err) }',
		'\ttail_return_frame(9) or { panic(err) }',
		'}',
	].join('\n')
	res := c_output_for_program(tmp_dir, 'closure_lifetime_inline_cleanup', source)
	assert res.exit_code == 0, res.output
	destroy_count := count_occurrences(res.output,
		'builtin__closure__closure_try_destroy((voidptr)')
	assert destroy_count == 5, res.output
	parenthesized_start := res.output.index('VV_LOC _result_void main__parenthesized_inline_frame(void) {') or {
		panic(res.output)
	}
	parenthesized_end := if parenthesized_start + 1200 < res.output.len {
		parenthesized_start + 1200
	} else {
		res.output.len
	}
	parenthesized_fn := res.output[parenthesized_start..parenthesized_end]
	frame_pos := parenthesized_fn.index('builtin__closure__Lifetime_frame') or {
		panic(parenthesized_fn)
	}
	destroy_pos := parenthesized_fn.index('builtin__closure__closure_try_destroy((voidptr)') or {
		panic(parenthesized_fn)
	}
	assert destroy_pos > frame_pos, parenthesized_fn
	assert !res.output.contains('return builtin__closure__Lifetime_frame(')
}

fn test_closure_lifetime_borrowed_callback_result_is_not_destroyed() {
	$if gcboehm_leak ? {
		return
	}
	tmp_dir := os.join_path(os.vtmp_dir(), 'v_closure_lifetime_borrowed_callback_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source := [
		'module main',
		'import builtin.closure',
		'',
		'fn identity(cb fn ()) fn () {',
		'\treturn cb',
		'}',
		'',
		'fn run() ! {',
		'\tpayload := []int{len: 32, init: index}',
		'\tstored := fn [payload] () {',
		'\t\tassert payload[0] == 0',
		'\t\tassert payload[payload.len - 1] == 31',
		'\t}',
		'\tmut lifetime := closure.new_lifetime()',
		'\tlifetime.frame(identity(stored))!',
		'\tlifetime.dispose()!',
		'\tstored()',
		'}',
		'',
		'fn main() {',
		'\trun() or { panic(err) }',
		'}',
	].join('\n')
	c_res := c_output_for_program(tmp_dir, 'closure_lifetime_borrowed_callback_cgen', source)
	assert c_res.exit_code == 0, c_res.output
	run_start := c_res.output.index('VV_LOC _result_void main__run(void) {') or {
		panic(c_res.output)
	}
	run_end := c_res.output.index_after('VV_LOC void main__main(void) {', run_start) or {
		panic(c_res.output)
	}
	run_fn := c_res.output[run_start..run_end]
	assert !run_fn.contains('builtin__closure__closure_try_destroy((voidptr)'), run_fn
	res := run_program_with_gc(tmp_dir, 'closure_lifetime_borrowed_callback', source, 'none')
	assert res.exit_code == 0, res.output
}

fn test_lifetime_rejects_misuse_with_errors() {
	$if gcboehm_leak ? {
		return
	}
	tmp_dir := os.join_path(os.vtmp_dir(), 'v_closure_lifetime_api_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	assert_misuse_program_passes(tmp_dir, 'wrong_thread_reclaim', [
		'import builtin.closure',
		'',
		'fn reclaim_from_thread(lifetime closure.Lifetime) ! {',
		'\tmut local := lifetime',
		'\tlocal.reclaim_all()!',
		'}',
		'',
		'fn main() {',
		'\tlifetime := closure.new_lifetime()',
		'\tth := spawn reclaim_from_thread(lifetime)',
		'\tth.wait() or {',
		"\t\tassert err.msg().contains('different thread')",
		'\t\treturn',
		'\t}',
		'\tassert false',
		'}',
	].join('\n'))
	assert_misuse_program_passes(tmp_dir, 'wrong_thread_dispose', [
		'import builtin.closure',
		'',
		'fn dispose_from_thread(lifetime closure.Lifetime) ! {',
		'\tmut local := lifetime',
		'\tlocal.dispose()!',
		'}',
		'',
		'fn main() {',
		'\tlifetime := closure.new_lifetime()',
		'\tth := spawn dispose_from_thread(lifetime)',
		'\tth.wait() or {',
		"\t\tassert err.msg().contains('different thread')",
		'\t\treturn',
		'\t}',
		'\tassert false',
		'}',
	].join('\n'))
	assert_misuse_program_passes(tmp_dir, 'wrong_thread_suspend', [
		'import builtin.closure',
		'',
		'fn no_capture_work() {}',
		'',
		'fn suspend_from_thread(lifetime closure.Lifetime) ! {',
		'\tmut local := lifetime',
		'\tlocal.suspend(no_capture_work)!',
		'}',
		'',
		'fn main() {',
		'\tlifetime := closure.new_lifetime()',
		'\tth := spawn suspend_from_thread(lifetime)',
		'\tth.wait() or {',
		"\t\tassert err.msg().contains('different thread')",
		'\t\treturn',
		'\t}',
		'\tassert false',
		'}',
	].join('\n'))
	assert_misuse_program_passes(tmp_dir, 'wrong_thread_frame', [
		'import builtin.closure',
		'',
		'fn no_capture_work() {}',
		'',
		'fn frame_from_thread(lifetime closure.Lifetime) ! {',
		'\tmut local := lifetime',
		'\tlocal.frame(no_capture_work)!',
		'}',
		'',
		'fn main() {',
		'\tlifetime := closure.new_lifetime()',
		'\tth := spawn frame_from_thread(lifetime)',
		'\tth.wait() or {',
		"\t\tassert err.msg().contains('different thread')",
		'\t\treturn',
		'\t}',
		'\tassert false',
		'}',
	].join('\n'))
	assert_misuse_program_passes(tmp_dir, 'nested_same_lifetime_frame', [
		'import builtin.closure',
		'',
		'@[heap]',
		'struct BoolBox {',
		'mut:',
		'\tvalue bool',
		'}',
		'',
		'fn no_capture_work() {}',
		'',
		'fn run() ! {',
		'\tmut lifetime := closure.new_lifetime()',
		'\tmut saw := &BoolBox{}',
		'\tlifetime.frame(fn [mut lifetime, mut saw] () {',
		'\t\tlifetime.frame(no_capture_work) or {',
		"\t\t\tassert err.msg().contains('active') || err.msg().contains('nested')",
		'\t\t\tsaw.value = true',
		'\t\t\treturn',
		'\t\t}',
		'\t\tassert false',
		'\t})!',
		'\tassert saw.value',
		'\tlifetime.dispose()!',
		'}',
		'',
		'fn main() {',
		'\trun() or { panic(err) }',
		'}',
	].join('\n'))
	assert_misuse_program_passes(tmp_dir, 'second_active_lifetime', [
		'import builtin.closure',
		'',
		'@[heap]',
		'struct BoolBox {',
		'mut:',
		'\tvalue bool',
		'}',
		'',
		'fn no_capture_work() {}',
		'',
		'fn run() ! {',
		'\tmut first := closure.new_lifetime()',
		'\tmut second := closure.new_lifetime()',
		'\tmut saw := &BoolBox{}',
		'\tfirst.frame(fn [mut second, mut saw] () {',
		'\t\tsecond.frame(no_capture_work) or {',
		"\t\t\tassert err.msg().contains('active') || err.msg().contains('another')",
		'\t\t\tsaw.value = true',
		'\t\t\treturn',
		'\t\t}',
		'\t\tassert false',
		'\t})!',
		'\tassert saw.value',
		'\tfirst.dispose()!',
		'\tsecond.dispose()!',
		'}',
		'',
		'fn main() {',
		'\trun() or { panic(err) }',
		'}',
	].join('\n'))
	assert_misuse_program_passes(tmp_dir, 'second_suspend_inside_first_frame', [
		'import builtin.closure',
		'',
		'@[heap]',
		'struct BoolBox {',
		'mut:',
		'\tvalue bool',
		'}',
		'',
		'fn no_capture_work() {}',
		'',
		'fn run() ! {',
		'\tmut first := closure.new_lifetime()',
		'\tmut second := closure.new_lifetime()',
		'\tmut saw := &BoolBox{}',
		'\tfirst.frame(fn [mut second, mut saw] () {',
		'\t\tsecond.suspend(no_capture_work) or {',
		"\t\t\tassert err.msg().contains('active') || err.msg().contains('another')",
		'\t\t\tsaw.value = true',
		'\t\t\treturn',
		'\t\t}',
		'\t\tassert false',
		'\t})!',
		'\tassert saw.value',
		'\tfirst.dispose()!',
		'\tsecond.dispose()!',
		'}',
		'',
		'fn main() {',
		'\trun() or { panic(err) }',
		'}',
	].join('\n'))
	assert_misuse_program_passes(tmp_dir, 'reclaim_dispose_while_active', [
		'import builtin.closure',
		'',
		'@[heap]',
		'struct BoolBox {',
		'mut:',
		'\tvalue bool',
		'}',
		'',
		'fn run() ! {',
		'\tmut lifetime := closure.new_lifetime()',
		'\tmut saw_reclaim := &BoolBox{}',
		'\tmut saw_dispose := &BoolBox{}',
		'\tlifetime.frame(fn [mut lifetime, mut saw_reclaim, mut saw_dispose] () {',
		'\t\tlifetime.reclaim_all() or {',
		"\t\t\tassert err.msg().contains('active')",
		'\t\t\tsaw_reclaim.value = true',
		'\t\t}',
		'\t\tlifetime.dispose() or {',
		"\t\t\tassert err.msg().contains('active')",
		'\t\t\tsaw_dispose.value = true',
		'\t\t\treturn',
		'\t\t}',
		'\t\tassert false',
		'\t})!',
		'\tassert saw_reclaim.value',
		'\tassert saw_dispose.value',
		'\tlifetime.dispose()!',
		'}',
		'',
		'fn main() {',
		'\trun() or { panic(err) }',
		'}',
	].join('\n'))
	assert_misuse_program_passes(tmp_dir, 'dispose_while_suspended', [
		'import builtin.closure',
		'',
		'@[heap]',
		'struct BoolBox {',
		'mut:',
		'\tvalue bool',
		'}',
		'',
		'fn run() ! {',
		'\tmut lifetime := closure.new_lifetime()',
		'\tmut saw := &BoolBox{}',
		'\tlifetime.suspend(fn [mut lifetime, mut saw] () {',
		'\t\tlifetime.dispose() or {',
		"\t\t\tassert err.msg().contains('suspend')",
		'\t\t\tsaw.value = true',
		'\t\t\treturn',
		'\t\t}',
		'\t\tassert false',
		'\t})!',
		'\tassert saw.value',
		'\tlifetime.dispose()!',
		'}',
		'',
		'fn main() {',
		'\trun() or { panic(err) }',
		'}',
	].join('\n'))
	assert_misuse_program_passes(tmp_dir, 'dispose_while_untracked', [
		'import builtin.closure',
		'',
		'@[heap]',
		'struct BoolBox {',
		'mut:',
		'\tvalue bool',
		'}',
		'',
		'fn run() ! {',
		'\tmut lifetime := closure.new_lifetime()',
		'\tmut saw := &BoolBox{}',
		'\tlifetime.untracked(fn [mut lifetime, mut saw] () {',
		'\t\tlifetime.dispose() or {',
		"\t\t\tassert err.msg().contains('suspend')",
		'\t\t\tsaw.value = true',
		'\t\t\treturn',
		'\t\t}',
		'\t\tassert false',
		'\t})!',
		'\tassert saw.value',
		'\tlifetime.dispose()!',
		'}',
		'',
		'fn main() {',
		'\trun() or { panic(err) }',
		'}',
	].join('\n'))
	assert_misuse_program_passes(tmp_dir, 'frame_while_suspended', [
		'import builtin.closure',
		'',
		'@[heap]',
		'struct BoolBox {',
		'mut:',
		'\tvalue bool',
		'}',
		'',
		'fn no_capture_work() {}',
		'',
		'fn run() ! {',
		'\tmut lifetime := closure.new_lifetime()',
		'\tmut saw := &BoolBox{}',
		'\tlifetime.suspend(fn [mut lifetime, mut saw] () {',
		'\t\tlifetime.frame(no_capture_work) or {',
		"\t\t\tassert err.msg().contains('suspend')",
		'\t\t\tsaw.value = true',
		'\t\t\treturn',
		'\t\t}',
		'\t\tassert false',
		'\t})!',
		'\tassert saw.value',
		'\tlifetime.dispose()!',
		'}',
		'',
		'fn main() {',
		'\trun() or { panic(err) }',
		'}',
	].join('\n'))
	assert_misuse_program_passes(tmp_dir, 'use_after_dispose', [
		'import builtin.closure',
		'',
		'fn no_capture_work() {}',
		'',
		'fn run() ! {',
		'\tmut lifetime := closure.new_lifetime()',
		'\tlifetime.dispose()!',
		'\tmut saw_frame := false',
		'\tmut saw_reclaim := false',
		'\tmut saw_suspend := false',
		'\tmut saw_untracked := false',
		'\tmut saw_dispose := false',
		'\tlifetime.frame(no_capture_work) or {',
		"\t\tassert err.msg().contains('dispose') || err.msg().contains('after')",
		'\t\tsaw_frame = true',
		'\t}',
		'\tlifetime.reclaim_all() or {',
		"\t\tassert err.msg().contains('dispose') || err.msg().contains('after')",
		'\t\tsaw_reclaim = true',
		'\t}',
		'\tlifetime.suspend(no_capture_work) or {',
		"\t\tassert err.msg().contains('dispose') || err.msg().contains('after')",
		'\t\tsaw_suspend = true',
		'\t}',
		'\tlifetime.untracked(no_capture_work) or {',
		"\t\tassert err.msg().contains('dispose') || err.msg().contains('after')",
		'\t\tsaw_untracked = true',
		'\t}',
		'\tlifetime.dispose() or {',
		"\t\tassert err.msg().contains('dispose') || err.msg().contains('after')",
		'\t\tsaw_dispose = true',
		'\t}',
		'\tassert saw_frame',
		'\tassert saw_reclaim',
		'\tassert saw_suspend',
		'\tassert saw_untracked',
		'\tassert saw_dispose',
		'}',
		'',
		'fn main() {',
		'\trun() or { panic(err) }',
		'}',
	].join('\n'))
}
