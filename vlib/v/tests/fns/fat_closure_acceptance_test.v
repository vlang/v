import os
import rand

const vexe = @VEXE

fn write_file(path string, lines []string) {
	os.write_file(path, lines.join('\n')) or { panic(err) }
}

fn cgen_for(workdir string, name string, lines []string) string {
	src_path := os.join_path(workdir, '${name}.v')
	c_path := os.join_path(workdir, '${name}.c')
	write_file(src_path, lines)
	res :=
		os.execute('${os.quoted_path(vexe)} -d fat_closures -gc none -o ${os.quoted_path(c_path)} ${os.quoted_path(src_path)}')
	assert res.exit_code == 0, 'fat-closure C generation failed for ${name}:\n${res.output}'
	return os.read_file(c_path) or { panic(err) }
}

fn fat_closure_workdir() string {
	workdir := os.join_path(os.vtmp_dir(), 'v_fat_closure_acceptance_${rand.ulid()}')
	os.mkdir_all(workdir) or { panic(err) }
	return workdir
}

fn test_fat_closures_do_not_use_trampolines_for_pure_v_closures() {
	$if fat_closures ? {
		workdir := fat_closure_workdir()
		defer {
			os.rmdir_all(workdir) or {}
		}
		c_src := cgen_for(workdir, 'pure_v_closure', [
			'type IntFn = fn (int) int',
			'',
			'struct Handler {',
			'\tcb IntFn',
			'}',
			'',
			'fn make_handler(base int) Handler {',
			'\treturn Handler{',
			'\t\tcb: fn [base] (x int) int {',
			'\t\t\treturn base + x',
			'\t\t}',
			'\t}',
			'}',
			'',
			'fn apply_all(callbacks []IntFn, x int) int {',
			'\tmut total := 0',
			'\tfor cb in callbacks {',
			'\t\ttotal += cb(x)',
			'\t}',
			'\treturn total',
			'}',
			'',
			'fn main() {',
			'\th := make_handler(40)',
			'\tassert h.cb(2) == 42',
			'\tcallbacks := [h.cb, fn [h] (x int) int {',
			'\t\treturn h.cb(x) + 1',
			'\t}]',
			'\tassert apply_all(callbacks, 1) == 83',
			'}',
		])
		assert !c_src.contains('builtin__closure__closure_create'), 'pure V closures should be GC-visible values, not runtime trampolines'
		assert !c_src.contains('builtin__closure__closure_alloc'), 'pure V closures should not allocate executable trampoline pages'
		assert !c_src.contains('builtin__closure__closure_init'), 'pure V closures should not initialise the trampoline runtime'
		assert !c_src.contains('builtin__memdup_uncollectable'), 'fat closure contexts must not be uncollectable allocations'
	} $else {
		eprintln('skipping ${@FN}; run with `v test -d fat_closures ${@FILE}`')
	}
}

fn test_fat_closures_still_have_an_explicit_raw_fn_pointer_boundary() {
	$if fat_closures ? {
		workdir := fat_closure_workdir()
		defer {
			os.rmdir_all(workdir) or {}
		}
		c_src := cgen_for(workdir, 'raw_fn_pointer_boundary', [
			'type AudioCallback = fn (buffer voidptr, frames u32)',
			'',
			'@[heap]',
			'struct App {',
			'mut:',
			'\tid int',
			'}',
			'',
			'fn (mut app App) callback(buffer voidptr, frames u32) {',
			'\tassert buffer != unsafe { nil }',
			'\tassert frames == 256',
			'\tapp.id += int(frames)',
			'}',
			'',
			'fn call_raw_callback(p voidptr) {',
			'\tcb := AudioCallback(p)',
			'\tmut buf := [4]u8{}',
			'\tcb(&buf[0], 256)',
			'}',
			'',
			'fn main() {',
			'\tmut app := &App{',
			'\t\tid: 1',
			'\t}',
			'\traw_cb := AudioCallback(app.callback)',
			'\tcall_raw_callback(raw_cb)',
			'\tassert app.id == 257',
			'}',
		])
		assert c_src.contains('builtin__closure__closure_create'), 'explicit closure-to-raw-fn-pointer coercions still need a C-callable boundary shim'
	} $else {
		eprintln('skipping ${@FN}; run with `v test -d fat_closures ${@FILE}`')
	}
}
