// vtest ownership
import os
import strings
import v.cmdexec
import v.tempname

const callback_tests_dir = os.dir(@FILE)
const callback_v3_dir = os.dir(callback_tests_dir)
const callback_vlib_dir = os.dir(callback_v3_dir)
const callback_v3_src = os.join_path(callback_v3_dir, 'v.v')
const callback_compiler_dir = os.join_path(os.vtmp_dir(), 'v3_callback_userdata_${tempname.unique_token()}')

fn testsuite_end() {
	os.rmdir_all(callback_compiler_dir) or {}
}

fn callback_ownership_compiler() string {
	shared := os.getenv('V3_TEST_UNIT_SHARED_V3_OWNERSHIP')
	if os.is_file(shared) && os.is_executable(shared)
		&& os.real_path(os.getenv('V3_TEST_UNIT_V3_SRC')) == os.real_path(callback_v3_src) {
		return os.real_path(shared)
	}
	bin := os.join_path(callback_compiler_dir, 'ownership') + $if windows { '.exe' } $else { '' }
	if os.is_executable(bin) {
		return bin
	}
	os.mkdir_all(callback_compiler_dir) or { panic(err) }
	build := os.exec([@VEXE, '-d', 'ownership', '-gc', 'none', '-path',
		'${callback_vlib_dir}' + '|@vlib|@vmodules', '-o', bin, '${callback_v3_src}'])
	assert build.exit_code == 0, build.output
	return bin
}

fn callback_write_source(root string, name string, source string) string {
	path := os.join_path(root, '${name}.v')
	os.write_file(path, source) or { panic(err) }
	return path
}

fn callback_write_project_file(root string, rel string, source string) string {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, source) or { panic(err) }
	return path
}

fn callback_adapter_names(generated string, prefix string) []string {
	mut names := []string{}
	for line in generated.split_into_lines() {
		if line.starts_with('static void ${prefix}') {
			name := line.all_after('static void ').all_before('(')
			if name !in names {
				names << name
			}
		}
	}
	return names
}

fn test_callback_userdata_fn_field_rejects_incompatible_types() {
	root := os.join_path(os.vtmp_dir(), 'v3_callback_userdata_rejections_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	v3_bin := callback_ownership_compiler()
	source := 'module main

struct FrameConfig {
	frame fn (voidptr)
}

struct Event {}
struct Other {}
struct App {}

struct PointerResultConfig {
	cb fn () &Event
}

struct EventConfig {
	event fn (&Event, voidptr)
}

struct PrimitiveParamConfig {
	cb fn (i64)
}

fn wrong_arity(a voidptr, b int) {}

fn wrong_return(a voidptr) int {
	return 1
}

fn ret_voidptr() voidptr {
	return voidptr(0)
}

fn wrong_param(e &Other, mut app App) {}

fn takes_int(x int) {}

fn main() {
	_ := FrameConfig{frame: wrong_arity}
	_ := FrameConfig{frame: wrong_return}
	_ := PointerResultConfig{cb: ret_voidptr}
	_ := EventConfig{event: wrong_param}
	// Function type identity distinguishes int and i64 even when their C widths match.
	_ := PrimitiveParamConfig{cb: takes_int}
}
'
	src := callback_write_source(root, 'rejected_callbacks', source)
	out := os.join_path(root, 'program')
	compile := os.exec([v3_bin, src, '-b', 'c', '-nocolor', '-o', out])
	assert compile.exit_code != 0, compile.output
	expected := {
		'FrameConfig{frame: wrong_arity}':      'expected `fn (voidptr)`, not `fn (voidptr, int)`'
		'FrameConfig{frame: wrong_return}':     'expected `fn (voidptr)`, not `fn (voidptr) int`'
		'PointerResultConfig{cb: ret_voidptr}': 'expected `fn () &Event`, not `fn () voidptr`'
		'EventConfig{event: wrong_param}':      'expected `fn (&Event, voidptr)`, not `fn (&Other, mut App)`'
		'PrimitiveParamConfig{cb: takes_int}':  'expected `fn (i64)`, not `fn (int)`'
	}
	lines := source.split_into_lines()
	diagnostics := compile.output.split_into_lines().filter(it.contains(': error: '))
	for initializer, message in expected {
		line := lines.index('\t_ := ${initializer}') + 1
		assert line > 0, initializer
		location := 'rejected_callbacks.v:${line}:'
		assert diagnostics.any(it.contains(location) && it.contains(message)), 'missing "${message}" at ${location}\n${compile.output}'
	}
}

fn test_callback_userdata_fn_field_adapters() {
	root := os.join_path(os.vtmp_dir(), 'v3_callback_userdata_adapters_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	v3_bin := callback_ownership_compiler()
	good_code := 'module main

__global C.user_data voidptr

struct Event {
	code int
}

struct App {
mut:
	frames int
	events int
	directs int
}

struct Config {
	frame fn (voidptr)
	event fn (&Event, voidptr)
	native_event fn (&Event, voidptr)
	direct fn (voidptr)
	user_data voidptr
}

type FrameAlias = fn (voidptr)
type EventAlias = fn (&Event, voidptr)

struct AliasConfig {
	alias_frame FrameAlias
	alias_event EventAlias
	user_data voidptr
}

struct StructAliasTarget {
	alias2_frame FrameAlias
	alias2_event EventAlias
	user_data voidptr
}

type StructAlias = StructAliasTarget

@[params]
struct Params {
	frame fn (voidptr)
	event fn (&Event, voidptr)
	native_event fn (&Event, voidptr)
	direct fn (voidptr)
	user_data voidptr
}

fn run_config(mut app App, cfg Config) int {
	cfg.frame(cfg.user_data)
	e := Event{code: 3}
	cfg.event(&e, cfg.user_data)
	cfg.native_event(&e, cfg.user_data)
	cfg.direct(cfg.user_data)
	return app.frames + app.events + app.directs
}

fn run_params(mut app App, params Params) int {
	params.frame(params.user_data)
	e := Event{code: 4}
	params.event(&e, params.user_data)
	params.native_event(&e, params.user_data)
	params.direct(params.user_data)
	return app.frames + app.events + app.directs
}

fn use_cb(cb fn (voidptr), data voidptr) {
	cb(data)
}

fn frame(mut app App) {
	app.frames++
}

fn on_event(e &Event, mut app App) {
	app.events += e.code
}

fn erased_event(e voidptr, data voidptr) {
	event := unsafe { &Event(e) }
	mut app := unsafe { &App(data) }
	app.events += event.code * 10
}

fn direct(ptr voidptr) {
	mut app := unsafe { &App(ptr) }
	app.directs++
}

fn main() {
	mut a := App{}
	cfg := Config{
		frame: frame
		event: on_event
		native_event: erased_event
		direct: direct
		user_data: voidptr(&a)
	}
	first := run_config(mut a, cfg)
	mut b := App{}
	second := run_params(mut b, frame: frame, event: on_event, native_event: erased_event, direct: direct, user_data: voidptr(&b))
	mut c := App{}
	use_cb(frame, voidptr(&c))
	alias_cfg := AliasConfig{alias_frame: frame, alias_event: on_event, user_data: voidptr(&c)}
	alias_target := StructAlias{alias2_frame: frame, alias2_event: on_event, user_data: voidptr(&c)}
	if alias_cfg.user_data == voidptr(0) || alias_target.user_data == voidptr(0) {
		println(int_str(-1))
	}
	println(int_str(first + second + c.frames))
}
'
	good_src := callback_write_source(root, 'good', good_code)
	good_out := os.join_path(root, 'good')
	good_compile := os.exec([v3_bin, '-enable-globals', '${good_src}', '-b', 'c', '-o', '${good_out}'])
	assert good_compile.exit_code == 0, good_compile.output
	run := os.exec([good_out])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '82'
	generated := os.read_file(good_out + '.c') or { panic(err) }
	assert generated.contains('callback_adapter'), generated
	assert generated.contains('frame_callback_adapter'), generated
	assert generated.contains('on_event_callback_adapter'), generated
	assert generated.contains('erased_event_callback_adapter'), generated
	assert generated.contains('frame((main__App*)arg0);'), generated
	assert generated.contains('on_event(arg0, (main__App*)arg1);'), generated
	assert generated.contains('erased_event((void*)arg0, arg1);'), generated
	assert generated.contains('use_cb(frame_callback_adapter_'), generated
	assert generated.contains('.alias_frame = frame_callback_adapter_'), generated
	assert generated.contains('.alias_event = on_event_callback_adapter_'), generated
	assert generated.contains('.alias2_frame = frame_callback_adapter_'), generated
	assert generated.contains('.alias2_event = on_event_callback_adapter_'), generated
	assert generated.contains('.direct = direct'), generated
	assert !generated.contains('.frame = frame,'), generated
	assert !generated.contains('.event = on_event,'), generated
	assert !generated.contains('.native_event = erased_event,'), generated
	assert !generated.contains('.alias_frame = frame,'), generated
	assert !generated.contains('.alias_event = on_event,'), generated
	assert !generated.contains('.alias2_frame = frame,'), generated
	assert !generated.contains('.alias2_event = on_event,'), generated
	assert !generated.contains('.frame = (_fn_ptr'), generated
	assert !generated.contains('.event = (_fn_ptr'), generated
	assert !generated.contains('.native_event = (_fn_ptr'), generated
	assert !generated.contains('use_cb((_fn_ptr'), generated
	assert !generated.contains('use_cb(frame,'), generated
	assert !generated.contains('direct_callback_adapter'), generated

	autofree_src := callback_write_source(root, 'autofree_callbacks', good_code.replace('fn main()', 'fn check_callback_adapters()') + '
struct BoundEvent {
	value int
}

@[heap]
struct BoundApp {
mut:
	value int
}

struct BoundConfig {
	on_event fn (&BoundEvent)
}

fn (mut app BoundApp) on_event(event &BoundEvent) {
	app.value = event.value
}

fn check_bound_method() {
	mut app := &BoundApp{}
	config := BoundConfig{on_event: app.on_event}
	config.on_event(&BoundEvent{value: 73})
	println(app.value)
}

fn main() {
	check_callback_adapters()
	check_bound_method()
}
')
	autofree_out := os.join_path(root, 'autofree')
	autofree_compile := os.exec([v3_bin, '-enable-globals', '-autofree', autofree_src, '-b', 'c',
		'-o', autofree_out])
	assert autofree_compile.exit_code == 0, autofree_compile.output
	autofree_run := os.exec([autofree_out])
	assert autofree_run.exit_code == 0, autofree_run.output
	assert autofree_run.output.trim_space().split_into_lines() == ['82', '73'], autofree_run.output
	autofree_c := os.read_file(autofree_out + '.c') or { panic(err) }
	assert autofree_c.contains('main__frame((main__App*)arg0);'), autofree_c
	assert autofree_c.contains('main__on_event(arg0, (main__App*)arg1);'), autofree_c
	assert autofree_c.contains('main__BoundApp_on_event('), autofree_c
	assert !autofree_c.contains('void BoundApp__on_event('), autofree_c

	symbol_root := os.join_path(root, 'callback_symbols')
	callback_write_project_file(symbol_root, 'left/left.v', 'module left

pub struct App {
pub mut:
	hits int
}

pub fn hit(mut app App) {
	app.hits += 10
}
')
	callback_write_project_file(symbol_root, 'right/right.v', 'module right

pub struct App {
pub mut:
	hits int
}

pub fn hit(mut app App) {
	app.hits += 100
}
')
	symbol_src := callback_write_project_file(symbol_root, 'main.v', 'module main

import left
import right

struct Config {
	cb fn (voidptr)
	user_data voidptr
}

fn run(cfg Config) {
	cfg.cb(cfg.user_data)
}

fn C.abs(int) int

struct App {
mut:
	hits int
}

fn abs(mut app App) int {
	app.hits = 99
	return app.hits
}

fn call_c(cb fn (int) int) int {
	return cb(-3)
}

fn check_c_function_value() {
	_ := App{}
	rc := call_c(C.abs)
	println(int_str(rc))
}

fn main() {
	_ := left.App{}
	mut app := right.App{}
	cfg := Config{
		cb: right.hit
		user_data: voidptr(&app)
	}
	run(cfg)
	println(int_str(app.hits))
	check_c_function_value()
}
')
	symbol_out := os.join_path(root, 'callback_symbols_out')
	symbol_compile := os.exec([v3_bin, symbol_src, '-b', 'c', '-o', symbol_out])
	assert symbol_compile.exit_code == 0, symbol_compile.output
	symbol_run := os.exec([symbol_out])
	assert symbol_run.exit_code == 0, symbol_run.output
	assert symbol_run.output.trim_space().split_into_lines() == ['100', '3'], symbol_run.output
	symbols := os.read_file(symbol_out + '.c') or { panic(err) }
	assert symbols.contains('right__hit_callback_adapter'), symbols
	assert symbols.contains('right__hit((right__App*)arg0);'), symbols
	assert !symbols.contains('left__hit_callback_adapter'), symbols
	assert symbols.contains('call_c(abs)'), symbols
	assert !symbols.contains('call_c((_fn_ptr'), symbols
	assert !symbols.contains('int abs(App*'), symbols
}

fn test_callback_parallel_wrapper_names_are_stable() {
	root := os.join_path(os.vtmp_dir(), 'v3_callback_userdata_parallel_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	mut src := strings.new_builder(128 * 1024)
	src.write_string('module main

struct EventA {
	code int
}

struct EventB {
	code int
}

struct App {
mut:
	total int
}

struct ConfigA {
	cb fn (&EventA, voidptr)
	user_data voidptr
}

struct ConfigB {
	cb fn (&EventB, voidptr)
	user_data voidptr
}

fn erased(e voidptr, data voidptr) {
	_ := e
	mut app := unsafe { &App(data) }
	app.total++
}

')
	for i in 0 .. 1100 {
		if i % 2 == 0 {
			src.write_string('fn use_${i}(mut app App) {
	e := EventA{code: ${i}}
	cfg := ConfigA{cb: erased, user_data: voidptr(app)}
	cfg.cb(&e, cfg.user_data)
}

')
		} else {
			src.write_string('fn use_${i}(mut app App) {
	e := EventB{code: ${i}}
	cfg := ConfigB{cb: erased, user_data: voidptr(app)}
	cfg.cb(&e, cfg.user_data)
}

')
		}
	}
	src.write_string('fn main() {
	mut app := App{}
')
	for i in 0 .. 1100 {
		src.write_string('	use_${i}(mut app)
')
	}
	src.write_string('	println(int_str(app.total))
}
')
	parallel_src := callback_write_source(root, 'parallel', src.str())
	parallel_out := os.join_path(root, 'parallel')
	parallel_compile := cmdexec.run_in_merged_with_env(@VEXE, [parallel_src, '-b', 'c', '-o',
		parallel_out], '', {
		'VJOBS': '2'
	})
	assert parallel_compile.exit_code == 0, parallel_compile.output
	parallel_run := os.exec([parallel_out])
	assert parallel_run.exit_code == 0, parallel_run.output
	assert parallel_run.output.trim_space() == '1100'
	parallel_c := os.read_file(parallel_out + '.c') or { panic(err) }
	names := callback_adapter_names(parallel_c, 'erased_callback_adapter_')
	assert names.len == 2, parallel_c
	assert names[0] != names[1]
}
