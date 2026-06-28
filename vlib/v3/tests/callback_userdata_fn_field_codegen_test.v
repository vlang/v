import os
import strings

const callback_vexe = @VEXE
const callback_tests_dir = os.dir(@FILE)
const callback_v3_dir = os.dir(callback_tests_dir)
const callback_vlib_dir = os.dir(callback_v3_dir)
const callback_v3_src = os.join_path(callback_v3_dir, 'v3.v')

fn callback_build_v3() string {
	return callback_build_v3_with_flags('serial', '')
}

fn callback_build_v3_parallel() string {
	return callback_build_v3_with_flags('parallel', '-d parallel')
}

fn callback_build_v3_with_flags(name string, flags string) string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_callback_userdata_test_${name}_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${callback_vexe} ${flags} -gc none -path "${callback_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${callback_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn callback_write_source(name string, source string) string {
	path := os.join_path(os.temp_dir(), 'v3_callback_userdata_${name}_${os.getpid()}.v')
	os.write_file(path, source) or { panic(err) }
	return path
}

fn callback_compile(v3_bin string, source string, name string) os.Result {
	out := os.join_path(os.temp_dir(), 'v3_callback_userdata_${name}_${os.getpid()}')
	return os.execute('${v3_bin} ${source} -b c -o ${out}')
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

fn test_callback_userdata_fn_field_adapters() {
	v3_bin := callback_build_v3()
	good_src := callback_write_source('good', 'module main

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
')
	good_out := os.join_path(os.temp_dir(), 'v3_callback_userdata_good_${os.getpid()}')
	good_compile := os.execute('${v3_bin} ${good_src} -b c -o ${good_out}')
	assert good_compile.exit_code == 0, good_compile.output
	run := os.execute(good_out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '82'
	generated := os.read_file(good_out + '.c') or { panic(err) }
	assert generated.contains('callback_adapter'), generated
	assert generated.contains('frame_callback_adapter'), generated
	assert generated.contains('on_event_callback_adapter'), generated
	assert generated.contains('erased_event_callback_adapter'), generated
	assert generated.contains('frame((App*)arg0);'), generated
	assert generated.contains('on_event(arg0, (App*)arg1);'), generated
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

	wrong_arity := callback_write_source('wrong_arity', 'module main
struct Config {
	frame fn (voidptr)
}
fn wrong(a voidptr, b int) {}
fn main() {
	_ := Config{frame: wrong}
}
')
	arity_compile := callback_compile(v3_bin, wrong_arity, 'wrong_arity')
	assert arity_compile.exit_code != 0, arity_compile.output

	wrong_return := callback_write_source('wrong_return', 'module main
struct Config {
	frame fn (voidptr)
}
fn wrong(a voidptr) int {
	return 1
}
fn main() {
	_ := Config{frame: wrong}
}
')
	return_compile := callback_compile(v3_bin, wrong_return, 'wrong_return')
	assert return_compile.exit_code != 0, return_compile.output

	wrong_return_pointer := callback_write_source('wrong_return_pointer', 'module main
struct Event {}
struct Config {
	cb fn () &Event
}
fn ret_voidptr() voidptr {
	return voidptr(0)
}
fn main() {
	_ := Config{cb: ret_voidptr}
}
')
	return_pointer_compile := callback_compile(v3_bin, wrong_return_pointer, 'wrong_return_pointer')
	assert return_pointer_compile.exit_code != 0, return_pointer_compile.output

	wrong_param := callback_write_source('wrong_param', 'module main
struct Event {}
struct Other {}
struct App {}
struct Config {
	event fn (&Event, voidptr)
}
fn wrong(e &Other, mut app App) {}
fn main() {
	_ := Config{event: wrong}
}
')
	param_compile := callback_compile(v3_bin, wrong_param, 'wrong_param')
	assert param_compile.exit_code != 0, param_compile.output

	wrong_primitive_param := callback_write_source('wrong_primitive_param', 'module main
struct Config {
	cb fn (i64)
}
fn takes_int(x int) {}
fn main() {
	_ := Config{cb: takes_int}
}
')
	primitive_param_compile := callback_compile(v3_bin, wrong_primitive_param,
		'wrong_primitive_param')
	assert primitive_param_compile.exit_code != 0, primitive_param_compile.output

	homonym_root := os.join_path(os.temp_dir(), 'v3_callback_userdata_homonym_${os.getpid()}')
	os.rmdir_all(homonym_root) or {}
	callback_write_project_file(homonym_root, 'left/left.v', 'module left

pub struct App {
pub mut:
	hits int
}

pub fn hit(mut app App) {
	app.hits += 10
}
')
	callback_write_project_file(homonym_root, 'right/right.v', 'module right

pub struct App {
pub mut:
	hits int
}

pub fn hit(mut app App) {
	app.hits += 100
}
')
	homonym_main := callback_write_project_file(homonym_root, 'main.v', 'module main

import left
import right

struct Config {
	cb fn (voidptr)
	user_data voidptr
}

fn run(cfg Config) {
	cfg.cb(cfg.user_data)
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
}
')
	homonym_out := os.join_path(os.temp_dir(), 'v3_callback_userdata_homonym_out_${os.getpid()}')
	homonym_compile := os.execute('${v3_bin} ${homonym_main} -b c -o ${homonym_out}')
	assert homonym_compile.exit_code == 0, homonym_compile.output
	homonym_run := os.execute(homonym_out)
	assert homonym_run.exit_code == 0, homonym_run.output
	assert homonym_run.output.trim_space() == '100'
	homonym_c := os.read_file(homonym_out + '.c') or { panic(err) }
	assert homonym_c.contains('right__hit_callback_adapter'), homonym_c
	assert homonym_c.contains('right__hit((right__App*)arg0);'), homonym_c
	assert !homonym_c.contains('left__hit_callback_adapter'), homonym_c

	c_fn_src := callback_write_source('c_fn_value', 'module main

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

fn main() {
	_ := App{}
	rc := call_c(C.abs)
	println(int_str(rc))
}
')
	c_fn_out := os.join_path(os.temp_dir(), 'v3_callback_userdata_c_fn_${os.getpid()}')
	c_fn_compile := os.execute('${v3_bin} ${c_fn_src} -b c -o ${c_fn_out}')
	assert c_fn_compile.exit_code == 0, c_fn_compile.output
	c_fn_run := os.execute(c_fn_out)
	assert c_fn_run.exit_code == 0, c_fn_run.output
	assert c_fn_run.output.trim_space() == '3'
	c_fn_c := os.read_file(c_fn_out + '.c') or { panic(err) }
	assert c_fn_c.contains('call_c(abs)'), c_fn_c
	assert !c_fn_c.contains('call_c((_fn_ptr'), c_fn_c
	assert !c_fn_c.contains('int abs(App*'), c_fn_c
}

fn test_callback_parallel_wrapper_names_are_stable() {
	v3_bin := callback_build_v3_parallel()
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
	parallel_src := callback_write_source('parallel', src.str())
	parallel_out := os.join_path(os.temp_dir(), 'v3_callback_userdata_parallel_${os.getpid()}')
	parallel_compile := os.execute('${v3_bin} ${parallel_src} -b c -o ${parallel_out}')
	assert parallel_compile.exit_code == 0, parallel_compile.output
	parallel_run := os.execute(parallel_out)
	assert parallel_run.exit_code == 0, parallel_run.output
	assert parallel_run.output.trim_space() == '1100'
	parallel_c := os.read_file(parallel_out + '.c') or { panic(err) }
	names := callback_adapter_names(parallel_c, 'erased_callback_adapter_')
	assert names.len == 2, parallel_c
	assert names[0] != names[1]
}
