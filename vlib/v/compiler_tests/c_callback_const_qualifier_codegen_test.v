import os
import strings
import v.cmdexec
import v.tempname

fn const_cb_write_project(source string) string {
	root := os.join_path(os.vtmp_dir(), 'v3_const_callback_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	header := '#ifndef V3_CONST_CALLBACK_NATIVE_H
#define V3_CONST_CALLBACK_NATIVE_H
typedef struct native_event {
	int value;
} native_event;
typedef struct native_desc {
	void (*cb)(const struct native_event*, void*);
	void (*plain)(struct native_event*, void*);
	void* user_data;
} native_desc;
typedef struct alias_desc {
	void (*cb)(const struct native_event*, void*);
	void* user_data;
} alias_desc;
typedef struct late_alias_desc {
	void (*cb)(const struct native_event*, void*);
	void* user_data;
} late_alias_desc;
#endif
'
	os.write_file(os.join_path(root, 'native_callback.h'), header) or { panic(err) }
	path := os.join_path(root, 'main.v')
	os.write_file(path, source) or { panic(err) }
	return path
}

fn test_c_callback_const_qualifier_codegen() {
	src := const_cb_write_project('module main

#include "@DIR/native_callback.h"

@[typedef]
struct C.native_event {
	value int
}

type Event = C.native_event
type ConstNativeCb = fn (const_event &Event, user_data voidptr)

@[typedef]
struct C.native_desc {
mut:
	cb fn (const_event &C.native_event, voidptr) = unsafe { nil }
	plain fn (&C.native_event, voidptr) = unsafe { nil }
	user_data voidptr
}

@[typedef]
struct C.alias_desc {
mut:
	cb ConstNativeCb = unsafe { nil }
	user_data voidptr
}

struct App {
mut:
	hits int
}

fn erased_event(e voidptr, data voidptr) {
	event := unsafe { &C.native_event(e) }
	mut app := unsafe { &App(data) }
	app.hits += event.value
}

fn plain_event(e &C.native_event, data voidptr) {
	mut app := unsafe { &App(data) }
	app.hits += e.value * 10
}

fn concrete_event(e &C.native_event, data voidptr) {
	mut app := unsafe { &App(data) }
	app.hits += e.value * 100
}

fn main() {
	mut app := App{}
	mut desc := C.native_desc{
		cb: erased_event
		plain: plain_event
		user_data: voidptr(&app)
	}
	event := C.native_event{value: 3}
	desc.cb = erased_event
	assert desc.cb == erased_event
	assert erased_event == desc.cb
	desc.cb(&event, desc.user_data)
	desc.plain(&event, desc.user_data)
	mut concrete := C.native_desc{
		user_data: voidptr(&app)
	}
	concrete.cb = concrete_event
	assert concrete.cb == concrete_event
	assert concrete_event == concrete.cb
	concrete.cb(&event, concrete.user_data)
	mut alias_desc := C.alias_desc{
		user_data: voidptr(&app)
	}
	alias_desc.cb = concrete_event
	assert alias_desc.cb == concrete_event
	alias_desc.cb(&event, alias_desc.user_data)
	desc.cb = concrete_event
	desc.plain = concrete_event
	assert desc.cb == desc.plain
	assert desc.plain == desc.cb
	assert !(desc.cb != desc.plain)
	assert !(desc.plain != desc.cb)
	println(int_str(app.hits))
}
')
	root := os.dir(src)
	defer { os.rmdir_all(root) or {} }
	out := os.join_path(root, 'out')
	compile := os.exec([@VEXE, '${src}', '-b', 'c', '-o', '${out}'])
	assert compile.exit_code == 0, compile.output
	run := os.exec([out])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '633'
	generated := os.read_file(out + '.c') or { panic(err) }
	assert generated.contains('typedef void (*'), generated
	assert generated.contains('(const native_event*, void*)'), generated
	assert generated.contains('(native_event*, void*)'), generated
	assert generated.contains('erased_event_callback_adapter_'), generated
	assert generated.contains('concrete_event_callback_adapter_'), generated
	assert generated.contains('const native_event* arg0, void* arg1'), generated
	assert generated.contains('erased_event((void*)arg0, arg1);'), generated
	assert generated.contains('concrete_event((native_event*)arg0, arg1);'), generated
	assert generated.contains('.cb = erased_event_callback_adapter_'), generated
	assert generated.contains('desc.cb = erased_event_callback_adapter_'), generated
	assert generated.contains('concrete.cb = concrete_event_callback_adapter_'), generated
	assert generated.contains('__v3_internal_symbol_local_alias_desc.cb = concrete_event_callback_adapter_'), generated
	assert generated.contains('.plain = plain_event'), generated
	assert !generated.contains('.cb = (_fn_ptr'), generated
	assert !generated.contains('desc.cb = (_fn_ptr'), generated
	assert !generated.contains('concrete.cb = concrete_event;'), generated
	assert !generated.contains('alias_desc.cb = concrete_event;'), generated
	assert !generated.contains('.plain = plain_event_callback_adapter_'), generated
}

fn test_generic_heap_and_late_alias_c_callback_codegen() {
	src := const_cb_write_project('module main

#include "@DIR/native_callback.h"

@[typedef]
struct C.late_alias_desc {
mut:
	cb fn (&Event, voidptr) = unsafe { nil }
	user_data voidptr
}

@[typedef]
struct C.native_event {
	value int
}

type Event = C.native_event

@[typedef]
struct C.native_desc {
mut:
	cb fn (const_event &C.native_event, voidptr) = unsafe { nil }
	plain fn (&C.native_event, voidptr) = unsafe { nil }
	user_data voidptr
}

struct App {
mut:
	hits int
}

fn concrete_event(e &C.native_event, data voidptr) {
	mut app := unsafe { &App(data) }
	app.hits += e.value
}

fn plain_event(e &C.native_event, data voidptr) {
	mut app := unsafe { &App(data) }
	app.hits += e.value * 10
}

fn make_desc[T](data T) &C.native_desc {
	return &C.native_desc{concrete_event, plain_event, voidptr(data)}
}

fn run_generic_heap_callback() {
	mut app := App{}
	event := C.native_event{value: 5}
	desc := make_desc(&app)
	desc.cb(&event, desc.user_data)
	desc.plain(&event, desc.user_data)
	println(int_str(app.hits))
}

fn late_alias_event(e &C.native_event, data voidptr) {
	mut app := unsafe { &App(data) }
	app.hits += e.value
}

fn main() {
	run_generic_heap_callback()
	mut app := App{}
	event := C.native_event{value: 11}
	mut desc := C.late_alias_desc{
		cb: late_alias_event
		user_data: voidptr(&app)
	}
	desc.cb(&event, desc.user_data)
	desc.cb = late_alias_event
	desc.cb(&event, desc.user_data)
	println(int_str(app.hits))
}
')
	root := os.dir(src)
	defer { os.rmdir_all(root) or {} }
	out := os.join_path(root, 'out')
	compile := os.exec([@VEXE, '${src}', '-b', 'c', '-o', '${out}'])
	assert compile.exit_code == 0, compile.output
	run := os.exec([out])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space().split_into_lines() == ['55', '22'], run.output
	generated := os.read_file(out + '.c') or { panic(err) }
	assert generated.contains('concrete_event_callback_adapter_'), generated
	assert generated.contains('.cb = concrete_event_callback_adapter_'), generated
	assert !generated.contains('.cb = concrete_event,'), generated
	assert generated.contains('(const native_event*, void*)'), generated
	assert generated.split_into_lines().any(it.contains('late_alias_event_callback_adapter_')
		&& it.contains('(const native_event* arg0, void* arg1)')), generated
	assert generated.contains('late_alias_event((native_event*)arg0, arg1);'), generated
	assert generated.contains('.cb = late_alias_event_callback_adapter_'), generated
	assert generated.contains('desc.cb = late_alias_event_callback_adapter_'), generated
	assert !generated.contains('main__Event*'), generated
	assert !generated.contains('.cb = late_alias_event,'), generated
	assert !generated.contains('desc.cb = late_alias_event;'), generated
}

fn test_c_callback_const_qualifier_parallel_worker_codegen() {
	mut src := strings.new_builder(96 * 1024)
	src.write_string('module main

#include "@DIR/native_callback.h"

@[typedef]
struct C.native_event {
	value int
}

type Event = C.native_event
type ConstNativeCb = fn (const_event &Event, user_data voidptr)

@[typedef]
struct C.alias_desc {
mut:
	cb ConstNativeCb = unsafe { nil }
	user_data voidptr
}

@[typedef]
struct C.native_desc {
mut:
	cb fn (const_event &C.native_event, voidptr) = unsafe { nil }
	plain fn (&C.native_event, voidptr) = unsafe { nil }
	user_data voidptr
}

struct App {
mut:
	hits int
}

fn concrete_event(e &C.native_event, data voidptr) {
	mut app := unsafe { &App(data) }
	app.hits += e.value
}

')
	for i in 0 .. 1100 {
		src.write_string('fn filler_${i}() int { return ${i} }\n')
	}
	src.write_string('
fn run_parallel(mut app App) {
	mut desc := C.native_desc{
		user_data: voidptr(app)
	}
	desc.cb = concrete_event
	desc.plain = concrete_event
	assert desc.cb == desc.plain
	assert desc.plain == desc.cb
	assert !(desc.cb != desc.plain)
	event := C.native_event{value: 7}
	desc.cb(&event, desc.user_data)
}

fn main() {
	mut app := App{}
	run_parallel(mut app)
	println(int_str(app.hits))
}
')
	path := const_cb_write_project(src.str())
	root := os.dir(path)
	defer { os.rmdir_all(root) or {} }
	out := os.join_path(root, 'out')
	compile := cmdexec.run_in_merged_with_env(@VEXE, [path, '-b', 'c', '-o', out], '', {
		'VJOBS': '2'
	})
	assert compile.exit_code == 0, compile.output
	run := os.exec([out])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '7'
	generated := os.read_file(out + '.c') or { panic(err) }
	assert generated.contains('concrete_event_callback_adapter_'), generated
	assert generated.contains('const native_event* arg0, void* arg1'), generated
	assert generated.contains('concrete_event((native_event*)arg0, arg1);'), generated
	assert generated.contains('desc.cb = concrete_event_callback_adapter_'), generated
	assert !generated.contains('const gg__Event'), generated
	assert !generated.contains('desc.cb = concrete_event;'), generated
}
