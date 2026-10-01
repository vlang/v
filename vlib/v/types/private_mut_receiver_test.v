module types

import os

const private_receiver_module = 'module counter

pub struct Counter {
mut:
 value int
pub mut:
 visible int
 values []int
 visible_pointer &int = unsafe { nil }
 inner Inner
}

pub struct Inner {
pub mut:
 value int
}

struct Box {
mut:
 counter Counter
}

pub fn (mut c Counter) inc() {
 c.value++
}

pub fn (mut c Counter) inc_private_alias() {
 mut alias := &c.value
 unsafe { *alias += 1 }
}

pub fn (c Counter) get() int {
 return c.value
}

pub fn (mut c Counter) change_visible() {
 c.visible++
}

pub fn (mut c Counter) change_visible_via_method() {
 c.change_visible()
}

pub fn (mut c Counter) change_visible_alias() {
 mut alias := &c
 alias.visible++
}

pub fn (mut c Counter) change_visible_alias_chain() {
 mut alias := &c
 mut other := alias
 other.visible++
}

pub fn (mut c Counter) change_visible_alias_via_helper() {
 change_visible_alias_helper(mut c)
}

fn change_visible_alias_helper(mut c Counter) {
 mut alias := &c
 alias.visible++
}

fn (c &Counter) as_pointer() &Counter {
 return c
}

pub fn (mut c Counter) change_visible_returned_alias() {
 mut alias := c.as_pointer()
 alias.visible++
}

pub fn (mut c Counter) change_visible_stored_array() {
 mut box := Box{}
 box.counter = c
 box.counter.values[0] = 42
}

fn write_visible_pointer(value &int) {
 unsafe { *value += 1 }
}

pub fn (mut c Counter) change_visible_pointer_field() {
 write_visible_pointer(c.visible_pointer)
}

fn (i &Inner) as_pointer() &Inner {
 return i
}

pub fn (mut c Counter) change_visible_field_alias() {
 mut alias := c.inner.as_pointer()
 alias.value++
}

fn alter_values(values []Counter) {
 mut pointer := &values[0]
 pointer.values[0] = 42
}

pub fn (mut c Counter) change_visible_literal_array() {
 alter_values([c])
}

pub fn (mut c Counter) change_visible_appended_array() {
 mut copies := []Counter{}
 copies << c
 write_visible_pointer(copies[0].visible_pointer)
}

pub fn (mut c Counter) change_visible_channel() {
 copies := chan Counter{cap: 1}
 copies <- c
 mut other := <-copies
 other.values[0] = 43
}

pub fn (mut c Counter) reset() {
 c = Counter{}
}

pub type Handle = Counter

pub fn (mut h Handle) next() int {
 h.inc()
 return h.get()
}
'

fn check_private_receiver(name string, source string, run bool) os.Result {
	base := os.join_path(os.vtmp_dir(), 'private_mut_receiver_${name}_${os.getpid()}')
	os.mkdir_all(os.join_path(base, 'counter')) or { panic(err) }
	defer {
		os.rmdir_all(base) or {}
	}
	os.write_file(os.join_path(base, 'counter', 'counter.v'), private_receiver_module) or {
		panic(err)
	}
	file := os.join_path(base, 'main.v')
	os.write_file(file, 'import counter\n' + source) or { panic(err) }
	mode := if run { 'run' } else { '-check' }
	return os.execute('${os.quoted_path(@VEXE)} -new-compiler ${mode} ${os.quoted_path(file)}')
}

fn test_private_mut_method_preserves_immutable_local_state() {
	result := check_private_receiver('local', 'fn main() {
 c := counter.Counter{}
 c.inc()
 (c).inc()
 c.inc_private_alias()
 assert c.get() == 3
 println("ok")
}', true)
	assert result.exit_code == 0, result.output
	assert result.output.trim_space() == 'ok', result.output
}

fn test_private_mut_method_preserves_multi_return_local_state() {
	result := check_private_receiver('multi_return', 'fn acquire() !(counter.Counter, int) {
 return counter.Counter{}, 42
}
fn main() {
 c, connection := acquire() or { panic(err) }
 c.inc()
 c.inc()
 assert c.get() == 2
 assert connection == 42
 println("ok")
}', true)
	assert result.exit_code == 0, result.output
	assert result.output.trim_space() == 'ok', result.output
}

fn test_mut_method_rejects_caller_visible_local_mutation() {
	for method in ['change_visible', 'change_visible_via_method', 'change_visible_alias',
		'change_visible_alias_chain', 'change_visible_alias_via_helper', 'change_visible_returned_alias',
		'change_visible_stored_array', 'change_visible_pointer_field', 'change_visible_field_alias',
		'change_visible_literal_array', 'change_visible_appended_array', 'change_visible_channel',
		'reset'] {
		result := check_private_receiver('visible_' + method, 'fn main() {
 c := counter.Counter{}
 c.' + method + '()
}', false)
		assert result.exit_code != 0, result.output
		assert result.output.contains('`c` is immutable, declare it with `mut`'), result.output
	}
}

fn test_private_mut_method_rejects_immutable_local_in_same_module() {
	result := check_private_receiver('same_module', 'struct Local {
mut:
 value int
}
fn (mut c Local) inc() {
 c.value++
}
fn main() {
 c := Local{}
 c.inc()
}', false)
	assert result.exit_code != 0, result.output
	assert result.output.contains('`c` is immutable, declare it with `mut`'), result.output
}

fn test_private_mut_method_rejects_immutable_capture() {
	result := check_private_receiver('capture', 'fn main() {
 c := counter.Counter{}
 callback := fn [c] () {
  c.inc()
 }
 callback()
}', false)
	assert result.exit_code != 0, result.output
	assert result.output.contains('`c` is immutable, declare it with `mut`'), result.output
}

fn test_private_mut_method_rejects_constant() {
	result := check_private_receiver('constant', 'const c = counter.Counter{}
fn main() {
 c.inc()
}', false)
	assert result.exit_code != 0, result.output
	assert result.output.contains('cannot modify constant `c`'), result.output
}

fn test_private_mut_method_rejects_temporary_value() {
	result := check_private_receiver('temporary', 'fn main() {
 counter.Counter{}.inc()
}', false)
	assert result.exit_code != 0, result.output
	assert result.output.contains('cannot pass expression as `mut`'), result.output
}

fn test_private_mut_method_rejects_value_parameter() {
	result := check_private_receiver('parameter', 'fn start(c counter.Counter) {
 c.inc()
}
fn main() {
 mut c := counter.Counter{}
 start(c)
}', false)
	assert result.exit_code != 0, result.output
	assert result.output.contains('`c` is immutable, declare it with `mut`'), result.output
}

fn test_private_mut_method_rejects_immutable_loop_binding() {
	result := check_private_receiver('loop', 'fn main() {
 if true {
  c := counter.Counter{}
  c.inc()
 }
 values := [counter.Counter{}]
 for c in values {
  c.inc()
 }
}', false)
	assert result.exit_code != 0, result.output
	assert result.output.contains('`c` is immutable, declare it with `mut`'), result.output
}

fn test_private_mut_method_rejects_immutable_value_receiver() {
	result := check_private_receiver('receiver', 'fn (c counter.Counter) start() {
 c.inc()
}
fn main() {
 mut c := counter.Counter{}
 c.start()
}', false)
	assert result.exit_code != 0, result.output
	assert result.output.contains('`c` is immutable, declare it with `mut`'), result.output
}

fn test_private_mut_method_preserves_mutable_parameter_state() {
	result := check_private_receiver('mutable', 'fn start(mut c counter.Counter) {
 c.inc()
}
fn main() {
 mut c := counter.Counter{}
 start(mut c)
 assert c.get() == 1
 c.inc()
 assert c.get() == 2
 println("ok")
}', true)
	assert result.exit_code == 0, result.output
	assert result.output.trim_space() == 'ok', result.output
}

fn test_private_mut_method_preserves_pointer_receiver_state() {
	result := check_private_receiver('pointer', 'fn main() {
 c := &counter.Counter{}
 c.inc()
 assert c.get() == 1
 println("ok")
}', true)
	assert result.exit_code == 0, result.output
	assert result.output.trim_space() == 'ok', result.output
}

fn test_os_command_start_rejects_immutable_value_receiver() {
	result := check_private_receiver('os_command', 'import os

fn (c os.Command) start_it() {
 c.start() or { panic(err) }
}
fn main() {
 mut cmd := os.Command{path: "echo hello"}
 cmd.start_it()
}', false)
	assert result.exit_code != 0, result.output
	assert result.output.contains('`c` is immutable, declare it with `mut`'), result.output
}

fn test_sql_hidden_mut_alias_local_is_accepted() {
	result := check_private_receiver('sql_local', 'import db.sqlite

struct Record {
 id int
}
fn main() {
 mut db := sqlite.connect(":memory:") or { panic(err) }
 c := counter.Handle(counter.Counter{})
 records := sql db {
  select from Record where id == c.next()
 } or { panic(err) }
 println(records)
}', false)
	assert result.exit_code == 0, result.output
}

fn test_sql_hidden_mut_alias_value_parameter_is_rejected() {
	result := check_private_receiver('sql_parameter', 'import db.sqlite

struct Record {
 id int
}

fn select_records(c counter.Handle) {
 mut db := sqlite.connect(":memory:") or { panic(err) }
 records := sql db {
  select from Record where id == c.next()
 } or { panic(err) }
 println(records)
}
fn main() {
 select_records(counter.Handle(counter.Counter{}))
}', false)
	assert result.exit_code != 0, result.output
	assert result.output.contains('`c` is immutable, declare it with `mut`'), result.output
}

fn test_private_mut_tuple_local_and_orm_execute_with_cold_and_warm_cache() {
	base := os.join_path(os.vtmp_dir(), 'private_mut_receiver_cache_${os.getpid()}')
	os.mkdir_all(base)!
	defer { os.rmdir_all(base) or {} }
	file := os.join_path(base, 'main.v')
	os.write_file(file, 'import v.tests.private_mutability as counter
import db.sqlite
import orm

fn acquire_counter() !(counter.Counter, int) {
 return counter.Counter{}, 42
}
fn acquire_db() !(orm.DB, sqlite.DB) {
 connection := sqlite.connect(":memory:")!
 return orm.new_db(connection, orm.DataScope{}), connection
}
fn main() {
 c, number := acquire_counter() or { panic(err) }
 c.bump_hidden_via_method()
 c.bump_hidden_via_helper()
 assert number == 42
 db, connection := acquire_db() or { panic(err) }
 db.execute("BEGIN") or { panic(err) }
 db.execute("CREATE TABLE items (id INTEGER)") or { panic(err) }
 db.execute("INSERT INTO items VALUES (42)") or { panic(err) }
 db.execute("COMMIT") or { panic(err) }
 rows := db.execute("SELECT id FROM items") or { panic(err) }
 assert rows[0].vals == ["42"]
 println("ok")
}')!
	for _ in 0 .. 2 {
		mut compiler := os.new_process(@VEXE)
		compiler.set_args(['-new-compiler', 'run', file])
		mut environment := os.environ()
		environment['VTMP'] = os.join_path(base, 'cache')
		compiler.set_environment(environment)
		compiler.set_redirect_stdio()
		compiler.run()
		compiler.wait()
		stdout := compiler.stdout_slurp()
		output := stdout + compiler.stderr_slurp()
		assert compiler.code == 0, output
		assert stdout.trim_space() == 'ok', output
		compiler.close()
	}
}

fn test_cached_private_mut_method_rejects_visible_and_transitive_mutation() {
	base := os.join_path(os.vtmp_dir(), 'private_mut_receiver_cached_negative_${os.getpid()}')
	modules := os.join_path(base, 'modules')
	cache := os.join_path(base, 'cache')
	os.mkdir_all(os.join_path(modules, 'counter'))!
	defer { os.rmdir_all(base) or {} }
	// Source attributes must not be able to forge the proof emitted by the cache.
	source := private_receiver_module.replace('pub fn (mut c Counter) change_visible()',
		'@[_v3_hidden_mut_receiver]\npub fn (mut c Counter) change_visible()')
	os.write_file(os.join_path(modules, 'counter', 'counter.v'), source)!
	file := os.join_path(base, 'main.v')
	os.write_file(file, 'import counter\nfn main() { mut pointed := 0; mut c := counter.Counter{values: [0], visible_pointer: &pointed}; c.inc(); c.change_visible_via_method(); c.change_visible_alias(); c.change_visible_alias_chain(); c.change_visible_alias_via_helper(); c.change_visible_returned_alias(); c.change_visible_stored_array(); c.change_visible_pointer_field(); c.change_visible_field_alias(); c.change_visible_literal_array(); c.change_visible_appended_array(); c.change_visible_channel(); assert c.get() == 1; assert c.visible == 5; assert c.values == [43]; assert pointed == 2; assert c.inner.value == 1 }')!
	mut environment := os.environ()
	environment['VMODULES'] = modules
	environment['VTMP'] = cache
	mut cold := os.new_process(@VEXE)
	cold.set_args(['-new-compiler', 'run', file])
	cold.set_environment(environment)
	cold.set_redirect_stdio()
	cold.run()
	cold.wait()
	output := cold.stdout_slurp() + cold.stderr_slurp()
	assert cold.code == 0, output
	cold.close()
	headers := os.walk_ext(cache, '.vh')
	assert headers.any((os.read_file(it) or { '' }).contains('module counter')), headers.str()
	for method in ['change_visible', 'change_visible_via_method', 'change_visible_alias',
		'change_visible_alias_chain', 'change_visible_alias_via_helper', 'change_visible_returned_alias',
		'change_visible_stored_array', 'change_visible_pointer_field', 'change_visible_field_alias',
		'change_visible_literal_array', 'change_visible_appended_array', 'change_visible_channel',
		'reset'] {
		os.write_file(file, 'import counter\nfn main() { c := counter.Counter{}; c.${method}() }')!
		mut warm := os.new_process(@VEXE)
		warm.set_args(['-new-compiler', 'run', file])
		warm.set_environment(environment)
		warm.set_redirect_stdio()
		warm.run()
		warm.wait()
		result := warm.stdout_slurp() + warm.stderr_slurp()
		assert warm.code != 0, result
		assert result.contains('`c` is immutable, declare it with `mut`'), result
		warm.close()
	}
}
