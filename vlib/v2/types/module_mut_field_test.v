module types

import os
import v2.ast
import v2.parser
import v2.pref
import v2.token

fn module_mut_tmp_dir(label string) string {
	return os.join_path(os.temp_dir(), 'v2_module_mut_field_${label}_${os.getpid()}')
}

fn write_module_mut_files(tmp_dir string, files map[string]string) []string {
	mut paths := []string{}
	for rel_path, code in files {
		path := os.join_path(tmp_dir, rel_path)
		os.mkdir_all(os.dir(path)) or { panic('cannot create ${os.dir(path)}') }
		os.write_file(path, code) or { panic('cannot write ${path}') }
		paths << path
	}
	return paths
}

fn run_module_mut_v2_check(label string, files map[string]string, main_rel_path string) (int, string) {
	return run_module_mut_v2_check_with_defines(label, files, main_rel_path, [])
}

fn run_module_mut_v2_check_with_defines(label string, files map[string]string, main_rel_path string, defines []string) (int, string) {
	tmp_dir := module_mut_tmp_dir(label)
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic('cannot create ${tmp_dir}') }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	write_module_mut_files(tmp_dir, files)
	main_path := os.join_path(tmp_dir, main_rel_path)
	out_path := os.join_path(tmp_dir, 'out.txt')
	define_flags := defines.map('-d ${it}').join(' ')
	cmd := '${os.quoted_path(@VEXE)} -v2 -backend v ${define_flags} -o ${os.quoted_path(out_path)} ${os.quoted_path(main_path)} 2>&1'
	res := os.execute(cmd)
	return res.exit_code, res.output
}

fn parse_module_mut_struct(code string) ast.StructDecl {
	return parse_module_mut_struct_with_defines(code, [])
}

fn parse_module_mut_struct_with_defines(code string, defines []string) ast.StructDecl {
	tmp_dir := module_mut_tmp_dir('parse')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic('cannot create ${tmp_dir}') }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	path := os.join_path(tmp_dir, 'main.v')
	os.write_file(path, code) or { panic(err) }
	prefs := &pref.Preferences{
		user_defines: defines
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([path], mut file_set)
	for stmt in files[0].stmts {
		if stmt is ast.StructDecl {
			return stmt
		}
	}
	panic('missing struct decl')
}

fn assert_module_mut_failure(label string, main_source string, expected string) {
	code, output := run_module_mut_v2_check(label, module_mut_sources(main_source), 'main.v')
	assert code != 0, '${label} should fail'
	assert output.contains(expected), output
}

fn module_mut_sources(main_source string) map[string]string {
	return {
		'state/state.v': 'module state

pub struct Inner {
pub mut:
	value int
}

pub fn (mut i Inner) inc() {
	i.value++
}

pub struct Counter {
@[module_mut]
pub:
	value int
	items []int
	inner Inner
	ptr &Inner
pub mut:
	open int
}

pub fn (mut c Counter) inc() {
	c.value++
}

pub fn new_counter_with_ptr() Counter {
	return Counter{
		ptr: &Inner{}
	}
}
'
		'main.v':        main_source
	}
}

fn test_module_mut_parser_field_metadata() {
	decl := parse_module_mut_struct('module state

pub struct Counter {
mut:
	hidden int
pub:
	visible int
@[module_mut]
pub:
	value int
	other int
pub mut:
	open int
}
')
	assert decl.fields[0].name == 'hidden'
	assert !decl.fields[0].is_public
	assert decl.fields[0].is_mut
	assert !decl.fields[0].is_module_mut
	assert decl.fields[1].name == 'visible'
	assert decl.fields[1].is_public
	assert !decl.fields[1].is_mut
	assert !decl.fields[1].is_module_mut
	assert decl.fields[2].name == 'value'
	assert decl.fields[2].is_public
	assert decl.fields[2].is_mut
	assert decl.fields[2].is_module_mut
	assert decl.fields[3].name == 'other'
	assert decl.fields[3].is_public
	assert decl.fields[3].is_mut
	assert decl.fields[3].is_module_mut
	assert decl.fields[4].name == 'open'
	assert decl.fields[4].is_public
	assert decl.fields[4].is_mut
	assert !decl.fields[4].is_module_mut
}

fn test_module_mut_parser_comptime_branch_access_state() {
	decl := parse_module_mut_struct_with_defines('module state

pub struct Counter {
$if active_access ? {
@[module_mut]
pub:
	selected int
} $else {
pub mut:
	selected int
}
	after int
}
', [
		'active_access',
	])
	assert decl.fields[0].name == 'selected'
	assert decl.fields[0].is_public
	assert decl.fields[0].is_mut
	assert decl.fields[0].is_module_mut
	assert decl.fields[1].name == 'after'
	assert decl.fields[1].is_public
	assert decl.fields[1].is_mut
	assert decl.fields[1].is_module_mut

	decl2 := parse_module_mut_struct_with_defines('module state

pub struct Counter {
$if inactive_access ? {
pub mut:
	skipped int
} $else {
pub:
	selected int
}
	after int
}
', [])
	assert decl2.fields[0].name == 'selected'
	assert decl2.fields[0].is_public
	assert !decl2.fields[0].is_mut
	assert !decl2.fields[0].is_module_mut
	assert decl2.fields[1].name == 'after'
	assert decl2.fields[1].is_public
	assert !decl2.fields[1].is_mut
	assert !decl2.fields[1].is_module_mut
}

fn test_module_mut_comptime_branch_access_state_reaches_checker() {
	code, output := run_module_mut_v2_check_with_defines('comptime_branch_checker', {
		'state/state.v': 'module state

pub struct Counter {
$if active_access ? {
@[module_mut]
pub:
	selected int
}
	after int
}
'
		'main.v':        'module main

import state

fn main() {
	mut c := state.Counter{}
	c.after++
}
'
	}, 'main.v', [
		'active_access',
	])
	assert code != 0, 'comptime_branch_checker should fail'
	assert output.contains('cannot mutate module-mutable field `state.Counter.after` outside module `state`'), output
}

fn test_module_mut_allows_external_read_and_internal_mutation() {
	code, output := run_module_mut_v2_check('external_read_internal_mutation', module_mut_sources('module main

import state

fn main() {
	mut c := state.Counter{}
	println(c.value)
	c.inc()
	println(c.value)
}
'),
		'main.v')
	assert code == 0, output
}

fn test_module_mut_keeps_pub_mut_publicly_mutable() {
	code, output := run_module_mut_v2_check('pub_mut_unchanged', module_mut_sources('module main

import state

fn main() {
	mut c := state.Counter{}
	c.open++
	c.open += 2
}
'),
		'main.v')
	assert code == 0, output
}

fn test_module_mut_rejects_external_assignment() {
	assert_module_mut_failure('assign', 'module main

import state

fn main() {
	mut c := state.Counter{}
	c.value = 1
}
',
		'cannot mutate module-mutable field `state.Counter.value` outside module `state`')
}

fn test_module_mut_rejects_external_compound_and_postfix() {
	assert_module_mut_failure('compound', 'module main

import state

fn main() {
	mut c := state.Counter{}
	c.value += 1
}
',
		'cannot mutate module-mutable field `state.Counter.value` outside module `state`')
	assert_module_mut_failure('postfix', 'module main

import state

fn main() {
	mut c := state.Counter{}
	c.value++
}
',
		'cannot mutate module-mutable field `state.Counter.value` outside module `state`')
}

fn test_module_mut_rejects_container_and_nested_mutation() {
	assert_module_mut_failure('append', 'module main

import state

fn main() {
	mut c := state.Counter{}
	c.items << 1
}
',
		'cannot mutate module-mutable field `state.Counter.items` outside module `state`')
	assert_module_mut_failure('index_assign', 'module main

import state

fn main() {
	mut c := state.Counter{}
	c.items[0] = 1
}
',
		'cannot mutate module-mutable field `state.Counter.items` outside module `state`')
	assert_module_mut_failure('nested', 'module main

import state

fn main() {
	mut c := state.Counter{}
	c.inner.value = 1
}
',
		'cannot mutate module-mutable field `state.Counter.inner` outside module `state`')
	assert_module_mut_failure('for_mut', 'module main

import state

fn main() {
	mut c := state.Counter{}
	for mut v in c.items {
		v++
	}
}
',
		'cannot mutate module-mutable field `state.Counter.items` outside module `state`')
	assert_module_mut_failure('for_key_mut_value', 'module main

import state

fn main() {
	mut c := state.Counter{}
	for _, mut v in c.items {
		v++
	}
}
',
		'cannot mutate module-mutable field `state.Counter.items` outside module `state`')
}

fn test_module_mut_rejects_mut_arg_ref_and_init() {
	assert_module_mut_failure('mut_arg', 'module main

import state

fn take(mut value int) {}

fn main() {
	mut c := state.Counter{}
	take(mut c.value)
}
',
		'cannot pass module-mutable field `state.Counter.value` as mut outside module `state`')
	assert_module_mut_failure('mut_ref', 'module main

import state

fn main() {
	mut c := state.Counter{}
	_ = &c.value
}
',
		'cannot take mutable reference to module-mutable field `state.Counter.value` outside module `state`')
	assert_module_mut_failure('init', 'module main

import state

fn main() {
	_ = state.Counter{ value: 1 }
}
',
		'cannot initialize module-mutable field `state.Counter.value` outside module `state`')
	assert_module_mut_failure('assoc_update', 'module main

import state

fn main() {
	base := state.Counter{}
	_ = state.Counter{
		...base
		value: 1
	}
}
',
		'cannot initialize module-mutable field `state.Counter.value` outside module `state`')
}

fn test_module_mut_rejects_mut_receiver_method_call_on_field() {
	assert_module_mut_failure('mut_receiver_method_call', 'module main

import state

fn main() {
	mut c := state.Counter{}
	c.inner.inc()
}
',
		'cannot mutate module-mutable field `state.Counter.inner` outside module `state`')
}

fn test_module_mut_rejects_pointer_deref_mutation() {
	assert_module_mut_failure('pointer_deref_assign', 'module main

import state

fn main() {
	mut c := state.new_counter_with_ptr()
	(*c.ptr).value = 1
}
',
		'cannot mutate module-mutable field `state.Counter.ptr` outside module `state`')
	assert_module_mut_failure('pointer_deref_mut_receiver', 'module main

import state

fn main() {
	mut c := state.new_counter_with_ptr()
	(*c.ptr).inc()
}
',
		'cannot mutate module-mutable field `state.Counter.ptr` outside module `state`')
}

fn test_module_mut_elides_field_before_module_mut_validation() {
	code, output := run_module_mut_v2_check('elide_field_module_mut_validation', {
		'main.v': 'module main

struct Counter {
mut:
	value int @[if absent_flag ?; module_mut]
@[module_mut]
	other int @[if absent_flag ?]
}

fn main() {}
'
	}, 'main.v')
	assert code == 0, output
}

fn test_module_mut_rejects_invalid_attribute_uses() {
	invalid_cases := {
		'file_attr':         [
			'@[module_mut]
module main
',
			'`@[module_mut]` can only be used on a struct field',
		]
		'bare_file_attr':    [
			'module main

@[module_mut]
',
			'`@[module_mut]` can only be used on a struct field',
		]
		'fn_attr':           [
			'module main

@[module_mut]
fn main() {}
',
			'`@[module_mut]` can only be used on a struct field',
		]
		'struct_attr':       [
			'module main

@[module_mut]
struct Counter {}
',
			'`@[module_mut]` can only be used on a struct field',
		]
		'enum_attr':         [
			'module main

@[module_mut]
enum Kind {
	a
}
',
			'`@[module_mut]` can only be used on a struct field',
		]
		'interface_attr':    [
			'module main

@[module_mut]
interface Reader {
	read() int
}
',
			'`@[module_mut]` can only be used on a struct field',
		]
		'const_attr':        [
			'module main

@[module_mut]
const value = 1
',
			'`@[module_mut]` can only be used on a struct field',
		]
		'global_attr':       [
			'module main

@[module_mut]
__global value = 1
',
			'`@[module_mut]` can only be used on a struct field',
		]
		'not_pub':           [
			'module main

struct Counter {
@[module_mut]
mut:
	value int
}
',
			'`@[module_mut]` requires `pub:`; use `@[module_mut] pub:`',
		]
		'pub_mut':           [
			'module main

struct Counter {
@[module_mut]
pub mut:
	value int
}
',
			'`@[module_mut]` cannot be combined with `pub mut:`; `pub mut:` fields are publicly mutable',
		]
		'block_other_attr':  [
			'module main

struct Counter {
@[if prod ?; module_mut]
pub:
	value int
}
',
			'`@[module_mut] pub:` cannot be combined with other attributes',
		]
		'embedded':          [
			'module main

struct Base {}

struct Counter {
@[module_mut]
	Base
}
',
			'`@[module_mut]` cannot be applied to embedded struct fields',
		]
		'embedded_in_block': [
			'module main

struct Base {}

struct Counter {
@[module_mut]
pub:
	Base
}
',
			'`@[module_mut]` cannot be applied to embedded struct fields',
		]
		'union':             [
			'module main

union Counter {
@[module_mut]
pub:
	value int
}
',
			'`@[module_mut]` is only supported on V struct fields',
		]
		'c_struct':          [
			'module main

struct C.Counter {
@[module_mut]
pub:
	value int
}
',
			'`@[module_mut]` is only supported on V struct fields',
		]
		'arguments':         [
			'module main

struct Counter {
@[module_mut: true]
pub:
	value int
}
',
			'`@[module_mut]` does not accept arguments',
		]
		'call_arguments':    [
			'module main

struct Counter {
@[module_mut()]
pub:
	value int
}
',
			'`@[module_mut]` does not accept arguments',
		]
		'duplicates':        [
			'module main

struct Counter {
@[module_mut; module_mut]
pub:
	value int
}
',
			'`@[module_mut]` specified more than once',
		]
	}
	for label, pair in invalid_cases {
		code, output := run_module_mut_v2_check('invalid_${label}', {
			'main.v': pair[0]
		}, 'main.v')
		assert code != 0, '${label} should fail'
		assert output.contains(pair[1]), output
	}
}
