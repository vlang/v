module types

import os
import v2.ast
import v2.gen.v as gen_v
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

fn parse_module_mut_interface(code string) ast.InterfaceDecl {
	tmp_dir := module_mut_tmp_dir('parse_interface')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic('cannot create ${tmp_dir}') }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	path := os.join_path(tmp_dir, 'main.v')
	os.write_file(path, code) or { panic(err) }
	prefs := &pref.Preferences{}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([path], mut file_set)
	for stmt in files[0].stmts {
		if stmt is ast.InterfaceDecl {
			return stmt
		}
	}
	panic('missing interface decl')
}

fn gen_module_mut_source(code string) string {
	tmp_dir := module_mut_tmp_dir('gen')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic('cannot create ${tmp_dir}') }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	path := os.join_path(tmp_dir, 'main.v')
	os.write_file(path, code) or { panic(err) }
	prefs := &pref.Preferences{}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([path], mut file_set)
	mut gen := gen_v.new_gen(prefs)
	gen.gen(files[0])
	return gen.output_string()
}

fn assert_module_mut_failure(label string, main_source string, expected string) {
	code, output := run_module_mut_v2_check(label, module_mut_sources(main_source), 'main.v')
	assert code != 0, '${label} should fail'
	assert output.contains(expected), output
}

fn assert_module_mut_failure_once(label string, main_source string, expected string) {
	code, output := run_module_mut_v2_check(label, module_mut_sources(main_source), 'main.v')
	assert code != 0, '${label} should fail'
	assert output.count(expected) == 1, output
}

fn assert_module_mut_interface_failure(label string, main_source string, expected string) {
	code, output := run_module_mut_v2_check(label, module_mut_interface_sources(main_source),
		'main.v')
	assert code != 0, '${label} should fail'
	assert output.contains(expected), output
}

fn assert_module_mut_interface_failure_once(label string, main_source string, expected string) {
	code, output := run_module_mut_v2_check(label, module_mut_interface_sources(main_source),
		'main.v')
	assert code != 0, '${label} should fail'
	assert output.count(expected) == 1, output
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

pub struct OtherInner {
pub mut:
	value int
}

pub fn (mut i OtherInner) inc() {
	i.value++
}

pub struct Counter {
pub module_mut:
	value int
	items []int
	inner Inner
	ptr &Inner
pub mut:
	open int
	open_items []int
	open_inner Inner
}

pub fn (mut c Counter) inc() {
	c.value++
}

pub fn (mut c Counter) bump_items() {
	for mut value in c.items {
		value++
	}
}

pub fn (mut c Counter) call_inner_inc_method_value() {
	f := c.inner.inc
	f()
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

fn module_mut_sumtype_mixed_sources(main_source string) map[string]string {
	return {
		'restricted/restricted.v': 'module restricted

pub struct Restricted {
pub module_mut:
	value int
}
'
		'open/open.v':             'module open

pub struct Open {
pub mut:
	value int
}
'
		'main.v':                  main_source
	}
}

fn module_mut_sumtype_pub_mut_sources(main_source string) map[string]string {
	return {
		'open/open.v':   'module open

pub struct Open {
pub mut:
	value int
}
'
		'freely/free.v': 'module freely

pub struct Free {
pub mut:
	value int
}
'
		'main.v':        main_source
	}
}

fn module_mut_interface_sources(main_source string) map[string]string {
	return {
		'state/state.v': 'module state

pub interface Mutator {
	read() int
mut:
	bump()
	reset()
}

pub struct LocalMutator {
mut:
	value int
}

pub fn (m LocalMutator) read() int {
	return m.value
}

pub fn (mut m LocalMutator) bump() {
	m.value++
}

pub fn (mut m LocalMutator) reset() {
	m.value = 0
}

pub struct InterfaceCounter {
pub module_mut:
	iface Mutator
pub mut:
	open_iface Mutator
}

pub fn new_interface_counter() InterfaceCounter {
	return InterfaceCounter{
		iface:      LocalMutator{}
		open_iface: LocalMutator{}
	}
}

pub fn (mut c InterfaceCounter) bump_iface() {
	c.iface.bump()
	c.iface.reset()
}

pub fn (mut c InterfaceCounter) call_iface_bump_method_value() {
	f := c.iface.bump
	f()
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
pub module_mut:
	value int
	other int
pub:
	readable int
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
	assert decl.fields[4].name == 'readable'
	assert decl.fields[4].is_public
	assert !decl.fields[4].is_mut
	assert !decl.fields[4].is_module_mut
	assert decl.fields[5].name == 'open'
	assert decl.fields[5].is_public
	assert decl.fields[5].is_mut
	assert !decl.fields[5].is_module_mut
}

fn test_module_mut_parser_interface_mut_block_metadata() {
	decl := parse_module_mut_interface('module state

interface Mutator {
	name string
mut:
	bump()
	reset()
}
')
	assert decl.fields[0].name == 'name'
	assert !decl.fields[0].is_mut
	assert decl.fields[1].name == 'bump'
	assert decl.fields[1].is_mut
	assert decl.fields[2].name == 'reset'
	assert decl.fields[2].is_mut
}

fn test_module_mut_parser_interface_method_metadata_distinguishes_fn_fields() {
	decl := parse_module_mut_interface('module state

type Handler = fn (int) bool

interface HandlerBox {
	type() string
	handle(int) bool
	convert[T](value T) T
	handler fn (int) bool
	callback Handler
mut:
	bump()
	mut_handler fn () bool
}
')
	assert decl.fields[0].name == 'type'
	assert decl.fields[0].is_interface_method
	assert decl.fields[1].name == 'handle'
	assert decl.fields[1].is_interface_method
	assert decl.fields[2].name == 'convert'
	assert decl.fields[2].is_interface_method
	assert decl.fields[3].name == 'handler'
	assert !decl.fields[3].is_interface_method
	assert decl.fields[4].name == 'callback'
	assert !decl.fields[4].is_interface_method
	assert decl.fields[5].name == 'bump'
	assert decl.fields[5].is_mut
	assert decl.fields[5].is_interface_method
	assert decl.fields[6].name == 'mut_handler'
	assert decl.fields[6].is_mut
	assert !decl.fields[6].is_interface_method
}

fn test_module_mut_gen_v_interface_mut_block_round_trip() {
	source := 'module state

interface Mutator {
	name string
mut:
	bump()
	reset()
}
'
	generated := gen_module_mut_source(source)
	assert generated.contains('mut:')
	assert generated.contains('bump()')
	assert generated.contains('reset()')
	assert !generated.contains('bump fn(')
	decl := parse_module_mut_interface(generated)
	assert decl.fields[0].name == 'name'
	assert !decl.fields[0].is_mut
	assert decl.fields[1].name == 'bump'
	assert decl.fields[1].is_mut
	assert decl.fields[1].is_interface_method
	assert decl.fields[2].name == 'reset'
	assert decl.fields[2].is_mut
	assert decl.fields[2].is_interface_method
}

fn test_module_mut_gen_v_interface_fn_field_round_trip() {
	source := 'module state

type Handler = fn (int) bool

interface HandlerBox {
	handle(int) bool
	handler fn (int) bool
	callback Handler
mut:
	bump()
}
'
	generated := gen_module_mut_source(source)
	assert generated.contains('handle(int) bool')
	assert generated.contains('handler fn(int) bool')
	assert generated.contains('callback Handler')
	assert generated.contains('bump()')
	assert !generated.contains('handler(int) bool')
	decl := parse_module_mut_interface(generated)
	assert decl.fields[0].name == 'handle'
	assert decl.fields[0].is_interface_method
	assert decl.fields[1].name == 'handler'
	assert !decl.fields[1].is_interface_method
	assert decl.fields[2].name == 'callback'
	assert !decl.fields[2].is_interface_method
	assert decl.fields[3].name == 'bump'
	assert decl.fields[3].is_interface_method
}

fn test_module_mut_parser_comptime_branch_access_state() {
	decl := parse_module_mut_struct_with_defines('module state

pub struct Counter {
$if active_access ? {
pub module_mut:
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

	decl3 := parse_module_mut_struct_with_defines('module state

pub struct Counter {
$if inactive_access ? {
pub mut:
	skipped int
} $else $if active_access ? {
pub module_mut:
	selected int
} $else {
pub:
	selected int
}
	after int
}
', [
		'active_access',
	])
	assert decl3.fields[0].name == 'selected'
	assert decl3.fields[0].is_public
	assert decl3.fields[0].is_mut
	assert decl3.fields[0].is_module_mut
	assert decl3.fields[1].name == 'after'
	assert decl3.fields[1].is_public
	assert decl3.fields[1].is_mut
	assert decl3.fields[1].is_module_mut
}

fn test_module_mut_comptime_branch_access_state_reaches_checker() {
	code, output := run_module_mut_v2_check_with_defines('comptime_branch_checker', {
		'state/state.v': 'module state

pub struct Counter {
$if active_access ? {
pub module_mut:
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

fn test_module_mut_find_field_info_empty_struct_placeholder_does_not_recurse() {
	empty_struct := Type(Struct{
		name: 'Loop'
	})
	mut scope := new_scope(unsafe { nil })
	scope.insert_type('Loop', empty_struct)
	mut prefs := pref.new_preferences()
	mut file_set := token.FileSet.new()
	mut c := Checker{
		pref:     &prefs
		file_set: &file_set
		env:      Environment.new()
		scope:    scope
	}
	if _ := c.find_field_info(empty_struct, 'missing') {
		assert false, 'empty placeholder should not resolve a missing field'
	}
}

fn test_module_mut_gen_v_round_trip() {
	source := 'module state

pub struct Counter {
pub module_mut:
	value int
pub mut:
	open int
}
'
	generated := gen_module_mut_source(source)
	assert generated.contains('pub module_mut:')
	assert !generated.contains('@[module_mut]')
	decl := parse_module_mut_struct(generated)
	assert decl.fields[0].name == 'value'
	assert decl.fields[0].is_public
	assert decl.fields[0].is_mut
	assert decl.fields[0].is_module_mut
	assert decl.fields[1].name == 'open'
	assert decl.fields[1].is_public
	assert decl.fields[1].is_mut
	assert !decl.fields[1].is_module_mut
}

fn test_module_mut_is_contextual_identifier() {
	code, output := run_module_mut_v2_check('contextual_identifier', {
		'main.v': 'module main

fn module_mut() int {
	return 1
}

struct Names {
pub:
	module_mut int
}

fn main() {
	module_mut_value := module_mut()
	module_mut := module_mut_value + 1
	n := Names{
		module_mut: module_mut
	}
	println(n.module_mut)
}
'
	}, 'main.v')
	assert code == 0, output
}

fn test_module_mut_allows_external_read_and_internal_mutation() {
	code, output := run_module_mut_v2_check('external_read_internal_mutation', module_mut_sources('module main

import state

fn main() {
	mut c := state.Counter{}
	println(c.value)
	c.inc()
	c.bump_items()
	println(c.value)
}
'),
		'main.v')
	assert code == 0, output
}

fn test_module_mut_allows_internal_mut_interface_method_call_on_field() {
	code, output := run_module_mut_v2_check('internal_mut_interface_method_call', module_mut_interface_sources('module main

import state

fn main() {
	mut c := state.new_interface_counter()
	c.bump_iface()
}
'),
		'main.v')
	assert code == 0, output
}

fn test_module_mut_allows_external_non_mut_interface_method_call_on_field() {
	code, output := run_module_mut_v2_check('external_non_mut_interface_method_call', module_mut_interface_sources('module main

import state

fn main() {
	mut c := state.new_interface_counter()
	println(c.iface.read())
}
'),
		'main.v')
	assert code == 0, output
}

fn test_module_mut_allows_internal_mut_interface_method_value_on_field() {
	code, output := run_module_mut_v2_check('internal_mut_interface_method_value', module_mut_interface_sources('module main

import state

fn main() {
	mut c := state.new_interface_counter()
	c.call_iface_bump_method_value()
}
'),
		'main.v')
	assert code == 0, output
}

fn test_module_mut_allows_pub_mut_interface_method_value_on_field() {
	code, output := run_module_mut_v2_check('pub_mut_interface_method_value', module_mut_interface_sources('module main

import state

fn main() {
	mut c := state.new_interface_counter()
	f := c.open_iface.bump
	f()
}
'),
		'main.v')
	assert code == 0, output
}

fn test_module_mut_interface_fn_field_requires_field_not_method() {
	handler_fn_type := FnType{
		params:      [
			Parameter{
				name: 'value'
				typ:  Type(int_)
			},
		]
		return_type: to_optional_type(Type(bool_))
	}
	handler_type := Type(handler_fn_type)
	field_iface := Interface{
		name:   'HasHandler'
		fields: [
			Field{
				name: 'handler'
				typ:  handler_type
			},
		]
	}
	method_iface := Interface{
		name:   'Handles'
		fields: [
			Field{
				name:                'handler'
				typ:                 handler_type
				is_interface_method: true
			},
		]
	}
	method_only := Type(Struct{
		name: 'MethodOnly'
	})
	field_backed := Type(Struct{
		name:   'FieldBacked'
		fields: [
			Field{
				name: 'handler'
				typ:  handler_type
			},
		]
	})
	method_only_iface := Type(Interface{
		name:   'MethodOnlyIface'
		fields: [
			Field{
				name:                'handler'
				typ:                 handler_type
				is_interface_method: true
			},
		]
	})
	field_backed_iface := Type(Interface{
		name:   'FieldBackedIface'
		fields: [
			Field{
				name: 'handler'
				typ:  handler_type
			},
		]
	})
	mut prefs := pref.new_preferences()
	mut file_set := token.FileSet.new()
	mut c := Checker{
		pref:     &prefs
		file_set: &file_set
		env:      Environment.new()
		scope:    new_scope(unsafe { nil })
	}
	c.register_method_type('MethodOnly', 'handler', handler_fn_type)
	assert c.type_satisfies_interface(field_backed, field_iface)
	assert !c.type_satisfies_interface(method_only, field_iface)
	assert c.type_satisfies_interface(method_only, method_iface)
	assert c.type_satisfies_interface(field_backed_iface, field_iface)
	assert !c.type_satisfies_interface(method_only_iface, field_iface)
	assert c.type_satisfies_interface(method_only_iface, method_iface)
	assert !c.type_satisfies_interface(field_backed_iface, method_iface)
}

fn test_module_mut_keeps_pub_mut_publicly_mutable() {
	code, output := run_module_mut_v2_check('pub_mut_unchanged', module_mut_sources('module main

import state

fn main() {
	mut c := state.Counter{}
	c.open++
	c.open += 2
	for mut value in c.open_items {
		value++
	}
	f := c.open_inner.inc
	f()
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
	assert_module_mut_failure('import_alias_assign', 'module main

import state as s

fn main() {
	mut c := s.Counter{}
	println(c.value)
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

fn test_module_mut_rejects_nested_inner_owner_from_outer_owner_module() {
	code, output := run_module_mut_v2_check('nested_inner_owner', {
		'state/inner/inner.v': 'module inner

pub struct Inner {
pub module_mut:
	value int
}
'
		'state/state.v':       'module state

import inner

pub struct Counter {
pub module_mut:
	inner inner.Inner
}

pub fn touch_inner() {
	mut c := Counter{}
	c.inner.value++
}
'
		'main.v':              'module main

import state

fn main() {
	state.touch_inner()
}
'
	}, 'main.v')
	assert code != 0, 'nested inner owner mutation should fail'
	assert output.contains('cannot mutate module-mutable field `inner.Inner.value` outside module `inner`'), output
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

fn test_module_mut_rejects_generic_field_mutation() {
	assert_module_mut_failure('generic_field_mutation', 'module main

import state

fn poke[T](mut x T) {
	x.value++
}

fn main() {
	mut c := state.Counter{}
	poke(mut c)
}
	',
		'cannot mutate module-mutable field `state.Counter.value` outside module `state`')
}

fn test_module_mut_rejects_generic_field_mutation_after_open_instantiation() {
	assert_module_mut_failure('generic_field_mutation_after_open_instantiation', 'module main

import state

struct OpenCounter {
pub mut:
	value int
}

fn poke[T](mut x T) {
	x.value++
}

fn main() {
	mut open := OpenCounter{}
	poke(mut open)
	mut c := state.Counter{}
	poke(mut c)
}
',
		'cannot mutate module-mutable field `state.Counter.value` outside module `state`')
}

fn test_module_mut_rejects_generic_mut_arg_after_open_instantiation() {
	assert_module_mut_failure('generic_mut_arg_after_open_instantiation', 'module main

import state

struct OpenCounter {
pub mut:
	value int
}

fn take(mut value int) {}

fn poke[T](mut x T) {
	take(mut x.value)
}

fn main() {
	mut open := OpenCounter{}
	poke(mut open)
	mut c := state.Counter{}
	poke(mut c)
}
',
		'cannot pass module-mutable field `state.Counter.value` as mut outside module `state`')
}

fn test_module_mut_rejects_generic_mut_arg_after_open_instantiation_with_different_field_type() {
	assert_module_mut_failure('generic_mut_arg_after_open_instantiation_different_field_type', 'module main

import state

struct OpenCounter {
pub mut:
	value i64
	inner state.OtherInner
}

fn take(mut value i64) {}

fn poke_take[T](mut x T) {
	take(mut x.value)
}

fn poke_inc[T](mut x T) {
	x.inner.inc()
}

fn main() {
	mut open := OpenCounter{}
	poke_take(mut open)
	poke_inc(mut open)
	mut c := state.Counter{}
	poke_take(mut c)
	poke_inc(mut c)
}
',
		'cannot pass module-mutable field `state.Counter.value` as mut outside module `state`')
}

fn test_module_mut_rejects_generic_nested_field_mutation() {
	assert_module_mut_failure('generic_nested_field_mutation', 'module main

import state

fn poke_inner[T](mut x T) {
	x.inner.value++
}

fn main() {
	mut c := state.Counter{}
	poke_inner(mut c)
}
	',
		'cannot mutate module-mutable field `state.Counter.inner` outside module `state`')
}

fn test_module_mut_rejects_generic_nested_method_call_after_open_instantiation() {
	assert_module_mut_failure_once('generic_nested_method_call_after_open_instantiation', 'module main

import state

struct OpenCounter {
pub mut:
	inner state.Inner
}

fn poke_inner[T](mut x T) {
	x.inner.inc()
}

fn main() {
	mut open := OpenCounter{}
	poke_inner(mut open)
	mut c := state.Counter{}
	poke_inner(mut c)
}
',
		'cannot mutate module-mutable field `state.Counter.inner` outside module `state`')
}

fn test_module_mut_rejects_generic_nested_method_call_after_open_instantiation_with_different_field_type() {
	assert_module_mut_failure_once('generic_nested_method_call_after_open_instantiation_different_field_type', 'module main

import state

struct OpenCounter {
pub mut:
	inner state.OtherInner
}

fn poke_inner[T](mut x T) {
	x.inner.inc()
}

fn main() {
	mut open := OpenCounter{}
	poke_inner(mut open)
	mut c := state.Counter{}
	poke_inner(mut c)
}
',
		'cannot mutate module-mutable field `state.Counter.inner` outside module `state`')
}

fn test_module_mut_rejects_generic_postfix_after_open_instantiation_with_different_field_type() {
	assert_module_mut_failure('generic_postfix_after_open_instantiation_different_field_type', 'module main

import state

struct OpenCounter {
pub mut:
	value i64
}

fn poke[T](mut x T) {
	x.value++
}

fn main() {
	mut open := OpenCounter{}
	poke(mut open)
	mut c := state.Counter{}
	poke(mut c)
}
',
		'cannot mutate module-mutable field `state.Counter.value` outside module `state`')
}

fn test_module_mut_rejects_generic_postfix_before_open_instantiation_with_different_field_type() {
	assert_module_mut_failure('generic_postfix_before_open_instantiation_different_field_type', 'module main

import state

struct OpenCounter {
pub mut:
	value i64
}

fn poke[T](mut x T) {
	x.value++
}

fn main() {
	mut c := state.Counter{}
	poke(mut c)
	mut open := OpenCounter{}
	poke(mut open)
}
',
		'cannot mutate module-mutable field `state.Counter.value` outside module `state`')
}

fn test_module_mut_rejects_sumtype_smartcast_lvalue_mutation() {
	assert_module_mut_failure('sumtype_smartcast_lvalue_mutation', 'module main

import state

struct Other {}

type Item = state.Counter | Other

fn touch(mut x Item) {
	if x is state.Counter {
		x.value = 1
	}
}

fn main() {
	mut item := Item(state.Counter{})
	touch(mut item)
}
',
		'cannot mutate module-mutable field `state.Counter.value` outside module `state`')
}

fn test_module_mut_rejects_sumtype_common_field_mixed_access_restricted_first() {
	code, output := run_module_mut_v2_check('sumtype_common_field_mixed_restricted_first', module_mut_sumtype_mixed_sources('module main

import open
import restricted

type Item = restricted.Restricted | open.Open

fn touch(mut x Item) {
	x.value = 1
}

fn main() {
	mut item := Item(restricted.Restricted{})
	touch(mut item)
}
'),
		'main.v')
	assert code != 0, 'sumtype_common_field_mixed_restricted_first should fail'
	assert output.contains('cannot mutate module-mutable field `restricted.Restricted.value` outside module `restricted`'), output
}

fn test_module_mut_rejects_sumtype_common_field_mixed_access_open_first() {
	code, output := run_module_mut_v2_check('sumtype_common_field_mixed_open_first', module_mut_sumtype_mixed_sources('module main

import open
import restricted

type Item = open.Open | restricted.Restricted

fn touch(mut x Item) {
	x.value = 1
}

fn main() {
	mut item := Item(open.Open{})
	touch(mut item)
}
'),
		'main.v')
	assert code != 0, 'sumtype_common_field_mixed_open_first should fail'
	assert output.contains('cannot mutate module-mutable field `restricted.Restricted.value` outside module `restricted`'), output
}

fn test_module_mut_allows_sumtype_common_field_when_all_variants_are_pub_mut() {
	code, output := run_module_mut_v2_check('sumtype_common_field_all_pub_mut', module_mut_sumtype_pub_mut_sources('module main

import freely
import open

type Item = open.Open | freely.Free

fn touch(mut x Item) {
	x.value = 1
}

fn main() {
	mut item := Item(open.Open{})
	touch(mut item)
}
'),
		'main.v')
	assert code == 0, output
}

fn test_module_mut_rejects_mut_receiver_method_call_on_field() {
	assert_module_mut_failure_once('mut_receiver_method_call', 'module main

import state

fn main() {
	mut c := state.Counter{}
	c.inner.inc()
}
',
		'cannot mutate module-mutable field `state.Counter.inner` outside module `state`')
}

fn test_module_mut_rejects_external_mut_receiver_method_value_on_field() {
	assert_module_mut_failure('mut_receiver_method_value', 'module main

import state

fn main() {
	mut c := state.Counter{}
	f := c.inner.inc
	f()
}
',
		'cannot mutate module-mutable field `state.Counter.inner` outside module `state`')
}

fn test_module_mut_allows_internal_mut_receiver_method_value_on_field() {
	code, output := run_module_mut_v2_check('internal_mut_receiver_method_value', module_mut_sources('module main

import state

fn main() {
	mut c := state.Counter{}
	c.call_inner_inc_method_value()
}
'),
		'main.v')
	assert code == 0, output
}

fn test_module_mut_rejects_mut_interface_method_call_on_field() {
	assert_module_mut_interface_failure_once('mut_interface_method_call', 'module main

import state

fn main() {
	mut c := state.new_interface_counter()
	c.iface.bump()
}
',
		'cannot mutate module-mutable field `state.InterfaceCounter.iface` outside module `state`')
}

fn test_module_mut_rejects_external_mut_interface_method_value_on_field() {
	assert_module_mut_interface_failure('mut_interface_method_value', 'module main

import state

fn main() {
	mut c := state.new_interface_counter()
	f := c.iface.bump
	f()
}
',
		'cannot mutate module-mutable field `state.InterfaceCounter.iface` outside module `state`')
}

fn test_module_mut_rejects_smartcast_mut_interface_method_call_on_field() {
	assert_module_mut_interface_failure_once('smartcast_mut_interface_method_call', 'module main

import state

fn main() {
	mut c := state.new_interface_counter()
	if c.iface is state.LocalMutator {
		c.iface.bump()
	}
}
',
		'cannot mutate module-mutable field `state.InterfaceCounter.iface` outside module `state`')
}

fn test_module_mut_rejects_smartcast_mut_interface_method_value_on_field() {
	assert_module_mut_interface_failure('smartcast_mut_interface_method_value', 'module main

import state

fn main() {
	mut c := state.new_interface_counter()
	if c.iface is state.LocalMutator {
		f := c.iface.bump
		f()
	}
}
',
		'cannot mutate module-mutable field `state.InterfaceCounter.iface` outside module `state`')
}

fn test_module_mut_rejects_cast_mut_interface_method_call_on_field() {
	assert_module_mut_interface_failure_once('as_cast_mut_interface_method_call', 'module main

import state

fn main() {
	mut c := state.new_interface_counter()
	(c.iface as state.LocalMutator).bump()
}
',
		'cannot mutate module-mutable field `state.InterfaceCounter.iface` outside module `state`')
}

fn test_module_mut_rejects_cast_mut_interface_method_value_on_field() {
	assert_module_mut_interface_failure('as_cast_mut_interface_method_value', 'module main

import state

fn main() {
	mut c := state.new_interface_counter()
	f := (c.iface as state.LocalMutator).bump
	f()
}
',
		'cannot mutate module-mutable field `state.InterfaceCounter.iface` outside module `state`')
}

fn test_module_mut_rejects_call_cast_mut_interface_method_call_on_field() {
	assert_module_mut_interface_failure_once('call_cast_mut_interface_method_call', 'module main

import state

fn main() {
	mut c := state.new_interface_counter()
	state.LocalMutator(c.iface).bump()
}
	',
		'cannot mutate module-mutable field `state.InterfaceCounter.iface` outside module `state`')
}

fn test_module_mut_rejects_call_cast_mut_interface_method_value_on_field() {
	assert_module_mut_interface_failure('call_cast_mut_interface_method_value', 'module main

import state

fn main() {
	mut c := state.new_interface_counter()
	f := state.LocalMutator(c.iface).bump
	f()
}
',
		'cannot mutate module-mutable field `state.InterfaceCounter.iface` outside module `state`')
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

fn test_module_mut_rejects_invalid_access_blocks() {
	invalid_cases := {
		'bare_module_mut':              [
			'module main

struct Counter {
module_mut:
	value int
}
',
			'`module_mut:` must be written as `pub module_mut:`',
		]
		'mut_module_mut':               [
			'module main

struct Counter {
mut module_mut:
	value int
}
',
			'`mut module_mut:` is invalid; use `pub module_mut:`',
		]
		'pub_mut_module_mut':           [
			'module main

struct Counter {
pub mut module_mut:
	value int
}
',
			'`pub mut module_mut:` is invalid; use `pub module_mut:`',
		]
		'pub_module_mut_mut':           [
			'module main

struct Counter {
pub module_mut mut:
	value int
}
',
			'`pub module_mut mut:` is invalid; use `pub module_mut:`',
		]
		'union':                        [
			'module main

union Counter {
pub module_mut:
	value int
}
',
			'`pub module_mut:` is only supported on V struct fields',
		]
		'c_struct':                     [
			'module main

struct C.Counter {
pub module_mut:
	value int
}
',
			'`pub module_mut:` is only supported on V struct fields',
		]
		'embedded':                     [
			'module main

struct Base {}

struct Counter {
pub module_mut:
	Base
}
',
			'`pub module_mut:` cannot be applied to embedded struct fields',
		]
		'leading_if_attribute':         [
			'module main

struct Counter {
@[if prod ?]
pub module_mut:
	value int
}
',
			'attributes on struct fields must be placed after the field declaration',
		]
		'leading_module_mut_attribute': [
			'module main

struct Counter {
@[module_mut]
pub module_mut:
	value int
}
',
			'attributes on struct fields must be placed after the field declaration',
		]
		'old_attribute_block':          [
			'module main

struct Counter {
@[module_mut]
pub:
	value int
}
',
			'attributes on struct fields must be placed after the field declaration',
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

fn test_module_mut_attribute_has_no_special_field_semantics() {
	decl := parse_module_mut_struct('module state

pub struct Counter {
pub:
	value int @[module_mut]
}
')
	assert decl.fields[0].name == 'value'
	assert decl.fields[0].is_public
	assert !decl.fields[0].is_mut
	assert !decl.fields[0].is_module_mut
}
