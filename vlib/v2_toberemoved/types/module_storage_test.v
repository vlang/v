module types

import os
import v2.parser
import v2.pref
import v2.token

fn module_storage_tmp_dir(label string) string {
	return os.join_path(os.temp_dir(), 'v2_module_storage_${label}_${os.getpid()}')
}

fn write_module_storage_files(tmp_dir string, files map[string]string) []string {
	mut paths := []string{}
	for rel_path, code in files {
		path := os.join_path(tmp_dir, rel_path)
		os.mkdir_all(os.dir(path)) or { panic('cannot create ${os.dir(path)}') }
		os.write_file(path, code) or { panic('cannot write ${path}') }
		paths << path
	}
	return paths
}

fn check_module_storage_files(files map[string]string) &Environment {
	tmp_dir := module_storage_tmp_dir('direct')
	os.mkdir_all(tmp_dir) or { panic('cannot create ${tmp_dir}') }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	paths := write_module_storage_files(tmp_dir, files)
	prefs := &pref.Preferences{}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	parsed_files := par.parse_files(paths, mut file_set)
	mut env := Environment.new()
	mut checker := Checker.new(prefs, file_set, env)
	checker.check_files(parsed_files)
	return env
}

fn check_module_storage_files_flat(files map[string]string) &Environment {
	tmp_dir := module_storage_tmp_dir('flat')
	os.mkdir_all(tmp_dir) or { panic('cannot create ${tmp_dir}') }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	paths := write_module_storage_files(tmp_dir, files)
	prefs := &pref.Preferences{}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	flat := par.parse_files_to_flat(paths, mut file_set)
	mut env := Environment.new()
	mut checker := Checker.new(prefs, file_set, env)
	checker.check_flat(&flat)
	return env
}

fn module_storage_global(env &Environment, module_name string, name string) Global {
	scope := env.get_scope(module_name) or { panic('missing module scope ${module_name}') }
	obj := scope.lookup_parent(name, 0) or { panic('missing ${module_name}.${name}') }
	assert obj is Global, '${module_name}.${name} should be a module storage object'
	return obj as Global
}

fn run_module_storage_v2_check(label string, files map[string]string, main_rel_path string) (int, string) {
	tmp_dir := module_storage_tmp_dir(label)
	os.mkdir_all(tmp_dir) or { panic('cannot create ${tmp_dir}') }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	write_module_storage_files(tmp_dir, files)
	main_path := os.join_path(tmp_dir, main_rel_path)
	out_path := os.join_path(tmp_dir, 'out.txt')
	cmd := '${os.quoted_path(@VEXE)} -v2 -backend v -o ${os.quoted_path(out_path)} ${os.quoted_path(main_path)} 2>&1'
	res := os.execute(cmd)
	return res.exit_code, res.output
}

fn module_storage_v2_fixture_sources(name string) map[string]string {
	fixture_dir := os.join_path(os.dir(os.dir(@FILE)), 'tests', name)
	mut sources := map[string]string{}
	files := os.walk_ext(fixture_dir, '.vv2')
	for file in files {
		rel_path := file.all_after(fixture_dir + os.path_separator).replace('.vv2', '.v')
		sources[rel_path] = os.read_file(file) or { panic('cannot read ${file}') }
	}
	return sources
}

fn test_module_storage_decl_metadata_keeps_spelling_out_of_semantics() {
	env := check_module_storage_files({
		'report/report.v': 'module report

pub __global mut errors = 0
__global hidden = 1
'
	})
	errors := module_storage_global(env, 'report', 'errors')
	assert errors.mod == 'report'
	assert errors.is_public
	assert errors.is_mut
	assert errors.name == 'errors'
	hidden := module_storage_global(env, 'report', 'hidden')
	assert hidden.mod == 'report'
	assert !hidden.is_public
	assert !hidden.is_mut
}

fn test_module_storage_decl_type_allows_forward_struct_reference() {
	env := check_module_storage_files({
		'main.v': 'module main

__global g_game &Game

struct Game {
	visible_i int
}

fn main() {
	_ = g_game
}
'
	})
	g_game := module_storage_global(env, 'main', 'g_game')
	assert g_game.typ.name() == '&Game'
}

fn test_module_storage_public_qualified_access_accepts_import_alias() {
	code, output := run_module_storage_v2_check('public_alias', {
		'report/report.v': 'module report

pub __global mut errors = 0
'
		'main.v':          'import report as r

fn main() {
	r.errors += 1
}
'
	}, 'main.v')
	assert code == 0, output
}

fn test_module_storage_private_same_module_files_accept_bare_access() {
	check_module_storage_files({
		'report/state.v': 'module report

__global mut errors = 0
'
		'report/api.v':   'module report

fn inc() {
	errors += 1
}
'
	})
}

fn test_module_storage_imported_names_are_not_bare_scope() {
	code, output := run_module_storage_v2_check('bare_import', {
		'report/report.v': 'module report

pub __global mut errors = 0
'
		'main.v':          'import report

fn main() {
	_ = errors
}
'
	}, 'main.v')
	assert code != 0, 'bare imported module storage should fail'
	assert output.contains('unknown ident `errors'), output
}

fn test_module_storage_private_qualified_access_fails() {
	code, output := run_module_storage_v2_check('private_qualified', {
		'report/report.v': 'module report

__global mut hidden = 0
'
		'main.v':          'import report

fn main() {
	report.hidden += 1
}
'
	}, 'main.v')
	assert code != 0, 'private module storage should not be reachable from another module'
	assert output.contains('module global `report.hidden` is private'), output
}

fn test_module_storage_selective_import_does_not_make_bare_global() {
	code, output := run_module_storage_v2_check('selective_import', {
		'report/report.v': 'module report

pub __global mut errors = 0
'
		'main.v':          'import report { errors }

fn main() {
	_ = errors
}
'
	}, 'main.v')
	assert code != 0, 'selective import must not expose module storage by bare name'
	assert output.contains('unknown ident `errors'), output
}

fn test_module_storage_flat_selective_import_does_not_make_bare_global() {
	env := check_module_storage_files_flat({
		'report/report.v': 'module report

pub __global mut errors = 0
'
		'main.v':          'module main

import report { errors }

fn main() {}
'
	})
	scope := env.get_scope('main') or { panic('missing main scope') }
	if obj := scope.lookup_parent('errors', 0) {
		_ = obj
		assert false, 'flat selective import should not expose module storage by bare name'
	}
}

fn test_imported_generic_receiver_init_mapping_resolves_result_return_type() {
	code, output := run_module_storage_v2_check('imported_generic_receiver_init_mapping', {
		'boxlib/boxlib.v': 'module boxlib

pub struct Queue[T] {
pub mut:
	value T
}

pub fn (mut queue Queue[T]) pop() !T {
	return queue.value
}
'
		'main.v':          'module main

import boxlib

fn expect_strings(values []string) {
	_ = values
}

fn expect_string(value string) {
	_ = value
}

fn main() {
	mut queue := boxlib.Queue[[]string]{value: ["alpha", "omega"]}
	v := queue.pop() or { return }
	expect_strings(v)
	expect_string(v[0])
}
'
	}, 'main.v')
	assert code == 0, output
}

fn test_generic_sumtype_match_branch_smartcasts_generic_variant_fields() {
	code, output := run_module_storage_v2_check('generic_sumtype_match_variant_field', {
		'main.v': 'module main

struct Empty {}

struct Node[T] {
	value T
}

type Tree[T] = Empty | Node[T]

fn value_is_positive(tree Tree[f64]) bool {
	return match tree {
		Node[f64] {
			_ := tree.value
			tree.value > 0.0
		}
		else {
			false
		}
	}
}

fn main() {
	tree := Tree[f64](Node[f64]{value: 1.0})
	_ = value_is_positive(tree)
}
'
	}, 'main.v')
	assert code == 0, output
}

fn test_generic_receiver_sumtype_match_branch_smartcasts_generic_variant_fields() {
	code, output := run_module_storage_v2_check('generic_receiver_sumtype_match_variant_field', {
		'main.v': r'module main

struct Empty {}

struct Node[T] {
	value T
	left Tree[T]
}

type Tree[T] = Empty | Node[T]

fn (tree Tree[T]) size[T]() int {
	return match tree {
		Node[T] {
			1 + tree.left.size()
		}
		else {
			0
		}
	}
}

fn (tree Tree[T]) insert[T](x T) Tree[T] {
	_ = x
	return tree
}

fn (tree Tree[T]) ok[T](x T) bool {
	return match tree {
		Node[T] {
			_ := tree.value
			tree.value == x
		}
		else {
			false
		}
	}
}

fn main() {
	mut tree := Tree[f64](Node[f64]{
		value: 1.0
		left: Empty{}
	})
	_ = tree.ok(1.0)
	tree = tree.insert(1.0)
	_ = tree.size()
	_ = "${tree.size()}"
}
'
	}, 'main.v')
	assert code == 0, output
}

fn test_generic_call_without_argument_or_receiver_inference_is_rejected() {
	code, output := run_module_storage_v2_check('generic_call_without_inference', {
		'main.v': 'module main

fn make[T]() T {
	return T(0)
}

fn outer[T](x T) {
	_ = x
	_ := make()
}

fn main() {
	outer(1)
}
'
	}, 'main.v')
	assert code != 0, output
	assert output.contains('cannot infer generic type `T`'), output
}

fn test_independent_receiver_generic_call_without_inference_is_rejected() {
	code, output := run_module_storage_v2_check('independent_receiver_generic_call_without_inference', {
		'main.v': 'module main

struct Empty {}

struct Node[T] {
	value T
}

type Tree[T] = Empty | Node[T]

fn (tree Tree[T]) insert[T](x T) Tree[T] {
	_ = x
	return tree
}

fn (tree Tree[T]) make[U]() U {
	_ = tree
	return U(0)
}

fn main() {
	mut tree := Tree[f64](Node[f64]{value: 1.0})
	tree = tree.insert(1.0)
	_ := tree.make()
}
'
	}, 'main.v')
	assert code != 0, output
	assert output.contains('cannot infer generic type `U`'), output
}

fn test_runtime_generic_args_like_index_condition_does_not_smartcast() {
	code, output := run_module_storage_v2_check('runtime_generic_args_index_no_smartcast', {
		'main.v': 'module main

struct Empty {}

struct Node[T] {
	value T
}

type Tree[T] = Empty | Node[T]

fn check(tree Tree[int], nodes []Node[int], i int) {
	match tree {
		nodes[i] {
			_ := tree.value
		}
		else {}
	}
}

fn main() {
	check(Tree[int](Node[int]{value: 1}), [Node[int]{value: 1}], 0)
}
'
	}, 'main.v')
	assert code != 0, output
	assert output.contains('value'), output
}

fn test_module_storage_legacy_assignment_without_mut_is_allowed_for_compat() {
	code, output := run_module_storage_v2_check('legacy_assignment', {
		'main.v': '__global frozen = 0

fn main() {
	frozen = 1
}
'
	}, 'main.v')
	assert code == 0, output
}

fn test_module_storage_legacy_compound_mutation_without_mut_is_allowed_for_compat() {
	code, output := run_module_storage_v2_check('legacy_compound', {
		'main.v': '__global frozen = 0

fn main() {
	frozen += 1
}
'
	}, 'main.v')
	assert code == 0, output
}

fn test_module_storage_legacy_postfix_mutation_without_mut_is_allowed_for_compat() {
	code, output := run_module_storage_v2_check('legacy_postfix', {
		'main.v': '__global frozen = 0

fn main() {
	frozen++
}
'
	}, 'main.v')
	assert code == 0, output
}

fn test_module_storage_same_short_names_in_distinct_modules_are_allowed() {
	env := check_module_storage_files({
		'a/a.v':  'module a

pub __global state = 1
'
		'b/b.v':  'module b

pub __global state = 2
'
		'main.v': 'module main

import a
import b

fn main() {
	_ = a.state + b.state
}
'
	})
	a_state := module_storage_global(env, 'a', 'state')
	b_state := module_storage_global(env, 'b', 'state')
	assert a_state.mod == 'a'
	assert b_state.mod == 'b'
}

fn test_module_storage_dunder_name_accepts_bare_access_in_declaring_module() {
	env := check_module_storage_files({
		'a/a.v':  'module a

pub __global mut state__x = 1

pub fn bump() int {
	state__x += 1
	return state__x
}
'
		'b/b.v':  'module b

pub __global mut state__x = 2

pub fn bump() int {
	state__x += 1
	return state__x
}
'
		'main.v': 'module main

import a
import b

fn main() {
	_ = a.bump() + b.bump()
	_ = a.state__x + b.state__x
}
'
	})
	a_state := module_storage_global(env, 'a', 'state__x')
	b_state := module_storage_global(env, 'b', 'state__x')
	assert a_state.mod == 'a'
	assert b_state.mod == 'b'
	assert a_state.is_mut
	assert b_state.is_mut
}

fn test_module_storage_c_global_decl_keeps_c_selector_path() {
	env := check_module_storage_files({
		'main.v': '@[c_extern]
__global C.errno int

@[c_extern]
__global C.stdin voidptr

@[c_extern]
__global C.stdout voidptr

@[c_extern]
__global C.stderr voidptr

fn main() {
	_ = C.errno
	_ = C.stdin
	_ = C.stdout
	_ = C.stderr
}
'
	})
	scope := env.get_scope('main') or { panic('missing main scope') }
	for name in ['C.errno', 'C.stdin', 'C.stdout', 'C.stderr'] {
		obj := scope.lookup_parent(name, 0) or { panic('missing ${name}') }
		assert obj is Global
		c_global := obj as Global
		assert !c_global.is_module_storage()
	}
}

fn test_module_storage_c_extern_global_attributes_keep_c_path() {
	env := check_module_storage_files({
		'main.v': '@[c_extern; export: "raw_counter"; weak; hidden; markused]
__global raw_counter int

fn main() {
	_ = raw_counter
}
'
	})
	scope := env.get_scope('main') or { panic('missing main scope') }
	obj := scope.lookup_parent('raw_counter', 0) or { panic('missing raw_counter') }
	assert obj is Global
	c_global := obj as Global
	assert !c_global.is_module_storage()
}

fn test_module_storage_example_fixture_checks() {
	code, output := run_module_storage_v2_check('example_fixture',
		module_storage_v2_fixture_sources('module_storage_example'), 'main.v')
	assert code == 0, output
}
