import os

const sql_alias_visibility_module = "module aliases

pub struct Holder {
pub:
	name string
}

pub type Names = []string

VISIBILITYfn (values Names) clone() []Holder {
	return [Holder{name: values[0]}]
}

pub fn (mut values Names) mutate() []Holder {
	values[0] = 'first'
	return [Holder{name: values[0]}]
}

pub fn (mut values Names) inspect() []Holder {
	return [Holder{name: values[0]}]
}

TYPE_VISIBILITYtype WrappedNames = Names

pub type WrappedRefs = &&WrappedNames

pub type MoreWrappedRefs = WrappedRefs

struct NamesPointer {
	values &WrappedNames
}

pub fn make() WrappedNames {
	return WrappedNames(Names(['first']))
}

pub fn load_names(ok bool) ?WrappedNames {
	if !ok {
		return none
	}
	return make()
}

pub fn find_names(ok bool) !WrappedNames {
	if !ok {
		return error('missing names')
	}
	return make()
}

pub fn make_ptr() &WrappedNames {
	values := make()
	return &values
}

pub fn make_ptr_ptr() &&WrappedNames {
	holder := &NamesPointer{values: make_ptr()}
	return &holder.values
}

pub fn make_refs() MoreWrappedRefs {
	return MoreWrappedRefs(WrappedRefs(make_ptr_ptr()))
}

pub fn make_names_ptr() &Names {
	values := Names(['first'])
	return &values
}
"

const sql_alias_visibility_main = 'module main

import aliases
import db.sqlite

struct Account {
	id int @[primary; sql: serial]
	name string
}

EXTRA_DECLARATIONS

PROBES

fn main() {
	PROBE_CALLS
}
'

const sql_alias_visibility_probe = "PARAMETER_QUERY

fn probe_OPERATION() ! {
	mut db := sqlite.connect(':memory:')!
	sql db { create table Account }!
	row := Account{name: 'first'}
	sql db { insert row into Account }!
	values := aliases.make()
	mut mutable_values := aliases.make()
	ptr_values := aliases.make_ptr()
	ptr_ptr_values := aliases.make_ptr_ptr()
	ref_values := aliases.make_refs()
	optional_none_values := aliases.load_names(false)
	optional_some_values := aliases.load_names(true)
	mut mutable_optional_values := aliases.load_names(false)
	EXTRA_BINDINGS
	STATEMENT
	selected := sql db { select from Account where name == 'first' }!
	assert selected.len == 1
}
"

enum SqlAliasVisibilityQuery {
	selection
	update
	both
}

fn sql_alias_visibility_queries(valid bool) []SqlAliasVisibilityQuery {
	// Compile accepted operations together. Rejected operations are isolated so
	// each must emit its own diagnostic in both compiler modes.
	return if valid { [.both] } else { [.selection, .update] }
}

fn sql_alias_visibility_module_source(is_public bool, is_type_public bool) string {
	return sql_alias_visibility_module.replace('TYPE_VISIBILITY', if is_type_public {
		'pub '
	} else {
		''
	}).replace('VISIBILITY', if is_public { 'pub ' } else { '' })
}

fn sql_alias_visibility_import(source string, import_name string) string {
	if import_name == 'aliases' {
		return source
	}
	return source.replace('import aliases\n', 'import aliases as ${import_name}\n')
		.replace('aliases.make()', '${import_name}.make()')
		.replace('aliases.make_ptr()', '${import_name}.make_ptr()')
		.replace('aliases.make_ptr_ptr()', '${import_name}.make_ptr_ptr()')
		.replace('aliases.make_refs()', '${import_name}.make_refs()')
		.replace('aliases.load_names(', '${import_name}.load_names(')
		.replace('aliases.WrappedNames', '${import_name}.WrappedNames')
}

fn sql_alias_visibility_wrapped_source(expression string, query SqlAliasVisibilityQuery, import_name string, wrapper string, lock_receiver string) string {
	closure_name := if expression.contains('whole_parameter_closure_items') {
		'whole_parameter_closure_items'
	} else if expression.contains('parameter_closure_items') {
		'parameter_closure_items'
	} else if expression.contains('nested_closure_items') {
		'nested_closure_items'
	} else if expression.contains('closure_items') {
		'closure_items'
	} else {
		''
	}
	parameter_name := if expression.contains('whole_parameter_closure_items') {
		'whole_parameter_closure_items'
	} else if expression.contains('whole_parameter_items') {
		'whole_parameter_items'
	} else if expression.contains('parameter_closure_items') {
		'parameter_closure_items'
	} else if expression.contains('parameter_items') {
		'parameter_items'
	} else {
		''
	}
	operations := if query == .both {
		[SqlAliasVisibilityQuery.selection, .update]
	} else {
		[query]
	}
	mut probes := []string{cap: operations.len}
	for operation in operations {
		probes << sql_alias_visibility_probe_source(operation, expression, wrapper, lock_receiver,
			closure_name, parameter_name)
	}
	main_source := sql_alias_visibility_main.replace('PROBES', probes.join('\n'))
		.replace('PROBE_CALLS', operations.map('probe_${it}()!').join('\n\t'))
		.replace('EXTRA_DECLARATIONS', if expression.contains('shared_collection') {
			'struct SharedNamesItems {\nmut:\n\tvalues []shared aliases.WrappedNames\n}'
		} else if expression.contains('holders[') || expression.contains('holders [') {
			'struct SharedNamesHolder {\n\tvalues shared aliases.WrappedNames\n}'
		} else {
			''
		})
		.replace('EXTRA_BINDINGS', if expression.contains('shared_values') {
			'shared shared_values := aliases.make()'
		} else if parameter_name.len > 0 {
			if parameter_name.starts_with('whole_') {
				'shared ${parameter_name} := [aliases.make()]'
			} else {
				'mut ${parameter_name} := []shared aliases.WrappedNames{}\n\t${parameter_name} << aliases.make()'
			}
		} else if closure_name.len > 0 {
			'mut ${closure_name} := []shared aliases.WrappedNames{}\n\t${closure_name} << aliases.make()'
		} else if expression.contains('shared_items') {
			'mut shared_items := []shared aliases.WrappedNames{}\n\tshared_items << aliases.make()'
		} else if expression.contains('shared_collection') {
			'mut shared_collection := SharedNamesItems{}\n\tshared_collection.values << aliases.make()'
		} else if expression.contains('holders[') || expression.contains('holders [') {
			"holders := {'primary': SharedNamesHolder{values: aliases.make()}}"
		} else {
			''
		})
	return sql_alias_visibility_import(main_source, import_name)
}

fn sql_alias_visibility_probe_source(operation SqlAliasVisibilityQuery, expression string, wrapper string, lock_receiver string, closure_name string, parameter_name string) string {
	mut statement := if operation == .update {
		'sql db { update Account set name = ${expression} where id == 1 }!'
	} else {
		'found := sql db { select from Account where name == ${expression} }!\n\tassert found.len == 1'
	}
	if wrapper.len > 0 {
		statement = '${wrapper} ${lock_receiver} {\n\t\t${statement}\n\t}'
	}
	if closure_name.len > 0 {
		body := if closure_name == 'nested_closure_items' {
			'inner := fn [mut db, ${closure_name}] () ! {\n\t\t${statement}\n\t}\n\tinner()!'
		} else {
			statement
		}
		capture := if closure_name.starts_with('whole_') {
			'shared ${closure_name}'
		} else {
			closure_name
		}
		statement = 'callback := fn [mut db, ${capture}] () ! {\n\t${body}\n}\n\tcallback()!'
	}
	mut parameter_query := ''
	if parameter_name.len > 0 {
		parameter_type := if parameter_name.starts_with('whole_') {
			'shared ${parameter_name} []aliases.WrappedNames'
		} else {
			'${parameter_name} []shared aliases.WrappedNames'
		}
		argument := if parameter_name.starts_with('whole_') {
			'shared ${parameter_name}'
		} else {
			parameter_name
		}
		parameter_query = 'fn query_${operation}(mut db sqlite.DB, ${parameter_type}) ! {\n\t${statement}\n}'
		statement = 'query_${operation}(mut db, ${argument})!'
	}
	return sql_alias_visibility_probe.replace('OPERATION', operation.str())
		.replace('PARAMETER_QUERY', parameter_query)
		.replace('STATEMENT', statement)
}

struct SqlAliasCloneCase {
	expression      string
	import_name     string = 'aliases'
	method_receiver string = 'aliases.WrappedNames'
}

fn check_sql_alias_clone_cases(cases []SqlAliasCloneCase, label string, is_public bool) ! {
	query_cases := cases.map(SqlAliasQueryCase{
		expression:  it.expression
		import_name: it.import_name
		diagnostic:  if is_public {
			''
		} else {
			'method `${it.method_receiver}.clone` is private'
		}
	})
	check_sql_alias_query_batch(query_cases, '${label}_${is_public}',
		sql_alias_visibility_queries(is_public), sql_alias_compile_and_check,
		sql_alias_visibility_module_source(is_public, true))!
}

fn test_imported_alias_conversions_reject_private_types_in_sql_values() {
	cases := ['aliases', 'renamed'].map(SqlAliasQueryCase{
		expression:  '${it}.WrappedNames(values).clone()[0].name'
		import_name: it
		diagnostic:  'type `aliases.WrappedNames` is private'
	})
	for is_method_public in [false, true] {
		check_sql_alias_query_batch(cases, 'conversion_${is_method_public}_false',
			sql_alias_visibility_queries(false), sql_alias_compile_and_check,
			sql_alias_visibility_module_source(is_method_public, false))!
	}
}

fn sql_alias_pointer_clone_cases() []SqlAliasCloneCase {
	mut cases := ['aliases.make_ptr().clone()[0].name', 'ptr_values.clone()[0].name',
		'(ptr_values).clone()[0].name'].map(SqlAliasCloneCase{
		expression:      it
		method_receiver: '&aliases.WrappedNames'
	})
	cases << SqlAliasCloneCase{
		expression:      'aliases.make_names_ptr().clone()[0].name'
		method_receiver: '&aliases.Names'
	}
	cases << SqlAliasCloneCase{
		expression:      'renamed.make_ptr().clone()[0].name'
		import_name:     'renamed'
		method_receiver: '&aliases.WrappedNames'
	}

	cases << ['aliases.make_ptr_ptr().clone()[0].name', 'ptr_ptr_values.clone()[0].name',
		'(ptr_ptr_values).clone()[0].name'].map(SqlAliasCloneCase{
		expression:      it
		method_receiver: '&&aliases.WrappedNames'
	})
	cases << SqlAliasCloneCase{
		expression:      'renamed.make_ptr_ptr().clone()[0].name'
		import_name:     'renamed'
		method_receiver: '&&aliases.WrappedNames'
	}

	cases << ['aliases.make_refs().clone()[0].name', 'ref_values.clone()[0].name',
		'(ref_values).clone()[0].name'].map(SqlAliasCloneCase{
		expression:      it
		method_receiver: 'aliases.MoreWrappedRefs'
	})
	cases << SqlAliasCloneCase{
		expression:      'renamed.make_refs().clone()[0].name'
		import_name:     'renamed'
		method_receiver: 'aliases.MoreWrappedRefs'
	}
	return cases
}

fn sql_alias_mutability_cases(expressions []string, valid bool) []SqlAliasQueryCase {
	mut cases := []SqlAliasQueryCase{cap: expressions.len}
	for expression in expressions {
		import_name := if expression.contains('renamed.') { 'renamed' } else { 'aliases' }
		diagnostic := if valid {
			''
		} else if expression.starts_with('values.') {
			'`values` is immutable'
		} else {
			'cannot pass expression as `mut`'
		}
		cases << SqlAliasQueryCase{
			expression:  expression
			import_name: import_name
			diagnostic:  diagnostic
		}
	}
	return cases
}

struct SqlAliasQueryCase {
	expression    string
	import_name   string = 'aliases'
	lock_receiver string
	wrapper       string
	diagnostic    string
}

const sql_alias_compile_and_check = [[]string{}, ['-check']]
const sql_alias_public_module = sql_alias_visibility_module_source(true, true)

fn check_sql_alias_query_batch(cases []SqlAliasQueryCase, label string, queries []SqlAliasVisibilityQuery, flag_sets [][]string, module_source string) ! {
	dir := os.join_path(os.vtmp_dir(), 'v3_sql_alias_query_${label}_${os.getpid()}')
	os.mkdir_all(os.join_path(dir, 'aliases'))!
	defer { os.rmdir_all(dir) or {} }
	os.write_file(os.join_path(dir, 'aliases', 'aliases.v'), module_source)!
	mut imports := []string{}
	mut calls := []string{}
	mut expected := map[string]string{}
	for i, case in cases {
		for query in queries {
			name := 'fixture_${i}_${query}'
			fixture_dir := os.join_path(dir, name)
			os.mkdir(fixture_dir)!
			program := sql_alias_visibility_wrapped_source(case.expression, query, case.import_name,
				case.wrapper, case.lock_receiver)
				.replace_once('module main', 'module ${name}')
				.replace_once('fn main() {', 'pub fn exercise() ! {')
			os.write_file(os.join_path(fixture_dir, '${name}.v'), program)!
			imports << 'import ${name}'
			calls << '${name}.exercise()!'
			if case.diagnostic != '' {
				expected['${name}.v:'] = case.diagnostic
			}
		}
	}
	assert expected.len == 0 || expected.len == calls.len
	os.write_file(os.join_path(dir, 'main.v'), 'module main\n\n${imports.join('\n')}\n\nfn main() {\n\t${calls.join('\n\t')}\n}\n')!
	executable := os.join_path(dir, 'main.exe')
	mut diagnostic_flags := ['-nocolor']
	// Larger batches need every error past the compiler's default limit.
	if expected.len > 20 {
		diagnostic_flags << ['-message-limit', '1000']
	}
	for flags in flag_sets {
		result := os.exec([@VEXE, ...diagnostic_flags, ...flags, '-o', executable, dir])
		if expected.len == 0 {
			assert result.exit_code == 0, result.output
			if flags.len == 0 {
				run := os.exec([executable])
				assert run.exit_code == 0, run.output
			}
			continue
		}
		assert result.exit_code != 0, result.output
		diagnostics := result.output.split_into_lines().filter(it.contains(': error: '))
		for location, diagnostic in expected {
			assert diagnostics.any(it.contains(location) && it.contains(diagnostic)), '${location}, flags=${flags}: ${result.output}'
		}
	}
}

fn sql_alias_lock_cases(lock_receivers []string, imports_by_method map[string]string, lock_whole_array bool) []SqlAliasQueryCase {
	mut cases := []SqlAliasQueryCase{}
	for method, import_name in imports_by_method {
		for lock_receiver in lock_receivers {
			receiver := if lock_whole_array { '${lock_receiver}[0]' } else { lock_receiver }
			for wrapper in ['', 'rlock', 'lock'] {
				diagnostic := if wrapper == 'lock' || (method == 'clone' && wrapper == 'rlock') {
					''
				} else if method == 'clone' {
					'must be `rlock`ed or `lock`ed'
				} else if wrapper == 'rlock' {
					'has an `rlock` but needs a `lock`'
				} else {
					'is `shared` and must be `lock`ed'
				}
				cases << SqlAliasQueryCase{
					expression:    '${receiver}.${method}()[0].name'
					import_name:   import_name
					lock_receiver: lock_receiver
					wrapper:       wrapper
					diagnostic:    diagnostic
				}
			}
		}
	}
	return cases
}

fn sql_alias_shared_element_and_captured_array_lock_cases() []SqlAliasQueryCase {
	receivers := ['shared_items[0]', '(shared_collection.values)[0]', 'parameter_items[0]',
		"holders['primary'].values", "(holders [ 'primary' ]).values", 'closure_items[0]',
		'parameter_closure_items[0]', 'nested_closure_items[0]']
	return sql_alias_lock_cases(receivers, {
		'mutate': 'aliases'
		'clone':  'renamed'
	}, false)
}

fn sql_alias_mutability_and_lock_cases() []SqlAliasQueryCase {
	// Since #29221 a `mut` receiver method needs mutable storage even when it is declared in
	// another module and only changes private state (`inspect`).
	mut cases := sql_alias_mutability_cases(['values.mutate()[0].name', '(values).mutate()[0].name',
		'aliases.make().mutate()[0].name', 'aliases.WrappedNames(values).mutate()[0].name',
		'renamed.MoreWrappedRefs(ref_values).mutate()[0].name',
		'(aliases.load_names(true) or { values }).mutate()[0].name',
		'(renamed.find_names(false) or { values }).mutate()[0].name',
		'(optional_none_values or { values }).mutate()[0].name', 'values.inspect()[0].name',
		'renamed.make().inspect()[0].name'], false)
	cases << sql_alias_mutability_cases(['mutable_values.mutate()[0].name',
		'ptr_values.mutate()[0].name', 'aliases.make_ptr().mutate()[0].name',
		'renamed.make_ptr_ptr().mutate()[0].name', 'aliases.make_refs().mutate()[0].name',
		'(mutable_optional_values or { values }).mutate()[0].name'], true)
	cases << sql_alias_lock_cases(['shared_values'], {
		'mutate':  'aliases'
		'inspect': 'renamed'
		'clone':   'aliases'
	}, false)
	cases << sql_alias_lock_cases(['whole_parameter_items', 'whole_parameter_closure_items'], {
		'mutate': 'aliases'
		'clone':  'aliases'
	}, true)
	return cases
}

fn test_mut_alias_methods_require_mutable_storage_and_locks_in_sql_values() {
	mut cases := sql_alias_shared_element_and_captured_array_lock_cases()
	check_sql_alias_query_batch(cases.filter(it.diagnostic == ''),
		'shared_element_and_captured_array_locks_accepted', [.both], sql_alias_compile_and_check,
		sql_alias_public_module)!
	storage_cases := sql_alias_mutability_and_lock_cases()
	check_sql_alias_query_batch(storage_cases.filter(it.diagnostic == ''), 'mutability_and_locks_accepted',
		[.both], sql_alias_compile_and_check, sql_alias_public_module)!
	cases << storage_cases
	check_sql_alias_query_batch(cases.filter(it.diagnostic != ''), 'storage_and_locks_rejected',
		sql_alias_visibility_queries(false), sql_alias_compile_and_check, sql_alias_public_module)!
}

fn sql_alias_inherited_clone_cases() []SqlAliasCloneCase {
	mut cases := ['aliases.make().clone()[0].name', 'values.clone()[0].name', '(values).clone()[0].name',
		'aliases.WrappedNames(values).clone()[0].name'].map(SqlAliasCloneCase{
		expression: it
	})
	cases << SqlAliasCloneCase{
		expression:  'renamed.WrappedNames(values).clone()[0].name'
		import_name: 'renamed'
	}
	for import_name in ['aliases', 'renamed'] {
		for expression in [
			'(${import_name}.load_names(true) or { values }).clone()[0].name',
			'(${import_name}.load_names(false) or { ${import_name}.make() }).clone()[0].name',
			'(${import_name}.find_names(true) or { values }).clone()[0].name',
			'(${import_name}.find_names(false) or { ${import_name}.make() }).clone()[0].name',
			'(optional_none_values or { values }).clone()[0].name',
			'(optional_some_values or { values }).clone()[0].name',
		] {
			cases << SqlAliasCloneCase{
				expression:  expression
				import_name: import_name
			}
		}
	}
	return cases
}

fn test_alias_clone_methods_keep_resolution_and_visibility_in_sql_values() {
	mut cases := sql_alias_pointer_clone_cases()
	check_sql_alias_clone_cases(cases, 'pointer_clone', true)!
	inherited := sql_alias_inherited_clone_cases()
	check_sql_alias_clone_cases(inherited, 'inherited_clone', true)!
	cases << inherited
	check_sql_alias_clone_cases(cases, 'private_clone', false)!
}

fn test_alias_methods_keep_declared_sql_result_types() {
	mut cases := ['aliases', 'renamed'].map(SqlAliasQueryCase{
		expression:  '${it}.make_ptr_ptr().clone()[0]'
		import_name: it
		diagnostic:  'this expression has type `aliases.Holder`'
	})
	for import_name in ['aliases', 'renamed'] {
		for expression in [
			'(${import_name}.load_names(false) or { values }).clone()[0]',
			'(${import_name}.find_names(false) or { values }).clone()[0]',
			'(optional_none_values or { values }).clone()[0]',
			'(optional_some_values or { values }).clone()[0]',
		] {
			cases << SqlAliasQueryCase{
				expression:  expression
				import_name: import_name
				diagnostic:  'this expression has type `aliases.Holder`'
			}
		}
	}
	check_sql_alias_query_batch(cases, 'declared_result_types', [.selection], [['-check']],
		sql_alias_public_module)!
}
