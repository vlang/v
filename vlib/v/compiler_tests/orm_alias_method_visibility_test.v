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

const sql_alias_visibility_main = "module main

import aliases
import db.sqlite

struct Account {
	id int @[primary; sql: serial]
	name string
}

EXTRA_DECLARATIONS

PARAMETER_QUERY

fn main() {
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

fn sql_alias_visibility_result(name string, is_public bool, expression string, is_update bool, check_only bool, is_type_public bool, import_name string) os.Result {
	return sql_alias_visibility_wrapped_result(name, is_public, expression, is_update,
		check_only, is_type_public, import_name, '', '')
}

fn sql_alias_visibility_wrapped_result(name string, is_public bool, expression string, is_update bool, check_only bool, is_type_public bool, import_name string, wrapper string, lock_receiver string) os.Result {
	dir := os.join_path(os.vtmp_dir(), 'v3_sql_alias_visibility_${name}_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(os.join_path(dir, 'aliases')) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	module_source := sql_alias_visibility_module.replace('TYPE_VISIBILITY', if is_type_public {
		'pub '
	} else {
		''
	}).replace('VISIBILITY', if is_public {
		'pub '
	} else {
		''
	})
	os.write_file(os.join_path(dir, 'aliases', 'aliases.v'), module_source) or { panic(err) }
	mut statement := if is_update {
		'sql db { update Account set name = ${expression} where id == 1 }!'
	} else {
		'found := sql db { select from Account where name == ${expression} }!\n\tassert found.len == 1'
	}
	if wrapper.len > 0 {
		statement = '${wrapper} ${lock_receiver} {\n\t\t${statement}\n\t}'
	}
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
	parameter_name := if expression.contains('whole_parameter_closure_items') {
		'whole_parameter_closure_items'
	} else if expression.contains('whole_parameter_items') {
		'whole_parameter_items'
	} else if expression.contains('parameter_closure_items') {
		'parameter_closure_items'
	} else if expression.contains('parameter_shadow_items') {
		'parameter_shadow_items'
	} else if expression.contains('parameter_items') {
		'parameter_items'
	} else {
		''
	}
	mut parameter_query := ''
	if parameter_name.len > 0 {
		body := if parameter_name == 'parameter_shadow_items' {
			'unsafe {\n\t\tunsafe {\n\t\t\tmut parameter_shadow_items := [aliases.make()]\n\t\t\t${statement}\n\t\t}\n\t}'
		} else {
			statement
		}
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
		parameter_query = 'fn query(mut db sqlite.DB, ${parameter_type}) ! {\n\t${body}\n}'
		statement = 'query(mut db, ${argument})!'
	}
	source := os.join_path(dir, 'main.v')
	main_source := sql_alias_visibility_main.replace('STATEMENT', statement)
		.replace('PARAMETER_QUERY', parameter_query)
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
		.replace('import aliases\n', if import_name == 'aliases' {
			'import aliases\n'
		} else {
			'import aliases as ${import_name}\n'
		})
		.replace('aliases.make()', '${import_name}.make()')
		.replace('aliases.make_ptr()', '${import_name}.make_ptr()')
		.replace('aliases.make_ptr_ptr()', '${import_name}.make_ptr_ptr()')
		.replace('aliases.make_refs()', '${import_name}.make_refs()')
		.replace('aliases.load_names(', '${import_name}.load_names(')
		.replace('aliases.WrappedNames', '${import_name}.WrappedNames')
	os.write_file(source, main_source) or {
		panic(err)
	}
	executable := os.join_path(dir, 'main.exe')
	flags := if check_only { '-check' } else { '' }
	result := os.exec([@VEXE, '-new-compiler', ...(os.split_args(flags) or { panic(err) }), '-o',
		executable, source])
	if result.exit_code != 0 || check_only {
		return result
	}
	return os.exec([executable])
}

fn test_public_inherited_alias_methods_work_in_imported_sql_values() {
	for i, expression in ['aliases.make().clone()[0].name', 'values.clone()[0].name',
		'(values).clone()[0].name', 'aliases.WrappedNames(values).clone()[0].name'] {
		for is_update in [false, true] {
			result := sql_alias_visibility_result('public_${i}_${is_update}', true, expression,
				is_update, false, true, 'aliases')
			assert result.exit_code == 0, result.output
		}
	}
}

fn test_private_inherited_alias_methods_are_rejected_in_sql_values() {
	for i, expression in ['aliases.make().clone()[0].name', 'values.clone()[0].name',
		'(values).clone()[0].name', 'aliases.WrappedNames(values).clone()[0].name'] {
		for is_update in [false, true] {
			for check_only in [false, true] {
				result := sql_alias_visibility_result('private_${i}_${is_update}_${check_only}',
					false, expression, is_update, check_only, true, 'aliases')
				assert result.exit_code != 0, result.output
				assert result.output.contains('method `aliases.WrappedNames.clone` is private'), result.output
			}
		}
	}
}

fn test_imported_alias_conversions_obey_type_and_method_visibility_in_sql_values() {
	for import_name in ['aliases', 'renamed'] {
		expression := '${import_name}.WrappedNames(values).clone()[0].name'
		for is_update in [false, true] {
			for check_only in [false, true] {
				for is_method_public in [false, true] {
					for is_type_public in [false, true] {
						result := sql_alias_visibility_result('conversion_${import_name}_${is_update}_${check_only}_${is_method_public}_${is_type_public}',
							is_method_public, expression, is_update, check_only, is_type_public,
							import_name)
						if !is_type_public {
							assert result.exit_code != 0, result.output
							assert result.output.contains('type `aliases.WrappedNames` is private'), result.output
						} else if !is_method_public {
							assert result.exit_code != 0, result.output
							assert result.output.contains('method `aliases.WrappedNames.clone` is private'), result.output
						} else {
							assert result.exit_code == 0, result.output
						}
					}
				}
			}
		}
	}
}

fn test_pointer_alias_methods_keep_alias_resolution_in_sql_values() {
	for i, expression in ['aliases.make_ptr().clone()[0].name', 'ptr_values.clone()[0].name',
		'(ptr_values).clone()[0].name', 'aliases.make_names_ptr().clone()[0].name',
		'renamed.make_ptr().clone()[0].name'] {
		import_name := if expression.starts_with('renamed.') { 'renamed' } else { 'aliases' }
		method_receiver := if i == 3 { 'Names' } else { 'WrappedNames' }
		for is_public in [true, false] {
			for is_update in [false, true] {
				for check_only in [false, true] {
					result := sql_alias_visibility_result('pointer_${i}_${is_public}_${is_update}_${check_only}',
						is_public, expression, is_update, check_only, true, import_name)
					if is_public {
						assert result.exit_code == 0, result.output
					} else {
						assert result.exit_code != 0, result.output
						assert result.output.contains('method `&aliases.${method_receiver}.clone` is private'), result.output
					}
				}
			}
		}
	}
}

fn test_multiple_pointer_alias_methods_keep_resolution_and_visibility_in_sql_values() {
	for i, expression in ['aliases.make_ptr_ptr().clone()[0].name', 'ptr_ptr_values.clone()[0].name',
		'(ptr_ptr_values).clone()[0].name', 'renamed.make_ptr_ptr().clone()[0].name'] {
		import_name := if expression.starts_with('renamed.') { 'renamed' } else { 'aliases' }
		for is_public in [true, false] {
			for is_update in [false, true] {
				for check_only in [false, true] {
					result := sql_alias_visibility_result('multiple_pointer_${i}_${is_public}_${is_update}_${check_only}',
						is_public, expression, is_update, check_only, true, import_name)
					if is_public {
						assert result.exit_code == 0, result.output
					} else {
						assert result.exit_code != 0, result.output
						assert result.output.contains('method `&&aliases.WrappedNames.clone` is private'), result.output
					}
				}
			}
		}
	}
}

fn test_multiple_pointer_alias_methods_keep_declared_sql_result_types() {
	for import_name in ['aliases', 'renamed'] {
		expression := '${import_name}.make_ptr_ptr().clone()[0]'
		result := sql_alias_visibility_result('multiple_pointer_result_${import_name}',
			true, expression, false, true, true, import_name)
		assert result.exit_code != 0, result.output
		assert result.output.contains('this expression has type `aliases.Holder`'), result.output
	}
}

fn test_hidden_pointer_alias_methods_keep_resolution_and_visibility_in_sql_values() {
	for i, expression in ['aliases.make_refs().clone()[0].name', 'ref_values.clone()[0].name',
		'(ref_values).clone()[0].name', 'renamed.make_refs().clone()[0].name'] {
		import_name := if expression.starts_with('renamed.') { 'renamed' } else { 'aliases' }
		for is_public in [true, false] {
			for is_update in [false, true] {
				for check_only in [false, true] {
					result := sql_alias_visibility_result('hidden_pointer_${i}_${is_public}_${is_update}_${check_only}',
						is_public, expression, is_update, check_only, true, import_name)
					if is_public {
						assert result.exit_code == 0, result.output
					} else {
						assert result.exit_code != 0, result.output
						assert result.output.contains('method `aliases.MoreWrappedRefs.clone` is private'), result.output
					}
				}
			}
		}
	}
}

fn test_mut_alias_methods_require_mutable_storage_in_sql_values() {
	for i, expression in ['values.mutate()[0].name', '(values).mutate()[0].name',
		'aliases.make().mutate()[0].name', 'aliases.WrappedNames(values).mutate()[0].name',
		'renamed.MoreWrappedRefs(ref_values).mutate()[0].name',
		'(aliases.load_names(true) or { values }).mutate()[0].name',
		'(renamed.find_names(false) or { values }).mutate()[0].name',
		'(optional_none_values or { values }).mutate()[0].name'] {
		import_name := if expression.contains('renamed.') { 'renamed' } else { 'aliases' }
		for is_update in [false, true] {
			for check_only in [false, true] {
				result := sql_alias_visibility_result('immutable_${i}_${is_update}_${check_only}',
					true, expression, is_update, check_only, true, import_name)
				assert result.exit_code != 0, result.output
				if i == 0 {
					assert result.output.contains('`values` is immutable'), result.output
				} else {
					assert result.output.contains('cannot pass expression as `mut`'), result.output
				}
			}
		}
	}
}

fn test_mut_alias_methods_keep_ordinary_storage_exceptions_in_sql_values() {
	for i, expression in ['mutable_values.mutate()[0].name', 'ptr_values.mutate()[0].name',
		'aliases.make_ptr().mutate()[0].name', 'renamed.make_ptr_ptr().mutate()[0].name',
		'aliases.make_refs().mutate()[0].name',
		'(mutable_optional_values or { values }).mutate()[0].name', 'values.inspect()[0].name',
		'renamed.make().inspect()[0].name'] {
		import_name := if expression.contains('renamed.') { 'renamed' } else { 'aliases' }
		for is_update in [false, true] {
			for check_only in [false, true] {
				result := sql_alias_visibility_result('mutable_${i}_${is_update}_${check_only}',
					true, expression, is_update, check_only, true, import_name)
				assert result.exit_code == 0, result.output
			}
		}
	}
}

fn test_alias_methods_require_ordinary_shared_locks_in_sql_values() {
	for method in ['mutate', 'inspect', 'clone'] {
		import_name := if method == 'inspect' { 'renamed' } else { 'aliases' }
		for wrapper in ['', 'rlock', 'lock'] {
			for is_update in [false, true] {
				for check_only in [false, true] {
					result := sql_alias_visibility_wrapped_result('shared_${method}_${wrapper}_${is_update}_${check_only}',
						true, 'shared_values.${method}()[0].name', is_update, check_only,
						true, import_name, wrapper, 'shared_values')
					if wrapper == 'lock' || (method == 'clone' && wrapper == 'rlock') {
						assert result.exit_code == 0, result.output
					} else {
						assert result.exit_code != 0, result.output
						if method == 'clone' {
							assert result.output.contains('must be `rlock`ed or `lock`ed'), result.output
						} else if wrapper == 'rlock' {
							assert result.output.contains('has an `rlock` but needs a `lock`'), result.output
						} else {
							assert result.output.contains('is `shared` and must be `lock`ed'), result.output
						}
					}
				}
			}
		}
	}
}

fn test_alias_methods_keep_shared_element_and_quoted_field_lock_keys_in_sql_values() {
	for i, receiver in ['shared_items[0]', '(shared_collection.values)[0]', 'parameter_items[0]',
		"holders['primary'].values", "(holders [ 'primary' ]).values"] {
		for method in ['mutate', 'clone'] {
			import_name := if method == 'clone' { 'renamed' } else { 'aliases' }
			for wrapper in ['', 'rlock', 'lock'] {
				for is_update in [false, true] {
					for check_only in [false, true] {
						result := sql_alias_visibility_wrapped_result('shared_element_${i}_${method}_${wrapper}_${is_update}_${check_only}',
							true, '${receiver}.${method}()[0].name', is_update, check_only,
							true, import_name, wrapper, receiver)
						if wrapper == 'lock' || (method == 'clone' && wrapper == 'rlock') {
							assert result.exit_code == 0, result.output
						} else {
							assert result.exit_code != 0, result.output
							if method == 'clone' {
								assert result.output.contains('must be `rlock`ed or `lock`ed'), result.output
							} else if wrapper == 'rlock' {
								assert result.output.contains('has an `rlock` but needs a `lock`'), result.output
							} else {
								assert result.output.contains('is `shared` and must be `lock`ed'), result.output
							}
						}
					}
				}
			}
		}
	}
}

fn test_alias_methods_keep_captured_shared_array_locks_in_sql_values() {
	for i, receiver in ['closure_items[0]', 'parameter_closure_items[0]', 'nested_closure_items[0]'] {
		for method in ['mutate', 'clone'] {
			import_name := if method == 'clone' { 'renamed' } else { 'aliases' }
			for wrapper in ['', 'rlock', 'lock'] {
				for is_update in [false, true] {
					for check_only in [false, true] {
						result := sql_alias_visibility_wrapped_result('shared_capture_${i}_${method}_${wrapper}_${is_update}_${check_only}',
							true, '${receiver}.${method}()[0].name', is_update, check_only,
							true, import_name, wrapper, receiver)
						if wrapper == 'lock' || (method == 'clone' && wrapper == 'rlock') {
							assert result.exit_code == 0, result.output
						} else {
							assert result.exit_code != 0, result.output
							if method == 'clone' {
								assert result.output.contains('must be `rlock`ed or `lock`ed'), result.output
							} else if wrapper == 'rlock' {
								assert result.output.contains('has an `rlock` but needs a `lock`'), result.output
							} else {
								assert result.output.contains('is `shared` and must be `lock`ed'), result.output
							}
						}
					}
				}
			}
		}
	}
}

fn test_alias_methods_keep_whole_shared_array_locks_in_sql_values() {
	for i, name in ['whole_parameter_items', 'whole_parameter_closure_items'] {
		for method in ['mutate', 'clone'] {
			for wrapper in ['', 'rlock', 'lock'] {
				for is_update in [false, true] {
					for check_only in [false, true] {
						result := sql_alias_visibility_wrapped_result('shared_whole_${i}_${method}_${wrapper}_${is_update}_${check_only}',
							true, '${name}[0].${method}()[0].name', is_update, check_only,
							true, 'aliases', wrapper, name)
						if wrapper == 'lock' || (method == 'clone' && wrapper == 'rlock') {
							assert result.exit_code == 0, result.output
						} else {
							assert result.exit_code != 0, result.output
							if method == 'clone' {
								assert result.output.contains('must be `rlock`ed or `lock`ed'), result.output
							} else if wrapper == 'rlock' {
								assert result.output.contains('has an `rlock` but needs a `lock`'), result.output
							} else {
								assert result.output.contains('is `shared` and must be `lock`ed'), result.output
							}
						}
					}
				}
			}
		}
	}
}

fn test_alias_methods_respect_shadowed_shared_array_parameters_in_sql_values() {
	for method in ['mutate', 'clone'] {
		for is_update in [false, true] {
			for check_only in [false, true] {
				result := sql_alias_visibility_result('shared_parameter_shadow_${method}_${is_update}_${check_only}',
					true, 'parameter_shadow_items[0].${method}()[0].name', is_update,
					check_only, true, 'aliases')
				assert result.exit_code == 0, result.output
			}
		}
	}
}

fn test_option_result_alias_fallback_methods_keep_resolution_and_visibility_in_sql_values() {
	for import_name in ['aliases', 'renamed'] {
		for i, expression in [
			'(${import_name}.load_names(true) or { values }).clone()[0].name',
			'(${import_name}.load_names(false) or { ${import_name}.make() }).clone()[0].name',
			'(${import_name}.find_names(true) or { values }).clone()[0].name',
			'(${import_name}.find_names(false) or { ${import_name}.make() }).clone()[0].name',
			'(optional_none_values or { values }).clone()[0].name',
			'(optional_some_values or { values }).clone()[0].name',
		] {
			for is_public in [true, false] {
				for is_update in [false, true] {
					for check_only in [false, true] {
						result := sql_alias_visibility_result('fallback_${import_name}_${i}_${is_public}_${is_update}_${check_only}',
							is_public, expression, is_update, check_only, true, import_name)
						if is_public {
							assert result.exit_code == 0, result.output
						} else {
							assert result.exit_code != 0, result.output
							assert result.output.contains('method `aliases.WrappedNames.clone` is private'), result.output
						}
					}
				}
			}
		}
	}
}

fn test_option_result_alias_fallback_methods_keep_declared_sql_result_types() {
	for import_name in ['aliases', 'renamed'] {
		for i, expression in [
			'(${import_name}.load_names(false) or { values }).clone()[0]',
			'(${import_name}.find_names(false) or { values }).clone()[0]',
			'(optional_none_values or { values }).clone()[0]',
			'(optional_some_values or { values }).clone()[0]',
		] {
			result := sql_alias_visibility_result('fallback_result_${import_name}_${i}',
				true, expression, false, true, true, import_name)
			assert result.exit_code != 0, result.output
			assert result.output.contains('this expression has type `aliases.Holder`'), result.output
		}
	}
}
