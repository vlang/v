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

TYPE_VISIBILITYtype WrappedNames = Names

pub fn make() WrappedNames {
	return WrappedNames(Names(['first']))
}

pub fn make_ptr() &WrappedNames {
	values := make()
	return &values
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

fn main() {
	mut db := sqlite.connect(':memory:')!
	sql db { create table Account }!
	row := Account{name: 'first'}
	sql db { insert row into Account }!
	values := aliases.make()
	ptr_values := aliases.make_ptr()
	STATEMENT
	selected := sql db { select from Account where name == 'first' }!
	assert selected.len == 1
}
"

fn sql_alias_visibility_result(name string, is_public bool, expression string, is_update bool, check_only bool, is_type_public bool, import_name string) os.Result {
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
	statement := if is_update {
		'sql db { update Account set name = ${expression} where id == 1 }!'
	} else {
		'found := sql db { select from Account where name == ${expression} }!\n\tassert found.len == 1'
	}
	source := os.join_path(dir, 'main.v')
	main_source := sql_alias_visibility_main.replace('STATEMENT', statement)
		.replace('import aliases\n', if import_name == 'aliases' {
			'import aliases\n'
		} else {
			'import aliases as ${import_name}\n'
		})
		.replace('aliases.make()', '${import_name}.make()')
		.replace('aliases.make_ptr()', '${import_name}.make_ptr()')
	os.write_file(source, main_source) or {
		panic(err)
	}
	executable := os.join_path(dir, 'main.exe')
	flags := if check_only { '-check' } else { '' }
	result := os.execute('${os.quoted_path(@VEXE)} -new-compiler ${flags} -o ${os.quoted_path(executable)} ${os.quoted_path(source)}')
	if result.exit_code != 0 || check_only {
		return result
	}
	return os.execute(os.quoted_path(executable))
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
