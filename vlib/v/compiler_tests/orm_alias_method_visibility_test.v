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

pub type WrappedNames = Names

pub fn make() WrappedNames {
	return WrappedNames(Names(['first']))
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
	STATEMENT
	selected := sql db { select from Account where name == 'first' }!
	assert selected.len == 1
}
"

fn sql_alias_visibility_result(name string, is_public bool, expression string, is_update bool, check_only bool) os.Result {
	dir := os.join_path(os.vtmp_dir(), 'v3_sql_alias_visibility_${name}_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(os.join_path(dir, 'aliases')) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	module_source := sql_alias_visibility_module.replace('VISIBILITY', if is_public {
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
	os.write_file(source, sql_alias_visibility_main.replace('STATEMENT', statement)) or {
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
		'(values).clone()[0].name'] {
		for is_update in [false, true] {
			result := sql_alias_visibility_result('public_${i}_${is_update}', true, expression,
				is_update, false)
			assert result.exit_code == 0, result.output
		}
	}
}

fn test_private_inherited_alias_methods_are_rejected_in_sql_values() {
	for i, expression in ['aliases.make().clone()[0].name', 'values.clone()[0].name',
		'(values).clone()[0].name'] {
		for is_update in [false, true] {
			for check_only in [false, true] {
				result := sql_alias_visibility_result('private_${i}_${is_update}_${check_only}',
					false, expression, is_update, check_only)
				assert result.exit_code != 0, result.output
				assert result.output.contains('method `aliases.WrappedNames.clone` is private'), result.output
			}
		}
	}
}
