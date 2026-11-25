// vtest build: present_sqlite3?

import os

fn test_sql_insert_only_diagnostics_in_compile_and_check_modes() ! {
	root := os.join_path(os.vtmp_dir(), 'orm_insert_only_parity_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	fixtures := os.join_path(os.dir(@VEXE), 'vlib', 'v', 'checker', 'tests')
	mut imports := []string{}
	for fixture in ['update', 'dynamic'] {
		module_name := 'fixture_${fixture}'
		module_dir := os.join_path(root, module_name)
		os.mkdir(module_dir)!
		source := os.read_file(os.join_path(fixtures, 'orm_sql_insert_only_${fixture}.vv'))!
		os.write_file(os.join_path(module_dir, '${fixture}.v'), 'module ${module_name}\n\n' +
			source.replace_once('fn main()', 'pub fn check() !'))!
		imports << 'import ${module_name} as _'
	}
	attribute_source := os.read_file(os.join_path(fixtures, 'orm_sql_insert_only_attribute.vv'))!
	os.write_file(os.join_path(root, 'attribute.v'), attribute_source.replace_once('module main',
		'module main\n\n${imports.join('\n')}\n'))!

	normal := os.exec([@VEXE, '-nocolor', root])
	checked := os.exec([@VEXE, '-nocolor', '-check', root])
	assert normal.exit_code == 1, normal.output
	assert checked.exit_code == 1, checked.output
	assert normal.output == checked.output
	diagnostics := normal.output.split_into_lines().filter(it.contains(': error: '))
	attributes := diagnostics.filter(it.contains('attribute.v:')).join('\n')
	assert attributes.contains('can only be applied to struct fields'), normal.output
	assert attributes.contains('does not accept arguments'), normal.output
	update_error := 'error: ORM: cannot update insert-only field `InsertOnlyUser.'
	assert diagnostics.any(it.contains('update.v:') && it.contains(update_error)), normal.output
	dynamic := diagnostics.filter(it.contains('dynamic.v:')).join('\n')
	assert dynamic.count(update_error) == 2, normal.output
}

fn test_sql_insert_only_dynamic_aliases_respect_scope_and_reassignment() {
	root := os.join_path(os.vtmp_dir(), 'orm_insert_only_aliases_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	os.write_file(path, "import db.sqlite
import orm
struct InsertOnlyUser {
 id int @[primary]
 creator string @[sql_insert_only]
 name string
}

fn runtime_assignments() orm.QueryData {
 return orm.QueryData{fields: ['name'], data: [orm.Primitive('safe')]}
}
fn unrelated() {
 assignments := { creator == 'protected' }
 _ = assignments
}
fn main() {
 db := sqlite.connect(':memory:')!
 assignments := { name == 'safe' }
 sql db { dynamic update InsertOnlyUser set assignments where id == 1 }!
 mut replacement := { creator == 'protected' }
 replacement = runtime_assignments()
 sql db { dynamic update InsertOnlyUser set replacement where id == 1 }!
}
")!
	for flags in [[]string{}, ['-check']] {
		result := os.exec([@VEXE, '-nocolor', ...flags, path])
		assert result.exit_code == 0, result.output
	}
}

fn test_sql_insert_only_dynamic_grouped_assignments_are_checked() {
	root := os.join_path(os.vtmp_dir(), 'orm_insert_only_grouped_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	rejected_dir := os.join_path(root, 'rejected')
	os.mkdir(rejected_dir)!
	cases := {
		'conjunction':      "name == 'safe' && creator == 'protected'"
		'disjunction':      "(name == 'safe' || creator == 'protected')"
		'nested_condition': "if creator == 'read' { if enabled { name == 'safe' && creator == 'protected' } }"
	}
	mut calls := []string{cap: cases.len}
	for name, assignment in cases {
		os.write_file(os.join_path(rejected_dir, '${name}.v'), "import db.sqlite
fn check_${name}() ! {
 db := sqlite.connect(':memory:')!
 enabled := true
 creator := 'read'
 assignments := { ${assignment} }
 sql db { dynamic update InsertOnlyUser set assignments where id == 1 }!
}
")!
		calls << ' check_${name}()!'
	}
	os.write_file(os.join_path(rejected_dir, 'main.v'), "struct InsertOnlyUser {
 id int @[primary]
 creator string @[sql_insert_only]
 name string
}
fn main() {
${calls.join('\n')}
}
")!
	for flags in [[]string{}, ['-check']] {
		result := os.exec([@VEXE, '-nocolor', ...flags, rejected_dir])
		assert result.exit_code == 1, result.output
		diagnostics := result.output.split_into_lines().filter(it.contains('error: ORM: cannot update insert-only field `InsertOnlyUser.creator`'))
		for name, _ in cases {
			assert diagnostics.any(it.contains('${name}.v:')), '${name}, flags=${flags}: ${result.output}'
		}
	}
	path := os.join_path(root, 'main.v')
	os.write_file(path, "import db.sqlite
struct InsertOnlyUser {
 id int @[primary]
 creator string @[sql_insert_only]
 name string
 active bool
}
fn main() {
 db := sqlite.connect(':memory:')!
 enabled := true
 creator := 'read'
 assignments := {
  if creator == 'read' { active == (enabled) }
 }
 sql db { dynamic update InsertOnlyUser set assignments where id == 1 }!
}
")!
	for flags in [[]string{}, ['-check']] {
		result := os.exec([@VEXE, '-nocolor', ...flags, path])
		assert result.exit_code == 0, result.output
	}
}
