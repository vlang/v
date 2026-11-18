// vtest build: present_sqlite3?

import os

fn test_immutable_diagnostics_in_compile_and_check_modes() ! {
	root := os.join_path(os.vtmp_dir(), 'orm_insert_only_parity_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	fixtures := os.join_path(os.dir(@VEXE), 'vlib', 'v', 'checker', 'tests')
	mut imports := []string{}
	for fixture in ['update', 'dynamic'] {
		module_name := 'fixture_${fixture}'
		module_dir := os.join_path(root, module_name)
		os.mkdir(module_dir)!
		source := os.read_file(os.join_path(fixtures, 'orm_immutable_${fixture}.vv'))!
		os.write_file(os.join_path(module_dir, '${fixture}.v'), 'module ${module_name}\n\n' +
			source.replace_once('fn main()', 'pub fn check() !'))!
		imports << 'import ${module_name} as _'
	}
	grouped_dir := os.join_path(root, 'fixture_grouped')
	os.mkdir(grouped_dir)!
	cases := {
		'conjunction':      "name == 'safe' && creator == 'protected'"
		'disjunction':      "(name == 'safe' || creator == 'protected')"
		'nested_condition': "if creator == 'read' { if enabled { name == 'safe' && creator == 'protected' } }"
	}
	mut calls := []string{cap: cases.len}
	for name, assignment in cases {
		os.write_file(os.join_path(grouped_dir, '${name}.v'), "module fixture_grouped

import db.sqlite
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
	os.write_file(os.join_path(grouped_dir, 'main.v'), "module fixture_grouped

struct InsertOnlyUser {
 id int @[primary]
 creator string @[immutable]
 name string
}
pub fn check() ! {
${calls.join('\n')}
}
")!
	imports << 'import fixture_grouped as _'
	attribute_source := os.read_file(os.join_path(fixtures, 'orm_immutable_attribute.vv'))!
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
	grouped_error := '${update_error}creator`'
	for name, _ in cases {
		assert diagnostics.any(it.contains('${name}.v:') && it.contains(grouped_error)), '${name}: ${normal.output}'
	}
}

fn test_immutable_accepts_valid_dynamic_assignments() {
	root := os.join_path(os.vtmp_dir(), 'orm_insert_only_valid_assignments_${os.getpid()}')
	grouped_dir := os.join_path(root, 'grouped_assignments')
	os.mkdir_all(grouped_dir)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	os.write_file(path, "import db.sqlite
import orm
import grouped_assignments
struct InsertOnlyUser {
 id int @[primary]
 creator string @[immutable]
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
 grouped_assignments.check()!
}
")!
	os.write_file(os.join_path(grouped_dir, 'grouped_assignments.v'), "module grouped_assignments

import db.sqlite
struct InsertOnlyUser {
 id int @[primary]
 creator string @[immutable]
 name string
 active bool
}
pub fn check() ! {
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
