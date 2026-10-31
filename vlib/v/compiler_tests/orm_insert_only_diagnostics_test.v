// vtest build: present_sqlite3?

import os

fn test_immutable_diagnostics_in_compile_and_check_modes() {
	for fixture in ['attribute', 'update', 'dynamic'] {
		path := os.join_path(os.dir(@VEXE), 'vlib', 'v', 'checker', 'tests',
			'orm_immutable_${fixture}.vv')
		normal := os.exec([@VEXE, '-new-compiler', '-nocolor', path])
		checked := os.exec([@VEXE, '-new-compiler', '-nocolor', '-check', path])
		assert normal.exit_code == 1, normal.output
		assert checked.exit_code == 1, checked.output
		assert normal.output == checked.output
		if fixture == 'attribute' {
			assert normal.output.contains('can only be applied to struct fields')
			assert normal.output.contains('does not accept arguments')
		} else {
			assert normal.output.contains('cannot update insert-only field `InsertOnlyUser.')
		}
		if fixture == 'dynamic' {
			assert normal.output.count('error: ORM: cannot update') == 2
		}
	}
}

fn test_immutable_dynamic_aliases_respect_scope_and_reassignment() {
	root := os.join_path(os.vtmp_dir(), 'orm_insert_only_aliases_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	os.write_file(path, "import db.sqlite
import orm
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
}
")!
	for flags in [[]string{}, ['-check']] {
		result := os.exec([@VEXE, '-new-compiler', '-nocolor', ...flags, path])
		assert result.exit_code == 0, result.output
	}
}

fn test_immutable_dynamic_grouped_assignments_are_checked() {
	root := os.join_path(os.vtmp_dir(), 'orm_insert_only_grouped_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	for assignment in [
		"name == 'safe' && creator == 'protected'",
		"(name == 'safe' || creator == 'protected')",
		"if creator == 'read' { if enabled { name == 'safe' && creator == 'protected' } }",
	] {
		os.write_file(path, "import db.sqlite
struct InsertOnlyUser {
 id int @[primary]
 creator string @[immutable]
 name string
}
fn main() {
 db := sqlite.connect(':memory:')!
 enabled := true
 creator := 'read'
 assignments := { ${assignment} }
 sql db { dynamic update InsertOnlyUser set assignments where id == 1 }!
}
")!
		for flags in [[]string{}, ['-check']] {
			result := os.exec([@VEXE, '-new-compiler', '-nocolor', ...flags, path])
			assert result.exit_code == 1, result.output
			assert result.output.contains('cannot update insert-only field `InsertOnlyUser.creator`'), result.output
		}
	}
	os.write_file(path, "import db.sqlite
struct InsertOnlyUser {
 id int @[primary]
 creator string @[immutable]
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
		result := os.exec([@VEXE, '-new-compiler', '-nocolor', ...flags, path])
		assert result.exit_code == 0, result.output
	}
}
