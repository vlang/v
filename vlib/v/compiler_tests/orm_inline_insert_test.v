import os

fn test_orm_inline_insert_requires_a_variable() {
	root := os.join_path(os.vtmp_dir(), 'orm_inline_insert_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, "import db.sqlite
struct Product {
	id int @[primary; sql: serial]
	name string
}
fn main() {
	mut db := sqlite.connect(':memory:')!
	sql db { create table Product }!
	sql db { insert Product{name: 'b'} into Product }!
}
")!
	for mode in ['', '-no-parallel'] {
		for check_mode in ['-check', '-o'] {
			mut args := [@VEXE, '-new-compiler', '-nocache']
			if mode != '' {
				args << mode
			}
			args << check_mode
			if check_mode == '-o' {
				args << os.join_path(root, 'main.c')
			}
			args << source
			result := os.exec(args)
			assert result.exit_code != 0, result.output
			assert result.output.contains('ORM insert requires a variable'), result.output
		}
	}
	variable_source := os.read_file(source)!
	os.write_file(source, variable_source.replace("sql db { insert Product{name: 'b'} into Product }!", "product := Product{name: 'b'}\n\tsql db { insert product into Product }!\n\trows := sql db { select from Product }!\n\tassert rows.len == 1\n\tassert rows[0].name == 'b'"))!
	result := os.exec([@VEXE, '-new-compiler', '-nocache', '-gc', 'none', 'run', source])
	assert result.exit_code == 0, result.output
}

fn test_orm_inline_insert_with_a_module_named_into_requires_a_variable() {
	root := os.join_path(os.vtmp_dir(), 'orm_inline_imported_insert_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'into'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'orm_inline_imported_insert' }\n")!
	os.write_file(os.join_path(root, 'into', 'product.v'), 'module into
pub struct Product {
 pub:
 id int @[primary; sql: serial]
 name string
}
')!
	source := os.join_path(root, 'main.v')
	os.write_file(source, "import db.sqlite
import into
fn main() {
 mut db := sqlite.connect(':memory:')!
 sql db { create table into.Product }!
 sql db { insert into.Product{name: 'b'} into into.Product }!
}
")!
	for mode in ['', '-no-parallel'] {
		for check_mode in ['-check', '-o'] {
			mut args := [@VEXE, '-new-compiler', '-nocache']
			if mode != '' {
				args << mode
			}
			args << check_mode
			if check_mode == '-o' {
				args << os.join_path(root, 'main.c')
			}
			args << source
			result := os.exec(args)
			assert result.exit_code != 0, result.output
			assert result.output.contains('ORM insert requires a variable'), result.output
		}
	}
	variable_source := os.read_file(source)!
	os.write_file(source, variable_source.replace("sql db { insert into.Product{name: 'b'} into into.Product }!", "product := into.Product{name: 'b'}\n sql db { insert product into into.Product }!\n rows := sql db { select from into.Product }!\n assert rows.len == 1\n assert rows[0].name == 'b'"))!
	result := os.exec([@VEXE, '-new-compiler', '-nocache', '-gc', 'none', 'run', source])
	assert result.exit_code == 0, result.output
}
