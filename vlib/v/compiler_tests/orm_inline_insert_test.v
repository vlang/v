import os

fn test_orm_inline_insert_requires_a_variable() {
	root := os.join_path(os.vtmp_dir(), 'orm_inline_insert_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'into'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'orm_inline_insert' }\n")!
	os.write_file(os.join_path(root, 'into', 'product.v'), 'module into
pub struct Product {
 pub:
 id int @[primary; sql: serial]
 name string
}
')!
	os.write_file(os.join_path(root, 'main.v'), 'import imported_case

fn main() {
	insert_local()!
	imported_case.insert_imported()!
}
')!
	fixtures := {
		'local_insert.v':                  "import db.sqlite
struct Product {
	id int @[primary; sql: serial]
	name string
}
fn insert_local() ! {
	mut db := sqlite.connect(':memory:')!
	sql db { create table Product }!
	sql db { insert Product{name: 'b'} into Product }!
}
"
		'imported_case/imported_insert.v': "module imported_case

import db.sqlite
import into
pub fn insert_imported() ! {
	mut db := sqlite.connect(':memory:')!
	sql db { create table into.Product }!
	sql db { insert into.Product{name: 'b'} into into.Product }!
}
"
	}
	for name, source in fixtures {
		path := os.join_path(root, name)
		os.mkdir_all(os.dir(path))!
		os.write_file(path, source)!
	}

	for mode in [[]string{}, ['-no-parallel']] {
		for output in [['-check'], ['-o', os.join_path(root, 'main.c')]] {
			result := os.exec([@VEXE, '-nocache', '-nocolor', ...mode, ...output, root])
			assert result.exit_code != 0, result.output
			diagnostics := result.output.split_into_lines().filter(it.contains(': error: ORM insert requires a variable'))
			for name, _ in fixtures {
				filename := os.file_name(name)
				assert diagnostics.any(it.contains('${filename}:')), '${name}, flags=${mode} ${output}: ${result.output}'
			}
		}
	}

	for name, product_type in {
		'local_insert.v':                  'Product'
		'imported_case/imported_insert.v': 'into.Product'
	} {
		inline_insert := "sql db { insert ${product_type}{name: 'b'} into ${product_type} }!"
		variable_insert := "product := ${product_type}{name: 'b'}
	sql db { insert product into ${product_type} }!
	rows := sql db { select from ${product_type} }!
	assert rows.len == 1
	assert rows[0].name == 'b'"
		os.write_file(os.join_path(root, name), fixtures[name].replace_once(inline_insert,
			variable_insert))!
	}
	result := os.exec([@VEXE, '-nocache', '-gc', 'none', 'run', root])
	assert result.exit_code == 0, result.output
}
