import os
import v.tempname

fn orm_null_text_gen_c(name string, src string) string {
	root := os.join_path(os.vtmp_dir(), 'v3_${name}_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	src_path := os.join_path(root, 'main.v')
	os.write_file(src_path, src) or { panic(err) }
	c_path := os.join_path(root, 'main.c')
	result := os.exec([@VEXE, src_path, '-o', c_path])
	assert result.exit_code == 0, result.output
	return os.read_file(c_path) or { panic(err) }
}

fn orm_null_text_compile_project(name string, files map[string]string) (os.Result, string) {
	root := os.join_path(os.vtmp_dir(), 'v3_${name}_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	for path, contents in files {
		full_path := os.join_path(root, path)
		os.mkdir_all(os.dir(full_path)) or { panic(err) }
		os.write_file(full_path, contents) or { panic(err) }
	}
	c_path := os.join_path(root, 'main.c')
	result := os.exec([@VEXE, root, '-o', c_path])
	if result.exit_code != 0 {
		return result, ''
	}
	return result, os.read_file(c_path) or { panic(err) }
}

fn orm_null_text_gen_c_project(name string, files map[string]string) string {
	result, c_code := orm_null_text_compile_project(name, files)
	assert result.exit_code == 0, result.output
	return c_code
}

fn test_orm_null_text_fallback_uses_declared_interned_empty_string() {
	c_code := orm_null_text_gen_c('orm_null_text_fallback', 'import db.sqlite

struct User {
	id int
	name string
}

fn main() {
	mut db := sqlite.connect(":memory:") or { panic(err) }
	rows := sql db {
		select from User
	}!
	_ = rows
}
')
	marker := '.name = _str_'
	marker_idx := c_code.index(marker) or {
		assert false, c_code
		return
	}
	after_marker := c_code[marker_idx + '.name = '.len..]
	symbol := after_marker.all_before('}').all_before(',').all_before(';').trim_space()
	assert symbol.starts_with('_str_'), c_code
	assert c_code.contains('_SL(${symbol}, "", 0);'), c_code
}

fn test_orm_select_and_multi_statement_codegen() {
	c_code := orm_null_text_gen_c_project('orm_select_and_multi_statement_codegen', {
		'v.mod':         "Module { name: 'orm_select_and_multi_statement_codegen' }\n"
		'main.v':        "module main

import db.sqlite
import model

struct User {
	id int @[primary]
}

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}
	first := User{1}
	second := User{2}
	sql db {
		insert first into User
		insert second into User
	}!
	model.load()
}
"
		'model/model.v': 'module model\n\nimport db.sqlite\n\nstruct User {\n\tid int\n\tname string\n}\n\npub fn load() {\n\tmut db := sqlite.connect(":memory:") or { panic(err) }\n\trows := sql db {\n\t\tselect from User\n\t}!\n\t_ = rows\n}\n'
	})
	assert c_code.contains('sizeof(model__User)'), c_code
	assert c_code.contains('model__User '), c_code
	assert !c_code.contains('sizeof(User)'), c_code
	mut found_stable_db := false
	for line in c_code.split_into_lines() {
		assert !(line.contains('sqlite__DB* __sql_db_') && line.contains(' = *db;')), line
		if line.contains('sqlite__DB __sql_db_') && line.contains(' = *db;') {
			found_stable_db = true
		}
	}
	assert found_stable_db, c_code
}

fn test_orm_sqlite_unsupported_queries_use_fallback() {
	c_code := orm_null_text_gen_c('orm_sqlite_fallbacks', 'import db.sqlite
import time

struct User {
	id int
	name string
}

struct TestDurationAlias {
	id int
	duration time.Duration
}

fn select_with_unsupported_clauses() ! {
	mut db := sqlite.connect(":memory:") or { panic(err) }
	rows1 := sql db {
		select from User where id == 1
	}!
	rows2 := sql db {
		select from User order by id
	}!
	_ = rows1
	_ = rows2
}

fn select_with_runtime_limit() ! {
	mut db := sqlite.connect(":memory:") or { panic(err) }
	n := 1
	rows := sql db {
		select from User limit n
	}!
	_ = rows
}

fn insert_bulk() ! {
	mut db := sqlite.connect(":memory:") or { panic(err) }
	users := [
		User{
			id: 1
			name: "a"
		},
	]
	sql db {
		insert users into User
	}!
}

fn create_table_with_unsupported_fields() ! {
	mut db := sqlite.connect(":memory:") or { panic(err) }
	sql db {
		create table TestDurationAlias
	}!
}

fn main() {
	select_with_unsupported_clauses()!
	select_with_runtime_limit()!
	insert_bulk()!
	create_table_with_unsupported_fields()!
}
')
	assert !c_code.contains('SELECT "id", "name" FROM "User";'), c_code
	assert !c_code.contains('SELECT "id", "name" FROM "User" LIMIT'), c_code
	assert !c_code.contains('INSERT INTO "User"'), c_code
	assert !c_code.contains('(users).name'), c_code
	assert !c_code.contains('CREATE TABLE IF NOT EXISTS "TestDurationAlias"'), c_code
}

fn test_orm_non_sqlite_db_uses_fallback() {
	c_code := orm_null_text_gen_c('orm_non_sqlite_db_fallback', 'import db.pg

struct User {
	id int
	name string
}

fn main() {
	mut db := pg.DB{}
	rows := sql db {
		select from User
	}!
	_ = rows
}
')
	assert !c_code.contains('sqlite3_prepare_v2'), c_code
	assert !c_code.contains('SELECT "id", "name" FROM "User"'), c_code
}

fn test_orm_insert_requires_qualified_value_type_match() {
	result, _ := orm_null_text_compile_project('orm_insert_qualified_value_mismatch', {
		'v.mod':         "Module { name: 'orm_insert_qualified_value_mismatch' }\n"
		'main.v':        'module main\n\nimport app\n\nfn main() {\n\tapp.save()\n}\n'
		'other/other.v': 'module other\n\npub struct User {\npub:\n\tid int\n\tname string\n}\n\npub fn make() User {\n\treturn User{\n\t\tid: 1\n\t\tname: "remote"\n\t}\n}\n'
		'app/app.v':     'module app\n\nimport db.sqlite\nimport other\n\nstruct User {\n\tid int\n\tlocal_name string\n}\n\npub fn save() {\n\tmut db := sqlite.connect(":memory:") or { panic(err) }\n\tuser := other.make()\n\tsql db {\n\t\tinsert user into User\n\t}!\n}\n'
	})
	assert result.exit_code != 0, result.output
	assert result.output.contains('cannot use `other.User` as `app.User`'), result.output
	assert !result.output.contains('C compilation failed'), result.output
}

fn test_orm_wide_integer_fields_use_sqlite_int64_apis() {
	c_code := orm_null_text_gen_c('orm_wide_integer_fields', 'import db.sqlite

struct Metric {
	id int
	small int
	wide_32 u32
	wide_i i64
	wide_u u64
	wide_is int
	wide_us uint
}

fn main() {
	mut db := sqlite.connect(":memory:") or { panic(err) }
	metric := Metric{
		small: 7
		wide_32: u32(1) << 31
		wide_i: i64(1) << 40
		wide_u: u64(1) << 40
		wide_is: int(1) << 40
		wide_us: uint(1) << 40
	}
	sql db {
		insert metric into Metric
	}!
	rows := sql db {
		select from Metric
	}!
	_ = rows
}
')
	assert c_code.contains('#include "sqlite3.h"'), c_code
	// `int` is 64-bit on 64-bit targets, where `Stmt.bind_int` uses the int64 API.
	$if x64 {
		assert !c_code.contains('sqlite3_bind_int('), c_code
		assert !c_code.contains('sqlite3_column_int('), c_code
	} $else {
		assert c_code.contains('sqlite3_bind_int('), c_code
		assert c_code.contains('sqlite3_column_int('), c_code
	}
	assert c_code.contains('sqlite3_bind_int64('), c_code
	assert c_code.contains('sqlite3_column_int64('), c_code
	assert c_code.contains('orm__append_field_value_data_T_v_int('), c_code
	assert c_code.contains('orm__append_field_value_data_T_u32('), c_code
	assert c_code.contains('orm__append_field_value_data_T_i64('), c_code
	assert c_code.contains('orm__append_field_value_data_T_u64('), c_code
	assert c_code.contains('orm__append_field_value_data_T_uint('), c_code
	assert c_code.contains('orm__primitive_from_field_value_T_u32('), c_code
	assert c_code.contains('orm__primitive_from_field_value_T_i64('), c_code
	assert c_code.contains('orm__primitive_from_field_value_T_u64('), c_code
	assert c_code.contains('orm__primitive_from_field_value_T_uint('), c_code
}
