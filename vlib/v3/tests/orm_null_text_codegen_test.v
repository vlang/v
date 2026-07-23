import os

const orm_null_text_vexe = @VEXE
const orm_null_text_tests_dir = os.dir(@FILE)
const orm_null_text_v3_dir = os.dir(orm_null_text_tests_dir)
const orm_null_text_vlib_dir = os.dir(orm_null_text_v3_dir)
const orm_null_text_v3_src = os.join_path(orm_null_text_v3_dir, 'v3.v')

fn orm_null_text_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_orm_null_text_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${orm_null_text_vexe} -gc none -path "${orm_null_text_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${orm_null_text_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn orm_null_text_gen_c(v3_bin string, name string, src string) string {
	src_path := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}.v')
	os.write_file(src_path, src) or { panic(err) }
	c_path := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}.c')
	os.rm(c_path) or {}
	result := os.execute('${v3_bin} ${src_path} -o ${c_path}')
	assert result.exit_code == 0, result.output
	return os.read_file(c_path) or { panic(err) }
}

fn orm_null_text_gen_c_project(v3_bin string, name string, files map[string]string) string {
	root := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	for path, contents in files {
		full_path := os.join_path(root, path)
		os.mkdir_all(os.dir(full_path)) or { panic(err) }
		os.write_file(full_path, contents) or { panic(err) }
	}
	c_path := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}.c')
	os.rm(c_path) or {}
	result := os.execute('${v3_bin} ${root} -o ${c_path}')
	assert result.exit_code == 0, result.output
	return os.read_file(c_path) or { panic(err) }
}

fn test_orm_null_text_fallback_uses_declared_interned_empty_string() {
	v3_bin := orm_null_text_build_v3()
	c_code := orm_null_text_gen_c(v3_bin, 'orm_null_text_fallback', 'import db.sqlite

struct User {
	id int
	name string
}

fn main() {
	mut db := sqlite.connect(":memory:") or { panic(err) }
	rows := sql db {
		select from User
	}
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
	assert c_code.contains('string ${symbol} ='), c_code
}

fn test_orm_module_local_select_uses_qualified_row_type() {
	v3_bin := orm_null_text_build_v3()
	c_code := orm_null_text_gen_c_project(v3_bin, 'orm_module_local_select', {
		'v.mod':         "Module { name: 'orm_module_local_select' }\n"
		'main.v':        'module main\n\nimport model\n\nfn main() {\n\tmodel.load()\n}\n'
		'model/model.v': 'module model\n\nimport db.sqlite\n\nstruct User {\n\tid int\n\tname string\n}\n\npub fn load() {\n\tmut db := sqlite.connect(":memory:") or { panic(err) }\n\trows := sql db {\n\t\tselect from User\n\t}\n\t_ = rows\n}\n'
	})
	assert c_code.contains('sizeof(model__User)'), c_code
	assert c_code.contains('model__User '), c_code
	assert !c_code.contains('sizeof(User)'), c_code
}

fn test_orm_unsupported_select_clauses_use_fallback() {
	v3_bin := orm_null_text_build_v3()
	c_code := orm_null_text_gen_c(v3_bin, 'orm_unsupported_select_clauses', 'import db.sqlite

struct User {
	id int
	name string
}

fn main() {
	mut db := sqlite.connect(":memory:") or { panic(err) }
	rows1 := sql db {
		select from User where id == 1
	}
	rows2 := sql db {
		select from User order by id
	}
	_ = rows1
	_ = rows2
}
')
	assert !c_code.contains('SELECT "id", "name" FROM "User";'), c_code
}

fn test_orm_runtime_select_limit_uses_fallback() {
	v3_bin := orm_null_text_build_v3()
	c_code := orm_null_text_gen_c(v3_bin, 'orm_runtime_select_limit_fallback', 'import db.sqlite

struct User {
	id int
	name string
}

fn main() {
	mut db := sqlite.connect(":memory:") or { panic(err) }
	n := 1
	rows := sql db {
		select from User limit n
	}
	_ = rows
}
')
	assert !c_code.contains('SELECT "id", "name" FROM "User" LIMIT'), c_code
}

fn test_orm_non_sqlite_db_uses_fallback() {
	v3_bin := orm_null_text_build_v3()
	c_code := orm_null_text_gen_c(v3_bin, 'orm_non_sqlite_db_fallback', 'import db.pg

struct User {
	id int
	name string
}

fn main() {
	mut db := pg.DB{}
	rows := sql db {
		select from User
	}
	_ = rows
}
')
	assert !c_code.contains('sqlite3_prepare_v2'), c_code
	assert !c_code.contains('SELECT "id", "name" FROM "User"'), c_code
}

fn test_orm_bulk_insert_uses_fallback() {
	v3_bin := orm_null_text_build_v3()
	c_code := orm_null_text_gen_c(v3_bin, 'orm_bulk_insert_fallback', 'import db.sqlite

struct User {
	id int
	name string
}

fn main() {
	mut db := sqlite.connect(":memory:") or { panic(err) }
	users := [
		User{
			id: 1
			name: "a"
		},
	]
	sql db {
		insert users into User
	}
}
')
	assert !c_code.contains('INSERT INTO "User"'), c_code
	assert !c_code.contains('(users).name'), c_code
}

fn test_orm_insert_requires_qualified_value_type_match() {
	v3_bin := orm_null_text_build_v3()
	c_code := orm_null_text_gen_c_project(v3_bin, 'orm_insert_qualified_value_mismatch', {
		'v.mod':         "Module { name: 'orm_insert_qualified_value_mismatch' }\n"
		'main.v':        'module main\n\nimport app\n\nfn main() {\n\tapp.save()\n}\n'
		'other/other.v': 'module other\n\npub struct User {\npub:\n\tid int\n\tname string\n}\n\npub fn make() User {\n\treturn User{\n\t\tid: 1\n\t\tname: "remote"\n\t}\n}\n'
		'app/app.v':     'module app\n\nimport db.sqlite\nimport other\n\nstruct User {\n\tid int\n\tlocal_name string\n}\n\npub fn save() {\n\tmut db := sqlite.connect(":memory:") or { panic(err) }\n\tuser := other.make()\n\tsql db {\n\t\tinsert user into User\n\t}\n}\n'
	})
	assert !c_code.contains('INSERT INTO "User"'), c_code
	assert !c_code.contains('(user).local_name'), c_code
}

fn test_orm_unsupported_field_tables_use_fallback() {
	v3_bin := orm_null_text_build_v3()
	c_code := orm_null_text_gen_c(v3_bin, 'orm_unsupported_field_table_fallback',
		'import db.sqlite\nimport time\n\nstruct TestDurationAlias {\n\tid int\n\tduration time.Duration\n}\n\nfn main() {\n\tmut db := sqlite.connect(":memory:") or { panic(err) }\n\tsql db {\n\t\tcreate table TestDurationAlias\n\t}\n}\n')
	assert !c_code.contains('CREATE TABLE IF NOT EXISTS "TestDurationAlias"'), c_code
}

fn test_orm_wide_integer_fields_use_sqlite_int64_apis() {
	v3_bin := orm_null_text_build_v3()
	c_code := orm_null_text_gen_c(v3_bin, 'orm_wide_integer_fields', 'import db.sqlite

struct Metric {
	id int
	small int
	wide_32 u32
	wide_i i64
	wide_u u64
	wide_is isize
	wide_us usize
}

fn main() {
	mut db := sqlite.connect(":memory:") or { panic(err) }
	metric := Metric{
		small: 7
		wide_32: u32(1) << 31
		wide_i: i64(1) << 40
		wide_u: u64(1) << 40
		wide_is: isize(1) << 40
		wide_us: usize(1) << 40
	}
	sql db {
		insert metric into Metric
	}
	rows := sql db {
		select from Metric
	}
	_ = rows
}
')
	assert c_code.contains('i64 sqlite3_column_int64('), c_code
	assert c_code.contains('i32 sqlite3_bind_int64('), c_code
	assert c_code.contains('sqlite3_bind_int('), c_code
	assert c_code.contains('sqlite3_bind_int64('), c_code
	assert c_code.contains('sqlite3_column_int('), c_code
	assert c_code.contains('sqlite3_column_int64('), c_code
	assert c_code.contains('orm__append_field_value_data_T_v_int('), c_code
	assert c_code.contains('orm__append_field_value_data_T_u32('), c_code
	assert c_code.contains('orm__append_field_value_data_T_i64('), c_code
	assert c_code.contains('orm__append_field_value_data_T_u64('), c_code
	assert c_code.contains('orm__append_field_value_data_T_isize('), c_code
	assert c_code.contains('orm__append_field_value_data_T_usize('), c_code
	assert c_code.contains('orm__primitive_from_field_value_T_u32('), c_code
	assert c_code.contains('orm__primitive_from_field_value_T_i64('), c_code
	assert c_code.contains('orm__primitive_from_field_value_T_u64('), c_code
	assert c_code.contains('orm__primitive_from_field_value_T_isize('), c_code
	assert c_code.contains('orm__primitive_from_field_value_T_usize('), c_code
}
