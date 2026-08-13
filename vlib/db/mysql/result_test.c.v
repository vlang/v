// vtest build: started_mysqld?
module mysql

fn test_result_fields_use_each_columns_metadata() {
	$if !network ? {
		eprintln('> Skipping test ${@FN}, since `-d network` is not passed.')
		eprintln('> This test requires a working mysql server running on localhost.')
		return
	}
	mut db := connect(Config{
		host:     '127.0.0.1'
		port:     u32($d('mysql_test_port', 3306))
		username: 'root'
		password: $d('mysql_test_password', '12345678')
		dbname:   'mysql'
	})!
	defer {
		db.close() or {}
	}
	db.exec_none('DROP TABLE IF EXISTS field_metadata_test')
	defer {
		db.exec_none('DROP TABLE IF EXISTS field_metadata_test')
	}
	db.exec('CREATE TABLE field_metadata_test (
		id INT,
		name VARCHAR(50),
		amount DECIMAL(10, 2)
	)')!
	result := db.query('SELECT id, name, amount FROM field_metadata_test')!
	defer {
		unsafe { result.free() }
	}
	fields := result.fields()
	assert fields.len == 3
	assert fields.map(it.name) == ['id', 'name', 'amount']
	assert fields[0].type == .type_long
	assert fields[1].type in [.type_var_string, .type_varchar]
	assert fields[2].type == .type_newdecimal

	original := C.mysql_fetch_fields(result.result)
	for i, field in fields {
		unsafe {
			assert field.length == original[i].length
			assert field.max_length == original[i].max_length
			assert field.name_length == original[i].name_length
			assert field.org_name_length == original[i].org_name_length
			assert field.table_length == original[i].table_length
			assert field.org_table_length == original[i].org_table_length
			assert field.db_length == original[i].db_length
			assert field.catalog_length == original[i].catalog_length
			assert field.def_length == original[i].def_length
			assert field.flags == original[i].flags
			assert field.decimals == original[i].decimals
			assert field.charsetnr == original[i].charsetnr
			assert field.type == FieldType(original[i].type)
		}
	}
	assert fields[1].length != fields[0].length
	assert fields[2].length != fields[0].length
	assert fields[1].charsetnr != fields[0].charsetnr
	assert fields[1].flags != fields[0].flags
}
