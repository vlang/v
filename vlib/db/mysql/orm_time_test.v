// vtest build: started_mysqld?
module mysql

import time

@[table: 'mysql_orm_time_binding']
struct OrmTimeBinding {
	id          int       @[primary; sql: serial]
	datetime    time.Time @[sql_type: 'DATETIME']
	timestamp   time.Time @[sql_type: 'TIMESTAMP']
	epoch       time.Time
	optional_at ?time.Time @[sql_type: 'DATETIME']
	count       u32
}

fn test_orm_time_values_follow_the_sql_column_types() {
	$if !network ? {
		return
	}
	mut db := connect(
		host:     '127.0.0.1'
		port:     u32($d('mysql_test_port', 3306))
		username: 'root'
		password: $d('mysql_test_password', '12345678')
		dbname:   'mysql'
	)!
	defer {
		db.close() or {}
	}
	db.exec('DROP TABLE IF EXISTS mysql_orm_time_binding')!
	defer {
		db.exec('DROP TABLE IF EXISTS mysql_orm_time_binding') or {}
	}
	sql db {
		create table OrmTimeBinding
	}!

	today := time.parse('2022-07-16 15:13:27')!
	values := [OrmTimeBinding{
		datetime:  today
		timestamp: today
		epoch:     today
		count:     42
	}, OrmTimeBinding{
		datetime:    today
		timestamp:   today
		epoch:       today
		optional_at: today
		count:       1234567890
	}]
	sql db {
		insert values into OrmTimeBinding
	}!
	rows := sql db {
		select from OrmTimeBinding order by id
	}!
	assert rows.len == values.len
	for i, row in rows {
		assert row.datetime == values[i].datetime
		assert row.timestamp == values[i].timestamp
		assert row.epoch == values[i].epoch
		assert row.optional_at == values[i].optional_at
		assert row.count == values[i].count
	}
	column_types := db.get_table_column_type_map('mysql_orm_time_binding')!
	assert column_types['epoch'] == 'datetime'
	assert column_types['count'] == 'int'
}

fn test_orm_table_name_matching_preserves_case() {
	$if !network ? {
		return
	}
	mut db := connect(
		host:     '127.0.0.1'
		port:     u32($d('mysql_test_port', 3306))
		username: 'root'
		password: $d('mysql_test_password', '12345678')
		dbname:   'mysql'
	)!
	defer {
		db.close() or {}
	}
	lower_table := 'mysql_orm_case_events'
	upper_table := 'MYSQL_ORM_CASE_EVENTS'
	db.exec('DROP TABLE IF EXISTS ${lower_table}')!
	db.exec('DROP TABLE IF EXISTS ${upper_table}')!
	defer {
		db.exec('DROP TABLE IF EXISTS ${lower_table}') or {}
		db.exec('DROP TABLE IF EXISTS ${upper_table}') or {}
	}
	// Case-distinct tables can only coexist with lower_case_table_names=0.
	// On case-insensitive servers the second CREATE would collide, so skip.
	mode_res := db.query('SELECT @@lower_case_table_names')!
	mode_rows := mode_res.rows()
	if mode_rows.len == 0 || mode_rows[0].vals[0] != '0' {
		eprintln('> Skipping ${@FN}: requires lower_case_table_names=0.')
		return
	}
	// Uppercase column on purpose: keys and type names stay normalized,
	// while the table name itself must match exactly.
	db.exec('CREATE TABLE ${lower_table} (TS DATETIME)')!
	db.exec('CREATE TABLE ${upper_table} (ts BIGINT)')!
	lower_types := db.get_table_column_type_map(lower_table)!
	upper_types := db.get_table_column_type_map(upper_table)!
	assert lower_types['ts'] == 'datetime'
	assert upper_types['ts'] == 'bigint'
}
