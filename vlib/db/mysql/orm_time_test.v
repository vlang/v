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
	assert column_types['epoch'] == 'int'
	assert column_types['count'] == 'int'
}
