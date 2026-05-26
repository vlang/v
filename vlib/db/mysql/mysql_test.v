// vtest build: started_mysqld?
import db.mysql
import orm

fn test_mysql() {
	$if !network ? {
		eprintln('> Skipping test ${@FN}, since `-d network` is not passed.')
		eprintln('> This test requires a working mysql server running on localhost.')
		return
	}
	config := mysql.Config{
		host:     '127.0.0.1'
		port:     3306
		username: 'root'
		password: '12345678'
		dbname:   'mysql'
	}

	mut db := mysql.connect(config)!
	defer {
		db.close() or {}
	}

	assert db.validate()!

	mut conn := orm.TransactionalConnection(db)
	mut tx := orm.begin(mut conn)!
	tx.transaction[int](fn (mut tx orm.Tx) !int {
		return 1
	})!
	tx.commit()!

	mut response := db.exec('drop table if exists users')!
	assert response == []mysql.Row{}

	response = db.exec('create table if not exists users (
                        id INT PRIMARY KEY AUTO_INCREMENT,
                        username TEXT,
						last_name TEXT NULL DEFAULT NULL
                      )')!
	assert response == []mysql.Row{}

	mut result_code := db.exec_none('insert into users (username) values ("jackson")')
	assert result_code == 0
	result_code = db.exec_none('insert into users (username) values ("shannon")')
	assert result_code == 0
	result_code = db.exec_none('insert into users (username) values ("bailey")')
	assert result_code == 0
	result_code = db.exec_none('insert into users (username) values ("blaze")')
	assert result_code == 0
	rows := db.exec_param('insert into users (username) values (?)', 'Hi')!
	assert rows == []mysql.Row{}

	// Regression testing to ensure the query and exec return the same values
	res := db.query('select * from users')!
	response = res.rows()
	assert response[0].vals[1] == 'jackson'
	response = db.exec('select * from users')!
	assert response[0].vals[1] == 'jackson'

	response = db.exec('select * from users where id = 400')!
	assert response.len == 0

	single_row := db.exec_one('select * from users')!
	assert single_row.vals[1] == 'jackson'

	response = db.exec_param_many('select * from users where username = ?', [
		'jackson',
	])!
	assert response[0] == mysql.Row{
		vals: ['1', 'jackson', '']
	}

	response = db.exec_param_many('select * from users where username = ? and id = ?', [
		'bailey',
		'3',
	])!
	assert response[0] == mysql.Row{
		vals: ['3', 'bailey', '']
	}

	response = db.exec_param_many('select * from users', [''])!
	assert response == [
		mysql.Row{
			vals: ['1', 'jackson', '']
		},
		mysql.Row{
			vals: ['2', 'shannon', '']
		},
		mysql.Row{
			vals: ['3', 'bailey', '']
		},
		mysql.Row{
			vals: ['4', 'blaze', '']
		},
		mysql.Row{
			vals: ['5', 'Hi', '']
		},
	]

	response = db.exec_param('select * from users where username = ?', 'blaze')!
	assert response[0] == mysql.Row{
		vals: ['4', 'blaze', '']
	}

	// transaction test
	// turn off `autocommit` first
	db.autocommit(false)!
	// begin a new transaction
	db.begin()!
	result_code = db.exec_none('insert into users (username) values ("tom")')
	assert result_code == 0
	// make a savepoint
	db.savepoint('savepoint1')!
	result_code = db.exec_none('insert into users (username) values ("kitty")')
	assert result_code == 0
	// rollback to `savepoint1`
	db.rollback_to('savepoint1')!
	result_code = db.exec_none('insert into users (username) values ("mars")')
	assert result_code == 0
	db.commit()!
	response = db.exec_param_many('select * from users', [''])!
	assert response == [
		mysql.Row{
			vals: ['1', 'jackson', '']
		},
		mysql.Row{
			vals: ['2', 'shannon', '']
		},
		mysql.Row{
			vals: ['3', 'bailey', '']
		},
		mysql.Row{
			vals: ['4', 'blaze', '']
		},
		mysql.Row{
			vals: ['5', 'Hi', '']
		},
		mysql.Row{
			vals: ['6', 'tom', '']
		},
		mysql.Row{
			vals: ['8', 'mars', '']
		},
	]
}

fn test_mysql_multi_statements() {
	$if !network ? {
		eprintln('> Skipping test ${@FN}, since `-d network` is not passed.')
		eprintln('> This test requires a working mysql server running on localhost.')
		return
	}
	config := mysql.Config{
		host:     '127.0.0.1'
		port:     3306
		username: 'root'
		password: '12345678'
		dbname:   'mysql'
		flag:     mysql.ConnectionFlag.client_multi_statements
	}

	mut db := mysql.connect(config)!
	defer {
		db.close() or {}
	}

	// exec_multi drains every result set so the connection stays usable.
	results := db.exec_multi('SELECT 1 AS a; SELECT 2 AS b; SELECT 3 AS c')!
	assert results.len == 3
	assert results[0][0].val(0) == '1'
	assert results[1][0].val(0) == '2'
	assert results[2][0].val(0) == '3'

	// The connection must remain reusable afterwards: regression test for #18061.
	follow_up := db.query('SELECT 42')!
	rows := follow_up.rows()
	assert rows[0].val(0) == '42'

	// Low-level drain via more_results/next_result/store_result also works.
	first := db.query('SELECT 10; SELECT 20')!
	assert first.rows()[0].val(0) == '10'
	unsafe { first.free() }
	assert db.more_results() == true
	assert db.next_result()! == true
	second := db.store_result()!
	assert second.rows()[0].val(0) == '20'
	unsafe { second.free() }
	assert db.next_result()! == false
	// After draining, new queries succeed (no "Commands out of sync" error).
	final := db.query('SELECT 99')!
	assert final.rows()[0].val(0) == '99'

	// Regression test for #18063: every statement in a multi-statement query
	// must complete on the server before exec_multi() returns, so that the
	// next query can immediately rely on its side effects (e.g. tables).
	_ := db.exec_multi('DROP TABLE IF EXISTS multi_a;
		DROP TABLE IF EXISTS multi_b;
		CREATE TABLE multi_a (id INT PRIMARY KEY);
		CREATE TABLE multi_b (id INT PRIMARY KEY);
		INSERT INTO multi_a (id) VALUES (1), (2);
		INSERT INTO multi_b (id) VALUES (10), (20), (30);')!
	count_a := db.query('SELECT COUNT(*) FROM multi_a')!.rows()
	count_b := db.query('SELECT COUNT(*) FROM multi_b')!.rows()
	assert count_a[0].val(0) == '2'
	assert count_b[0].val(0) == '3'
	db.exec_none('DROP TABLE IF EXISTS multi_a')
	db.exec_none('DROP TABLE IF EXISTS multi_b')
}

fn mysql_query_count_from_shared_connection(db mysql.DB) !int {
	result := db.query('SELECT COUNT(*) as table_count FROM information_schema.tables')!
	rows := result.maps()
	return rows[0]['table_count'].int()
}

fn test_query_is_serialized_for_shared_connections() {
	$if !network ? {
		eprintln('> Skipping test ${@FN}, since `-d network` is not passed.')
		eprintln('> This test requires a working mysql server running on localhost.')
		return
	}
	config := mysql.Config{
		host:     '127.0.0.1'
		port:     3306
		username: 'root'
		password: '12345678'
		dbname:   'mysql'
	}

	mut db := mysql.connect(config)!
	defer {
		db.close() or {}
	}

	threads := [
		spawn mysql_query_count_from_shared_connection(db),
		spawn mysql_query_count_from_shared_connection(db),
		spawn mysql_query_count_from_shared_connection(db),
		spawn mysql_query_count_from_shared_connection(db),
	]
	results := threads.wait()!
	assert results.len == 4
	for count in results {
		assert count > 0
	}
}
