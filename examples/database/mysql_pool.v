// vtest build: !(macos || windows)
import db.mysql
import pool
import time

// Define your connection factory function
fn create_conn() !&pool.ConnectionPoolable {
	config := mysql.Config{
		host:     '127.0.0.1'
		port:     3306
		username: 'root'
		password: '12345678'
		dbname:   'mysql'
	}
	db := mysql.connect(config)!
	return &db
}

fn main() {
	// Configure pool parameters
	config := pool.ConnectionPoolConfig{
		max_conns:      50
		min_idle_conns: 5
		max_lifetime:   2 * time.hour
		idle_timeout:   30 * time.minute
		get_timeout:    5 * time.second
	}

	// Create connection pool
	mut my_pool := pool.new_connection_pool(create_conn, config)!
	defer {
		// When application exits
		my_pool.close()
	}

	// Acquire connection
	mut conn := my_pool.get()!
	defer {
		// Return connection to pool
		my_pool.put(conn) or { println(err) }
	}

	// Convert `conn` to a `mysql.DB` object
	mut db := conn as mysql.DB

	assert db.validate()!

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

	response = db.exec_param_many('select * from users where username = ? and id = ?',
		['bailey', '3'])!
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
