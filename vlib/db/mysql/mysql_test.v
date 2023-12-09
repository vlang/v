import db.mysql

fn test_mysql() {
	config := mysql.Config{
		host: '127.0.0.1'
		port: 3306
		username: 'root'
		password: ''
		dbname: 'mysql'
	}

	db := mysql.connect(config)!

	mut response := db.exec('drop table if exists users')!
	assert response == []mysql.Row{}

	response = db.exec('create table if not exists users (
                        id INT PRIMARY KEY AUTO_INCREMENT,
                        username TEXT
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
		vals: ['1', 'jackson']
	}

	response = db.exec_param_many('select * from users where username = ? and id = ?',
		['bailey', '3'])!
	assert response[0] == mysql.Row{
		vals: ['3', 'bailey']
	}

	response = db.exec_param_many('select * from users', [''])!
	assert response == [
		mysql.Row{
			vals: ['1', 'jackson']
		},
		mysql.Row{
			vals: ['2', 'shannon']
		},
		mysql.Row{
			vals: ['3', 'bailey']
		},
		mysql.Row{
			vals: ['4', 'blaze']
		},
	]

	response = db.exec_param('select * from users where username = ?', 'blaze')!
	assert response[0] == mysql.Row{
		vals: ['4', 'blaze']
	}
}
