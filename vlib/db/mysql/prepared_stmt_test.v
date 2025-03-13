// vtest build: started_mysqld?
import db.mysql

fn test_prep() {
	$if !network ? {
		eprintln('> Skipping test ${@FN}, since `-d network` is not passed.')
		eprintln('> This test requires a working mysql server running on localhost.')
		return
	}
	config := mysql.Config{
		host:     '127.0.0.1'
		port:     3306
		username: 'root'
		password: ''
		dbname:   'mysql'
	}

	db := mysql.connect(config)!

	mut response := db.exec('drop table if exists test')!
	assert response == []mysql.Row{}

	response = db.exec('create table if not exists test (
                        id INT PRIMARY KEY AUTO_INCREMENT,
                        value TEXT)')!
	assert response == []mysql.Row{}

	stmt := db.prepare('insert into test (value) values (?)')!
	defer {
		stmt.close()
	}

	names := ['jackson', 'hello', 'Disney', 'Marz', 'Bailey', 'Claxton']
	for name in names {
		response = stmt.execute([name])!
		assert response == []mysql.Row{}
	}

	response = db.exec_param_many('select * from test', [''])!
	assert response == [
		mysql.Row{
			vals: ['1', 'jackson']
		},
		mysql.Row{
			vals: ['2', 'hello']
		},
		mysql.Row{
			vals: ['3', 'Disney']
		},
		mysql.Row{
			vals: ['4', 'Marz']
		},
		mysql.Row{
			vals: ['5', 'Bailey']
		},
		mysql.Row{
			vals: ['6', 'Claxton']
		},
	]
}
