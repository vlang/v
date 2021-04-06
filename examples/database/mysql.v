import mysql

fn main() {
	mut conn := mysql.Connection{
		host: 'localhost'
		port: 3306
		username: 'root'
		password: ''
		dbname: 'mysql'
	}
	conn.connect() ?
	res := conn.query('show tables') ?
	a := res.rows()[0].vals[0]
	for row in res.rows() {
		println(row.vals.join(', '))
	}
	conn.close()
}
