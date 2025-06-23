// vtest build: !(macos || windows)
import db.mysql

fn main() {
	mut conn := mysql.connect(
		host:     'localhost'
		port:     3306
		username: 'root'
		password: ''
		dbname:   'mysql'
	)!
	res := conn.query('show tables')!
	for row in res.rows() {
		println(row.vals.join(', '))
	}
	conn.close()!
}
