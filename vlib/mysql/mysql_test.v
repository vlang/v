module mysql

const (
	connection = Connection{ username: 'test', dbname: 'test', password: 'test1234' }
)

fn test_create_table() {
	connection.connect() or { panic(err) }

	create_user_query := 
	"CREATE TABLE IF NOT EXISTS `users` (
		`user_id` int(10) unsigned NOT NULL AUTO_INCREMENT,
		`username` varchar(100) NOT NULL DEFAULT '',
		PRIMARY KEY (`user_id`)
	) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;"
	connection.query(create_user_query) or { panic(err) }

	create_user_query2 := 
	"CREATE TABLE IF NOT EXISTS `posts` (
		`post_id` int(10) unsigned NOT NULL AUTO_INCREMENT,
		`title` varchar(100) NOT NULL DEFAULT '',
		PRIMARY KEY (`post_id`)
	) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;"
	connection.query(create_user_query2) or { panic(err) }
	tables := connection.tables('') or { panic(err) }
	assert tables.len == 2

	connection.close()
}

fn test_insert() {
	connection.connect() or { panic(err) }

	insert_user_query := 
	"INSERT INTO `users` (`username`)
	VALUES ('don'), ('john'), ('ariel'), ('brian');"
	connection.query(insert_user_query) or { panic(err) }
	assert connection.affected_rows() == 4

	connection.close()
}
