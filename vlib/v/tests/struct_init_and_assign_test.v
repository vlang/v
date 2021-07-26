struct FooFoo {
pub mut:
	conn Foo
}

struct Foo {
	host     string = '127.0.0.1'
	port     u32    = 3306
	username string
	password string
	dbname   string
}

fn test_struct_init_and_assign() {
	mut sql := FooFoo{}
	sql.conn = Foo{
		username: 'username'
		password: 'abc'
		dbname: 'test'
	}
	assert sql.conn.host == '127.0.0.1'
	assert sql.conn.port == 3306
	assert sql.conn.username == 'username'
	assert sql.conn.password == 'abc'
	assert sql.conn.dbname == 'test'
}
