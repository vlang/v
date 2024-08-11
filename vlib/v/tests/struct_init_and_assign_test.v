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
	mut sql_ := FooFoo{}
	sql_.conn = Foo{
		username: 'username'
		password: 'abc'
		dbname:   'test'
	}
	assert sql_.conn.host == '127.0.0.1'
	assert sql_.conn.port == 3306
	assert sql_.conn.username == 'username'
	assert sql_.conn.password == 'abc'
	assert sql_.conn.dbname == 'test'
}
