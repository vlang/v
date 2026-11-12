struct Response[T] {
	code int
	msg  string
	data ?T
}

struct UserInfo {
	name string
	age  int
}

fn g[T]() string {
	return 'abc'
}

fn response_to_string[T]() string {
	return g[T]()
}

fn test_main() {
	res := response_to_string[UserInfo]()
	assert res == 'abc'
}
