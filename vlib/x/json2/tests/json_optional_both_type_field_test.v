struct Response[T] {
	code int
	msg  string
	data ?T
}

struct UserInfo {
	name string
	age  int
}

fn g[T](x T) string {
	return 'abc'
}

fn response_to_string[T](response Response[T]) string {
	return g(response)
}

fn test_main() {
	res := response_to_string(Response{1, 'success', ?UserInfo{'Jay Chou', 46}})
	assert res == 'abc'
}
