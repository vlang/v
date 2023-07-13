fn issue(data string) ?(int, string) {
	if data.len == 0 {
		return none
	}
	return data.len, data
}

fn wrapper(data string) ?(int, string) {
	return issue(data)
}

fn test_return_option_call() {
	dump(wrapper('issue'))
	dump(wrapper('foobar'))
	dump(wrapper(''))
	assert true
}
