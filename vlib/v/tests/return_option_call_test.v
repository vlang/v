fn issue(data string) ?(int, string) {
	if data == '' {
		return none
	}
	return data.len, data
}

fn wrapper(data string) ?(int, string) {
	return issue(data)
}

fn test_return_option_call() {
	dump(wrapper('issue')?)
	dump(wrapper('foobar')?)
	if a, b := wrapper('') {
		assert false, '${a}, ${b}'
	}
}
