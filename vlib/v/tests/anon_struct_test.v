fn test(config struct{name string, value int}) struct{ value string } {
	assert config.name == 'hello'
	assert config.value == 42
	return {
		value: 'world'
	}
}

fn test_anon_struct() {
	ret := test({ name: 'hello', value: 42 })
	assert ret.value == 'world'
}
