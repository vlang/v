fn fake_prompt[T](msg string, default ?T) string {
	return build_prompt(msg, default)
}

fn build_prompt[T](msg string, default ?T) string {
	mut s := msg
	if default != none {
		s += ' [${default}]'
	}
	s += ': '
	return s
}

fn test_main() {
	p1 := fake_prompt[string]('hello', none)
	p2 := fake_prompt[string]('hello', 'world')
	assert p1 == 'hello: '
	assert p2 == 'hello [world]: '
}
