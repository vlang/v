type FirstPath = string
type SecondPath = string

fn (path FirstPath) resolve(value string) string {
	return 'first:' + value
}

fn (path FirstPath) mark() string {
	return 'first:' + string(path)
}

fn (path SecondPath) mark() string {
	return 'second:' + string(path)
}

fn (path SecondPath) resolve(base FirstPath, value string) string {
	return base.resolve(value) + ':' + path.mark()
}

fn (path SecondPath) mark_other(base FirstPath) string {
	return base.mark()
}

fn (path SecondPath) resolve_parenthesized(base FirstPath, value string) string {
	return base.resolve(value)
}

fn (path SecondPath) resolve_cast(base FirstPath, value string) string {
	return FirstPath(base).resolve(value)
}

fn test_alias_method_receiver_uses_the_parameter_alias() {
	path := SecondPath('b')
	assert path.resolve(FirstPath('a'), 'y') == 'first:y:second:b'
	assert path.mark_other(FirstPath('a')) == 'first:a'
	assert path.resolve_parenthesized(FirstPath('a'), 'y') == 'first:y'
	assert path.resolve_cast(FirstPath('a'), 'y') == 'first:y'
	assert FirstPath('a').mark() == 'first:a'
	assert path.mark() == 'second:b'
}
