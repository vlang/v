module main

struct None {}

pub type Maybe<T> = None | T

pub fn (m Maybe<T>) str<T>() string {
	return if m is T {
		x := m as T
		'Some(${x})'
	} else {
		'Noth'
	}
}

pub fn some<T>(v T) Maybe<T> {
	return Maybe<T>(v)
}

fn test_generic_sumtype_str() {
	a := some(123)
	b := some('abc')

	println(a.str())
	println(a)
	println('${a}')
	assert '${a}' == 'Some(123)'

	println(b.str())
	println(b)
	println('${b}')
	assert '${b}' == 'Some(abc)'
}
