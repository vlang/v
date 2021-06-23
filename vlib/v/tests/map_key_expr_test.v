const (
	alpha = 'a'
	beta  = 'b'
	m     = map{
		alpha: 'Alpha'
		beta:  'Beta'
	}
)

fn test_const_keys() {
	assert m[alpha] == 'Alpha'
	assert m[beta] == 'Beta'
}

enum Enum {
	a
	b
}

const (
	m2 = map{
		Enum.a.str(): 'first'
		Enum.b.str(): 'second'
	}
)

fn test_method_call() {
	assert m2.keys() == ['a', 'b']
	assert m2[Enum.a.str()] == 'first'
	assert m2[Enum.b.str()] == 'second'
}
