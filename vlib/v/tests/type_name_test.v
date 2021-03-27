fn simple<T>(p T) string {
	tname := T.name
	assert tname == typeof(p).name
	return tname
}

struct FunkyStruct {}

fn test_generic_type_name() {
	i := 42
	assert simple(i) == 'int'
	f := 3.14
	assert simple(f) == 'f64'
	assert simple('FuBar') == 'string'
	assert simple(FunkyStruct{}) == 'FunkyStruct'
	assert simple(fn () {}) == 'fn ()'
	// assert simple(test_generic_type_name) == "fn ()"
}
