module main

pub type Value = f64 | []Value

fn test_no_ref() {
	mut m := map[string]Value{}
	var := Value([Value(1.0), Value(2.0), Value(3.0)])
	arr := (var as []Value)
	m['var'] = arr
	dump(m)
	if item := m['var'] {
		assert (item as []Value)[1] == Value(2.0)
	} else {
		assert false
	}
}

fn test_ref() {
	mut m := map[string]Value{}
	var := Value([Value(1.0), Value(2.0), Value(3.0)])
	arr := &(var as []Value)
	m['var'] = arr
	dump(m)
	if item := m['var'] {
		assert (item as []Value)[1] == Value(2.0)
	} else {
		assert false
	}
}
