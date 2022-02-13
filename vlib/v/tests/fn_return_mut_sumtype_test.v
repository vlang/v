type Sum = Struct | int

struct Struct {
mut:
	value int
}

fn update(mut s Struct) Sum {
	s.value += 1
	return s
}

fn test_fn_return_mut_sumtype() {
	mut s := Sum(Struct{
		value: 1
	})
	if mut s is Struct {
		s = update(mut s)
		assert s.value == 2
	} else {
		assert false
	}
}
