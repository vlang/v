type Sum = Struct1 | int

struct Struct1 {
mut:
	value int
}

fn update_sum(mut sum Sum) {
	match mut sum {
		Struct1 {
			sum.value = 42
		}
		else {}
	}
}

fn test_fn_call_mut_sumtype_args() {
	mut s := Sum(Struct1{
		value: 6
	})

	update_sum(mut s)

	if mut s is Struct1 {
		println(s.value)
		assert s.value == 42
	} else {
		assert false
	}
}
