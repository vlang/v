type Sum1 = Struct1 | int

struct Struct1 {
mut:
	value int
}

type Sum2 = Struct1 | Struct2 | int

struct Struct2 {
mut:
	sum Sum2
}

fn update_sum_1(mut sum Sum1) {
	match mut sum {
		Struct1 {
			sum.value = 42
		}
		else {}
	}
}

fn update_sum_2(mut sum Sum2) {
	match mut sum {
		Struct1 {
			sum.value = 42
		}
		Struct2 {
			update_sum_2(mut sum.sum)
		}
		else {}
	}
}

fn test_fn_call_mut_sumtype_args() {
	mut s := Sum1(Struct1{
		value: 6
	})

	update_sum_1(mut s)

	if mut s is Struct1 {
		println(s.value)
		assert s.value == 42
	} else {
		assert false
	}
}

fn test_fn_call_mut_sumtype_args_field() {
	mut s := Sum2(Struct2{
		sum: Sum2(Struct1{
			value: 6
		})
	})

	update_sum_2(mut s)

	if mut s is Struct2 {
		if mut s.sum is Struct1 {
			println(s.sum.value)
			assert s.sum.value == 42
		} else {
			assert false
		}
	} else {
		assert false
	}
}
