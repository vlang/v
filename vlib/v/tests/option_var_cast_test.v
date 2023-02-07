type SumType = f64 | int

struct Struct {
	a ?int
}

type Alias = int

fn test_main() {
	arr := [1, 2, 3]
	mut t := ?[]int([]int{len: 10, init: 2})
	assert t != none
	t = ?[]int(arr)
	assert t != none
	t = ?[]int([]int{len: 1, init: 0})
	assert t != none

	mut t2 := ?int(1)
	assert t2 != none
	mut t3 := ?f64(1.2)
	assert t3 != none
	mut t4 := ?string('')
	assert t4 != none
	mut t5 := ?SumType(1)
	assert t5 != none
	mut t6 := ?SumType(none)
	assert t6 == none
	mut t7 := ?Struct(Struct{})
	assert t7 == none
}
