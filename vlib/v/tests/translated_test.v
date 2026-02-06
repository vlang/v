@[translated]
module main

fn test_NotSnakeCaseFunction() {
	assert true
	assert 8 == 2 * 4
	assert 2 * 3 == 6
}

const ssf = [1, 2, 3]!

fn test_const_name_without_main_prefix() {
	assert ssf[0] == 1
}

struct ASMOperand {
	constraint [2]i8
}

fn test_pointer_assign_without_memcpy() {
	op := ASMOperand{
		constraint: [i8(5), 4]!
	}
	str := &i8(0)
	str = op.constraint
	assert !isnil(str)

	ops := [3]ASMOperand{}
	pop := &ASMOperand(0)
	pop = ops
	assert pop[1].constraint[0] == 0
}

struct StubIndex {
pub mut:
	data [5][5]map[string]string
}

fn test_pointer_assign_with_memcpy() {
	mut s := StubIndex{}
	s.data[1] = [5]map[string]string{}

	mut data := s.data[1]
	data[0]['abc'] = '123'
	k := data[0]['abc']
	assert k == '123'
}
