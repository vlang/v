import bf

import rand

fn test_bf_new_size_del() {
	mut result := 1
	instance := bf.new(5)
	if bf.size(instance) != 5 {result = 0}
	bf.del(instance)
	if bf.size(instance) == 5 {result = 0}
	assert result == 1
}

fn test_bf_set_clear_toggle_get() {
	mut result := 1
	instance := bf.new(5)
	bf.setbit(instance, 4)
	if bf.getbit(instance, 4) != 1 {result = 0}
	bf.clearbit(instance, 4)
	if bf.getbit(instance, 4) != 0 {result = 0}
	bf.togglebit(instance, 4)
	if bf.getbit(instance, 4) != 1 {result = 0}
	assert result == 1
}

fn test_bf_and_not_or_xor() {
	rand.seed()
	len := 80
	input1 := bf.new(len)
	input2 := bf.new(len)
	mut i := 0
	for i < len {
		if rand.next(2) == 1 {
			bf.setbit(input1, i)
		}
		if rand.next(2) == 1{
			bf.setbit(input2, i)
		}
		i++
	}
	output1 := bf.bfxor(input1, input2)
	bfand := bf.bfand(input1, input2)
	bfor := bf.bfor(input1, input2)
	bfnot := bf.bfnot(bfand)
	output2 := bf.bfand(bfor, bfnot)
	mut result := 1
	for i < len {
		if bf.getbit(output1, i) != bf.getbit(output2, i) {result = 0}
	}
	assert result == 1
}
