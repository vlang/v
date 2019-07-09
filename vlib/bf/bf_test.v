import bf

import rand

fn test_bf_new_size() {
	instance := bf.new(5)
	assert instance.getsize() == 5
}

fn test_bf_set_clear_toggle_get() {
	mut instance := bf.new(5)
	instance.setbit(4)
	assert instance.getbit(4) == 1
	instance.clearbit(4)
	assert instance.getbit(4) == 0
	instance.togglebit(4)
	assert instance.getbit(4) == 1
}

fn test_bf_and_not_or_xor() {
	rand.seed()
	len := 80
	mut input1 := bf.new(len)
	mut input2 := bf.new(len)
	mut i := 0
	for i < len {
		if rand.next(2) == 1 {
			input1.setbit(i)
		}
		if rand.next(2) == 1{
			input2.setbit(i)
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
		if output1.getbit(i) != output2.getbit(i) {result = 0}
	}
	assert result == 1
}
