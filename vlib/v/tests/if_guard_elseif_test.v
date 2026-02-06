fn byte_str(num u32) !string {
	return if num > 0xff { error('larger than byte') } else { num.str() }
}

fn short_str(num int) !string {
	return if num > 0xffff { error('larger than short') } else { num.str() }
}

fn test_main() {
	num := u32(0xfffff)
	mut str := ''
	if bs := byte_str(num) {
		str = 'byte: ${bs}'
	} else if ss := short_str(int(num)) {
		str = 'short: ${ss}'
	} else {
		str = 'err: ${err}'
	}

	assert str.contains('err:')
}
