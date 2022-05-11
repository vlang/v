module hex

fn test_decode() ? {
	assert decode('')? == []
	assert decode('0')? == [u8(0x0)]
	assert decode('f')? == [u8(0xf)]
	assert decode('0f')? == [u8(0x0f)]
	assert decode('ff')? == [u8(0xff)]
	assert decode('123')? == [u8(0x1), 0x23]
	assert decode('1234')? == [u8(0x12), 0x34]
	assert decode('12345')? == [u8(0x1), 0x23, 0x45]
	assert decode('0123456789abcdef')? == [u8(0x01), 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef]
	assert decode('123456789ABCDEF')? == [u8(0x01), 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef]
}

fn test_decode_fails() ? {
	if x := decode('foo') {
		return error('expected decode to fail, got $x')
	}
	if x := decode('g') {
		return error('expected decode to fail, got $x')
	}
	if x := decode('000000000g') {
		return error('expected decode to fail, got $x')
	}
	if x := decode('_') {
		return error('expected decode to fail, got $x')
	}
	if x := decode('!') {
		return error('expected decode to fail, got $x')
	}
}

fn test_encode() ? {
	assert encode(decode('')?) == ''
	assert encode(decode('0')?) == '00'
	assert encode(decode('f')?) == '0f'
	assert encode(decode('0f')?) == '0f'
	assert encode(decode('ff')?) == 'ff'
	assert encode(decode('123')?) == '0123'
	assert encode(decode('1234')?) == '1234'
	assert encode(decode('12345')?) == '012345'
	assert encode(decode('abcdef')?) == 'abcdef'
	assert encode(decode('ABCDEF')?) == 'abcdef'
}

fn test_decode_0x() ? {
	assert decode('0x')? == []
	assert decode('0x0')? == [u8(0x0)]
	assert decode('0X1234')? == [u8(0x12), 0x34]
	assert decode('0x12345')? == [u8(0x1), 0x23, 0x45]
	assert decode('0x0123456789abcdef')? == [u8(0x01), 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef]
	assert decode('0X123456789ABCDEF')? == [u8(0x01), 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef]
}
