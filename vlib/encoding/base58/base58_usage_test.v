import encoding.base58
import encoding.hex

fn test_encode() ? {
	for input, expected in {
		'':                    ''
		'6263':                '2PMCRQ'
		'hello world':         'StV1DL6CwTryKyV'
		'\x00\x00hello world': '11StV1DL6CwTryKyV'
	} {
		output := base58.encode(input)
		println('> input: `$input` | $input.bytes().hex() | => output: `$output`')
		assert output == expected
	}
}

fn test_encode_int() ? {
	for input, expected in {
		0x6263:   '8VG'
		0x61:     '2g'
		0x626262: 'a3gV'
		0x636363: 'aPEr'
	} {
		output := base58.encode_int(input)?
		println('> input: 0x${input:x} | => output: `$output`')
		assert output == expected
	}
}

fn test_decode() ? {
	for output, expected in {
		'USm3fpXnKG5EUBx2ndxBDMPVciP5hGey2Jh4NDv6gmeo1LkMeiKrLJUUBk6Z': 'The quick brown fox jumps over the lazy dog.'
		'11StV1DL6CwTryKyV':                                            '\x00\x00hello world'
		'2NEpo7TZRRrLZSi2U':                                            'Hello World!'
		'14cxpo3MBCYYWCgF74SWTdcmxipnGUsPw3':                           hex.decode('0027b5891b01da2db74cde1689a97a2acbe23d5fb1c0205bf6')?.bytestr()
		'3vQB7B6MrGQZaxCuFg4oh':                                        hex.decode('68656c6c6f20776f726c64bc62d4b8')?.bytestr()
	} {
		input := base58.decode(output)?
		println('> output: `$output` | decoded input: `$input` | bytes: $input.bytes().hex()')
		assert input.bytes().hex() == expected.bytes().hex()
	}
}
