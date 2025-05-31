// Copyright (c) 2023 Kim Shrier. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// Package blake3 implements the Blake3 cryptographic hash
// as described in:
// https://github.com/BLAKE3-team/BLAKE3-specs/blob/master/blake3.pdf
// Version 20211102173700

module blake3

// build input strings of various lengths
const fua = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
const bua = 'ZYXWVUTSRQPONMLKJIHGFEDCBA'

const fla = 'abcdefghijklmnopqrstuvwxyz'
const bla = 'zyxwvutsrqponmlkjihgfedcba'

const fnr = '0123456789'
const bnr = '9876543210'

const ll = fua + ' ' + fla + ' ' + fnr + ' ' + bnr + ' ' + bla + ' ' + bua

const lb = ll + '\n' + ll + '\n' + ll + '\n' + ll + '\n' + ll + '\n' + ll + '\n' + ll + '\n' + ll

struct TestInput {
	input_string string
	key_words    []u32
	chunk_number u64
	flags        u32
}

struct TestCase {
	input   TestInput // inputs to the chunk's process_input method
	results Chunk     // the resulting chunk
	words   []u32     // the final output state as 32-bit words
}

// Chunk structures after processing varying amounts of input
//
// The expected values were derived from instrumenting the C reference
// implementation and dumping the appropriate values from their variables
// when processing the same input as the test case.
const test_cases = [
	// chunk 0 with 0 bytes input
	TestCase{
		input:   TestInput{
			input_string: ''
			key_words:    iv
			chunk_number: 0
			flags:        0
		}
		results: Chunk{
			chunk_number:   0
			chaining_value: iv.clone()
			block_words:    [u32(0), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
			flags:          0x0b
		}
		words:   [u32(0xb94913af), 0xa6a1f9f5, 0xea4d40a0, 0x49c9dc36, 0xc925cb9b, 0xb712c1ad,
			0xca939acc, 0x62321fe4, 0xe7030fe0, 0x6bf29ab6, 0x9ff0aa7f, 0x503033cd, 0xe0df8d33,
			0x86ccb885, 0x208ba99c, 0x3a24086c]
	},
	// chunk 0 with 1 byte input
	TestCase{
		input:   TestInput{
			input_string: 'A'
			key_words:    iv
			chunk_number: 0
			flags:        0
		}
		results: Chunk{
			chunk_number:   0
			chaining_value: iv.clone()
			block_words:    [u32(0x00000041), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
			flags:          0x0b
		}
		words:   [u32(0xfa4b6832), 0x4dc8c028, 0x1105216f, 0xfc0eceaa, 0x88c77151, 0x89ba4891,
			0xa25a8d20, 0x98fa0597, 0x4c0088e3, 0xafa7cbb8, 0xc586c5f7, 0x66dbef0e, 0x8c91d56a,
			0xa0b3daff, 0xe919be05, 0x997a1ce7]
	},
	// chunk 0 with 3 bytes input
	TestCase{
		input:   TestInput{
			input_string: 'abc'
			key_words:    iv
			chunk_number: 0
			flags:        0
		}
		results: Chunk{
			chunk_number:   0
			chaining_value: iv.clone()
			block_words:    [u32(0x00636261), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
			flags:          0x0b
		}
		words:   [u32(0xacb33764), 0x33514638, 0x753bb6ff, 0xb58d3a27, 0x4658c548, 0x03db795d,
			0x6c9c35fd, 0x859dbdd5, 0xae50b21f, 0xd0f59373, 0x5db61328, 0x490d1a52, 0x9ca09b2d,
			0x4c7fcef7, 0xf200d9ff, 0x0bbf7433]
	},
	// chunk 0 with 63 bytes input
	TestCase{
		input:   TestInput{
			input_string: ll[..63]
			key_words:    iv
			chunk_number: 0
			flags:        0
		}
		results: Chunk{
			chunk_number:   0
			chaining_value: iv.clone()
			block_words:    [u32(0x44434241), 0x48474645, 0x4c4b4a49, 0x504f4e4d, 0x54535251,
				0x58575655, 0x61205a59, 0x65646362, 0x69686766, 0x6d6c6b6a, 0x71706f6e, 0x75747372,
				0x79787776, 0x3130207a, 0x35343332, 0x00383736]
			flags:          0x0b
		}
		words:   [u32(0x39c41fc6), 0x2dd7c57b, 0xb8b16421, 0x360cbedb, 0x462d5672, 0x56713bb5,
			0x15132543, 0x7a92c4ba, 0x15c4ac13, 0x2354f573, 0xdc14f100, 0x35be07e8, 0xf98b17b9,
			0x573f38f8, 0xc8d2fbf4, 0xc06588e5]
	},
	// chunk 0 with 64 bytes input
	TestCase{
		input:   TestInput{
			input_string: ll[..64]
			key_words:    iv
			chunk_number: 0
			flags:        0
		}
		results: Chunk{
			chunk_number:   0
			chaining_value: iv.clone()
			block_words:    [u32(0x44434241), 0x48474645, 0x4c4b4a49, 0x504f4e4d, 0x54535251,
				0x58575655, 0x61205a59, 0x65646362, 0x69686766, 0x6d6c6b6a, 0x71706f6e, 0x75747372,
				0x79787776, 0x3130207a, 0x35343332, 0x39383736]
			flags:          0x0b
		}
		words:   [u32(0x6010817a), 0x21deb495, 0x0826485b, 0x6f895da5, 0x9363242a, 0x176b60a9,
			0x383215a7, 0x2b95570f, 0x57fe7082, 0x45b13a10, 0x007af189, 0x4b5e7ec7, 0x9574b5d8,
			0x109362a0, 0x282d14c2, 0x3a134380]
	},
	// chunk 0 with 65 bytes input
	TestCase{
		input:   TestInput{
			input_string: ll[..65]
			key_words:    iv
			chunk_number: 0
			flags:        0
		}
		results: Chunk{
			chunk_number:   0
			chaining_value: [u32(0xbb99f549), 0x3b4b2903, 0x436d199e, 0x6eea5980, 0x82ebb968,
				0x33cc3c4a, 0x90f4944b, 0x9480e10a]
			block_words:    [u32(0x00000020), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
			flags:          0x0a
		}
		words:   [u32(0xb06cf0f5), 0xd6f8f23d, 0x0f06389e, 0x20dec0a4, 0x69a20569, 0xdbbb4453,
			0x09f0bb52, 0xe22c6707, 0x6b530f62, 0x9ac8bfbd, 0xc0aa57b0, 0xdb30223c, 0xd6e52c79,
			0x42d84d38, 0xcfb4105f, 0xb42f1bd3]
	},
	// chunk 0 with 127 bytes input
	TestCase{
		input:   TestInput{
			input_string: ll[..127]
			key_words:    iv
			chunk_number: 0
			flags:        0
		}
		results: Chunk{
			chunk_number:   0
			chaining_value: [u32(0xbb99f549), 0x3b4b2903, 0x436d199e, 0x6eea5980, 0x82ebb968,
				0x33cc3c4a, 0x90f4944b, 0x9480e10a]
			block_words:    [u32(0x37383920), 0x33343536, 0x20303132, 0x7778797a, 0x73747576,
				0x6f707172, 0x6b6c6d6e, 0x6768696a, 0x63646566, 0x5a206162, 0x56575859, 0x52535455,
				0x4e4f5051, 0x4a4b4c4d, 0x46474849, 0x00434445]
			flags:          0x0a
		}
		words:   [u32(0x589a304d), 0x49f8a607, 0x55a03867, 0xe4fec410, 0x1a6bb2f6, 0x11dfecb3,
			0xf9989552, 0xb2d18382, 0x9fc6329a, 0xcc93199f, 0x5431cfc5, 0x1f5bddc6, 0x1d039fc5,
			0x09af900e, 0x55ce0ba2, 0x9707b1e6]
	},
	// chunk 0 with 128 bytes input
	TestCase{
		input:   TestInput{
			input_string: ll[..128]
			key_words:    iv
			chunk_number: 0
			flags:        0
		}
		results: Chunk{
			chunk_number:   0
			chaining_value: [u32(0xbb99f549), 0x3b4b2903, 0x436d199e, 0x6eea5980, 0x82ebb968,
				0x33cc3c4a, 0x90f4944b, 0x9480e10a]
			block_words:    [u32(0x37383920), 0x33343536, 0x20303132, 0x7778797a, 0x73747576,
				0x6f707172, 0x6b6c6d6e, 0x6768696a, 0x63646566, 0x5a206162, 0x56575859, 0x52535455,
				0x4e4f5051, 0x4a4b4c4d, 0x46474849, 0x42434445]
			flags:          0x0a
		}
		words:   [u32(0xd0d12158), 0x8802f9a4, 0x5bd125fb, 0xf2751b9d, 0x8fb2a4d2, 0x27744bfa,
			0x6ea287b1, 0xae9cfdb2, 0x8e0c2651, 0xeb2cfa50, 0x84654cbf, 0xb97b6f7b, 0xd2d737a2,
			0x46eaad72, 0x4d0235f0, 0xcaf8abb7]
	},
	// chunk 0 with 129 bytes input
	TestCase{
		input:   TestInput{
			input_string: ll[..129]
			key_words:    iv
			chunk_number: 0
			flags:        0
		}
		results: Chunk{
			chunk_number:   0
			chaining_value: [u32(0xccf04979), 0x9cbf983e, 0x9e274997, 0xb88c707b, 0x482b00d8,
				0x7aedc034, 0x1efdc297, 0x4de9f7c5]
			block_words:    [u32(0x00000041), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
			flags:          0x0a
		}
		words:   [u32(0xa896b53e), 0x1a64b264, 0xb08c7ea7, 0x09990e6d, 0x30470999, 0x762e9f2c,
			0xb6c7bf5f, 0x64fd723a, 0x6cb02e2f, 0xa3849bf2, 0xede8ea18, 0x14c88505, 0xfbf2ad67,
			0x6bc0a779, 0x8e731b77, 0x643a82c6]
	},
	// chunk 0 with 1023 bytes input
	TestCase{
		input:   TestInput{
			input_string: lb[..1023]
			key_words:    iv
			chunk_number: 0
			flags:        0
		}
		results: Chunk{
			chunk_number:   0
			chaining_value: [u32(0x78fd494b), 0xcd7eeddd, 0x0cb98e9b, 0x7a6a754e, 0x38ff2d32,
				0x88c4ca4c, 0xbc7baf18, 0xf7684da9]
			block_words:    [u32(0x207a7978), 0x33323130, 0x37363534, 0x39203938, 0x35363738,
				0x31323334, 0x797a2030, 0x75767778, 0x71727374, 0x6d6e6f70, 0x696a6b6c, 0x65666768,
				0x61626364, 0x58595a20, 0x54555657, 0x00515253]
			flags:          0x0a
		}
		words:   [u32(0x96bcc611), 0x8ccfc351, 0x89ec78f7, 0x2f748832, 0xf75ee10a, 0xc739f876,
			0x6adddebb, 0xe28853ab, 0x6983883d, 0x1ca0378d, 0x11f4296c, 0x6638ad9a, 0x0c639f8a,
			0xebf03d1f, 0x2c0e3844, 0x0989b826]
	},
	// chunk 0 with 1024 bytes input
	TestCase{
		input:   TestInput{
			input_string: lb[..1024]
			key_words:    iv
			chunk_number: 0
			flags:        0
		}
		results: Chunk{
			chunk_number:   0
			chaining_value: [u32(0x78fd494b), 0xcd7eeddd, 0x0cb98e9b, 0x7a6a754e, 0x38ff2d32,
				0x88c4ca4c, 0xbc7baf18, 0xf7684da9]
			block_words:    [u32(0x207a7978), 0x33323130, 0x37363534, 0x39203938, 0x35363738,
				0x31323334, 0x797a2030, 0x75767778, 0x71727374, 0x6d6e6f70, 0x696a6b6c, 0x65666768,
				0x61626364, 0x58595a20, 0x54555657, 0x50515253]
			flags:          0x0a
		}
		words:   [u32(0x50dbfcc6), 0x7dd05a7f, 0xa641cc37, 0x11721a4e, 0x6f33eea2, 0x834877a1,
			0x1cb36c9c, 0xf8d78dce, 0xb7539a0e, 0x7c7b57f4, 0xeef982da, 0x82c6c442, 0x8a451e9b,
			0xb9cc8414, 0xdef7ad58, 0x65ecfb6b]
	},
	// chunk 1 with 1 byte input
	// this is what the second chunk of input sees
	// after the first 1024 bytes have been consumed
	TestCase{
		input:   TestInput{
			input_string: 'O'
			key_words:    iv
			chunk_number: 1
			flags:        0
		}
		results: Chunk{
			chunk_number:   1
			chaining_value: iv.clone()
			block_words:    [u32(0x0000004f), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
			flags:          0x0b
		}
		words:   [u32(0xfd795319), 0x4448fd94, 0xa8054dbb, 0x526517ad, 0x7e8e2e4c, 0xf54cf835,
			0xb498c9a7, 0x341396fa, 0x753298b2, 0xd721328c, 0x4013c5d6, 0xaf64d891, 0x77893790,
			0xe486143b, 0x13764172, 0x0cae81d0]
	},
]

fn test_various_test_cases() {
	for test_case in test_cases {
		mut chunk := Chunk{}
		input := test_case.input
		chunk.process_input(input.input_string.bytes(), input.key_words, input.chunk_number,
			input.flags, true)

		results := test_case.results
		assert chunk.chunk_number == results.chunk_number

		for i, value in chunk.chaining_value {
			assert value == results.chaining_value[i], 'i: ${i}, left: ${value:08x} right: ${results.chaining_value[i]:08x}'
		}

		for i in 0 .. 16 {
			assert chunk.block_words[i] == results.block_words[i], 'i: ${i}, left: ${chunk.block_words[i]:08x} right: ${results.block_words[i]}'
		}

		assert chunk.flags == results.flags
	}
}
