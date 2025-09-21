module chacha20

import rand
import encoding.hex

// Test for Stream counter handling.
// See the discussion at [here](https://discord.com/channels/592103645835821068/592114487759470596/1417900997090607215)
fn test_stream_counter_handling() ! {
	// creates a original mode of the cipher with 64-bit counter
	mut ctx := new_cipher(rand.bytes(32)!, rand.bytes(8)!)!
	// set the cipher's counter near the maximum of 64-bit counter
	ctr := max_u64 - 2
	ctx.set_counter(ctr)

	// by setting internal counter into near of max 64-bit counter,
	// it need a message with minimum length of  2*block_size bytes to reach the limit.
	// let's build this message with 2 * block_size bytes in size
	msg0 := []u8{len: 2 * block_size}
	mut dst := []u8{len: msg0.len}
	ctx.xor_key_stream(mut dst, msg0)
	// at this step, the counter has reached the maximum_64bit_counter, but still not overflow
	assert ctx.Stream.overflow == false
	assert ctx.Stream.ctr() == max_64bit_counter

	// after above process, the counter should reach the maximum limit
	// we use keystream_full to test this counter handling, because
	// xor_key_stream would panic on counter reset
	msg1 := []u8{len: block_size}
	ctx.Stream.keystream_full(mut dst[..block_size], msg1) or {
		assert ctx.Stream.overflow == true
		assert err == error('chacha20: internal counter overflow')
		return
	}
}

fn test_qround_on_state() {
	mut s := State{}
	s[0] = 0x11111111
	s[1] = 0x01020304
	s[2] = 0x9b8d6f43
	s[3] = 0x01234567

	qround_on_state(mut s, 0, 1, 2, 3)
	assert s[0] == 0xea2a92f4
	assert s[1] == 0xcb1cf8ce
	assert s[2] == 0x4581472e
	assert s[3] == 0x5881c4bb
}

fn test_state_of_chacha20_block_simple() ! {
	key := '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f'
	key_bytes := hex.decode(key)!

	nonce := '000000090000004a00000000'
	nonce_bytes := hex.decode(nonce)!

	mut stream := new_stream(key_bytes, nonce_bytes)!

	mut block := []u8{len: block_size}
	stream.set_ctr(1)
	stream.keystream_full(mut block, block)!

	expected_raw_bytes := '10f1e7e4d13b5915500fdd1fa32071c4c7d1f4c733c068030422aa9ac3d46c4ed2826446079faa0914c2d705d98b02a2b5129cd1de164eb9cbd083e8a2503c4e'
	exp_bytes := hex.decode(expected_raw_bytes)!

	assert block == exp_bytes
}

fn test_keystream_encryption() ! {
	for val in blocks_testcases {
		key := hex.decode(val.key)!
		nonce := hex.decode(val.nonce)!

		mut stream := new_stream(key, nonce)!
		stream.set_ctr(val.counter)

		mut block := []u8{len: block_size}
		stream.keystream_full(mut block, block)!
		exp_bytes := hex.decode(val.output)!

		assert block == exp_bytes
	}
}

struct BlockCase {
	key     string
	nonce   string
	counter u32
	output  string
}

const blocks_testcases = [
	// section 2.3.4 https://datatracker.ietf.org/doc/html/rfc8439#section-2.3.2
	BlockCase{
		key:     '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f'
		nonce:   '000000090000004a00000000'
		counter: u32(1)
		output:  '10f1e7e4d13b5915500fdd1fa32071c4c7d1f4c733c068030422aa9ac3d46c4ed2826446079faa0914c2d705d98b02a2b5129cd1de164eb9cbd083e8a2503c4e'
	},
	// https://datatracker.ietf.org/doc/html/rfc8439#appendix-A.1.1
	BlockCase{
		key:     '0000000000000000000000000000000000000000000000000000000000000000'
		nonce:   '000000000000000000000000'
		counter: u32(0)
		output:  '76b8e0ada0f13d90405d6ae55386bd28bdd219b8a08ded1aa836efcc8b770dc7da41597c5157488d7724e03fb8d84a376a43b8f41518a11cc387b669b2ee6586'
	},
	// #appendix-A.1.2
	BlockCase{
		key:     '0000000000000000000000000000000000000000000000000000000000000000'
		nonce:   '000000000000000000000000'
		counter: u32(1)
		output:  '9f07e7be5551387a98ba977c732d080dcb0f29a048e3656912c6533e32ee7aed29b721769ce64e43d57133b074d839d531ed1f28510afb45ace10a1f4b794d6f'
	},
]
