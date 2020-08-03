module main

import hash.fnv1a
import hash as wyhash
import rand
import time

fn main() {
	rand.seed([u32(42), 0])
	mut aaa := u64(0)
	sample_size := 10000000
	min_str_len := 20
	max_str_len := 40
	println('Generating $sample_size strings between $min_str_len - $max_str_len chars long...')
	mut bytepile := []byte{}
	for _ in 0 .. sample_size * max_str_len {
		bytepile << byte(rand.int_in_range(40, 125))
	}
	mut str_lens := []int{}
	for _ in 0 .. sample_size {
		str_lens << rand.int_in_range(min_str_len, max_str_len)
	}
	println('Hashing each of the generated strings...')
	t0 := time.ticks()
	mut start_pos := 0
	aaa = 0
	for len in str_lens {
		end_pos := start_pos + len
		str := string(bytepile[start_pos..end_pos], len)
		aaa ^= wyhash.wyhash_c(&str.str, u64(str.len), 1)
		start_pos = end_pos
	}
	println('aaa: $aaa')
	t1 := time.ticks()
	d1 := t1 - t0
	println(' * wyhash4 C: ${d1}ms')
	start_pos = 0
	aaa = 0
	for len in str_lens {
		end_pos := start_pos + len
		str := string(bytepile[start_pos..end_pos], len)
		aaa ^= wyhash.sum64_string(str, 1)
		start_pos = end_pos
	}
	println('aaa: $aaa')
	t2 := time.ticks()
	d2 := t2 - t1
	println(' * wyhash4: ${d2}ms')
	start_pos = 0
	aaa = 0
	for len in str_lens {
		end_pos := start_pos + len
		str := string(bytepile[start_pos..end_pos], len)
		aaa ^= fnv1a.sum64_string(str)
		start_pos = end_pos
	}
	println('aaa: $aaa')
	t3 := time.ticks()
	d3 := t3 - t2
	println(' * fnv1a64: ${d3}ms')
}
