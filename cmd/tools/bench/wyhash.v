module main

import hash.fnv1a
import hash as wyhash
import rand
import benchmark

fn main() {
	rand.seed([u32(42), 0])
	sample_size := 10000000
	min_str_len := 20
	max_str_len := 40
	println('Generating $sample_size strings between $min_str_len - $max_str_len chars long...')
	mut checksum := u64(0)
	mut start_pos := 0
	mut bgenerating := benchmark.start()
	mut bytepile := []byte{}
	for _ in 0 .. sample_size * max_str_len {
		bytepile << byte(rand.int_in_range(40, 125))
	}
	mut str_lens := []int{}
	for _ in 0 .. sample_size {
		str_lens << rand.int_in_range(min_str_len, max_str_len)
	}
	bgenerating.measure('generating strings')
	println('Hashing each of the generated strings...')
	//
	mut bhashing_1 := benchmark.start()
	start_pos = 0
	checksum = 0
	for len in str_lens {
		end_pos := start_pos + len
		str := string(bytepile[start_pos..end_pos], len)
		checksum ^= wyhash.wyhash_c(&str.str, u64(str.len), 1)
		start_pos = end_pos
	}
	bhashing_1.measure('wyhash.wyhash_c     | checksum: ${checksum:22}')
	mut bhashing_2 := benchmark.start()
	start_pos = 0
	checksum = 0
	for len in str_lens {
		end_pos := start_pos + len
		str := string(bytepile[start_pos..end_pos], len)
		checksum ^= wyhash.sum64_string(str, 1)
		start_pos = end_pos
	}
	bhashing_2.measure('wyhash.sum64_string | checksum: ${checksum:22}')
	mut bhashing_3 := benchmark.start()
	start_pos = 0
	checksum = 0
	for len in str_lens {
		end_pos := start_pos + len
		str := string(bytepile[start_pos..end_pos], len)
		checksum ^= fnv1a.sum64_string(str)
		start_pos = end_pos
	}
	bhashing_3.measure('fnv1a.sum64_string  | checksum: ${checksum:22}')
}
