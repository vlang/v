import (
	hash.wyhash
	rand
	time
)

const (
	fnv64_prime = 1099511628211
	fnv64_offset_basis = 14695981039346656037
	fnv32_offset_basis = u32(2166136261)
	fnv32_prime = u32(16777619)
)

[inline]
fn fnv1a64(data string) u64 {
	mut hash := fnv64_offset_basis
	for i := 0; i < data.len; i++ {
		hash = (hash ^ u64(data[i])) * fnv64_prime
	}
	return hash
}

fn test_benchmark() {
    sample_size := 10000000
	min_word_len := 20
	max_word_len := 40
	mut bytepile := []byte
	for _ in 0..sample_size {
        for _ in 0..40 {
            bytepile << byte(40+rand.next(125-40))
        }
    }
	mut positions := []int
	for _ in 0..sample_size {
		positions << min_word_len+rand.next(max_word_len-min_word_len)
	}

	t0 := time.ticks()

	mut start_pos := 0
	for i, len in positions {
		end_pos := start_pos+len
		word := string(bytepile[start_pos..end_pos], len)
		_ = wyhash.wyhash_c(&word.str, u64(word.len), 1)
		start_pos = end_pos
	}
	t1 := time.ticks()
	d1 := t1-t0
	println('wyhash4 C: ${d1}ms')

	start_pos = 0
	for i, len in positions {
		end_pos := start_pos+len
		word := string(bytepile[start_pos..end_pos], len)
		_ = wyhash.sum64_string(word, 1)
		start_pos = end_pos
	}
	t2 := time.ticks()
	d2 := t2-t1
	println('wyhash4: ${d2}ms')

	start_pos = 0
	for i, len in positions {
		end_pos := start_pos+len
		word := string(bytepile[start_pos..end_pos], len)
		_ = fnv1a64(word)
		start_pos = end_pos
	}
	t3 := time.ticks()
	d3 := t3-t2
	println('fnv1a64: ${d3}ms')
}
