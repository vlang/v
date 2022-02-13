import rand

fn test_rand_bytes() ? {
	mut randoms := []string{}
	for i in 0 .. 100 {
		x := rand.bytes(i) ?.hex()
		if x.len > 0 {
			randoms << x
		}
		assert x.len == i * 2
	}
	mut differences := 0
	for idx in 1 .. randoms.len {
		start := randoms[idx]#[0..8]
		prev_start := randoms[idx - 1]#[0..8]
		if start != prev_start {
			differences++
		}
	}
	assert differences > 95 // normally around 98
	dump(differences)
}

fn test_rand_read() ? {
	max := 50
	mut a := []byte{len: max}
	mut differences := 0
	for j in 1 .. max {
		start := '00'.repeat(j)
		for k in j + 1 .. max {
			end := '00'.repeat(max - k)
			middle := '00'.repeat(k - j)
			// eprintln('> j: $j | k: $k | start: $start | middle: $middle | end: $end')
			for i in 0 .. max {
				a[i] = 0
			}
			assert a[j..k].hex() == middle
			for i in 0 .. 10 {
				rand.read(mut a[j..k])
				// dump(a.hex())
				assert a[0..j].hex() == start
				assert a[k..].hex() == end
				if a[j..k].hex() != middle {
					differences++
				}
			}
		}
	}
	dump(differences)
	assert differences > 11700 // normally around 11758
}
