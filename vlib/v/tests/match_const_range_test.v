const start = 1
const start_2 = 4
const end = 3
const end_2 = 8


fn test_match_const_ranges() {
	mut results := []int{}
	for x in 0 .. 10 {
		match x {
			start...end { results << 1 }
			start_2...5 { results << 2 }
			6...end_2 { results << -1 }
			else { results << 0 }
		}
	}
	assert results == [0, 1, 1, 1, 2, 2, -1, -1, -1, 0]
}
