import rand.config

const data = [1, 2, 3, 4, 5, 6, 7, 8]

fn test_validate_for_accepts_a_full_range() {
	config.ShuffleConfigStruct{}.validate_for(data) or { panic(err) }
	config.ShuffleConfigStruct{
		start: 0
		end:   data.len
	}.validate_for(data) or {
		panic(err)
	}
	config.ShuffleConfigStruct{
		start: 3
		end:   7
	}.validate_for(data) or {
		panic(err)
	}
	config.ShuffleConfigStruct{
		start: 3
	}.validate_for(data) or {
		panic(err)
	}
}

fn test_validate_for_rejects_an_out_of_range_start() {
	for bad in [-1, data.len, data.len + 1] {
		config.ShuffleConfigStruct{
			start: bad
		}.validate_for(data) or {
			assert err.msg() == "argument 'config.start' must be in range [0, a.len)"
			continue
		}
		assert false, 'start=${bad} should have been rejected'
	}
}

fn test_validate_for_rejects_an_out_of_range_end() {
	for bad in [-1, data.len + 1] {
		config.ShuffleConfigStruct{
			start: 0
			end:   bad
		}.validate_for(data) or {
			assert err.msg() == "argument 'config.end' must be in range [0, a.len]"
			continue
		}
		assert false, 'end=${bad} should have been rejected'
	}
}

fn test_validate_for_rejects_end_not_after_start() {
	for pair in [[4, 4], [5, 4]] {
		config.ShuffleConfigStruct{
			start: pair[0]
			end:   pair[1]
		}.validate_for(data) or {
			assert err.msg() == "argument 'config.end' must be greater than 'config.start'"
			continue
		}
		assert false, 'start=${pair[0]} end=${pair[1]} should have been rejected'
	}
}

fn test_validate_for_empty_array_rejects_every_start() {
	empty := []int{}
	config.ShuffleConfigStruct{}.validate_for(empty) or {
		assert err.msg() == "argument 'config.start' must be in range [0, a.len)"
		return
	}
	assert false, 'a start index of 0 into an empty array should have been rejected'
}
