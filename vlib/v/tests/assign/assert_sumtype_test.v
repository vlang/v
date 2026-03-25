struct Moon {}

struct Mars {}

type World = Mars | Moon

interface Val {}

const expected_val = 'cool'

fn get_val() Val {
	return expected_val
}

fn test_assert_sumtype() {
	w := World(Moon{})
	assert w is Moon
}

fn test_consecutive_assert_smartcast() {
	v := get_val()
	assert v is string
	assert v == expected_val
}
