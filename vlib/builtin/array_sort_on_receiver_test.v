type Scores = []int

pub fn (mut scores Scores) top_three() []int {
	mut result := scores.sorted(b < a)
	result.trim(3)
	return result
}

fn test_main() {
	a := Scores([3, 2, 1])
	b := a.sorted()
	assert dump(a) == [3, 2, 1]
	assert dump(b) == [1, 2, 3]
}
