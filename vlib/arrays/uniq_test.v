import arrays

const a = [1, 5, 5, 1, 5, 2, 1, 1, 9, 2]

const s = [1, 1, 1, 1, 2, 2, 5, 5, 5, 9] // a.sorted()

const many_repeats = [1, 1, 1, 1, 2, 2, 5, 5, 5, 9, 1, 1, 9, 5, 5]

const astrings = ['b', 'b', 'a', 'b', 'z', 'a', 'a', 'd']

const sstrings = ['a', 'a', 'a', 'b', 'b', 'b', 'd', 'z'] // astrings.sorted()

fn test_uniq() {
	assert arrays.uniq([]int{}) == []
	assert arrays.uniq([1, 1]) == [1]
	assert arrays.uniq([2, 1]) == [2, 1]
	assert arrays.uniq(a) == [1, 5, 1, 5, 2, 1, 9, 2]
	assert arrays.uniq(s) == [1, 2, 5, 9]
	assert arrays.uniq(astrings) == ['b', 'a', 'b', 'z', 'a', 'd']
	assert arrays.uniq(sstrings) == ['a', 'b', 'd', 'z']
	assert arrays.uniq(many_repeats) == [1, 2, 5, 9, 1, 9, 5]
}

fn test_uniq_only() {
	assert arrays.uniq_only([]int{}) == []
	assert arrays.uniq_only([1, 5]) == [1, 5]
	assert arrays.uniq_only([5, 5]) == []
	assert arrays.uniq_only([5, 1, 1]) == [5]
	assert arrays.uniq_only(a) == [1, 1, 5, 2, 9, 2]
	assert arrays.uniq_only(s) == [9]
	assert arrays.uniq_only(astrings) == ['a', 'b', 'z', 'd']
	assert arrays.uniq_only(sstrings) == ['d', 'z']
	assert arrays.uniq_only(many_repeats) == [9, 9]
}

fn test_uniq_only_repeated() {
	assert arrays.uniq_only_repeated([]int{}) == []
	assert arrays.uniq_only_repeated([1, 5]) == []
	assert arrays.uniq_only_repeated([5, 5]) == [5]
	assert arrays.uniq_only_repeated([5, 5, 5]) == [5]
	assert arrays.uniq_only_repeated([5, 1, 1]) == [1]
	assert arrays.uniq_only_repeated([1, 1, 5]) == [1]
	assert arrays.uniq_only_repeated([1, 5, 1]) == []
	assert arrays.uniq_only_repeated([0, 5, 1, 1]) == [1]
	assert arrays.uniq_only_repeated([0, 1, 1, 5]) == [1]
	assert arrays.uniq_only_repeated([0, 1, 5, 1]) == []
	assert arrays.uniq_only_repeated([5, 1, 1, 0]) == [1]
	assert arrays.uniq_only_repeated([1, 1, 5, 0]) == [1]
	assert arrays.uniq_only_repeated([1, 5, 1, 0]) == []
	assert arrays.uniq_only_repeated([5, 5, 5, 5, 1]) == [5]
	assert arrays.uniq_only_repeated([1, 5, 5, 5, 5, 1]) == [5]
	assert arrays.uniq_only_repeated([1, 5, 5, 2, 5, 5, 3]) == [5, 5]
	assert arrays.uniq_only_repeated([1, 5, 5, 2, 5, 5, 3, 5]) == [5, 5]
	assert arrays.uniq_only_repeated([1, 5, 5, 2, 5, 5, 3, 5, 5]) == [5, 5, 5]
	assert arrays.uniq_only_repeated([5, 1, 5, 5, 2, 5, 5, 3, 5, 5]) == [5, 5, 5]
	assert arrays.uniq_only_repeated([5, 5, 1, 5, 5, 2, 5, 5, 3, 5, 5]) == [5, 5, 5, 5]
	assert arrays.uniq_only_repeated([5, 5, 5, 1, 5, 5, 2, 5, 5, 3, 5, 5]) == [5, 5, 5, 5]
	assert arrays.uniq_only_repeated(a) == [5, 1]
	assert arrays.uniq_only_repeated(s) == [1, 2, 5]
	assert arrays.uniq_only_repeated(many_repeats) == [1, 2, 5, 1, 5]
}

fn test_uniq_all_repeated() {
	assert arrays.uniq_all_repeated([]int{}) == []
	assert arrays.uniq_all_repeated([1, 5]) == []
	assert arrays.uniq_all_repeated([5, 5]) == [5, 5]
	assert arrays.uniq_all_repeated([5, 5, 5]) == [5, 5, 5]
	assert arrays.uniq_all_repeated([5, 1, 1]) == [1, 1]
	assert arrays.uniq_all_repeated([1, 1, 5]) == [1, 1]
	assert arrays.uniq_all_repeated([1, 5, 1]) == []
	assert arrays.uniq_all_repeated([0, 5, 1, 1]) == [1, 1]
	assert arrays.uniq_all_repeated([0, 1, 1, 5]) == [1, 1]
	assert arrays.uniq_all_repeated([0, 1, 5, 1]) == []
	assert arrays.uniq_all_repeated([5, 1, 1, 0]) == [1, 1]
	assert arrays.uniq_all_repeated([1, 1, 5, 0]) == [1, 1]
	assert arrays.uniq_all_repeated([1, 5, 1, 0]) == []
	assert arrays.uniq_all_repeated([5, 5, 5, 5, 1]) == [5, 5, 5, 5]
	assert arrays.uniq_all_repeated([1, 5, 5, 5, 5, 1]) == [5, 5, 5, 5]
	assert arrays.uniq_all_repeated([1, 5, 5, 2, 5, 5, 3]) == [5, 5, 5, 5]
	assert arrays.uniq_all_repeated([1, 5, 5, 2, 5, 5, 3, 5]) == [5, 5, 5, 5]
	assert arrays.uniq_all_repeated([1, 5, 5, 2, 5, 5, 3, 5, 5]) == [5, 5, 5, 5, 5, 5]
	assert arrays.uniq_all_repeated([5, 1, 5, 5, 2, 5, 5, 3, 5, 5]) == [5, 5, 5, 5, 5, 5]
	assert arrays.uniq_all_repeated([5, 5, 1, 5, 5, 2, 5, 5, 3, 5, 5]) == [5, 5, 5, 5, 5, 5, 5,
		5]
	assert arrays.uniq_all_repeated([5, 5, 5, 1, 5, 5, 2, 5, 5, 3, 5, 5]) == [5, 5, 5, 5, 5, 5,
		5, 5, 5]
	assert arrays.uniq_all_repeated(a) == [5, 5, 1, 1]
	assert arrays.uniq_all_repeated(s) == [1, 1, 1, 1, 2, 2, 5, 5, 5]
	assert arrays.uniq_all_repeated(many_repeats) == [1, 1, 1, 1, 2, 2, 5, 5, 5, 1, 1, 5, 5]
}

fn test_distinct() {
	assert arrays.distinct(a) == arrays.distinct(s)
	assert arrays.distinct(a) == [1, 2, 5, 9]
	assert arrays.distinct(many_repeats) == [1, 2, 5, 9]
}
