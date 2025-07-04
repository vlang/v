module arrays

// map_of_indexes returns a map, where each key is an unique value in `array`.
// Each value in that map for that key, is an array, containing the indexes in `array`, where that value has been found.
// Example: arrays.map_of_indexes([1,2,3,4,4,2,1,4,4,999]) == {1: [0, 6], 2: [1, 5], 3: [2], 4: [3, 4, 7, 8], 999: [9]}
pub fn map_of_indexes[T](array []T) map[T][]int {
	mut result := map[T][]int{}
	for i, e in array {
		if _ := result[e] {
			result[e] << i
		} else {
			result[e] = [i]
		}
	}
	return result
}

// map_of_counts returns a map, where each key is an unique value in `array`.
// Each value in that map for that key, is how many times that value occurs in `array`.
// It can be useful for building histograms of discrete measurements.
// Example: arrays.map_of_counts([1,2,3,4,4,2,1,4,4]) == {1: 2, 2: 2, 3: 1, 4: 4}
pub fn map_of_counts[T](array []T) map[T]int {
	mut result := map[T]int{}
	for e in array {
		result[e]++
	}
	return result
}
