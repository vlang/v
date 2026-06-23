fn main() {
	mut matrix := [][]int{len: 4}
	mut worklist := []int{}
	worklist << 2
	idx := worklist.pop()
	matrix[idx] << 10
	matrix[idx] << 5
	mut total := 0
	for value in matrix[2] {
		total += value
	}
	println(total)
}
