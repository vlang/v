fn main() {
	pairs := [
		[1, 2]!,
		[2, 3]!,
		[1, 0]!,
	]
	mut got := []int{}
	for pair in pairs {
		got << pair[0]
		got << pair[1]
	}
	println(got)
	for i := 0; i < pairs.len; i++ {
		pair := pairs[i]
		println('${pair[0]},${pair[1]}')
	}
}
