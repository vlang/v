module main

fn main() {
	mut lists := [][]int{len: 4}
	for i := 0; i < lists.len; i++ {
		lists[i] << (i + 10)
		lists[i] << (i + 20)
	}
	mut total := 0
	for i := 0; i < lists.len; i++ {
		total += lists[i].len
		total += lists[i][0]
		total += lists[i][1]
	}
	println(total)
	println(lists[3][0])
	println(lists[3][1])
}
