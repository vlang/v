type MyArray = [10]int
type MyArray2 = []int

fn test_main() {
	a := MyArray{}

	assert dump(a.len) == 10

	mut count := 0
	for w in a {
		dump(w)
		count++
	}
	assert count == 10

	b := MyArray2{}
	z := dump(b)
	assert z.len == 0
}
