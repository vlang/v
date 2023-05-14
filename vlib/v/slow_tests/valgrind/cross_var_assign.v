fn main() {
	mut a := []string{len: 2}
	a[0] = 'x'
	a[1] = 'y'

	dump(a)
	a[0], a[1] = a[1], a[0]
	dump(a)
}
