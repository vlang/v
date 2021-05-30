fn main() {
	mut a := 'false'
	if true {
		b := 'test'
		defer {
			eprintln(b)
		}
	}
}