fn xyz() {
	for i in 0 .. 5 {
		println('${@FN} - i: ${i}')
	}
}

fn abc() {
	xyz()
	xyz()
}

fn main() {
	abc()
}
