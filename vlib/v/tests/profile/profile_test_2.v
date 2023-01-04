import v.profile

fn abc() {
	eprintln(@FN)
}

fn main() {
	profile.on(false)
	for _ in 0 .. 3 {
		abc()
	}
	println('>>>>>>>>>>')

	profile.on(true)
	abc()
	profile.on(false)

	println('>>>>>>>>>>')
	for _ in 0 .. 3 {
		abc()
	}
	profile.on(true)
}
