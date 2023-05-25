struct Parent {
mut:
	val   string
	child struct {
	mut:
		val string
	}
}

fn test_main() {
	mut p := Parent{}
	p.child = struct {'abc'}
	assert p.child == struct {'abc'}
}
