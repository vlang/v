struct Abc {
mut:
	name     string
	children []Abc
}

[manualfree]
fn main() {
	mut a := &Abc{}
	a.name = 'aaa'
	a.children = [
		Abc{
			name: 'xyz'
			children: [
				Abc{
					name: 'xx'
				},
				Abc{
					name: 'yy'
				},
			]
		},
		Abc{
			name: 'def'
			children: [
				Abc{
					name: 'dd'
				},
				Abc{
					name: 'ee'
				},
			]
		},
	]
	dump(a)
	unsafe { a.free() }
	unsafe { free(a) }
}
