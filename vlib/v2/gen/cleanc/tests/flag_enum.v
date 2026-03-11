module main

@[flag]
enum Perms {
	read
	write
	execute
}

fn main() {
	// .has() and .all()
	mut p := Perms.read | Perms.write
	println(p.has(.read))
	println(p.has(.execute))
	println(p.all(.read | .write))
	println(p.all(.read | .execute))

	// .set()
	p.set(.execute)
	println(p.has(.execute))
	println(p.all(.read | .write | .execute))

	// .clear()
	p.clear(.write)
	println(p.has(.write))
	println(p.has(.read))
}
