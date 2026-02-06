struct User {
	name string
	age  int
}

fn simple() {
	user := &User{'Bob', 31}
	println(user.name)
	println('done')
}

fn main() {
	simple()
}
