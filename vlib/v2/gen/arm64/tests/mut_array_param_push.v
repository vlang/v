fn push_name(mut names []string, name string) {
	names << name
	println(names.len)
}

fn wrap(mut names []string, name string) {
	push_name(mut names, name)
	println(names.len)
}

fn main() {
	mut names := []string{}
	wrap(mut names, 'abc')
	println(names.len)
	if names.len > 0 {
		println(names[0])
	}
}
