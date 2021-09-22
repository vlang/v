fn identity(s string) string {
	return s
}

fn main() {
	sa := 'abc'
	sb := 'def'
	sc := sa + sb
	sd := identity(sc)
	if sc != sd {
		exit(1)
	}
}
