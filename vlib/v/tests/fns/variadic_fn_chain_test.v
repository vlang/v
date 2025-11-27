fn called(s string, l ...string) {}

fn caller(s string, l ...string) {
	called(s, l)
}

fn test_main() {
	caller('')
}
