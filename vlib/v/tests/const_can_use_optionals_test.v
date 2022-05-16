const (
	aaa = iopt()?
	bbb = sopt()?
)

fn iopt() ?int {
	return 1234
}

fn sopt() ?string {
	return 'xyz'
}

fn test_iconsts_are_resolved() {
	z := aaa
	assert z == 1234
}

fn test_sconsts_are_resolved() {
	z := bbb
	assert z == 'xyz'
}
