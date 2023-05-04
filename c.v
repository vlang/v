fn test() (int, int) {
	return 1, 2
}

fn atest() (int, int, int) {
	return 1, test()
}