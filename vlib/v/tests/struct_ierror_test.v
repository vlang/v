struct Test {
	err IError
}

fn test_init_with_none() {
	t := Test{
		err: none
	}
	// compiles successfully
}
