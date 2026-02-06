struct MyError {
	code  int
	state string
}

pub fn (e MyError) msg() string {
	return 'something went wrong'
}

pub fn (e MyError) code() int {
	return e.code
}

fn foo() ! {
	return MyError{}
}

fn test_interface_err_msg_print() {
	foo() or {
		if err is MyError {
			eprintln(err.msg())
			exit(err.code())
		} else {
			panic(err)
		}
	}
}
