// Common interface to all error custom types.
interface ESpeaker {
	IError
	speak() string
}

// One custom error implementation.
struct MyError {
	// Mandatory fields from IError.
	msg  string
	code int
	// Custom field.
	blah string
}

// Interface implementation for this example custom type.
fn (e MyError) speak() string {
	return e.blah
}

fn (e MyError) msg() string {
	return e.msg
}

fn (e MyError) code() int {
	return e.code
}

// An example function that returns a custom error.
fn foo() ?string {
	return MyError{
		msg: 'foo'
		blah: 'world'
	}
}

fn test_interface_embedding_smartcast() {
	x := foo() or {
		if err is ESpeaker {
			err.speak()
		} else {
			'undefined'
		}
	}
	println(x)
	assert x == 'world'
}
