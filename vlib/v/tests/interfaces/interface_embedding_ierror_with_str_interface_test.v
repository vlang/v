// `Stringy` is satisfied by interfaces too, `IError` among them through its own
// `str` method. `ESpeaker` embeds IError but has no `str` dispatcher of its own;
// the `Stringy.str` dispatch must not call one.
interface Stringy {
	str() string
}

fn stringify(value Stringy) string {
	return value.str()
}

interface ESpeaker {
	IError
	speak() string
}

struct MyError {
	Error
}

fn (e MyError) speak() string {
	return 'world'
}

fn fails() !string {
	return MyError{}
}

fn test_str_interface_with_an_ierror_embedding_interface_compiles() {
	x := fails() or {
		if err is ESpeaker {
			err.speak()
		} else {
			'undefined'
		}
	}
	assert x == 'world'
	assert stringify(1) == '1'
}
