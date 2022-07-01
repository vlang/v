import json

struct Result<T> {
	ok     bool
	result T
}

struct User {
	id       int
	username string
}

fn func<T>() ?T {
	text := '{"ok": true, "result":{"id":37467243, "username": "ciao"}}'
	a := json.decode(Result<T>, text)?
	return a.result
}

fn test_decode_with_generic_struct() ? {
	ret := func<User>()?
	println(ret)
	assert ret.id == 37467243
	assert ret.username == 'ciao'
}
