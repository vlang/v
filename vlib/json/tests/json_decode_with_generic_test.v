import json

struct Result[T] {
	ok     bool
	result T
}

struct User {
	id       int
	username string
}

struct Box[T] {
	items []T
}

fn func[T]() !T {
	text := '{"ok": true, "result":{"id":37467243, "username": "ciao"}}'
	a := json.decode(Result[T], text)!
	return a.result
}

fn string_decode[T](text string) !T {
	return json.decode(T, text)
}

fn open_box[T](text string) ![]T {
	box := string_decode[Box[T]](text)!
	return box.items
}

fn test_decode_with_generic_struct() {
	ret := func[User]()!
	println(ret)
	assert ret.id == 37467243
	assert ret.username == 'ciao'
}

fn test_decode_with_nested_generic_struct() {
	text := '{"items":[{},{"id":37467243,"username":"ciao"},{}]}'
	items := open_box[User](text)!
	assert items.len == 3
	assert items[0].id == 0
	assert items[0].username == ''
	assert items[1].id == 37467243
	assert items[1].username == 'ciao'
	assert items[2].id == 0
	assert items[2].username == ''
}
