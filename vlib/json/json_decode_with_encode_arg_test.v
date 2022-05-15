import json

struct TodoDto {
	foo int
}

fn test_decode_with_encode_arg() ? {
	body := TodoDto{}
	ret := json.decode(TodoDto, json.encode(body))?
	println(ret)
	assert ret.foo == 0
}
