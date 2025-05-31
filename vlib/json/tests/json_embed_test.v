import json

struct Json3 {
	embed f64
}

struct Json2 {
	Json3
	inner []f64
}

struct Json {
	Json2
	test f64
}

fn test_main() {
	str := '{"inner": [1, 2, 3, 4, 5],"test": 1.0, "embed": 2.0}'
	data := json.decode(Json, str) or {
		eprintln('Failed to decode json, error: ${err}')
		return
	}
	println(data)
	assert data.inner.len == 5
	assert data.inner[0] == 1.0
	assert data.inner[4] == 5.0
	assert data.test == 1.0
	assert data.embed == 2.0

	assert dump(json.encode(data)) == '{"embed":2,"inner":[1,2,3,4,5],"test":1}'
}
