import json

struct Response[T] {
	time string
	cars []T
}

struct Car {
	brand string
	power int
}

fn string_to_response[T](text string) {
	result := json.decode([]Response[T], text) or { panic(err) }
	println(result)
	assert result.len == 2
	assert result[0].time == '12ms'
	assert result[0].cars[0].brand == 'Ferrari'
	assert result[0].cars[0].power == 232
	assert result[0].cars[1].brand == 'Lamborghini'
	assert result[0].cars[1].power == 345
	assert result[1].time == '12ms'
	assert result[1].cars[0].brand == 'Ferrari'
	assert result[1].cars[0].power == 232
	assert result[1].cars[1].brand == 'Lamborghini'
	assert result[1].cars[1].power == 345
}

fn test_json_decode_with_generic_array() {
	text := '[
{ "time":"12ms",
  "cars":[
	{"brand":"Ferrari","power":232},
	{"brand":"Lamborghini", "power":345}
  ]},
{ "time":"12ms",
  "cars":[
	{"brand":"Ferrari","power":232},
	{"brand":"Lamborghini", "power":345}
  ]}
]'

	string_to_response[Car](text)
}
