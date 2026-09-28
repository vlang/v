import json
import x.json2

struct Person {
	name string
	age  int
}

fn test_reused_local_name_keeps_stringification_storage_type() ! {
	s := '{"name":"Bilbo","age":99}'
	for _ in 0 .. 2 {
		p := json2.decode[Person](s)!
		if p.age == 99 {
			assert '${p}'.contains('Bilbo')
		}
	}
	for _ in 0 .. 2 {
		p := json.decode(Person, s)!
		if p.age == 99 {
			assert '${p}'.contains('Bilbo')
		}
	}
	p := json.decode(Person, s)!
	if p.age == 99 {
		assert '${p}'.contains('Bilbo')
	}

	assert json2.encode(p).contains('Bilbo')
	assert json.encode(p).contains('Bilbo')
}
