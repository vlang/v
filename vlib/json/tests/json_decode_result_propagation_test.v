// vtest vflags: -w
import json

struct DecodePropagationPerson {
	name string
	age  int
}

fn decode_propagation_person(s string) !DecodePropagationPerson {
	p := json.decode(DecodePropagationPerson, s)!
	return p
}

fn test_json_decode_result_propagation_in_test_fn() {
	p := json.decode(DecodePropagationPerson, '{"name":"a","age":3}')!
	assert p.name == 'a'
	assert p.age == 3
	people := json.decode([]DecodePropagationPerson, '[{"name":"b"},{"name":"c"}]')!
	assert people.map(it.name) == ['b', 'c']
}

fn test_json_decode_result_propagation_in_result_fn() {
	p := decode_propagation_person('{"name":"d","age":4}')!
	assert p.name == 'd'
	assert p.age == 4
	if _ := decode_propagation_person('{') {
		assert false
	} else {
		assert err.msg().len > 0
	}
}

fn test_json_decode_result_or_block() {
	p := json.decode(DecodePropagationPerson, '{"name":"e"}') or {
		DecodePropagationPerson{
			name: 'fallback'
		}
	}
	assert p.name == 'e'
	q := json.decode(DecodePropagationPerson, 'not json') or {
		DecodePropagationPerson{
			name: 'fallback'
		}
	}
	assert q.name == 'fallback'
}
