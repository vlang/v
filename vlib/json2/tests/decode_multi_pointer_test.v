import json2

struct MultiPointerBase {
	d &&int
}

// The embedded struct is decoded by a separate path from plain struct fields.
struct MultiPointerFields {
	MultiPointerBase
	e ?&&&string
	n &&int
	m ?&&int
}

fn test_multi_pointer_fields() {
	decoded := json2.decode[MultiPointerFields]('{"d": 7, "e": "x", "n": null, "m": null}')!
	assert **decoded.d == 7
	e := decoded.e or { panic('e should be set') }
	assert ***e == 'x'
	assert decoded.n == unsafe { nil }
	assert decoded.m == none
}

struct MultiPointerElem {
	value int = 7
}

struct MultiPointerContainers {
	list   []&&int
	fixed  [2]&&string
	by_key map[string]&&MultiPointerElem
	triple []&&&int
	opts   []?&&int
}

fn test_multi_pointer_container_elements() {
	decoded := json2.decode[MultiPointerContainers]('{"list":[1,null,2],"fixed":["a","b"],"by_key":{"k":{"value":3}},"triple":[9],"opts":[5,null]}')!
	assert decoded.list.len == 3
	assert **decoded.list[0] == 1
	assert **decoded.list[2] == 2
	assert **decoded.fixed[0] == 'a'
	assert **decoded.fixed[1] == 'b'
	elem := decoded.by_key['k'] or { panic('k should be set') }
	assert (**elem).value == 3
	assert ***decoded.triple[0] == 9
	first := decoded.opts[0] or { panic('the first option should be set') }
	assert **first == 5
	assert decoded.opts[1] == none
	top := json2.decode[[]&&int]('[3]')!
	assert **top[0] == 3
}

struct DeepPointers {
	value &&&&int
	opt   ?&&&&&string
	list  []&&&&int
	by_id map[string]&&&&int
}

fn test_pointers_of_any_depth() {
	assert ****json2.decode[&&&&int]('5')! == 5
	deep := json2.decode[DeepPointers]('{"value":1,"opt":"x","list":[2],"by_id":{"k":3}}')!
	assert ****deep.value == 1
	opt := deep.opt or { panic('opt should be set') }
	assert *****opt == 'x'
	assert ****deep.list[0] == 2
	by_id := deep.by_id['k'] or { panic('k should be set') }
	assert ****by_id == 3
}
