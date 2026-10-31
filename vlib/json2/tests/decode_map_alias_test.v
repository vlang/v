import json2

type Props = map[string]int
type MyInt = int
type Ids = []int

struct AliasFields {
	by_name  map[string]Props
	opt_map  ?Props
	opt_nest ?map[string]Props
	opt_int  ?MyInt
	opt_ids  ?Ids
}

// A map whose values are a map alias was decoded to an empty map, and an option
// of a map alias crashed.
fn test_map_alias_values() {
	top := json2.decode[map[string]Props]('{"a":{"x":1,"y":2},"b":{}}')!
	assert top['a'] == Props({
		'x': 1
		'y': 2
	})
	assert top['b'].len == 0

	fields := json2.decode[AliasFields]('{"by_name":{"n":{"k":1}},"opt_map":{"z":5},"opt_nest":{"p":{"q":3}},"opt_int":7,"opt_ids":[1,2]}')!
	assert fields.by_name['n']['k'] == 1
	opt_map := (fields.opt_map or { panic('opt_map should be set') }).clone()
	assert opt_map['z'] == 5
	opt_nest := (fields.opt_nest or { panic('opt_nest should be set') }).clone()
	assert opt_nest['p']['q'] == 3
	assert fields.opt_int? == MyInt(7)
	opt_ids := fields.opt_ids or { panic('opt_ids should be set') }
	assert opt_ids == Ids([1, 2])
}
