type Map = map[string]string

pub fn new_map() Map {
	return Map(map{
		'23': 'str'
	})
}

fn (a Map) + (b Map) Map {
	str := b['23']
	return Map(map{
		'34': str + '12'
	})
}

fn test_map_alias_op_overloading() {
	a := new_map()
	b := new_map()
	assert a + b == Map(map{
		'34': 'str12'
	})
	assert '${a + b}' == "Map({'34': 'str12'})"
}
