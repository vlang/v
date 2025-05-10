module main

interface Value {}

struct CreateRegionData {
	name          string
	currency_code string
	tax_rate      f32
	tax_code      string @[omitempty]
	countries     []string
	includes_tax  bool @[omitempty]
}

fn test_main() {
	rd := CreateRegionData{}
	id := 'bro'
	the_array := [Value(id), rd.name]
	println(the_array)
	assert '${the_array}' == "[Value('bro'), Value('')]"
}
