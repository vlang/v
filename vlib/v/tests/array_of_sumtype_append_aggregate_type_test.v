struct StructA {
	value int
}

struct StructB {
	value  int
	offset int
}

type AB = StructA | StructB

fn test_sumtype_array_append_aggregate_type() {
	mut arr := []AB{}
	arr << StructA{0}
	arr << StructB{0, 1}

	mut arr2 := []AB{}

	for a_or_b in arr {
		match a_or_b {
			StructA, StructB {
				arr2 << a_or_b
			}
		}
	}

	println(arr2)
	assert arr2.len == 2
}
