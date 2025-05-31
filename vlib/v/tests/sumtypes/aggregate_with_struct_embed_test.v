type SumType = StructA | StructB

struct StructA {
	member string
}

struct StructB {
	StructA
}

fn get_member(st SumType) string {
	match st {
		StructA, StructB {
			return st.member
		}
	}
}

fn test_aggregate_with_struct_embed() {
	struct_a := StructA{'hello'}
	ret := get_member(struct_a)
	println(ret)
	assert ret == 'hello'
}
