enum En {
	ea
	eb
}

struct St {
mut:
	m map[En]string
}

fn test_map_init_with_enum_keys() {
	mut st := St{}

	st.m = {
		.ea: 'a'
	}

	println(st.m)
	assert '$st.m' == "{ea: 'a'}"
}
