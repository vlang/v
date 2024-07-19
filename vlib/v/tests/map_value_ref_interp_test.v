module main

struct Inner {
	value int
}

struct Outer {
	value map[string]&Inner
}

fn test_main() {
	outer := Outer{
		value: {
			'test': &Inner{
				value: 1
			}
		}
	}
	expected := "Outer{
    value: {'test': &Inner{
        value: 1
    }}
}"
	assert '${outer}' == expected
}
