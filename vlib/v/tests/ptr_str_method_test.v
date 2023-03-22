module main

struct Name {
	age int = 222
}

fn (n &Name) str() string {
	return n.age.str()
}

fn test_str_method_with_ptr() {
	name := &Name{}
	mut mp := map[string]&Name{}
	mp['aaa'] = name

	assert mp.str() == "{'aaa': 222}"
	assert name.str() == '222'
}
