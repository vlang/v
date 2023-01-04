struct Access {}

fn (access &Access) acc() bool {
	return true
}

struct Field {
	Access
}

fn test_embed_method_call() {
	mut fields := []&Field{}
	fields << &Field{}

	mut rets := []bool{}
	for mut field in fields {
		println(field.acc())
		rets << field.acc()
	}

	assert rets.len == 1
	assert rets[0]
}
