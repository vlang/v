struct User {
	name string
	age  int
}

fn (u User) a() {}

fn (u User) b() {}

fn (u User) c() {}

fn test_method_name() {
	mut out := []string{}
	$for field in User.methods {
		$if field.name == 'b' {
			out << 'ok'
		}
	}
	assert out.len == 1
}

fn test_field_name() {
	mut out := []string{}
	$for field in User.fields {
		$if field.name == 'name' {
			out << 'ok'
		}
	}
	assert out.len == 1
}
