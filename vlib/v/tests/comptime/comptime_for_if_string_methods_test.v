// A `$if` in a reflection loop can test the loop variable's string metadata with `.len`,
// `.starts_with()`, `.ends_with()` and `.contains()`.
@[table: 'users']
struct User {
	id    int
	name  string
	email string
}

struct Routes {}

fn (r Routes) get_user() int {
	return 1
}

fn (r Routes) list_users() int {
	return 2
}

fn (r Routes) delete_user(id int) int {
	return id
}

enum Color {
	red
	green
	dark_red
}

fn test_field_names() {
	mut names := []string{}
	$for field in User.fields {
		$if field.name.starts_with('e') || field.name.ends_with('d') {
			names << field.name
		}
	}
	assert names == ['id', 'email']
}

fn test_method_names() {
	mut names := []string{}
	$for method in Routes.methods {
		$if method.name.contains('user') && !method.name.starts_with('delete') {
			names << method.name
		}
	}
	assert names == ['get_user', 'list_users']
}

fn test_param_names() {
	mut names := []string{}
	$for method in Routes.methods {
		$for param in method.params {
			$if param.name.contains('i') {
				names << '${method.name}:${param.name}'
			}
		}
	}
	assert names == ['delete_user:id']
}

fn test_attribute_and_enum_value_names() {
	mut args := []string{}
	$for attr in User.attributes {
		$if attr.arg.ends_with('ers') {
			args << attr.arg
		}
	}
	assert args == ['users']
	mut reds := []string{}
	$for value in Color.values {
		$if value.name.ends_with('red') {
			reds << value.name
		} $else $if value.name.contains("'") {
			reds << 'quote'
		}
	}
	assert reds == ['red', 'dark_red']
}

fn test_name_len() {
	mut names := []string{}
	$for field in User.fields {
		$if field.name.len > 2 && field.name.len != 5 {
			names << field.name
		}
	}
	assert names == ['name']
	mut short := []string{}
	$for value in Color.values {
		$if value.name.len == 3 || value.name.len in [8] {
			short << value.name
		}
	}
	assert short == ['red', 'dark_red']
}

fn test_literals_with_brackets() {
	mut hits := []string{}
	$for method in Routes.methods {
		$if method.name.contains('(') || method.name.starts_with('get') {
			hits << method.name
		}
		$if (method.name.ends_with('s)')) {
			hits << 'paren'
		}
	}
	assert hits == ['get_user']
}
