// A `$if` in a reflection loop is decided per item, also when a string literal in its
// condition contains brackets, operators or commas.
struct Routes {}

fn (r Routes) get_user() int {
	return 1
}

fn (r Routes) list_users() int {
	return 2
}

fn test_literal_with_brackets() {
	mut hits := []string{}
	$for method in Routes.methods {
		$if (method.name == 'a)b') {
			hits << 'close'
		}
		$if 'get(' != method.name && method.name == 'get_user' {
			hits << method.name
		}
		$if method.name !in ['x]', 'get_user'] {
			hits << method.name
		}
	}
	assert hits == ['get_user', 'list_users']
}

fn test_literal_with_operators() {
	mut hits := []string{}
	$for method in Routes.methods {
		$if method.name == 'x || y' || method.name == 'list_users' {
			hits << method.name
		}
		$if method.name != 'a && b' && method.name == 'get_user' {
			hits << method.name
		}
	}
	assert hits == ['get_user', 'list_users']
}

fn test_literal_with_commas() {
	mut hits := []string{}
	$for method in Routes.methods {
		$if method.name in ['x,get_user,y', 'none'] {
			hits << 'joined'
		}
		$if method.name in ['a,b', 'list_users'] {
			hits << method.name
		}
	}
	assert hits == ['list_users']
}
