const static_route = 'GET /users/:id/posts'.all_after(' ').trim_left('/')
const static_separator = '/'
const static_slice = static_route[6..9]
const static_yes = static_route.starts_with('users/')
const static_needle = ' USERS '.trim_space().to_lower()

struct StringSourceApp {}

@['GET /users/:id/posts']
fn (app &StringSourceApp) user_posts() int {
	return 1
}

@['POST /users']
fn (app &StringSourceApp) create_user() int {
	return 2
}

fn static_string_dispatch(app &StringSourceApp, verb string, segs []string) int {
	$for method in StringSourceApp.methods {
		$for attr in method.attributes {
			route_verb := attr.name.all_before(' ')
			route_path := attr.name.all_after(' ').trim_left('/')
			if verb == route_verb && segs.len == route_path.count('/') + 1 {
				mut ok := true
				mut index := 0
				$for segment in route_path.split(static_separator) {
					$if !segment.starts_with(':') {
						ok = ok && segs[index] == segment
					}
					index++
				}
				if ok {
					return app.$method()
				}
			}
		}
	}
	return 404
}

fn test_reflected_route_is_parsed_at_compile_time() {
	app := StringSourceApp{}
	assert static_string_dispatch(&app, 'GET', ['users', '42', 'posts']) == 1
	assert static_string_dispatch(&app, 'GET', ['users', 'name', 'posts']) == 1
	assert static_string_dispatch(&app, 'GET', ['users', '42', 'comments']) == 404
	assert static_string_dispatch(&app, 'GET', ['users', '42']) == 404
	assert static_string_dispatch(&app, 'POST', ['users']) == 2
	assert static_string_dispatch(&app, 'DELETE', ['users']) == 404
}

fn test_static_string_sources_use_builtin_split_semantics() {
	mut segments := []string{}
	$for part in static_route.split(static_separator) {
		segments << part
	}
	assert segments == ['users', ':id', 'posts']
	mut any := []string{}
	$for part in 'alpha,beta;gamma'.split_any(',;') {
		any << part
	}
	assert any == ['alpha', 'beta', 'gamma']
	mut words := []string{}
	$for word in ' alpha\t beta\n gamma '.fields() {
		words << word
	}
	assert words == ['alpha', 'beta', 'gamma']
	mut empty := 0
	$for part in ''.fields() {
		empty++
	}
	assert empty == 0
	mut nested := []string{}
	$for part in 'A/B'.split('/') {
		outer := part.to_lower()
		$for part in 'X,Y'.split(',') {
			inner := part.to_lower()
			nested << outer + inner
		}
		nested << outer
	}
	assert nested == ['ax', 'ay', 'a', 'bx', 'by', 'b']
}

fn test_immutable_string_locals_and_constants_select_branches() {
	path := static_route
	name := path.all_before('/').to_lower()
	needle := static_needle
	flag := name.starts_with('users')
	$if flag && static_yes {
		assert true
	} $else {
		assert false
	}
	choice := $if name == 'users' { 'yes' } $else { 'no' }
	assert choice == 'yes'
	$if name == 'users' && name.contains(needle) && 'ser' in name && name in ['users', 'other'] {
		assert name == 'users'
	} $else {
		assert false
	}
	$if static_route.count('/') == 2 {
		assert true
	} $else {
		assert false
	}
	$if 'ABC'.to_lower() == 'abc' {
		assert true
	} $else {
		assert false
	}
	suffix := path[6..].all_after('/')
	assert suffix == 'posts'
	assert 'abcdef'[0x1..0x3] == 'bc'
	$if suffix == 'posts' && static_slice == ':id' && path[6..9].starts_with(':') {
		assert true
	} $else {
		assert false
	}
	if name.len > 0 {
		local := 'inner'.to_upper()
		$if local == 'INNER' {
			assert true
		} $else {
			assert false
		}
	}
	mut runtime := 'first'
	runtime = 'second'
	assert runtime.to_upper() == 'SECOND'
}

struct StringSourceFields {
	prefix_first_name  string
	prefix_second_name string
}

fn test_reflection_local_bindings_are_isolated_per_field() {
	mut names := []string{}
	$for field in StringSourceFields.fields {
		name := field.name.trim_string_left('prefix_')
		first := name == 'first_name' && name.len == 10
		member := name in ['first_name', 'unused']
		$if first && member && 'first' in name {
			names << name.to_upper()
		} $else {
			names << name.to_lower()
		}
		$for piece in name.split('_') {
			names << piece
		}
	}
	assert names == ['FIRST_NAME', 'first', 'name', 'second_name', 'second', 'name']
}

fn static_generic_field_parts[T]() []string {
	mut result := []string{}
	$for field in T.fields {
		name := field.name.trim_string_left('prefix_')
		$for part in name.split('_') {
			result << part
		}
	}
	return result
}

fn test_generic_reflection_string_source() {
	assert static_generic_field_parts[StringSourceFields]() == ['first', 'name', 'second', 'name']
}

fn runtime_string_source_bytes(value string) []u8 {
	return value.bytes()
}

fn runtime_string_source_result(value bool) !bool {
	return value
}

fn test_runtime_membership_and_unwrapped_booleans_keep_runtime_values() {
	bytes := runtime_string_source_bytes('12.3')
	mut dots := 0
	if `.` !in bytes {
		dots++
	}
	if `.` in bytes {
		dots += 2
	}
	assert dots == 2
	value := runtime_string_source_result(true) or { false }
	mut branches := 0
	if !value {
		branches++
	} else {
		branches += 2
	}
	assert branches == 2
}
