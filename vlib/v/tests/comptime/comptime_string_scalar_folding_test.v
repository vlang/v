const folded_route_path = 'GET /users/:id'.all_after(' ').trim_left('/')
const folded_route_verb = 'GET /users'.all_before(' ').to_lower()
const folded_route_count = 'users/:id/posts'.count('/')

struct ScalarStringApp {}

@['GET /users/:id']
fn (app &ScalarStringApp) get_user() {}

@['POST /users']
fn (app &ScalarStringApp) post_user() {}

fn test_pure_string_literal_calls() {
	assert folded_route_path == 'users/:id'
	assert folded_route_verb == 'get'
	assert folded_route_count == 2
	assert 'value-suffix'.trim_string_right('-suffix').to_upper() == 'VALUE'
	assert '  a,b,c  '.trim_space().replace(',', '/').all_after_last('/') == 'c'
	assert 'a/b/c'.all_before_last('/') == 'a/b'
	assert 'name'.trim_right('e').trim_left('n') == 'am'
	assert 'GET /users'.starts_with('GET ')
	assert 'GET /users'.ends_with('users')
	assert 'GET /users'.contains('/u')
	assert '\x41'.to_lower() == 'a'
	assert r'A\B'.to_lower() == r'a\b'
	assert '"ABC"'.to_lower() == '"abc"'
	assert "'ABC'".to_lower() == "'abc'"
	assert 'a\n\tb'.replace('\n', ',').contains(',\t')
	mut runtime_value := 'GET /users'
	runtime_value = 'POST /users'
	assert runtime_value.all_before(' ').to_lower() == 'post'
}

fn test_reflection_string_method_chains() {
	mut routes := []string{}
	$for method in ScalarStringApp.methods {
		$for attr in method.attributes {
			$if attr.name.all_before(' ').to_lower() == 'get' {
				routes << method.name
			}
			$if attr.name.all_after(' ').trim_left('/').count('/') == 1 {
				assert method.name == 'get_user'
			}
		}
	}
	assert routes == ['get_user']
}
