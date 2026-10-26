// Typed custom attribute materialization (vlang/rfcs#35): a use whose name
// matches a registered `@[attribute]` schema yields that struct in
// `$for attr in X.attributes`, narrowed by `$if attr is Schema`.
@[attribute]
struct TestRoute {
	path   string
	count  int
	active bool
}

@[TestRoute('/index', count: 3, active: true)]
@[deprecated: 'old']
fn test_route_fn() {}

fn test_attribute_schema_is_narrowing() {
	mut seen_route := false
	mut seen_other := ''
	$for attr in test_route_fn.attributes {
		$if attr is TestRoute {
			seen_route = true
			assert attr.path == '/index'
			assert attr.count == 3
			assert attr.active == true
		} $else {
			seen_other = attr.name
		}
	}
	assert seen_route
	assert seen_other == 'deprecated'
}

fn test_attribute_schema_negation() {
	mut legacy := []string{}
	$for attr in test_route_fn.attributes {
		$if attr !is TestRoute {
			legacy << attr.name
		}
	}
	assert legacy == ['deprecated']
}

fn test_attribute_schema_name_view() {
	mut names := []string{}
	$for attr in test_route_fn.attributes {
		names << attr.name
	}
	assert names == ['TestRoute', 'deprecated']
}
