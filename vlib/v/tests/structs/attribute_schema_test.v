@[attribute]
struct Route {
	path   string
	method string
}

fn (r Route) key() string {
	return r.path
}

fn test_attribute_schema_decl_compiles() {
	assert true
}
