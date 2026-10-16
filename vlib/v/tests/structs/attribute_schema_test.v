@[attribute]
struct Route {
	path   string
	method string
}

fn (r Route) key() string {
	return r.path
}

enum HttpMethod {
	get
	post
}

@[attribute]
struct GetRoute {
	path   string
	method HttpMethod
}

@[attribute: 'fn']
struct FnOnly {
	name string
}

@[attribute: 'value']
struct Val {
	note string
}

@[GetRoute('/index', method: .get)]
@[FnOnly('traced')]
fn index() {
}

struct Cfg {
	user_name string @[GetRoute: '/cfg'; GetRoute_method: ; GetRoute_1: post]
}

enum Level {
	debug  @[Val: 'd']
	info
}

fn test_attribute_schema_decl_compiles() {
	assert true
}
