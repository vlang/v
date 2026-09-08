type Handler = fn (req &Request, mut wr ResponseWriter)

struct Request {}

struct ResponseWriter {}

struct Router {
mut:
	methods map[string]Handler
}

fn (mut r Router) register(method string, handler Handler) bool {
	r.methods[method] = handler
	return true
}

fn startup[T]() {
	mut r := Router{}
	r.register('x', fn (req &Request, mut wr ResponseWriter) {
	})
	assert r.methods.len == 1
}

fn test_inline_fn_literal_matches_fn_type_alias_inside_generic_fn() {
	startup[App]()
}

struct App {}