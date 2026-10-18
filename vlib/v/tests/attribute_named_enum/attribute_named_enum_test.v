import attrs

enum Method {
	get
	post
}

@[attribute]
struct Route {
	method Method
}

@[Route(method: .get)]
fn local_route() int {
	return 1
}

@[attrs.Route(method: .post)]
fn imported_route() int {
	return 2
}

fn test_first_named_enum_argument() {
	assert local_route() == 1
	assert imported_route() == 2
}
