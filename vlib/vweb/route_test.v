module vweb

struct RoutePair {
	url   string
	route string
}

fn (rp RoutePair) test() ?[]string {
	url := rp.url.split('/').filter(it != '')
	route := rp.route.split('/').filter(it != '')
	return route_matches(url, route)
}

fn (rp RoutePair) test_match() {
	rp.test() or { panic('should match: $rp') }
}

fn (rp RoutePair) test_no_match() {
	rp.test() or { return }
	panic('should not match: $rp')
}

fn (rp RoutePair) test_param(expected []string) {
	res := rp.test() or { panic('should match: $rp') }
	assert res == expected
}

fn test_route_no_match() {
	tests := [
		RoutePair{
			url: '/a'
			route: '/a/b/c'
		},
		RoutePair{
			url: '/a/'
			route: '/a/b/c'
		},
		RoutePair{
			url: '/a/b'
			route: '/a/b/c'
		},
		RoutePair{
			url: '/a/b/'
			route: '/a/b/c'
		},
		RoutePair{
			url: '/a/c/b'
			route: '/a/b/c'
		},
		RoutePair{
			url: '/a/c/b/'
			route: '/a/b/c'
		},
		RoutePair{
			url: '/a/b/c/d'
			route: '/a/b/c'
		},
	]
	for test in tests {
		test.test_no_match()
	}
}

fn test_route_exact_match() {
	tests := [
		RoutePair{
			url: '/a/b/c'
			route: '/a/b/c'
		},
		RoutePair{
			url: '/a/b/c/'
			route: '/a/b/c'
		},
		RoutePair{
			url: '/a'
			route: '/a'
		},
		RoutePair{
			url: '/'
			route: '/'
		},
	]
	for test in tests {
		test.test_match()
	}
}

fn test_route_params_match() {
	RoutePair{
		url: '/a/b/c'
		route: '/:a/b/c'
	}.test_match()

	RoutePair{
		url: '/a/b/c'
		route: '/a/:b/c'
	}.test_match()

	RoutePair{
		url: '/a/b/c'
		route: '/a/b/:c'
	}.test_match()

	RoutePair{
		url: '/a/b/c'
		route: '/:a/b/:c'
	}.test_match()

	RoutePair{
		url: '/a/b/c'
		route: '/:a/:b/:c'
	}.test_match()

	RoutePair{
		url: '/one/two/three'
		route: '/:a/:b/:c'
	}.test_match()

	RoutePair{
		url: '/one/b/c'
		route: '/:a/b/c'
	}.test_match()

	RoutePair{
		url: '/one/two/three'
		route: '/:a/b/c'
	}.test_no_match()

	RoutePair{
		url: '/one/two/three'
		route: '/:a/:b/c'
	}.test_no_match()

	RoutePair{
		url: '/one/two/three'
		route: '/:a/b/:c'
	}.test_no_match()

	RoutePair{
		url: '/a/b/c/d'
		route: '/:a/:b/:c'
	}.test_no_match()

	RoutePair{
		url: '/1/2/3/4'
		route: '/:a/:b/:c'
	}.test_no_match()

	RoutePair{
		url: '/a/b'
		route: '/:a/:b/:c'
	}.test_no_match()

	RoutePair{
		url: '/1/2'
		route: '/:a/:b/:c'
	}.test_no_match()
}

fn test_route_params() {
	RoutePair{
		url: '/a/b/c'
		route: '/:a/b/c'
	}.test_param(['a'])

	RoutePair{
		url: '/one/b/c'
		route: '/:a/b/c'
	}.test_param(['one'])

	RoutePair{
		url: '/one/two/c'
		route: '/:a/:b/c'
	}.test_param(['one', 'two'])

	RoutePair{
		url: '/one/two/three'
		route: '/:a/:b/:c'
	}.test_param(['one', 'two', 'three'])

	RoutePair{
		url: '/one/b/three'
		route: '/:a/b/:c'
	}.test_param(['one', 'three'])
}

fn test_route_params_array_match() {
	// array can only be used on the last word (TODO: add parsing / tests to ensure this)

	RoutePair{
		url: '/a/b/c'
		route: '/a/b/:c...'
	}.test_match()

	RoutePair{
		url: '/a/b/c/d'
		route: '/a/b/:c...'
	}.test_match()

	RoutePair{
		url: '/a/b/c/d/e'
		route: '/a/b/:c...'
	}.test_match()

	RoutePair{
		url: '/one/b/c/d/e'
		route: '/:a/b/:c...'
	}.test_match()

	RoutePair{
		url: '/one/two/c/d/e'
		route: '/:a/:b/:c...'
	}.test_match()

	RoutePair{
		url: '/one/two/three/four/five'
		route: '/:a/:b/:c...'
	}.test_match()

	RoutePair{
		url: '/a/b'
		route: '/:a/:b/:c...'
	}.test_no_match()

	RoutePair{
		url: '/a/b/'
		route: '/:a/:b/:c...'
	}.test_no_match()
}

fn test_route_params_array() {
	RoutePair{
		url: '/a/b/c'
		route: '/a/b/:c...'
	}.test_param(['c'])

	RoutePair{
		url: '/a/b/c/d'
		route: '/a/b/:c...'
	}.test_param(['c/d'])

	RoutePair{
		url: '/a/b/c/d/'
		route: '/a/b/:c...'
	}.test_param(['c/d'])

	RoutePair{
		url: '/a/b/c/d/e'
		route: '/a/b/:c...'
	}.test_param(['c/d/e'])

	RoutePair{
		url: '/one/b/c/d/e'
		route: '/:a/b/:c...'
	}.test_param(['one', 'c/d/e'])

	RoutePair{
		url: '/one/two/c/d/e'
		route: '/:a/:b/:c...'
	}.test_param(['one', 'two', 'c/d/e'])

	RoutePair{
		url: '/one/two/three/d/e'
		route: '/:a/:b/:c...'
	}.test_param(['one', 'two', 'three/d/e'])
}
