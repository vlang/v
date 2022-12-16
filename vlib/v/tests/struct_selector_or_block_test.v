module main

interface Greeting {
	tt ?string
}

struct Hello {
	tt    ?string = none
	value string
}

struct Hi {
	tt ?string = none
}

fn greet(g Greeting) string {
	// Problematic piece of code here
	// If I leave it unchecked, the compiler complains (as expected)
	t := g.tt or { 'UNKNOWN' }
	return t
}

fn test_main() {
	assert greet(Hello{}) == 'UNKNOWN'
	assert greet(Hello{'cool', ''}) == 'cool'
}
