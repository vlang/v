interface Greeting {
	tt ?string
}

struct Hello {
	tt    ?string
	value string
}

struct Hi {
	tt ?string
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

struct CnameTest {
	long  ?string
	short ?string
}

fn test_cname_opt_field_selecor() {
	x := CnameTest{
		short: 'xyz'
	}
	assert (x.long or { 'NOPE' }) == 'NOPE'
	assert (x.short or { 'NOPE' }) == 'xyz'
}
