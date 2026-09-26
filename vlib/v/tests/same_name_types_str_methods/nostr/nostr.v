module nostr

// Mirrors `toml.Any`: the same short type names as `withstr`, without `str` methods.
pub type Any = []Any | map[string]Any | int | string

pub fn (a Any) string() string {
	match a {
		string { return a.clone() }
		else { return a.str() }
	}
}

pub fn (a []Any) text() string {
	return a.str()
}

pub struct Point {
pub:
	x int
}

pub fn (p Point) text() string {
	return p.str()
}

pub enum Color {
	red
}

pub fn (c Color) text() string {
	return c.str()
}
