import mod

// A local schema with the same short name: the dotted use must resolve to
// mod.Tag (field `a`), the bare use to the local Tag (field `b`).
@[attribute]
struct Tag {
	b string
}

@[mod.Tag('x')]
fn dotted_ok() {
}

@[Tag('x')]
fn bare_resolves_local() {
}

struct Fields {
	ok      string @[mod.Tag('y')]
	missing string @[mod.Tag]
}

fn main() {
}