module attrscomposition

@[attribute]
pub struct Route {
	path  string
	count int
}

// vfmt off
pub struct Box[T] {
	value T @[Route('module-field', count: 4)]
}

pub enum Color {
	first  @[Route('module-enum', count: 6)]
	plain
}

// tagged_declaration exposes a schema with two arguments for imported reflection.
@[Route('module-declaration', count: 9)]
pub fn tagged_declaration() {}
// vfmt on
