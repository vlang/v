module attrscomposition

@[attribute]
pub struct Route {
	path  string
	count int
}

pub struct Box[T] {
	value T @[Route: 'module-field'; Route_count: 4]
}

pub enum Color {
	first  @[Route: 'module-enum'; Route_count: 6]
	plain
}

@[Route: 'module-declaration'; Route_count: 9]
pub fn tagged_declaration() {}
