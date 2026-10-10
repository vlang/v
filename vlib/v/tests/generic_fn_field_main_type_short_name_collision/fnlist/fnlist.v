module fnlist

import glyph

// glyph_width reads the imported type while keeping its short name visible to this module.
pub fn glyph_width(i glyph.Item) f64 {
	return i.width
}

pub struct List[T] {
	key fn (T) string @[required]
}

// new_list stores the callback for a list of the caller's item type.
pub fn new_list[T](key fn (T) string) &List[T] {
	return &List[T]{
		key: key
	}
}

// first_key calls the stored callback for the first item.
pub fn (l &List[T]) first_key(items []T) string {
	return l.key(items[0])
}
