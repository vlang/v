module fnlist

import glyph

pub fn glyph_width(i glyph.Item) f64 {
	return i.width
}

pub struct List[T] {
	key fn (T) string = unsafe { nil }
}

pub fn new_list[T](key fn (T) string) &List[T] {
	return &List[T]{
		key: key
	}
}

pub fn (l &List[T]) first_key(items []T) string {
	return l.key(items[0])
}
