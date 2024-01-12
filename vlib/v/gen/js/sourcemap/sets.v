module sourcemap

struct Sets {
mut:
	value map[string]u32
}

// adds a new element to a Set if new and returns index position of new or existing element
fn (mut s Sets) add(element string) u32 {
	index := s.value[element] or {
		index := u32(s.value.len)
		s.value[element] = index
		return index
	}
	return index
}
