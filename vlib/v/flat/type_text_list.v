module flat

// TypeTextList stores an ordered sequence of stable source-spelling identities.
pub struct TypeTextList {
pub:
	ids []u32
}

// from_names indexes spellings supplied by parsing or an integration boundary.
pub fn TypeTextList.from_names(names []string) TypeTextList {
	mut ids := []u32{cap: names.len}
	for name in names {
		ids << source_type_id(name)
	}
	return TypeTextList{ ids: ids }
}

// names resolves spellings for a consumer that accepts textual types.
pub fn (values TypeTextList) names() []string {
	mut names := []string{cap: values.ids.len}
	for id in values.ids {
		names << source_type_name(id)
	}
	return names
}

// clone copies numeric storage without formatting or cloning spellings.
pub fn (values TypeTextList) clone() TypeTextList {
	ids := values.ids
	if ids.len == 0 {
		return TypeTextList{ ids: g_empty_source_type_ids }
	}
	return TypeTextList{ ids: ids.clone() }
}

// at resolves one element without allocating an intermediate spelling array.
@[inline]
pub fn (values TypeTextList) at(index int) string {
	return source_type_name(values.ids[index])
}
