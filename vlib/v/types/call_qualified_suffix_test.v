module types

fn test_call_owner_qualified_suffix_matches_formatted_suffix_exactly() {
	mut names := ['', '.', '..', 'int', '.int', 'pkg.int', 'pkg..int', 'pkg.Box[int]', 'fn (int) string',
		'pkg.fn (int) string', 'p.é', 'é', 'a\x00.b']
	for first in ['a', '.', ' ', '&', '[', 'é', '\x00'] {
		for second in ['', 'a', '.', 'b', 'é', '\x00'] {
			names << first + second
		}
	}
	for name in names {
		for suffix in names {
			assert call_owner_has_qualified_suffix(name, suffix) == name.ends_with('.${suffix}'), 'name=${name}, suffix=${suffix}'
		}
	}
}
