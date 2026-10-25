module parser

// Keep the old spellings readable by the formatter, but compile them as their
// replacements and point the warning at the original token. Preserve the legacy
// signed type identity for compatibility with existing methods and aliases.
fn (mut p Parser) canonical_integer_type(name string, start int, end int) string {
	if name.len != 5 || p.prefs.is_fmt || (p.cur_module == 'builtin' && name == 'isize') {
		return name
	}
	replacement := match name {
		'isize' { 'int' }
		'usize' { 'uint' }
		else { return name }
	}
	p.record_warning_span('`${name}` is deprecated; use `${replacement}` instead', start, end)
	return if name == 'usize' { replacement } else { name }
}
