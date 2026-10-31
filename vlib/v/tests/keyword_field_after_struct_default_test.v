struct KeywordAfterDefault {
	x  int = 2
	in int
	is int
	as int
	or int
}

union KeywordUnionAfterDefault {
	x  int = 2
	in int
}

struct TrailingOpDefault {
	x  int = 1 +
		2
	in int
}

struct LeadingOpDefault {
	a  bool = true
		&& false
	b  int = 1 +
		2
	in int
}

struct SemicolonSeparatedKeywordField {
	x  int = 2
	in int
}

fn test_keyword_field_after_struct_default() {
	s := KeywordAfterDefault{
		in: 3
		is: 4
		as: 5
		or: 6
	}
	assert s.x == 2
	assert s.in == 3
	assert s.is == 4
	assert s.as == 5
	assert s.or == 6
	assert KeywordAfterDefault{}.x == 2
	u := KeywordUnionAfterDefault{}
	assert u.x == 2
	t := TrailingOpDefault{}
	assert t.x == 3
	assert t.in == 0
	l := LeadingOpDefault{}
	assert l.a == false
	assert l.b == 3
	assert l.in == 0
	m := SemicolonSeparatedKeywordField{ in: 7 }
	assert m.x == 2
	assert m.in == 7
}
