// Regression test for https://github.com/vlang/v/issues/27056 and
// https://github.com/vlang/v/issues/27006: returning `?T` / `!T` from a
// function whose return type is `?Alias` / `!Alias` (where `type Alias = T`)
// must compile, both for the direct alias case and for containers (`?[]Alias`,
// `![]Alias`, etc.).

struct Bb {
	s string
}

type Aa = Bb

fn returns_b_option() ?Bb {
	return Bb{
		s: 'b'
	}
}

fn returns_b_result() !Bb {
	return Bb{
		s: 'b'
	}
}

fn returns_bs_option() ?[]Bb {
	return [Bb{ s: 'b' }]
}

fn returns_bs_result() ![]Bb {
	return [Bb{ s: 'b' }]
}

fn case_a() !Aa {
	return returns_b_result()
}

fn case_b() ?Aa {
	return returns_b_option()
}

fn case_c() ?[]Aa {
	return returns_bs_option()
}

fn case_d() ![]Aa {
	return returns_bs_result()
}

fn case_e() !Aa {
	return returns_b_result()
}

fn case_f() ?Aa {
	return returns_b_option()
}

fn test_result_alias_direct() {
	a := case_a() or { panic(err) }
	assert a.s == 'b'
}

fn test_option_alias_direct() {
	b := case_b() or { panic(err) }
	assert b.s == 'b'
}

fn test_option_alias_array() {
	c := case_c() or { panic(err) }
	assert c.len == 1
	assert c[0].s == 'b'
}

fn test_result_alias_array() {
	d := case_d() or { panic(err) }
	assert d.len == 1
	assert d[0].s == 'b'
}

fn test_result_alias_paren_wrapped() {
	e := case_e() or { panic(err) }
	assert e.s == 'b'
}

fn test_option_alias_paren_wrapped() {
	f := case_f() or { panic('none') }
	assert f.s == 'b'
}
