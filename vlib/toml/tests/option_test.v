import toml

enum JobTitle {
	worker
	executive
}

struct Nested {
	n int
}

struct DefaultNested {
	count int = 11
	text  string
}

struct DefaultNestedOptions {
	plain    DefaultNested  = DefaultNested{42, 'initial'}
	optional ?DefaultNested = DefaultNested{42, 'initial'}
	empty    ?DefaultNested
}

struct Optionals {
	a      ?int
	b      ?string
	c      ?bool
	f      ?f64
	n      ?u16
	role   ?JobTitle
	day    ?toml.Date
	nested ?Nested
	list   ?[]int
}

fn test_decode_option_fills_from_value() {
	o := toml.decode[Optionals]('a = 5\nb = "hi"\nc = true\nf = 1.5\nn = 8080\nrole = 1\nday = 2026-01-02\n\n[nested]\nn = 3\n') or {
		panic(err)
	}
	assert (o.a or { -1 }) == 5
	assert (o.b or { 'unset' }) == 'hi'
	assert (o.c or { false }) == true
	assert (o.f or { 0.0 }) == 1.5
	assert (o.n or { u16(0) }) == u16(8080)
	assert (o.role or { JobTitle.worker }) == JobTitle.executive
	assert (o.day or { toml.Date{} }).str() == '2026-01-02'
	assert (o.nested or { Nested{} }).n == 3
}

// A key that is not in the document leaves the field at `none`, which is the
// point of an Option: absence is distinguishable from a zero value.
fn test_decode_option_stays_none_for_missing_key() {
	o := toml.decode[Optionals]('a = 5') or { panic(err) }
	assert (o.a or { -1 }) == 5
	assert (o.b or { 'unset' }) == 'unset'
	assert (o.c or { false }) == false
	assert (o.f or { 0.0 }) == 0.0
	assert (o.n or { u16(7) }) == u16(7)
	assert (o.role or { JobTitle.worker }) == JobTitle.worker
	assert (o.day or { toml.Date{} }).str() == ''
	assert (o.nested or { Nested{} }).n == 0
}

fn test_encode_option_writes_the_payload() {
	s := toml.encode[Optionals](Optionals{
		a:      5
		b:      'hi'
		n:      u16(8080)
		role:   JobTitle.executive
		day:    toml.Date{'2026-01-02'}
		nested: Nested{3}
	})
	doc := toml.parse_text(s) or { panic(err) }
	assert doc.value('a').int() == 5
	assert doc.value('b').string() == 'hi'
	assert doc.value('n').int() == 8080
	assert doc.value('role').int() == 1
	assert doc.value('day').date().str() == '2026-01-02'
	assert doc.value('nested.n').int() == 3
}

// TOML has no null, so a `none` field must not produce a key at all. It used to
// be written as the string "Option(none)".
fn test_encode_option_omits_none() {
	s := toml.encode[Optionals](Optionals{})
	assert !s.contains('none')
	assert s.trim_space() == ''
	doc := toml.parse_text(toml.encode[Optionals](Optionals{ a: 5 })) or { panic(err) }
	assert doc.value('a').int() == 5
	assert doc.value('b') == toml.Any(toml.Null{})
}

fn test_encode_decode_option_round_trip() {
	original := Optionals{
		a:      5
		b:      'hi'
		c:      true
		f:      1.5
		n:      u16(8080)
		role:   JobTitle.executive
		day:    toml.Date{'2026-01-02'}
		nested: Nested{3}
	}
	back := toml.decode[Optionals](toml.encode[Optionals](original)) or { panic(err) }
	assert back == original
}

// An out of range value for a narrow payload is rejected the same way it is for
// a plain field, so the option stays at its previous value.
fn test_decode_option_rejects_out_of_range() {
	o := toml.decode[Optionals]('n = 99999') or { panic(err) }
	assert (o.n or { u16(7) }) == u16(7)
}

fn test_decode_optional_struct_keeps_initialized_defaults() {
	o := toml.decode[DefaultNestedOptions]('[plain]\ntext = "changed"\n[optional]\ntext = "changed"\n[empty]\ntext = "new"')!
	optional := o.optional or { panic('expected optional struct') }
	assert optional.count == 42
	assert optional.text == 'changed'
	assert optional == o.plain
	empty := o.empty or { panic('expected newly initialized struct') }
	assert empty.count == 11
	assert empty.text == 'new'
}

fn test_decode_optional_struct_empty_and_missing_tables_keep_defaults() {
	for input in ['', '[plain]\n[optional]'] {
		o := toml.decode[DefaultNestedOptions](input)!
		optional := o.optional or { panic('expected initialized struct') }
		assert optional.count == 42
		assert optional.text == 'initial'
		assert optional == o.plain
		assert o.empty == none
	}
	updated := toml.decode[DefaultNestedOptions]('[optional]\ncount = 0')!
	assert (updated.optional or { panic('expected updated struct') }).count == 0
	assert (updated.optional or { panic('expected updated struct') }).text == 'initial'
}
