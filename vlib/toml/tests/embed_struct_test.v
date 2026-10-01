import toml

struct Foo {
	foo int
}

struct FooBar {
	Foo
	bar int
	baz int
}

struct Inner {
	Foo
	deep string
}

struct Outer {
	Inner
	tag string
}

struct Skipped {
	Foo  @[skip]
	bar int
}

struct Remapped {
	Foo
	barbaz string @[toml: barbaz]
}

// The embedding struct's own field shadows the same-named embedded field.
struct Shadowed {
	Foo
	foo2 int @[toml: foo]
}

fn test_decode_embedded_flattened() {
	parsed := toml.decode[FooBar]('foo = 1\nbar = 2\nbaz = 3') or { panic(err) }
	assert parsed.foo == 1
	assert parsed.bar == 2
	assert parsed.baz == 3
}

fn test_decode_embedded_table() {
	// Keys placed after a `[Foo]` header belong to that table, so the fields of the
	// embedding struct have to come before it.
	parsed := toml.decode[FooBar]('bar = 2\nbaz = 3\n\n[Foo]\nfoo = 1') or {
		panic(err)
	}
	assert parsed.foo == 1
	assert parsed.bar == 2
	assert parsed.baz == 3
}

fn test_decode_embedded_table_wins() {
	// The `[Foo]` table takes precedence over the flattened `foo` key.
	parsed := toml.decode[FooBar]('foo = 10\nbar = 2\n\n[Foo]\nfoo = 1') or { panic(err) }
	assert parsed.foo == 1
	assert parsed.bar == 2
}

fn test_decode_embedded_nested() {
	parsed := toml.decode[Outer]('foo = 1\ndeep = "x"\ntag = "t"') or { panic(err) }
	assert parsed.foo == 1
	assert parsed.deep == 'x'
	assert parsed.tag == 't'
}

fn test_decode_embedded_skip() {
	parsed := toml.decode[Skipped]('foo = 1\nbar = 2') or { panic(err) }
	assert parsed.foo == 0
	assert parsed.bar == 2
}

fn test_decode_embedded_attr() {
	parsed := toml.decode[Remapped]('foo = 1\nbarbaz = "def"') or { panic(err) }
	assert parsed.foo == 1
	assert parsed.barbaz == 'def'
}

fn test_decode_embedded_keeps_defaults() {
	parsed := toml.decode[FooBar]('bar = 2') or { panic(err) }
	assert parsed.foo == 0
	assert parsed.bar == 2
	assert parsed.baz == 0
}

fn test_encode_embedded_flattened() {
	encoded := toml.encode[FooBar](FooBar{Foo{1}, 2, 3})
	doc := toml.parse_text(encoded) or { panic(err) }
	assert doc.value('foo').int() == 1
	assert doc.value('bar').int() == 2
	assert doc.value('baz').int() == 3
	assert doc.value('Foo') == toml.Any(toml.Null{})
}

fn test_encode_and_decode_embedded() {
	x := FooBar{Foo{1}, 2, 3}
	assert toml.decode[FooBar](toml.encode[FooBar](x))! == x
}

fn test_encode_embedded_nested() {
	x := Outer{Inner{Foo{1}, 'x'}, 't'}
	encoded := toml.encode[Outer](x)
	doc := toml.parse_text(encoded) or { panic(err) }
	assert doc.value('foo').int() == 1
	assert doc.value('deep').string() == 'x'
	assert doc.value('tag').string() == 't'
	assert doc.value('Inner') == toml.Any(toml.Null{})
	assert doc.value('Outer') == toml.Any(toml.Null{})
	assert toml.decode[Outer](encoded)! == x
}

fn test_encode_embedded_skip() {
	encoded := toml.encode[Skipped](Skipped{Foo{1}, 2})
	doc := toml.parse_text(encoded) or { panic(err) }
	assert doc.value('foo') == toml.Any(toml.Null{})
	assert doc.value('bar').int() == 2
}

fn test_encode_embedded_shadowed() {
	encoded := toml.encode[Shadowed](Shadowed{Foo{1}, 42})
	doc := toml.parse_text(encoded) or { panic(err) }
	assert doc.value('foo').int() == 42
}

// `toml.Date`, `toml.Time` and `toml.DateTime` are TOML scalars, so an embedded one
// is kept as a single value under its type name instead of being flattened.
struct DateStamp {
	toml.Date
}

struct TimeStamp {
	toml.Time
	name string
}

struct DateTimeStamp {
	toml.DateTime
}

fn test_encode_and_decode_embedded_date() {
	original := DateStamp{toml.Date{'2026-09-30'}}
	encoded := toml.encode[DateStamp](original)
	assert encoded == 'Date = 2026-09-30'
	restored := toml.decode[DateStamp](encoded)!
	assert restored.date == original.date
}

fn test_encode_and_decode_embedded_time() {
	original := TimeStamp{toml.Time{'07:32:59'}, 'x'}
	encoded := toml.encode[TimeStamp](original)
	assert encoded == 'Time = 07:32:59\nname = "x"'
	restored := toml.decode[TimeStamp](encoded)!
	assert restored.time == original.time
	assert restored.name == original.name
}

fn test_encode_and_decode_embedded_datetime() {
	original := DateTimeStamp{toml.DateTime{'1979-05-27T07:32:00Z'}}
	encoded := toml.encode[DateTimeStamp](original)
	assert encoded == 'DateTime = 1979-05-27T07:32:00Z'
	restored := toml.decode[DateTimeStamp](encoded)!
	assert restored.datetime == original.datetime
}

fn test_decode_embedded_date_keeps_default() {
	parsed := toml.decode[DateStamp]('other = 1') or { panic(err) }
	assert parsed.date == ''
}
