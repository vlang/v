import toml

const (
	toml_table_text = 'inline = {a.b = 42}

many.dots.here.dot.dot.dot = {a.b.c = 1, a.b.d = 2}

a = {   a.b  =  1   }
b = {   "a"."b"  =  1   }
c = {   a   .   b  =  1   }
d = {   \'a\'   .   "b"  =  1   }
e = {a.b=1}

[tbl]
a.b.c = {d.e=1}

[tbl.x]
a.b.c = {d.e=1}

[[arr]]
t = {a.b=1}
T = {a.b=1}

[[arr]]
t = {a.b=2}
T = {a.b=2}'
)

fn test_tables() {
	mut toml_doc := toml.parse_text(toml_table_text) or { panic(err) }

	mut value := toml_doc.value('inline.a.b')
	assert value.int() == 42

	value = toml_doc.value('many.dots.here.dot.dot.dot.a.b.c')
	assert value.int() == 1

	value = toml_doc.value('many.dots.here.dot.dot.dot.a.b.d')
	assert value.int() == 2

	value = toml_doc.value('a.a.b')
	assert value.int() == 1

	value = toml_doc.value('b.a.b')
	assert value.int() == 1

	value = toml_doc.value('c.a.b')
	assert value.int() == 1

	value = toml_doc.value('d.a.b')
	assert value.int() == 1

	value = toml_doc.value('e.a.b')
	assert value.int() == 1

	value = toml_doc.value('tbl.a.b.c.d.e')
	assert value.int() == 1

	value = toml_doc.value('tbl.x.a.b.c.d.e')
	assert value.int() == 1

	mut m := toml_doc.value('tbl') as map[string]toml.Any

	value = m.value('a.b.c.d.e')
	assert value.int() == 1

	value = m.value('x.a.b.c.d.e')
	assert value.int() == 1

	arr := toml_doc.value('arr') as []toml.Any

	for i := 0; i < arr.len; i++ {
		entry := (arr[i] as map[string]toml.Any)
		value = entry.value('t.a.b')
		assert value.int() == i + 1
		value = entry.value('T.a.b')
		assert value.int() == i + 1
	}

	arr0 := arr[0] as map[string]toml.Any
	value = arr0.value('t.a.b')
	assert value.int() == 1
	value = arr0.value('T.a.b')
	assert value.int() == 1

	arr1 := arr[1] as map[string]toml.Any
	value = arr1.value('t.a.b')
	assert value.int() == 2
	value = arr1.value('T.a.b')
	assert value.int() == 2
}
