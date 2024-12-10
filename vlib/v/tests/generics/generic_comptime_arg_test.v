import toml

struct Parent {
	name   string
	child1 Child1
	child2 Child2
}

struct Child1 {
	name string
}

struct Child2 {
	age int
}

fn decode[T](toml_str string) !T {
	doc := toml.parse_text(toml_str)!

	return decode_struct(doc.to_any(), &T{})
}

// `T` param serves here as a workaround to allow to infer types of nested struct fields.
fn decode_struct[T](doc toml.Any, typ &T) T {
	mut res := T{}
	$for field in T.fields {
		val := doc.value(field.name)
		$if field.typ is string {
			res.$(field.name) = val.string()
		} $else $if field.typ is int {
			res.$(field.name) = val.int()
		} $else $if field.is_struct {
			typ_ := typ.$(field.name)
			res.$(field.name) = decode_struct(val, &typ_)
		}
	}
	return res
}

fn test_main() {
	toml_str := 'name = "John"
	child1 = { name = "abc" }
	child2 = { age = 5 }'

	a := dump(decode[Parent](toml_str)!)
	assert a.name == 'John'
	assert a.child1 == Child1{
		name: 'abc'
	}
	assert a.child2 == Child2{
		age: 5
	}
}
