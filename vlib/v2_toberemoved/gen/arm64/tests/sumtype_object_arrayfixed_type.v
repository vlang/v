module main

type Type = ArrayFixed | Primitive

struct Primitive {
	size int
}

struct ArrayFixed {
	len       int
	elem_type Type
}

struct ObjectCommon {
	name string
	typ  Type
}

struct Const {
	ObjectCommon
}

struct Global {
	ObjectCommon
}

type Object = Const | Global | Type

struct Scope {
mut:
	objects map[string]Object
}

fn (s &Scope) lookup(name string) ?Object {
	if obj := s.objects[name] {
		return obj
	}
	return none
}

fn (obj &Object) typ() Type {
	match obj {
		Const {
			return obj.typ
		}
		Global {
			return obj.typ
		}
		Type {
			return obj
		}
	}
}

fn array_len(t Type) int {
	return match t {
		ArrayFixed {
			t.len
		}
		Primitive {
			-1
		}
	}
}

fn main() {
	mut s := Scope{
		objects: map[string]Object{}
	}
	s.objects['buf'] = Object(Global{
		name: 'buf'
		typ:  Type(ArrayFixed{
			len:       128
			elem_type: Type(Primitive{
				size: 8
			})
		})
	})
	if obj := s.lookup('buf') {
		println(array_len(obj.typ()))
		return
	}
	println(-2)
}
