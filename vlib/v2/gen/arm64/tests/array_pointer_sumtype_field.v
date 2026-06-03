struct FnType {
	x int
}

struct Void {}

type Type = FnType | Void

struct Fn {
	name string
	typ  Type
}

fn lookup(methods []&Fn, method_name string) bool {
	for method in methods {
		if method.name == method_name {
			return method.typ is FnType
		}
	}
	return false
}

fn main() {
	method := &Fn{
		name: 'write_string'
		typ:  FnType{
			x: 1
		}
	}
	mut methods := []&Fn{}
	methods << method
	println(lookup(methods, 'write_string'))
}
