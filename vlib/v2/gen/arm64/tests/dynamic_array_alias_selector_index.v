type ValueID = int
type TypeID = int

struct Value {
	id    int
	typ   TypeID
	index int
}

struct Function {
	id   int
	name string
	typ  TypeID
mut:
	blocks []int
	params []ValueID
}

struct Module {
mut:
	values []Value
	funcs  []Function
}

fn sum_param_types(mut m Module) int {
	mut out := 0
	params := m.funcs[0].params.clone()
	for i in [0, 1] {
		if i < params.len {
			param := params[i]
			out += int(m.values[param].typ)
		}
	}
	return out
}

fn main() {
	mut m := Module{}
	for i := 0; i < 200; i++ {
		m.values << Value{
			id:    i
			typ:   TypeID(i + 10)
			index: i
		}
	}
	m.funcs << Function{
		id:     0
		name:   'f'
		typ:    TypeID(0)
		params: [ValueID(134), ValueID(135)]
	}
	println(sum_param_types(mut m))
}
