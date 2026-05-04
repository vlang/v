interface Named {
	name string
}

fn (n Named) hello() string {
	return 'hello ${n.name}'
}

struct Sa {
	name string
	age  int
}

fn (s Sa) ttt() string {
	return '${s.name} // ${s.age}'
}

struct Sb {
	name string
}

type Sum = Sa | Sb

fn rebound_interface_method(t Sum) string {
	match t {
		Sa {
			tt := t as Sa
			mut out := unsafe { (&Sa(&t)).ttt }
			iface := unsafe { &Named(&tt) }
			out = iface.hello
			return out()
		}
		Sb {
			return 'sb'
		}
	}
}

fn test_reassign_bound_method_from_interface_pointer_inside_sumtype_match() {
	assert rebound_interface_method(Sa{ name: 'mytype', age: 10 }) == 'hello mytype'
}
