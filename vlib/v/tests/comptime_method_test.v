struct Test {}

struct Test2 {}

fn (t Test) t_0() ?int {
	return none
}

fn (t Test) t_1() int {
	return 0
}

fn (t Test) t_2() bool {
	return true
}

fn (t Test) t_3() Test {
	return t
}

fn test_main() {
	t := Test{}
	assert t_bool(t, 't_2')? == true
	assert t_int(t, 't_1')? == 0
	t_struct(t, 't_3')
}

fn t_bool[T](val T, method_name string) ?bool {
	$for f in T.methods {
		if method_name == f.name {
			$if f.return_type is bool {
				return val.$method()
			}
			$if f.return_type is int {
				val.$method()
			}
		}
	}
	return none
}

fn t_int[T](val T, method_name string) ?int {
	$for f in T.methods {
		if method_name == f.name {
			$if f.return_type is bool {
				val.$method()
			}
			$if f.return_type is int {
				return val.$method()
			}
		}
	}
	return none
}

fn t_struct[T](val T, method_name string) {
	mut s := map[string]string{}

	$for f in T.methods {
		$if f.return_type is Test2 {
			b := s[f.name] or {
				println('z')
				''
			}
			dump(b)
		}
		$if f.return_type is Test {
			c := val.$method() or { Test{} }
			dump(c)

			s[f.name] = f.name
			dump(s)
			assert f.name == 't_3'
			assert s[f.name] == 't_3'

			b := s[f.name] or {
				println('z')
				''
			}
			dump(b)
			return
		}
	}
	assert false
}
