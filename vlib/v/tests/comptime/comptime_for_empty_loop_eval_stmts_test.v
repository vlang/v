module main

fn encode_struct[T](val T) {
	$for _ in T.fields {
		a, b := 'fdsf'.split_once(',') or { '1', '2' }
		assert a == '1'
		assert b == '2'
	}
	$for _ in T.attributes {
		a, b := 'fdsf'.split_once(',') or { '1', '2' }
		assert a == '1'
		assert b == '2'
	}
	$for _ in T.methods {
		a, b := 'fdsf'.split_once(',') or { '1', '2' }
		assert a == '1'
		assert b == '2'
	}
	$for m in T.methods {
		$for _ in m.params {
			a, b := 'fdsf'.split_once(',') or { '1', '2' }
			assert a == '1'
			assert b == '2'
		}
	}
}

struct EmptyStruct {
}

struct EmptyParam {
}

// method without params
fn (e EmptyParam) method() {
}

fn test_comptime_for_empty_loop_eval_stmts() {
	x := EmptyStruct{}
	encode_struct(x)

	y := EmptyParam{}
	encode_struct(y)
}
