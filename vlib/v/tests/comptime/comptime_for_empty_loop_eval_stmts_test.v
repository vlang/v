module main

fn encode_struct[T](val T) {
	$for attr in T.attributes {
		a, b := 'fdsf'.split_once(',') or { '1', '2' }
		assert a == '1'
		assert b == '2'
	}
}

struct MyS {
}

fn test_comptime_for_empty_loop_eval_stmts() {
	x := MyS{}
	encode_struct(x)
}
