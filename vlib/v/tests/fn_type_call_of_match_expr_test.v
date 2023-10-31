type FnZ = fn () string

type FnO = fn (int) string

type FnQ = FnO | FnZ

fn fnz_z() string {
	return 'Got zero'
}

fn fnz_o(one int) string {
	return 'Got one ${one}'
}

fn test_fn_type_call_of_match_expr() {
	mut arr := [FnQ(FnZ(fnz_z)), FnQ(FnO(fnz_o))]
	for item in arr {
		match item {
			FnZ {
				println(item())
				assert item() == 'Got zero'
			}
			FnO {
				println(item(42))
				assert item(42) == 'Got one 42'
			}
		}
	}
}
