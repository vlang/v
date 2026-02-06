type Abc = bool | int | string

fn test_aggregate_is_nodetype() {
	x := Abc('test')

	match x {
		string, int {
			if x is string {
				println('it is a string')
				assert true
			} else {
				assert false
			}
		}
		else {
			assert false
		}
	}
}
