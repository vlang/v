struct Struct {}

fn (s Struct) func() {}

fn test_main() {
	$for method in Struct.methods {
		println('${method.name}: ${method.return_type}')
		$if method.return_type == 1 {
			assert true
		}
		$if method.return_type == 11 {
			assert false
		}
	}
}
