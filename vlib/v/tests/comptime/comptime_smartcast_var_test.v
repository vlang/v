type TestSum = bool | f64 | int | string

fn test_main() {
	a := TestSum(true)
	$for v in TestSum.variants {
		if a is v {
			$if a is bool {
				assert a == true
			}
			$if a is string {
				assert a == ''
			}
			$if a is f64 {
				assert a == 0
			}
			$if a is int {
				assert a == 0
			}
		}
	}
	assert true
}
