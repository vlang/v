struct TestStruct {
	test string
}

fn test[T](val T) string {
	$if T is $struct {
		$for attribute in T.fields {
			$if attribute.name == 'test' {
				$if val.test in [u32, i32, $int] {
					return 'struct field ${typeof(val.test).name}'
				} $else {
					return 'got type: ${typeof(val.test).name}'
				}
			}
		}
		return 'no test field in struct'
	} $else {
		return 'empty'
	}
}

fn test_main() {
	assert test(TestStruct{'7'}) == 'got type: string'
}
