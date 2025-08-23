struct OtherStruct {
	test string
}

struct I32Struct {
	test i32
}

struct U32Struct {
	test u32
}

fn test[T](val T) string {
	$if val is $struct {
		println(T.name)
		$for attribute in T.fields {
			$if attribute.name == 'test' {
				$if val.test in [u32, i32] {
					return 'struct field ${typeof(val.test).name}'
				} $else {
					return 'not u32 or i32 struct field, got type: ${typeof(val.test).name}'
				}
			}
		}
		return 'no test field in struct'
	} $else {
		return 'else block'
	}
}

fn test_main() {
	assert test(u32(7)) == 'else block'
	assert test(OtherStruct{'7'}) == 'struct field string'
	assert test(I32Struct{-7}) == 'struct field i32'
	assert test(U32Struct{7}) == 'struct field u32'
}
