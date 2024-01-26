// vtest vflags: -autofree

fn return_array_with_result() ![]int {
	mut arr := []int{}
	arr << 123
	return arr
}

fn return_array_with_option() ?[]int {
	mut arr := []int{}
	arr << 123
	return arr
}

fn return_strng(s string) string {
	return s
}

fn test_main() {
	arr1 := return_array_with_result()!
	assert arr1 == [123]
	arr2 := return_array_with_option()?
	assert arr2 == [123]

	// test ident
	str1 := '${'123'}abc'
	str2 := str1
	assert str2 == '123abc'

	// test CallExpr
	str3 := return_strng(str1)
	assert str3 == '123abc'

	// test MatchExpr
	str4 := match true {
		true {
			str1
		}
		else {
			str1
		}
	}
	assert str4 == '123abc'

	// test IfExpr
	str5 := if true { str1 } else { str1 }
	assert str5 == '123abc'

	// test ParExpr
	// vfmt off
	str6 := (str1)
	// vfmt on
	assert str6 == '123abc'

	// test UnsafeExpr
	str7 := unsafe { str1 }
	assert str7 == '123abc'
}
