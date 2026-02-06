struct MyStruct {
	f_array_int    []int
	f_array_u8     []u8
	f_array_string []string
}

fn arrarr[T](x []T) string {
	$if T is $string {
		return 'arrarr string => ${x}'
	} $else $if T is u8 {
		return 'arrarr u8  => ${x}'
	} $else $if T is $int {
		return 'arrarr int  => ${x}'
	} $else {
		return 'arrarr unknown  => ${x}'
	}
}

fn test_comptime_for_selector_array() {
	x := MyStruct{
		f_array_int:    [10244]
		f_array_u8:     [u8(13)]
		f_array_string: ['hello', 'world']
	}
	mut out := ''
	$for f in MyStruct.fields {
		out += arrarr(x.$(f.name))
	}
	assert out == "arrarr int  => [10244]arrarr u8  => [13]arrarr string => ['hello', 'world']"
}
