fn assert_map[T]() {
	$if T is $map {
		assert true
	} $else {
		assert false
	}
}

fn assert_array[T]() {
	$if T is $array {
		assert true
	} $else {
		assert false
	}
}

fn assert_array_dynamic[T]() {
	$if T is $array_dynamic {
		assert true
	} $else {
		assert false
	}
}

fn assert_array_fixed[T]() {
	$if T is $array_fixed {
		assert true
	} $else {
		assert false
	}
}

fn assert_struct[T]() {
	$if T is $struct {
		assert true
	} $else {
		assert false
	}
}

fn assert_not_struct[T]() {
	$if T is $struct {
		assert false
	} $else {
		assert true
	}
}

fn assert_not_map[T]() {
	$if T is $map {
		assert false
	} $else {
		assert true
	}
}

fn assert_not_array[T]() {
	$if T is $array {
		assert false
	} $else {
		assert true
	}
}

fn assert_not_array_dynamic[T]() {
	$if T is $array_dynamic {
		assert false
	} $else {
		assert true
	}
}

fn assert_not_array_fixed[T]() {
	$if T is $array_fixed {
		assert false
	} $else {
		assert true
	}
}

struct Abc {}

struct Bc {}

struct Cd {}

fn test_kind_map() {
	assert_map[map[int]int]()
	assert_map[map[string]int]()
	assert_map[map[i64]i8]()

	assert_not_map[Abc]()
	assert_not_map[int]()
	assert_not_map[[]int]()
}

fn test_kind_array() {
	assert_array[[]int]()
	assert_array[[]f32]()
	assert_array[[]string]()

	assert_not_array[Abc]()
	assert_not_array[string]()
	assert_not_array[int]()
	assert_not_array[map[int]int]()
}

fn test_kind_array_dynamic() {
	assert_array_dynamic[[]int]()
	assert_array_dynamic[[]f32]()
	assert_array_dynamic[[]string]()

	assert_not_array_dynamic[Abc]()
	assert_not_array_dynamic[string]()
	assert_not_array_dynamic[int]()
	assert_not_array_dynamic[map[int]int]()
	assert_not_array_dynamic[[3]int]()
}

fn test_kind_array_fixed() {
	assert_array_fixed[[3]int]()
	assert_array_fixed[[5]f32]()
	assert_array_fixed[[6]string]()

	assert_not_array_fixed[Abc]()
	assert_not_array_fixed[string]()
	assert_not_array_fixed[int]()
	assert_not_array_fixed[map[int]int]()
	assert_not_array_fixed[[]int]()
}

fn test_kind_struct() {
	assert_struct[Abc]()
	assert_struct[Bc]()
	assert_struct[Cd]()

	assert_not_struct[int]()
	assert_not_struct[[]int]()
	assert_not_struct[map[int]int]()
}

//

type AliasOfAbc = Abc
type AliasOfint = int
type AliasOfstring = string

fn assert_alias[T]() {
	$if T is $alias {
		assert true
	} $else {
		assert false
	}
}

fn assert_not_alias[T]() {
	$if T is $alias {
		assert false
	} $else {
		assert true
	}
}

fn test_kind_alias() {
	assert_alias[AliasOfAbc]()
	assert_alias[AliasOfint]()
	assert_alias[AliasOfstring]()

	assert_not_alias[int]()
	assert_not_alias[f32]()
	assert_not_alias[[]int]()
	assert_not_alias[map[int]int]()
	assert_not_alias[Abc]()
}

//
fn assert_function[T](f T) {
	$if T is $function {
		assert true
	} $else {
		assert false
	}
}

fn assert_not_function[T](f T) {
	$if T is $function {
		assert false
	} $else {
		assert true
	}
}

fn test_kind_function() {
	assert_function(test_kind_function)
	assert_not_function(123)
	assert_function('abc'.contains)
	assert_function(5.str)
}
