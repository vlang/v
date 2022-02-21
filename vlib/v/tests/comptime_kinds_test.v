fn assert_map<T>() {
	$if T is $Map {
		assert true
	} $else {
		assert false
	}
}

fn assert_array<T>() {
	$if T is $Array {
		assert true
	} $else {
		assert false
	}
}

fn assert_struct<T>() {
	$if T is $Struct {
		assert true
	} $else {
		assert false
	}
}

fn assert_not_struct<T>() {
	$if T is $Struct {
		assert false
	} $else {
		assert true
	}
}

fn assert_not_map<T>() {
	$if T is $Map {
		assert false
	} $else {
		assert true
	}
}

fn assert_not_array<T>() {
	$if T is $Array {
		assert false
	} $else {
		assert true
	}
}

struct Abc {}

struct Bc {}

struct Cd {}

fn test_kind_map() {
	assert_map<map[int]int>()
	assert_map<map[string]int>()
	assert_map<map[i64]i8>()

	assert_not_map<Abc>()
	assert_not_map<int>()
	assert_not_map<[]int>()
}

fn test_kind_array() {
	assert_array<[]int>()
	assert_array<[]f32>()
	assert_array<[]string>()

	assert_not_array<Abc>()
	assert_not_array<string>()
	assert_not_array<int>()
	assert_not_array<map[int]int>()
}

fn test_kind_struct() {
	assert_struct<Abc>()
	assert_struct<Bc>()
	assert_struct<Cd>()

	assert_not_struct<int>()
	assert_not_struct<[]int>()
	assert_not_struct<map[int]int>()
}
