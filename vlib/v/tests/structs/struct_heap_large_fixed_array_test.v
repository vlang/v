// Test @[heap] structs with large fixed arrays.

@[heap]
struct LargeData {
	array [1024 * 1024]int
}

@[heap]
struct LargeDataWithFields {
	array [1024 * 1024]int
	value int = 42
	name  string
}

fn get_large_ref() &LargeData {
	d := LargeData{}
	return &d
}

fn get_large_with_fields_ref(name string) &LargeDataWithFields {
	d := LargeDataWithFields{
		name: name
	}
	return &d
}

fn test_heap_large_fixed_array_basic() {
	// Basic test: create a heap struct with large fixed array
	d := LargeData{}
	assert d.array[0] == 0
	assert d.array[1024 * 1024 - 1] == 0
}

fn test_heap_large_fixed_array_reference() {
	// Test returning reference to heap struct with large array
	ptr := get_large_ref()
	assert ptr.array[0] == 0
	assert ptr.array[500000] == 0
}

fn test_heap_large_fixed_array_with_fields() {
	// Test struct with large array and other fields
	d := LargeDataWithFields{
		name: 'test'
	}
	assert d.array[0] == 0
	assert d.value == 42 // default value
	assert d.name == 'test' // explicit value
}

fn test_heap_large_fixed_array_with_fields_ref() {
	// Test reference to struct with large array and fields
	ptr := get_large_with_fields_ref('hello')
	assert ptr.array[0] == 0
	assert ptr.value == 42
	assert ptr.name == 'hello'
}

fn test_heap_large_fixed_array_multiple() {
	// Test creating multiple large heap structs
	a := LargeData{}
	b := LargeData{}
	c := LargeDataWithFields{
		value: 100
		name:  'multi'
	}
	assert a.array[0] == 0
	assert b.array[0] == 0
	assert c.value == 100
	assert c.name == 'multi'
}
