// Test 1: Basic callbacks with struct fields
struct FooProps {
	name        string
	update      fn (name string) @[required] // Required callback
	optional_cb ?fn () // Optional callback
}

fn foo(params FooProps) {
	params.update(params.name)
	if params.optional_cb != none {
		params.optional_cb()
	}
}

// Test 2: Callbacks with return values
struct CalculateProps {
	operation fn (a int, b int) int @[required]
}

fn calculate(a int, b int, calc CalculateProps) int {
	return calc.operation(a, b)
}

// Test 3: Callbacks in arrays
fn map_array(arr []int, mapper fn (int) int) []int {
	mut result := []int{cap: arr.len}
	for item in arr {
		result << mapper(item)
	}
	return result
}

// Test 4: Nested callbacks
fn execute_with_callback(data string, processor fn (string, fn (string) string) string, formatter fn (string) string) string {
	return processor(data, formatter)
}

// Test 5: Callback with mutable reference
struct Counter {
mut:
	count int
}

fn with_counter(mut c Counter, callback fn (mut c Counter)) {
	callback(mut c)
}

fn main() {
	println('=== Test 1: Basic callbacks with struct fields ===')

	// 1.1: Anonymous function callback with optional callback
	println('1.1: Anonymous function with optional callback')
	foo(
		name:        'item 1'
		update:      fn (name string) {
			println('update ${name}')
		}
		optional_cb: fn () {
			println('optional callback')
		}
	)

	// 1.2: Lambda function callback without optional callback
	println('\n1.2: Lambda function without optional callback')
	update := fn (name string) {
		println('update ${name}')
	}
	foo(name: 'item 2', update: update)

	// 1.3: Anonymous function field
	println('\n1.3: Anonymous function field')
	item_3 := FooProps{
		name:   'item 3'
		update: fn (name string) {
			println('update ${name}')
		}
	}
	item_3.update(item_3.name)

	// 1.4: Lambda function field
	println('\n1.4: Lambda function field')
	item_4 := FooProps{
		name:   'item 4'
		update: update
	}
	item_4.update(item_4.name)

	println('\n=== Test 2: Callbacks with return values ===')

	// 2.1: Addition callback
	println('2.1: Addition callback')
	add := fn (a int, b int) int {
		return a + b
	}
	add_calc := CalculateProps{
		operation: add
	}
	println('5 + 3 = ${calculate(5, 3, add_calc)}')

	// 2.2: Anonymous multiplication callback
	println('\n2.2: Anonymous multiplication callback')
	mult_result := calculate(4, 6, CalculateProps{
		operation: fn (a int, b int) int {
			return a * b
		}
	})
	println('4 * 6 = ${mult_result}')

	println('\n=== Test 3: Callbacks in arrays ===')

	// 3.1: Double each element
	println('3.1: Double each element')
	numbers := [1, 2, 3, 4, 5]
	doubled := map_array(numbers, fn (n int) int {
		return n * 2
	})
	println('Original: ${numbers}')
	println('Doubled: ${doubled}')

	// 3.2: Square each element using lambda
	println('\n3.2: Square each element')
	square := fn (n int) int {
		return n * n
	}
	squared := map_array(numbers, square)
	println('Original: ${numbers}')
	println('Squared: ${squared}')

	println('\n=== Test 4: Nested callbacks ===')

	// 4.1: Process and format text
	println('4.1: Process and format text')
	processor := fn (data string, formatter fn (string) string) string {
		processed := data + ' processed'
		return formatter(processed)
	}

	formatter := fn (s string) string {
		return '[${s.to_upper()}]'
	}

	result := execute_with_callback('data', processor, formatter)
	println('Result: ${result}')

	println('\n=== Test 5: Callback with mutable reference ===')

	// 5.1: Increment counter
	println('5.1: Increment counter')
	mut counter := Counter{
		count: 0
	}

	increment := fn (mut c Counter) {
		c.count++
		println('Counter incremented to ${c.count}')
	}

	with_counter(mut counter, increment)
	with_counter(mut counter, increment)
	with_counter(mut counter, increment)

	println('Final counter value: ${counter.count}')
}
