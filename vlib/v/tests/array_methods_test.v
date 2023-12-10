struct Counter {
mut:
	val int
}

// if this is called more than once, the test'll fail
fn (mut c Counter) new_arr(msg string) []int {
	if c.val > 0 {
		panic(msg)
	}
	c.val++
	return [1, 3, 2]
}

fn test_array_eval_count() {
	// `new_arr()` should only be evaluated once, not on every iteration
	mut a1 := Counter{}
	assert a1.new_arr('map() failed').map(it * 2) == [2, 6, 4]

	mut a2 := Counter{}
	assert a2.new_arr('filter() failed').filter(it < 3) == [1, 2]

	mut a3 := Counter{}
	assert a3.new_arr('any() failed').any(it == 2) == true
	a3 = Counter{}
	assert a3.new_arr('any() failed').any(it < 0) == false

	mut a4 := Counter{}
	assert a4.new_arr('all() failed').all(it > 0) == true
	a4 = Counter{}
	assert a4.new_arr('all() failed').all(it == 2) == false
}

fn opt_bool_fn() ?bool {
	return true
}

fn test_any_called_with_opt_bool_fn() ? {
	_ := [1, 2, 3].any(opt_bool_fn()?)
	assert true
}

interface Args {}

const some_strings = ['one', 'two', 'three']

// For test `gen array contains method`
fn test_array_contains_method_with_interface() {
	arg := Args('one')
	match arg {
		string {
			if arg in some_strings {
				assert true
				return
			}
		}
		else {}
	}
	assert false
}

// For test `gen string_eq method`
fn test_string_eq_method_with_interface() {
	arg := Args('three')
	match arg {
		string {
			if arg in ['one', 'two', 'three'] {
				assert true
				return
			}
		}
		else {}
	}
	assert false
}

// test deref when alias as receiver of methods
type Array = []int

pub fn (mut arr Array) alias_as_receiver_deref() []int {
	return arr.sorted(b < a)
}

fn test_alias_as_receiver_deref() {
	mut arr := Array([1, 2, 3])
	assert arr.alias_as_receiver_deref() == [3, 2, 1]
}
