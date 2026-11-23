import itmethods

const words = ['a', 'b']

struct Tally {
	seen string
}

fn (t Tally) count(word string) int {
	return t.seen.count(word)
}

fn (t Tally) any(word string) bool {
	return t.seen.count(word) == 1
}

fn (t Tally) all(word string) bool {
	return t.seen.count(word) == 1
}

fn (t Tally) filter(word string) string {
	return word
}

fn (t Tally) map(word string) string {
	return word
}

fn test_string_count_uses_outer_it() {
	seen := 'ab'
	assert words.all(seen.count(it) == 1)
	assert words.any(seen.count(it) == 1)
	assert words.filter(seen.count(it) == 1) == ['a', 'b']
	assert words.map(seen.count(it)) == [1, 1]
	assert words.count(seen.count(it) == 1) == 2
}

fn test_user_methods_use_outer_it() {
	tally := Tally{'ab'}
	assert words.all(tally.count(it) == 1)
	assert words.all(tally.any(it))
	assert words.all(tally.all(it))
	assert words.all(tally.filter(it) == it)
	assert words.all(tally.map(it) == it)
	assert words.map(tally.count(it)) == [1, 1]
	assert words.count(tally.any(it)) == 2
}

type Numbers = []int

type FixedNumbers = [2]int

struct App {
	nums []int
}

fn (app App) numbers() []int {
	return app.nums
}

type ArrayValue = []int | string

fn mut_parameter(mut nums []int) [2]bool {
	outer := [10, 20]
	return [outer.all(nums.any(it == 2)), outer.all(nums.count(it == 2) == 1)]!
}

fn reference_parameter(nums &[]int) [2]bool {
	outer := [10, 20]
	return [outer.all(nums.any(it == 2)), outer.all(nums.count(it == 2) == 1)]!
}

fn variadic_parameter(nums ...int) [2]bool {
	outer := [10, 20]
	return [outer.all(nums.any(it == 2)), outer.all(nums.count(it == 2) == 1)]!
}

fn generic_parameter[T](nums []T) [2]bool {
	outer := [10, 20]
	return [outer.all(nums.any(it == 2)), outer.all(nums.count(it == 2) == 1)]!
}

fn smartcast_parameter(value ArrayValue) [2]bool {
	outer := [10, 20]
	if value is []int {
		return [outer.all(value.any(it == 2)), outer.all(value.count(it == 2) == 1)]!
	}
	return [false, false]!
}

fn optional_numbers() ?[]int {
	return [1, 2]
}

fn test_nested_array_methods_keep_inner_it() {
	outer := [10, 20]
	mut nums := [1, 2]
	assert outer.all(nums.any(it == 2))
	assert outer.all(nums.count(it == 2) == 1)
	fixed := [1, 2]!
	assert outer.all(fixed.all(it < 3))
	aliased := Numbers([1, 2])
	assert outer.all(aliased.filter(it == 2) == [2])
	fixed_alias := FixedNumbers([1, 2]!)
	assert outer.all(fixed_alias.any(it == 2))

	assert mut_parameter(mut nums) == [true, true]!
	assert reference_parameter(&nums) == [true, true]!
	app := App{[1, 2]}
	assert outer.all(app.nums.any(it == 2))
	assert outer.all(app.nums.count(it == 2) == 1)
	p := &App{[1, 2]}
	assert outer.all(p.nums.any(it == 2))
	assert outer.all(p.nums.count(it == 2) == 1)
	assert outer.all(app.numbers().any(it == 2))
	assert outer.all(app.numbers().count(it == 2) == 1)
	grid := [[1, 2]]
	assert outer.all(grid[0].any(it == 2))
	assert outer.all(grid[0].count(it == 2) == 1)
	m := {
		'k': [1, 2]
	}
	assert outer.all(m['k'].any(it == 2))
	assert outer.all(m['k'].count(it == 2) == 1)
	assert outer.all((optional_numbers() or { []int{} }).any(it == 2))
	assert outer.all((optional_numbers() or { []int{} }).count(it == 2) == 1)
	assert variadic_parameter(1, 2) == [true, true]!
	assert generic_parameter(nums) == [true, true]!
	captured := fn [nums] () [2]bool {
		outer := [10, 20]
		return [outer.all(nums.any(it == 2)), outer.all(nums.count(it == 2) == 1)]!
	}
	assert captured() == [true, true]!
	assert smartcast_parameter(ArrayValue(nums)) == [true, true]!
}

fn test_array_smartcast_if_inside_callback() {
	outer := [10, 20]
	value := ArrayValue([1, 2])
	assert outer.all(if value is []int { value.any(it == 2) } else { false })
	assert outer.all(if value is []int { value.count(it == 2) == 1 } else { false })
	assert outer.any(if value is []int { value.any(it == 2) } else { false })
	assert outer.any(if value is []int { value.count(it == 2) == 1 } else { false })
	assert outer.filter(if value is []int { value.any(it == 2) } else { false }) == [
		10,
		20,
	]
	assert outer.filter(if value is []int { value.count(it == 2) == 1 } else { false }) == [
		10,
		20,
	]
	assert outer.all(if value is []int { value.all(it < 3) } else { false })
	assert outer.all(if value is []int { value.filter(it == 2) == [2] } else { false })
	assert outer.all(if value is []int { value.map(it + 1) == [2, 3] } else { false })
}

fn test_array_smartcast_match_inside_callback() {
	outer := [10, 20]
	value := ArrayValue([1, 2])
	assert outer.all(match value {
		[]int { value.any(it == 2) }
		string { false }
	})
	assert outer.all(match value {
		[]int { value.count(it == 2) == 1 }
		string { false }
	})
	assert outer.any(match value {
		[]int { value.any(it == 2) }
		string { false }
	})
	assert outer.any(match value {
		[]int { value.count(it == 2) == 1 }
		string { false }
	})
	assert outer.filter(match value {
		[]int { value.any(it == 2) }
		string { false }
	}) == [10, 20]
	assert outer.filter(match value {
		[]int { value.count(it == 2) == 1 }
		string { false }
	}) == [10, 20]
}

fn test_array_smartcast_and_inside_callback() {
	outer := [10, 20]
	value := ArrayValue([1, 2])
	assert outer.all(value is []int && value.any(it == 2))
	assert outer.all(value is []int && value.count(it == 2) == 1)
	assert outer.any(value is []int && value.any(it == 2))
	assert outer.any(value is []int && value.count(it == 2) == 1)
	assert outer.filter(value is []int && value.any(it == 2)) == [10, 20]
	assert outer.filter(value is []int && value.count(it == 2) == 1) == [10, 20]
}

interface Box {}

fn test_interface_array_smartcast_inside_callback() {
	outer := [10, 20]
	value := Box([1, 2])
	assert outer.all(if value is []int { value.any(it == 2) } else { false })
	assert outer.all(if value is []int { value.count(it == 2) == 1 } else { false })
	assert outer.filter(value is []int && value.count(it == 2) == 1) == [10, 20]
}

fn assert_generic_receiver_scope[T](value T) {
	outer := [10, 20]
	assert outer.all(value.any(it == 2))
	assert outer.all(value.count(it == 2) == 1)
}

fn result_numbers() ![]int {
	return [1, 2]
}

fn test_wrapped_and_generic_array_callback_receivers() {
	outer := [10, 20]
	value := ?[]int([1, 2])
	assert outer.all(if nums := value { nums.any(it == 2) } else { false })
	assert outer.all(if nums := value { nums.count(it == 2) == 1 } else { false })
	assert_generic_receiver_scope([1, 2])
	assert outer.all((result_numbers() or { []int{} }).any(it == 2))
	assert outer.all((result_numbers() or { []int{} }).count(it == 2) == 1)
}

struct Receiver {}

struct ChainedReceiver {
	n int
}

fn (r ChainedReceiver) map(x int) ChainedReceiver {
	return ChainedReceiver{ n: x }
}

fn (r ChainedReceiver) any(x int) bool {
	return r.n == x
}

fn assert_chained_receiver(it int) {
	assert it == 7
	r := ChainedReceiver{}
	outer := [10, 20]
	assert outer.all(r.map(it).any(it))
}

fn test_chained_receiver_uses_callback_it_in_both_calls() {
	assert_chained_receiver(7)
}

fn (r Receiver) any(f fn (int) bool) bool {
	return f(2)
}

fn (r Receiver) map(f fn (int) bool) bool {
	return f(2)
}

fn (r Receiver) check(f fn (int) bool) bool {
	return f(2)
}

fn test_function_literal_parameter_shadows_outer_it() {
	outer := [10, 20]
	r := Receiver{}
	assert outer.all(r.any(fn (it int) bool { return it == 2 }))
	assert outer.all(r.map(fn (it int) bool { return it == 2 }))
	assert outer.all(r.check(fn (it int) bool { return it == 2 }))
}

struct ShadowRow {
	rows []int
}

fn assert_shadowed_parameter(it int, rows [][]int, expected_all bool, expected_any bool, expected_filtered [][]int) {
	assert rows.all(it.any(it == 2)) == expected_all
	assert rows.all(it.all(it < 4)) == expected_all
	assert rows.all(it.count(it == 2) == 1) == expected_all
	assert rows.all(it.filter(it == 2) == [2]) == expected_all
	assert rows.all(it.map(it < 4) == [true, true]) == expected_all
	assert rows.any(it.any(it == 2)) == expected_any
	assert rows.any(it.all(it < 4)) == expected_any
	assert rows.any(it.count(it == 2) == 1) == expected_any
	assert rows.any(it.filter(it == 2) == [2]) == expected_any
	assert rows.any(it.map(it < 4) == [true, true]) == expected_any
	assert rows.filter(it.any(it == 2)) == expected_filtered
	assert rows.filter(it.all(it < 4)) == expected_filtered
	assert rows.filter(it.count(it == 2) == 1) == expected_filtered
	assert rows.filter(it.filter(it == 2) == [2]) == expected_filtered
	assert rows.filter(it.map(it < 4) == [true, true]) == expected_filtered
}

fn assert_shadowed_element_receivers(it int) {
	assert [ShadowRow{ rows: [1, 2] }].all(it.rows.any(it == 2))
	assert [[[1, 2]]].all(it[0].count(it == 2) == 1)
}

fn test_nested_array_methods_shadow_enclosing_it() {
	assert_shadowed_parameter(7, [[1, 2], [2, 3]], true, true, [[1, 2], [2, 3]])
	assert_shadowed_parameter(7, [[1, 5], [2, 3]], false, true, [[2, 3]])
	assert_shadowed_parameter(7, [[1, 5], [4, 5]], false, false, [][]int{})
	for it in [7] {
		assert [[1, 2], [2, 3]].all(it.count(it == 2) == 1)
	}
	it := 7
	assert [[1, 2], [2, 3]].all(it.any(it == 2))
	captured := fn [it] () bool {
		return [[1, 2], [2, 3]].all(it.any(it == 2))
	}
	assert captured()
	assert_shadowed_element_receivers(7)
}

fn assert_shadowed_array_binding(it []int, words []string, expected_all bool, expected_any bool, expected_filtered []string) {
	assert it == [7]
	assert words.all(it.count('a') == 1) == expected_all
	assert words.any(it.count('a') == 1) == expected_any
	assert words.filter(it.count('a') == 1) == expected_filtered
}

fn test_string_element_shadows_enclosing_array_it() {
	assert_shadowed_array_binding([7], ['a', 'ba'], true, true, ['a', 'ba'])
	assert_shadowed_array_binding([7], ['bb', 'ba'], false, true, ['ba'])
	assert_shadowed_array_binding([7], ['bb', 'cc'], false, false, []string{})
}

fn rebound_numbers() ?[]int {
	return [1, 2]
}

fn rebound_grid() ?[][]int {
	return [[1, 2], [3]]
}

fn rebound_row() ?ShadowRow {
	return ShadowRow{ rows: [1, 2] }
}

fn rebound_index() ?[][]int {
	return [[1, 2]]
}

fn test_array_methods_with_rebound_it() {
	outer := [10, 20]
	assert outer.all(if it := rebound_grid() { it.any(it == [1, 2]) } else { false })
	assert !outer.all(if it := rebound_grid() { it.all(it.len == 2) } else { false })
	assert outer.filter(if it := rebound_grid() { it.any(it.len == 1) } else { false }) == outer
	assert outer.all(if it := rebound_numbers() { it.any(it == 2) } else { false })
	assert outer.any(if it := rebound_numbers() { it.any(it == 2) } else { false })
	assert outer.filter(if it := rebound_numbers() { it.any(it == 2) } else { false }) == outer
	assert outer.all(if it := rebound_row() { it.rows.any(it == 2) } else { false })
	assert outer.filter(if it := rebound_index() { it[0].any(it == 2) } else { false }) == outer
	grid := [[[1, 2], [3]]]
	assert outer.all(if true {
		mut ok := false
		for it in grid {
			ok = it.any(it == [1, 2])
		}
		ok
	} else {
		false
	})
	names := ['a', 'b']
	groups := {
		'a': [1, 2]
		'b': [3]
	}
	assert !names.all(if it := groups[it] { it.any(it == 2) } else { false })
	assert names.any(if it := groups[it] { it.any(it == 2) } else { false })
	assert names.filter(if it := groups[it] { it.any(it == 2) } else { false }) == ['a']
	assert outer.filter([[1, 2]].any(|it| it.any(it == 2))) == outer
}

struct MatchReceiver {}

fn (r MatchReceiver) any(x bool) bool { return x }

type ReturnChoice = []int | MatchReceiver | string

fn return_array_first(c ReturnChoice, it int) bool {
	outer := [10, 20]
	return match c {
		[]int, MatchReceiver { outer.any(c.any(it > 15)) }
		string { false }
	}
}

fn return_array_last(c ReturnChoice, it int) bool {
	outer := [10, 20]
	return match c {
		MatchReceiver, []int { outer.any(c.any(it > 15)) }
		string { false }
	}
}

fn test_return_match_keeps_array_it() {
	assert !return_array_first(ReturnChoice([1, 2]), 7)
	assert !return_array_last(ReturnChoice([1, 2]), 7)
}

struct InitReceiver {}

fn (r InitReceiver) any(xs []int) string { return xs.str() }

fn (r InitReceiver) check(xs []int) string { return xs.str() }

fn test_initializer_it_is_the_index() {
	it := 7
	r := InitReceiver{}
	outer := [10, 20]
	// Preserve the legacy spelling to exercise its index binding.
	// vfmt off
	assert outer.map(r.any([]int{len: 2, init: it})) == ['[0, 1]', '[0, 1]']
	assert outer.map(r.check([]int{len: 2, init: it})) == ['[0, 1]', '[0, 1]']
	assert outer.filter(r.any([]int{len: 2, init: it}) == '[0, 1]') == outer
	assert outer.filter(r.check([]int{len: 2, init: it}) == '[0, 1]') == outer
	assert outer.map(r.any([]int{len: it / 10, init: it})) == ['[0]', '[0, 1]']
	// vfmt on
}

struct ValueReceiver {}

fn (r ValueReceiver) any(x int) bool { return x == 10 }

fn ValueReceiver.any(xs []int) bool { return xs == [10] }

fn test_value_patterns_static_and_module_calls() {
	it := 7
	r := ValueReceiver{}
	outer := [10, 20]
	tag := 1
	assert outer.any(match tag {
		1, 2 { r.any(it) }
		else { false }
	})
	assert outer.any(ValueReceiver.any([it]))
	assert outer.any(itmethods.any(it))
	assert outer.any(itmethods.count([it]))
}
