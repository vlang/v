const (
	unsorted    = [2, 30, 10, 20, 1]
	sorted_asc  = [1, 2, 10, 20, 30]
	sorted_desc = [30, 20, 10, 2, 1]
)

fn test_sorting_simple() {
	mut a := unsorted.clone()
	a.sort()
	eprintln(' a: $a')
	assert a == sorted_asc
}

fn test_sorting_with_condition_expression() {
	mut a := unsorted.clone()
	a.sort(a > b)
	eprintln(' a: $a')
	assert a == sorted_desc
}

fn test_sorting_primitives_with_condition_expression() {
	mut x := ['9', '87', '3210', '654']
	x.sort(a.len < b.len)
	assert x == ['9', '87', '654', '3210']
}

fn get_score(word string) int {
	mut total := 0
	for letter in word {
		total += int(letter) - 97
	}
	return total
}

fn test_sorting_with_fn_call_in_condition_expression() {
	mut words := ['aaaa', 'a', 'b', 'foo', 'bar']
	words.sort(get_score(a) < get_score(b))
}

fn mysort(mut a []int) {
	a.sort()
}

fn test_sorting_by_passing_a_mut_array_to_a_function() {
	mut a := unsorted.clone()
	mysort(mut a)
	eprintln(' a: $a')
	assert a == sorted_asc
}

/*
fn test_sorting_by_passing_an_anonymous_sorting_function() {
	mut a := unsorted
	a.sort(fn(a &int, b &int) int {	return *b - *a })
	eprintln(' a: $a')
	assert a == sort_desc
}
*/
fn test_sorting_u64s() {
	mut a := [u64(3), 2, 1, 9, 0, 8]
	a.sort()
	eprintln(' a: $a')
	assert a == [u64(0), 1, 2, 3, 8, 9]
	a.sort(a > b)
	eprintln(' a: $a')
	assert a == [u64(9), 8, 3, 2, 1, 0]
}
