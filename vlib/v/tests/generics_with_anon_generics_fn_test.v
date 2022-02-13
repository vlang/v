struct MyStruct<T> {
	arr []T
}

fn (mut s MyStruct<T>) get_data(pos int) T {
	return s.arr[pos]
}

fn (mut s MyStruct<T>) iterate(handler fn (T) int) int {
	mut sum := 0
	mut i := 0
	for {
		k := s.get_data<T>(i)
		sum += handler(k)
		i++
		if i > 4 {
			break
		}
	}
	return sum
}

pub fn consume(data int) int {
	return data
}

pub fn consume_str(data string) string {
	return data
}

fn call<T>(f fn (T) T, v T) T {
	return f(v)
}

struct Pair<T, U> {
	a T
	b U
}

fn test_generics_with_anon_generics_fn() {
	mut s := MyStruct<int>{
		arr: [1, 2, 3, 4, 5]
	}
	y := s.iterate<int>(consume)
	println(y)
	assert y == 15

	assert call<int>(consume, 1) == 1
	assert call<string>(consume_str, '1') == '1'
	assert call(consume, 1) == 1
	assert call(consume_str, '1') == '1'

	pair := Pair<int, string>{1, 's'}
	assert call(fn (v Pair<int, string>) Pair<int, string> {
		return v
	}, pair) == pair
}
