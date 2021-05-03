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

fn test_generics_with_anon_generics_fn() {
	mut s := MyStruct<int>{
		arr: [1, 2, 3, 4, 5]
	}
	y := s.iterate<int>(consume)
	println(y)
	assert y == 15
}
