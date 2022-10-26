struct Set<T> {
pub mut:
	arr []T
}

fn (mut s Set<T>) add(elem T) bool {
	return match s.has(elem) {
		true {
			false
		}
		else {
			s.arr << elem
			true
		}
	}
}

fn (s Set<T>) has(elem T) bool {
	return elem in s.arr
}

fn test_generics_struct_with_array() {
	mut s1 := Set<int>{}
	println('declared a int-Set ${s1}')
	s1.add(1)
	println('the int-Set ${s1}')
	assert s1.arr == [1]

	mut s2 := Set<bool>{}
	println('declared a bool-Set ${s2}')
	s2.add(true)
	println('the bool-Set ${s2}')
	assert s2.arr == [true]

	mut s3 := Set<f64>{}
	println('declared a float-Set ${s3}')
	s3.add(2.22)
	println('the float-Set ${s3}')
	assert s3.arr == [2.22]

	mut s4 := Set<string>{}
	println('declared a string-Set ${s4}')
	s4.add('smth')
	println('the string-Set ${s4} ')
	assert s4.arr == ['smth']
}
