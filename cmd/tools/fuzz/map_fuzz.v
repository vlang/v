import rand
import time

fn generate_strings(str_len int, arr_len int) []string {
	mut arr := []string{len: arr_len}
	for i in 0 .. arr_len {
		arr[i] = rand.string(str_len)
	}
	return arr
}

fn fuzz1() {
	amount := 200000 - rand.intn(100000) or { 0 }
	amount2 := 200000 - rand.intn(100000) or { 0 }
	len := 25 - rand.intn(10) or { 0 }
	arr := generate_strings(len, amount)
	arr2 := generate_strings(len, amount2)
	mut m := map[string]int{}
	for i in 0 .. amount {
		m[arr[i]] = i
		assert i == m[arr[i]]
	}
	for i in 0 .. amount {
		assert i == m[arr[i]]
	}
	for i in 0 .. amount2 {
		assert 0 == m[arr2[i]]
	}
	unsafe {
		m.free()
		arr.free()
	}
}

fn fuzz2() {
	mut m := map[string]int{}
	amount := rand.intn(500000) or { 0 } + 1
	len := 25 - rand.intn(10) or { 0 }
	arr := generate_strings(len, amount)
	for i, x in arr {
		m[x] = i
	}
	mut i := 0
	for key, val in m {
		assert key == arr[i]
		assert val == i
		i++
	}
	unsafe {
		m.free()
		arr.free()
	}
}

fn fuzz3() {
	mut m := map[string]int{}
	amount := rand.intn(500000) or { 0 } + 1
	len := 25 - rand.intn(10) or { 0 }
	arr := generate_strings(len, amount)
	for i, x in arr {
		if (i % 10000) == 0 {
			keys := m.keys()
			assert keys.len == i
			assert keys == arr[0..i]
		}
		m[x] = i
	}
	assert m.keys() == arr
	assert m.keys().len == amount
	unsafe {
		m.free()
		arr.free()
	}
}

fn fuzz4() {
	amount := rand.intn(500000) or { 0 }
	len := 25 - rand.intn(10) or { 0 }
	arr := generate_strings(len, amount)
	mut m := map[string]int{}
	for i in 0 .. amount {
		m[arr[i]] = i
	}
	for i in 0 .. amount {
		m.delete(arr[i])
		assert m[arr[i]] == 0
	}
	assert m.len == 0
	unsafe {
		m.free()
		arr.free()
	}
}

fn fuzz5() {
	amount := rand.intn(500000) or { 0 } + 1
	arr := generate_strings(20, amount)
	mut m := map[string]int{}
	for i in 0 .. amount {
		m[arr[i]] = i
		assert (arr[i] in m) == true
	}
	for i in 0 .. amount {
		m.delete(arr[i])
		assert (arr[i] !in m) == true
		assert m.len == amount - i - 1
	}
	assert m.len == 0
	unsafe {
		m.free()
		arr.free()
	}
}

fn fuzz6() {
	mut m := map[string]int{}
	amount := rand.intn(500000) or { 0 } + 1
	len := 25 - rand.intn(10) or { 0 }
	arr := generate_strings(len, amount)
	for i, x in arr {
		m[x]++
		m[x] += i
		assert m[x] == i + 1
	}
	for i, x in arr {
		assert m[x] == i + 1
	}
	unsafe {
		m.free()
		arr.free()
	}
}

fn main() {
	seed := u32(time.ticks())
	println('seed: $seed.hex()')
	rand.seed([seed, seed])
	fuzz1()
	fuzz2()
	fuzz3()
	fuzz4()
	fuzz5()
	fuzz6()
}
