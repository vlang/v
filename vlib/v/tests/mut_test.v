struct Aaa {
pub mut:
	v []int
}

struct Bbb {
pub mut:
	a []Aaa
}

fn foo(b int, mut a []int) {
	a[0] = 7
	// a << 4
}

fn test_mut() {
	mut numbers := [1, 2, 3]
	foo(7, mut numbers)
	assert numbers.len == 3
	// TODO bring back once << works with mutable args
	// assert numbers.len == 4
	// assert numbers[0] == 7
	// assert numbers[3] == 4
	println(numbers)
	n := 1
	mut b := (&n)
	//
	(*b) = 10
	// mut b := mut a
	// b = 10
}

fn test_mut_2() {
	zero := 0
	mut b := Bbb{}
	b.a << Aaa{}
	b.a[0].v = [9, 8, 7]
	b.a[0].v << 6
	b.a[zero].v << 5
	b.a[0].v[zero] = 3
	b.a[0].v[b.a[zero].v[zero]] += 2 - 1 // TODO
	b.a[0].v[b.a[0].v[zero]] += 2 - 1 // TODO
	assert b.a[0].v.len == 5
	assert b.a[0].v[0] == 3
	assert b.a[0].v[1] == 8
	assert b.a[0].v[2] == 7
	assert b.a[0].v[3] == 8
	assert b.a[0].v[4] == 5
}

fn test_mut_3() {
	mut indices := []int{len: 3}
	mut results := []string{}

	for i, mut v in indices {
		v = i
		a := v
		println('$i $v $a')
		results << '$i $v $a'
	}
	assert results[0] == '0 0 0'
	assert results[1] == '1 1 1'
	assert results[2] == '2 2 2'
}

struct St {
mut:
	n int
}

fn f(mut x St) {
	mut y := St{
		n: 2
	}
	a := x
	b := y
	x.n = 3
	y.n = 4
	println('$a.n $b.n')
	assert '$a.n $b.n' == '1 2'
}

fn test_mut_4() {
	mut x := St{
		n: 1
	}
	f(mut x)
}

fn test_mut_5() {
	mut arr1 := []int{len: 2}
	mut arr2 := []int{len: 2}
	mut results := []string{}

	for i, mut v in arr1 {
		for ii, mut vv in arr2 {
			v = i
			a := v
			println('$i $v $a')
			results << '$i $v $a'

			vv = ii
			aa := vv
			println('$ii $vv $aa')
			results << '$ii $vv $aa'
		}
	}

	assert results[0] == '0 0 0'
	assert results[1] == '0 0 0'
	assert results[2] == '0 0 0'
	assert results[3] == '1 1 1'
	assert results[4] == '1 1 1'
	assert results[5] == '0 0 0'
	assert results[6] == '1 1 1'
	assert results[7] == '1 1 1'
}

fn test_mut_6() {
	mut results := []int{}
	mut arr := []int{len: 3}
	for _, mut v in arr {
		v = v + 1
		println(v)
		results << v
	}
	assert results[0] == 1
	assert results[1] == 1
	assert results[2] == 1
}

fn test_mut_7() {
	mut arr := []int{len: 3}
	mut results := []int{}
	for _, mut v in arr {
		v = v + 1 // v: 1
		mut vv := v // vv: 1, v: 1
		vv = vv + v // vv: 2, v: 1
		println(v)
		println(vv)
		results << v
		results << vv
	}
	assert results[0] == 1
	assert results[1] == 2
	assert results[2] == 1
	assert results[3] == 2
	assert results[4] == 1
	assert results[5] == 2
}

fn test_mut_8() {
	mut indices := []int{len: 1}
	for i, mut v in indices {
		v = i
		mut b := v
		println(typeof(i).name)
		println(typeof(v).name)
		println(typeof(b).name)
		u := [v, 5, 6]
		println(typeof(u).name)
		println(u)
		assert typeof(b).name == 'int'
		assert typeof(u).name == '[]int'
		assert u == [0, 5, 6]
	}
}

fn test_mut_9() {
	mut arr := [0, 0, 0]
	mut results := []string{}
	for _, mut v in arr {
		v = v + 1 // v: 1
		mut vv := v // vv: 1, v: 1
		vv = vv + v // vv: 2, v: 1
		foo := map{
			'a': v
			'b': vv
		}
		println(v)
		println(vv)
		println(foo)
		results << '$v'
		results << '$vv'
		results << '$foo'
	}
	assert results[0] == '1'
	assert results[1] == '2'
	assert results[2] == "{'a': 1, 'b': 2}"
	assert results[3] == '1'
	assert results[4] == '2'
	assert results[5] == "{'a': 1, 'b': 2}"
	assert results[6] == '1'
	assert results[7] == '2'
	assert results[8] == "{'a': 1, 'b': 2}"
}

fn foo1(mut arr [][]int) {
	mut results := []int{}
	for _, mut j in arr {
		for _, mut k in j {
			k = k + 1 // k: 1
			mut kk := k // kk: 1, k: 1
			kk = kk + k // kk: 2, k: 1
			k++ // kk: 2, k: 2
			kk++ // kk: 3, k: 2
			println(k)
			println(kk)
			results << k
			results << kk
		}
	}
	assert results[0] == 2
	assert results[1] == 3
	assert results[2] == 2
	assert results[3] == 3
}

fn test_mut_10() {
	mut arr := [[0, 0]]
	foo1(mut arr)
}

fn foo2(mut arr [][]int) {
	mut results := []int{}
	for _, mut j in arr {
		for _, mut k in j {
			k = k + 1 // k: 1
			mut kk := k // kk: 1, k: 1
			kk = kk + k // kk: 2, k: 1
			k-- // kk: 2, k: 2
			kk-- // kk: 3, k: 2
			println(k)
			println(kk)
			results << k
			results << kk
		}
	}
	assert results[0] == 0
	assert results[1] == 1
	assert results[2] == 0
	assert results[3] == 1
}

fn test_mut_11() {
	mut arr := [[0, 0]]
	foo2(mut arr)
}

fn foo3(mut arr [][]int) {
	mut results := []string{}
	for _, mut j in arr {
		j[0] += 2
		println(j) // [2, 0]
		results << '$j'
	}
	assert results[0] == '[2, 0]'
}

fn test_mut_12() {
	mut arr := [[0, 0]]
	foo3(mut arr)
}

struct Foo {
mut:
	foo int
}

fn foo4(mut f Foo) {
	f2 := &f
	f.foo = 100
	println(f.foo)
	println(f2.foo)
	assert f.foo == 100
	assert f2.foo == 100
}

fn test_mut_13() {
	mut f := Foo{
		foo: 1
	}
	foo4(mut f)
}

fn foo5(mut arr []int) {
	arr2 := &arr
	arr[0] = 0
	println(arr[0]) // 0
	assert arr[0] == 0
	unsafe {
		println(arr2[0]) // 0
		assert arr2[0] == 0
	}
}

fn test_mut_14() {
	mut arr := [1, 2, 3]
	foo5(mut arr)
}

fn foo6(mut arr [3]int) {
	arr2 := &arr
	arr[0] = 0
	println(arr[0]) // 0
	assert arr[0] == 0
	unsafe {
		println(arr2[0]) // 0
		assert arr2[0] == 0
	}
}

fn test_mut_15() {
	mut arr := [1, 2, 3]!
	foo6(mut arr)
}

fn foo7(mut m map[string]int) {
	m2 := &m
	m['one'] = 1
	println(m['one']) // 1
	assert m['one'] == 1
	unsafe {
		println(m2['one']) // 1
		assert m2['one'] == 1
	}
}

fn test_mut_16() {
	mut m := map{
		'one': 100
		'two': 2
	}
	foo7(mut m)
}

fn test_mut_17() {
	mut arr := [map{
		'foo': 1
	}]
	for _, mut j in arr {
		mut k := j.clone()
		j['foo'] = 0
		unsafe {
			k['foo'] = 10
		}
		println(j)
		println(k)
		assert j == map{
			'foo': 0
		}
		assert k == map{
			'foo': 10
		}
	}
}

fn foo8(mut a [1]int) {
	a2 := a
	a[0] = 100
	println(a)
	println(a2)
	assert '$a' == '[100]'
	assert '$a2' == '[1]'
}

fn test_mut_18() {
	mut a := [1]!
	foo8(mut a)
}
