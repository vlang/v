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
    mut y := St{n: 2}
    a := x
    b := y
    x.n = 3
    y.n = 4
    println('$a.n $b.n')
	assert '$a.n $b.n' == '1 2'
}

fn test_mut_4() {
    mut x := St{ n: 1 }
    f(mut x)
}

fn test_mut_5() {
    mut arr1 := []int{len:2}
    mut arr2 := []int{len:2}
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
			results << "$ii $vv $aa"
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
