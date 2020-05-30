fn test_todo() {
}

fn simple<T>(p T) T {
	return p
}

fn plus<T>(xxx, b T) T {
	// x := a
	// y := b
	// ww := ww
	// q := xx + 1
	return xxx + b
}

fn test_generic_fn() {
	assert simple<int>(1) == 1
	assert simple<int>(1 + 0) == 1
	assert simple<string>('g') == 'g'
	assert simple<string>('g') + 'h' == 'gh'
	a := plus<int>(2, 3)
	println(a)
	assert a == 5
	assert plus<int>(10, 1) == 11
	assert plus<string>('a', 'b') == 'ab'
}

fn sum<T>(l []T) T {
	mut r := T(0)
	for e in l {
		r += e
	}
	return r
}

fn test_foo() {
	b := [1, 2, 3]
	assert sum<int>(b) == 6
}

fn create<T>() {
	a := T{}
	mut xx := T{}
	xx.foo = 'foo'
	println(xx.foo)
	assert xx.foo == 'foo'
	xx.init()
}

struct User {
mut:
	foo string
}

struct City {
mut:
	foo string
}

fn (u User) init() {
}

fn (c City) init() {
}

fn test_create() {
	create<User>()
	create<City>()
	// create<User>()
}

/*
fn map_f<T,U>(l []T, f fn(T)U) []U {
    mut r := []U{}
    for e in l {
        r << f(e)
    }
    return r
}

fn foldl<T>(l []T, nil T, f fn(T,T)T) T {
    mut r := nil
    for e in l {
        r = f(r, e)
    }
    return r
}

fn square(x int) int {
    return x*x
}

fn mul_int(x int, y int) int {
    return x*y
}

fn assert_eq<T>(a, b T) {
    r := a == b
    println('$a == $b: ${r.str()}')
    assert r
}

fn print_nice<T>(x T, indent int) {
    mut space := ''
    for _ in 0..indent {
        space = space + ' '
    }
    println('$space$x')
}

fn test_generic_fn() {
    assert_eq(simple(0+1), 1)
    assert_eq(simple('g') + 'h', 'gh')
    assert_eq(sum([5.1,6.2,7.0]), 18.3)
    assert_eq(plus(i64(4), i64(6)), i64(10))
    a := [1,2,3,4]
    b := map_f(a, square)
    assert_eq(sum(b), 30)     // 1+4+9+16 = 30
    assert_eq(foldl(b, 1, mul_int), 576)   // 1*4*9*16 = 576
    print_nice('str', 8)
}

struct Point {
mut:
    x f64
    y f64
}

fn (mut p Point) translate<T>(x, y T) {
    p.x += x
    p.y += y
}

fn test_generic_method() {
    mut p := Point{}
    p.translate(2, 1.0)
    assert p.x == 2.0 && p.y == 1.0
}

fn get_values<T>(i T) []T {
    return [i]
}

fn test_generic_fn_in_for_in_expression() {
    for value in get_values(1) {
        assert value == 1
    }

    for i, val in get_values(0) {
        assert i == val
    }

    for value in get_values('a') {
        assert value == 'a'
    }
}

// test generic struct
struct DB {
    driver string
}

struct User {
	db DB
mut:
	name string
}

struct Repo<T> {
	db DB
mut:
	model  T
}

fn new_repo<U>(db DB) Repo<U> {
	return Repo<U>{db: db}
}

fn test_generic_struct() {
	mut a :=  new_repo<User>(DB{})
	a.model.name = 'joe'
	mut b := Repo<User>{db: DB{}
	}
	b.model.name = 'joe'
	assert a.model.name == 'joe'
	assert b.model.name == 'joe'
}

//

struct Abc{ x int y int z int }

fn p<T>(args ...T) {
	size:=sizeof(T)
	print('p called with size: ${size:3d} | ')
	for _,x in args {
		print(x)
		print(' ')
	}
	println('')
	assert true
}

fn test_generic_fn_with_variadics(){
	s:='abc'
	i:=1
	abc:=Abc{1,2,3}
	// these calls should all compile, and print the arguments,
	// even though the arguments are all a different type and arity:
	p(s)
	p(i)
	p(abc)
	p('Good', 'morning', 'world')
}
*/
