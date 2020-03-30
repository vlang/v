fn simple<T>(p T) T {
    return p
}

fn sum<T>(l []T) T {
    mut r := T(0)
    for e in l {
        r += e
    }
    return r
}

fn map_f<T,U>(l []T, f fn(T)U) []U {
    mut r := []U
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

fn plus<T>(a T, b T) T {
    return a+b
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

fn (p mut Point) translate<T>(x, y T) {
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
	mut b := Repo<User>{db: DB{}}
	b.model.name = 'joe'
	assert a.model.name == 'joe'
	assert b.model.name == 'joe'
}

