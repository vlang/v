import simplemodule

fn simple[T](p T) T {
	return p
}

fn test_identity() {
	assert simple[int](1) == 1
	assert simple[int](1 + 0) == 1
	assert simple[string]('g') == 'g'
	assert simple[string]('g') + 'h' == 'gh'

	assert simple[[]int]([1])[0] == 1
	assert simple[map[string]string]({
		'a': 'b'
	})['a'] == 'b'

	assert simple[simplemodule.Data](simplemodule.Data{ value: 8 }).value == 8
	x := &simplemodule.Data{
		value: 123
	}
	assert simple[&simplemodule.Data](x).value == 123
}

fn plus[T](xxx T, b T) T {
	// x := a
	// y := b
	// ww := ww
	// q := xx + 1
	return xxx + b
}

fn test_infix_expr() {
	a := plus[int](2, 3)
	assert a == 5
	assert plus[int](10, 1) == 11
	assert plus[string]('a', 'b') == 'ab'
}

fn plus_one[T](a T) T {
	mut b := a
	b++
	return b
}

fn minus_one[T](a T) T {
	mut b := a
	b--
	return b
}

fn test_postfix_expr() {
	assert plus_one(-1) == 0
	assert plus_one(u8(0)) == 1
	assert plus_one(u16(1)) == 2
	assert plus_one(u32(2)) == 3
	assert plus_one(u64(3)) == 4
	assert plus_one(i8(-10)) == -9
	assert plus_one(i16(-9)) == -8
	assert plus_one(int(-8)) == -7
	assert plus_one(i64(-7)) == -6
	assert minus_one(0) == -1
	assert minus_one(u8(1)) == 0
	assert minus_one(u16(2)) == 1
	assert minus_one(u32(3)) == 2
	assert minus_one(u64(4)) == 3
	assert minus_one(i8(-8)) == -9
	assert minus_one(i16(-7)) == -8
	assert minus_one(int(-6)) == -7
	assert minus_one(i64(-5)) == -6

	// the point is to see if it compiles, more than if the result
	// is correct, so 1e-6 isn't necessarily the right value to do this
	// but it's not important
	delta := 1e-6
	assert plus_one(1.1) - 2.1 < delta
	assert plus_one(f32(2.2)) - 3.2 < delta
	assert plus_one(f64(3.3)) - 4.3 < delta
	assert minus_one(1.1) - 0.1 < delta
	assert minus_one(f32(2.2)) - 1.2 < delta
	assert minus_one(f64(3.3)) - 2.3 < delta
}

fn sum[T](l []T) T {
	mut r := T(0)
	for e in l {
		r += e
	}
	return r
}

fn test_array() {
	b := [1, 2, 3]
	assert sum(b) == 6
}

fn max[T](brug string, a ...T) T {
	mut max := a[0]
	for item in a[1..] {
		if max < item {
			max = item
		}
	}
	return max
}

fn test_generic_variadic() {
	assert max('krkr', 1, 2, 3, 4) == 4
	a := [f64(1.2), 3.2, 0.1, 2.2]
	assert max('krkr', ...a) == 3.2
	assert max('krkr', ...[u8(4), 3, 2, 1]) == 4
}

fn create[T]() {
	_ := T{}
	mut xx := T{}
	xx.name = 'foo'
	assert xx.name == 'foo'
	xx.init()
}

struct User {
mut:
	name string
}

struct City {
mut:
	name string
}

fn (u User) init() {
}

fn (c City) init() {
}

fn mut_arg[T](mut x T) {
	// println(x.name) // = 'foo'
}

fn mut_arg2[T](mut x T) T {
	// println(x.name) // = 'foo'
	return *x
}

fn test_create() {
	create[User]()
	create[City]()
	mut u := User{}
	mut_arg[User](mut u)
	mut_arg2[User](mut u)
}

fn return_array[T](arr []T) []T {
	return arr
}

fn test_return_array() {
	a1 := return_array[int]([1, 2, 3])
	assert a1 == [1, 2, 3]
	a2 := return_array[f64]([1.1, 2.2, 3.3])
	assert a2 == [1.1, 2.2, 3.3]
	a3 := return_array[string](['a', 'b', 'c'])
	assert a3 == ['a', 'b', 'c']
	a4 := return_array[bool]([true, false, true])
	assert a4 == [true, false, true]
}

fn opt[T](v T) ?T {
	if sizeof(T) > 1 {
		return v
	}
	return none
}

fn test_option() {
	s := opt('hi') or { '' }
	assert s == 'hi'
	i := opt(5) or { 0 }
	assert i == 5
	b := opt(s[0]) or { 99 }
	assert b == 99
}

fn ptr[T](v T) &T {
	a := [v]
	return a.data
}

fn test_ptr() {
	assert *ptr(4) == 4
	assert *ptr('aa') == 'aa'
}

fn map_f[T, U](l []T, f fn (T) U) []U {
	mut r := []U{}
	for e in l {
		r << f(e)
	}
	return r
}

/*
fn foldl[T](l []T, nil T, f fn(T,T)T) T {
	mut r := nil
	for e in l {
		r = f(r, e)
	}
	return r
}
*/
fn square(x int) int {
	return x * x
}

fn mul_int(x int, y int) int {
	return x * y
}

fn assert_eq[T](a T, b T) {
	r := a == b
	assert r
}

fn print_nice[T](x T, indent int) string {
	mut space := ''
	for _ in 0 .. indent {
		space = space + ' '
	}
	return '${space}${x}'
}

fn test_generic_fn() {
	assert_eq(simple(0 + 1), 1)
	assert_eq(simple('g') + 'h', 'gh')
	assert_eq(sum([5.1, 6.2, 7.0]), 18.3)
	assert_eq(plus(i64(4), i64(6)), i64(10))
	a := [1, 2, 3, 4]
	b := map_f(a, square)
	assert_eq(sum(b), 30) // 1+4+9+16 = 30
	// assert_eq(foldl(b, 1, mul_int), 576)   // 1*4*9*16 = 576
	assert print_nice('str', 8) == '        str'
}

struct Point {
mut:
	x f64
	y f64
}

fn (mut p Point) translate[T](x T, y T) {
	p.x += x
	p.y += y
}

fn test_generic_method() {
	mut p := Point{}
	p.translate(2, 1.0)
	assert p.x == 2.0 && p.y == 1.0
}

fn get_values[T](i T) []T {
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

struct Group {
pub mut:
	name       string
	group_name string
}

struct Permission {
pub mut:
	name string
}

struct Repo[T, U] {
	db DB
pub mut:
	model      T
	permission U
}

// TODO: multiple type generic struct  needs fixing in return for fn
// fn new_repo[T](db DB) Repo[T,U] {
// return Repo[T,Permission]{db: db}
// }
fn test_generic_struct() {
	mut a := Repo[User, Permission]{
		model: User{
			name: 'joe'
		}
	}
	assert a.model.name == 'joe'
	mut b := Repo[Group, Permission]{
		permission: Permission{
			name: 'superuser'
		}
	}
	b.model.name = 'admins'
	assert b.model.name == 'admins'
	assert b.permission.name == 'superuser'
	assert typeof(a.model).name == 'User'
	assert typeof(b.model).name == 'Group'
}

struct Foo[T] {
pub:
	data T
}

fn (f Foo[int]) value() string {
	return f.data.str()
}

fn test_generic_struct_method() {
	foo_int := Foo[int]{2}
	assert foo_int.value() == '2'
}

fn test_struct_from_other_module() {
	g := simplemodule.ThisIsGeneric[Permission]{}
	assert g.msg.name == ''
}

fn test_generic_struct_print_array_as_field() {
	foo := Foo[[]string]{
		data: []string{}
	}
	assert foo.str() == 'Foo[[]string]{\n    data: []\n}'
}

struct Abc {
	x int
	y int
	z int
}

fn p[T](args ...T) {
	size := sizeof(T)
	print('p called with size: ${size:3} | ')
	for _, x in args {
		print(x)
		print(' ')
	}
	println('')
	assert true
}

fn test_generic_fn_with_variadics() {
	s := 'abc'
	i := 1
	abc := Abc{1, 2, 3}

	// these calls should all compile, and print the arguments,
	// even though the arguments are all a different type and arity:
	p(s)
	p(i)
	p(abc)
	p('Good', 'morning', 'world')
}

struct Context {}

struct App {
mut:
	context Context
}

fn test[T](mut app T) {
	nested_test[T](mut app)
}

fn nested_test[T](mut app T) {
	app.context = Context{}
}

fn test_pass_generic_to_nested_function() {
	mut app := App{}
	test(mut app)
}

fn generic_return_map[M]() map[string]M {
	return {
		'': M{}
	}
}

fn test_generic_return_map() {
	assert typeof(generic_return_map[string]()).name == 'map[string]string'
}

fn generic_return_nested_map[M]() map[string]map[string]M {
	return {
		'': {
			'': M{}
		}
	}
}

fn test_generic_return_nested_map() {
	assert typeof(generic_return_nested_map[string]()).name == 'map[string]map[string]string'
}

fn multi_return[A, B]() (A, B) {
	return A{}, B{}
}

struct Foo1 {}

struct Foo2 {}

struct Foo3 {}

struct Foo4 {}

fn test_multi_return() {
	// compiles
	multi_return[Foo1, Foo2]()
	multi_return[Foo3, Foo4]()
}

fn multi_generic_args[T, V](t T, v V) bool {
	return true
}

fn test_multi_generic_args() {
	assert multi_generic_args('Super', 2021)
}

fn new[T]() T {
	return T{}
}

fn test_generic_init() {
	// array init
	mut a := new[[]string]()
	assert a.len == 0
	a << 'a'
	assert a.len == 1
	assert a[0] == 'a'

	// map init
	mut b := new[map[string]string]()
	assert b.len == 0
	b['b'] = 'b'
	assert b.len == 1
	assert b['b'] == 'b'

	// struct init
	mut c := new[User]()
	c.name = 'c'
	assert c.name == 'c'
}

fn return_one[T](rec int, useless T) T {
	if rec == 0 || 0 < return_one[T](rec - 1, useless) {
		return T(1)
	}
	return T(0)
}

struct MultiLevel[T] {
	foo T
}

fn get_multilevel_foo[T](bar MultiLevel[T]) int {
	return bar.foo.foo
}

fn get_multilevel_foo_2[T, U](bar T, baz U) int {
	return bar.foo.foo + baz.foo.foo
}

fn test_multi_level_generics() {
	one := MultiLevel[int]{
		foo: 10
	}
	two := MultiLevel[MultiLevel[int]]{
		foo: one
	}
	assert two.foo.foo == 10
	three := MultiLevel[MultiLevel[MultiLevel[int]]]{
		foo: two
	}
	assert three.foo.foo.foo == 10
	assert get_multilevel_foo[MultiLevel[int]](two) == 10
	assert get_multilevel_foo_2[MultiLevel[MultiLevel[int]], MultiLevel[MultiLevel[int]]](two,
		two) == 20
}

struct Empty_ {}

fn (e1 Empty_) < (e2 Empty_) bool {
	return true
}

struct TandU[T, U] {
	t T
	u U
}

fn boring_function[T](t T) bool {
	return true
}

fn test_generic_detection() {
	v1, v2 := -1, 1

	// not generic
	a1, a2 := v1 < v2, v2 > v1
	assert a1 && a2
	b1, b2 := v1 < simplemodule.zero, v2 > v1
	assert b1 && b2

	// generic
	assert multi_generic_args[int, string](0, 's')
	assert multi_generic_args[Foo1, Foo2](Foo1{}, Foo2{})
	assert multi_generic_args[Foo[int], Foo[int]](Foo[int]{}, Foo[int]{})

	// TODO: assert multi_generic_args<Foo<int>, Foo<int>>(Foo1{}, Foo2{})
	assert multi_generic_args[simplemodule.Data, int](simplemodule.Data{}, 0)
	assert multi_generic_args[int, simplemodule.Data](0, simplemodule.Data{})
	assert multi_generic_args[[]int, int]([]int{}, 0)
	assert multi_generic_args[map[int]int, int](map[int]int{}, 0)
	assert 0 < return_one[int](10, 0)

	// "the hardest cases"
	foo, bar, baz := 1, 2, 16
	res1, res2 := foo < bar, baz >> (foo + 1 - 1)
	assert res1
	assert res2 == 8
	res3, res4 := Empty_{} < Empty_{}, baz >> (foo + 1 - 1)
	assert res3
	assert res4 == 8
	assert boring_function[TandU[Empty_, int]](TandU[Empty_, int]{
		t: Empty_{}
		u: 10
	})

	assert boring_function[MultiLevel[MultiLevel[int]]](MultiLevel[MultiLevel[int]]{
		foo: MultiLevel[int]{
			foo: 10
		}
	})

	assert boring_function[TandU[MultiLevel[int], []int]](TandU[MultiLevel[int], []int]{
		t: MultiLevel[int]{
			foo: 10
		}
		u: [10]
	})

	// this final case challenges your scanner :-)
	assert boring_function[TandU[TandU[int, MultiLevel[Empty_]], map[string][]int]](TandU[TandU[int, MultiLevel[Empty_]], map[string][]int]{
		t: TandU[int, MultiLevel[Empty_]]{
			t: 20
			u: MultiLevel[Empty_]{
				foo: Empty_{}
			}
		}
		u: {
			'bar': [40]
		}
	})
}
