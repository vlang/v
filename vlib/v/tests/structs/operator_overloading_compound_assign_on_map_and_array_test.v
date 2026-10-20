// A compound assignment on a map entry or an array element calls the value
// type's overloaded operator, like the same assignment on a local variable.
struct Vec {
	x int
	y int
}

fn (a Vec) + (b Vec) Vec {
	return Vec{a.x + b.x, a.y + b.y}
}

fn (a Vec) - (b Vec) Vec {
	return Vec{a.x - b.x, a.y - b.y}
}

fn (a Vec) * (b Vec) Vec {
	return Vec{a.x * b.x, a.y * b.y}
}

fn (a Vec) / (b Vec) Vec {
	return Vec{a.x / b.x, a.y / b.y}
}

fn (a Vec) % (b Vec) Vec {
	return Vec{a.x % b.x, a.y % b.y}
}

struct Body {
mut:
	v Vec
}

struct PairBody {
mut:
	p Pair[f64]
}

struct Holder {
mut:
	m map[string]Vec
}

// Money adds 1000 on every `+`, so a plain integer addition shows up.
type Money = int

fn (a Money) + (b Money) Money {
	return Money(int(a) + int(b) + 1000)
}

type Bank = map[string]Money

// Color declares its own `+`, which must win over the one on Vec.
type Color = Vec

fn (a Color) + (b Color) Color {
	return Color(Vec{a.x + b.x + 100, a.y + b.y + 100})
}

// Pos has no operator of its own and uses the one on Vec.
type Pos = Vec

// Name joins with `-` on `+`; Label is a plain alias of string.
type Name = string

fn (a Name) + (b Name) Name {
	return Name(a.str() + '-' + b.str())
}

type Label = string

struct Person {
mut:
	name Name
}

struct Pair[T] {
	a T
	b T
}

fn (x Pair[T]) + (y Pair[T]) Pair[T] {
	return Pair[T]{x.a + y.a, x.b + y.b}
}

fn maybe_vec(ok bool) ?Vec {
	if ok {
		return Vec{1, 1}
	}
	return none
}

fn add_twice_to_map[T](mut m map[string]T, v T) {
	m['g'] += v
	m['g'] += v
}

fn add_to_first[T](mut a []T, v T) {
	a[0] += v
}

fn next_key(mut calls []int) string {
	calls << 1
	return 'k'
}

fn bump_map_param(mut m map[string]Vec) {
	m['p'] += Vec{7, 7}
}

@[direct_array_access]
fn bump_direct(mut a []Vec, mut b []Money) {
	a[0] += Vec{10, 20}
	b[0] += Money(2)
}

fn test_map_value_struct_operators() {
	mut m := map[string]Vec{}
	m['a'] = Vec{10, 20}
	m['a'] += Vec{1, 2}
	assert m['a'] == Vec{11, 22}
	m['b'] += Vec{3, 4}
	assert m['b'] == Vec{3, 4}
	m['a'] -= Vec{1, 2}
	assert m['a'] == Vec{10, 20}
	m['a'] *= Vec{2, 3}
	assert m['a'] == Vec{20, 60}
	m['a'] /= Vec{4, 6}
	assert m['a'] == Vec{5, 10}
	m['a'] %= Vec{3, 7}
	assert m['a'] == Vec{2, 3}
	mut im := map[int]Vec{}
	im[3] += Vec{1, 1}
	assert im[3] == Vec{1, 1}
}

fn test_nested_map_value_struct_operators() {
	mut m := map[string]map[string]Vec{}
	m['a']['b'] += Vec{1, 2}
	m['a']['b'] += Vec{1, 2}
	m['a']['b'] -= Vec{1, 1}
	m['a']['b'] *= Vec{3, 3}
	assert m['a']['b'] == Vec{3, 9}
}

fn test_map_in_struct_param_and_shared() {
	mut h := Holder{}
	h.m['a'] += Vec{1, 2}
	h.m['a'] -= Vec{0, 1}
	assert h.m['a'] == Vec{1, 1}
	mut pm := map[string]Vec{}
	bump_map_param(mut pm)
	bump_map_param(mut pm)
	assert pm['p'] == Vec{14, 14}
	shared sm := map[string]Vec{}
	lock sm {
		sm['a'] += Vec{1, 1}
		sm['a'] += Vec{1, 1}
	}
	rlock sm {
		assert sm['a'] == Vec{2, 2}
	}
}

fn test_map_value_field_struct_operators() {
	mut m := map[string]Body{}
	m['a'] = Body{
		v: Vec{1, 2}
	}
	m['a'].v += Vec{10, 20}
	m['a'].v -= Vec{1, 1}
	m['a'].v *= Vec{2, 2}
	assert m['a'].v == Vec{20, 42}
}

fn test_map_key_and_or_value_are_evaluated_once() {
	mut calls := []int{}
	mut km := map[string]Vec{}
	km[next_key(mut calls)] += Vec{3, 3}
	assert km['k'] == Vec{3, 3}
	assert calls.len == 1
	mut om := map[string]Vec{}
	om['a'] += maybe_vec(true) or { Vec{100, 100} }
	om['a'] += maybe_vec(false) or { Vec{100, 100} }
	assert om['a'] == Vec{101, 101}
}

fn test_array_element_struct_operators() {
	mut a := [Vec{1, 2}, Vec{3, 4}]
	a[0] += Vec{10, 20}
	a[1] -= Vec{1, 1}
	a[1] *= Vec{2, 2}
	a[1] /= Vec{2, 3}
	a[1] %= Vec{3, 3}
	assert a == [Vec{11, 22}, Vec{2, 2}]
	mut f := [2]Vec{}
	f[1] += Vec{5, 6}
	assert f[1] == Vec{5, 6}
	mut b := [Body{
		v: Vec{1, 2}
	}]
	b[0].v += Vec{10, 20}
	assert b[0].v == Vec{11, 22}
	mut mm := map[string][]Vec{}
	mm['x'] = [Vec{1, 1}]
	mm['x'][0] += Vec{2, 2}
	assert mm['x'] == [Vec{3, 3}]
	mut d := [Vec{1, 2}]
	mut dm := [Money(1)]
	bump_direct(mut d, mut dm)
	assert d == [Vec{11, 22}]
	assert dm == [Money(1003)]
}

fn test_alias_operators_on_map_and_array() {
	mut m := map[string]Money{}
	m['a'] += Money(5)
	assert m['a'] == Money(1005)
	mut bank := Bank{}
	bank['a'] += Money(5)
	assert bank['a'] == Money(1005)
	mut nested := map[string]map[string]Money{}
	nested['a']['b'] += Money(1)
	assert nested['a']['b'] == Money(1001)
	mut c := map[string]Color{}
	c['a'] += Color(Vec{1, 2})
	assert c['a'] == Color(Vec{101, 102})
	mut p := map[string]Pos{}
	p['a'] += Pos(Vec{1, 2})
	p['a'] += Pos(Vec{1, 2})
	assert p['a'] == Pos(Vec{2, 4})
	mut ma := [Money(1)]
	ma[0] += Money(2)
	assert ma == [Money(1003)]
	mut ca := [Color(Vec{1, 1})]
	ca[0] += Color(Vec{1, 1})
	assert ca == [Color(Vec{102, 102})]
}

fn test_generic_operators_on_map_and_array() {
	mut gv := map[string]Vec{}
	add_twice_to_map(mut gv, Vec{2, 3})
	assert gv['g'] == Vec{4, 6}
	mut gi := map[string]int{}
	add_twice_to_map(mut gi, 4)
	assert gi['g'] == 8
	mut gm := map[string]Money{}
	add_twice_to_map(mut gm, Money(1))
	assert gm['g'] == Money(2002)
	mut ga := [Money(1)]
	add_to_first(mut ga, Money(1))
	assert ga == [Money(1002)]
	mut pm := map[string]Pair[int]{}
	pm['a'] += Pair[int]{1, 2}
	pm['a'] += Pair[int]{1, 2}
	assert pm['a'] == Pair[int]{2, 4}
	mut pa := [Pair[f64]{0.5, 1.5}]
	pa[0] += Pair[f64]{1.0, 1.0}
	assert pa[0] == Pair[f64]{1.5, 2.5}
	mut pf := [2]Pair[i64]{}
	pf[1] += Pair[i64]{3, 4}
	assert pf[1] == Pair[i64]{3, 4}
	mut pg := [Pair[u8]{1, 2}]
	add_to_first(mut pg, Pair[u8]{3, 4})
	assert pg[0] == Pair[u8]{4, 6}
	mut ab := [PairBody{}]
	ab[0].p += Pair[f64]{1.0, 2.0}
	assert ab[0].p == Pair[f64]{1.0, 2.0}
	mut mb := map[string]PairBody{}
	mb['k'] = PairBody{}
	mb['k'].p += Pair[f64]{0.5, 0.25}
	mb['k'].p += Pair[f64]{0.5, 0.25}
	assert mb['k'].p == Pair[f64]{1.0, 0.5}
}

fn test_string_alias_operator_wins_over_concatenation() {
	mut n := Name('a')
	n += Name('b')
	assert n == Name('a-b')
	mut p := Person{
		name: Name('a')
	}
	p.name += Name('b')
	assert p.name == Name('a-b')
	mut m := map[string]Name{}
	m['x'] = Name('a')
	m['x'] += Name('b')
	assert m['x'] == Name('a-b')
	mut mp := map[string]Person{}
	mp['x'] = Person{
		name: Name('a')
	}
	mp['x'].name += Name('b')
	assert mp['x'].name == Name('a-b')
	mut a := [Name('a')]
	a[0] += Name('b')
	assert a == [Name('a-b')]
	mut s := 'a'
	s += 'b'
	assert s == 'ab'
	mut ms := map[string]string{}
	ms['x'] += 'a'
	ms['x'] += 'b'
	assert ms['x'] == 'ab'
	mut l := Label('a')
	l += Label('b')
	assert l == Label('ab')
	mut ml := map[string]Label{}
	ml['x'] += Label('a')
	ml['x'] += Label('b')
	assert ml['x'] == Label('ab')
	mut al := [Label('a')]
	al[0] += Label('b')
	assert al == [Label('ab')]
}
