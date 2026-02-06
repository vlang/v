interface Abc {}

enum EnumTest {
	a
	b
}

type Alias = Abc

type Sumtype = int | string

struct Foo {}

fn dummy_fn() {}

struct Test {
	a Abc
	b map[string]string
	c []int
	d EnumTest
	e int
	f f64
	g Alias
	h Foo
	i fn () = dummy_fn
	j Sumtype
	k ?int
}

fn test_comptime_types() {
	mut i := 0
	mut a := 0
	mut m := 0
	mut e := 0
	mut c_int := 0
	mut c_float := 0
	mut c_alias := 0
	mut c_struct := 0
	mut c_fn := 0
	mut c_sum := 0
	mut c_option := 0
	mut fixeda := 0
	$for f in Test.fields {
		$if f.typ is $alias {
			c_alias++
		} $else $if f.typ is $interface {
			i++
		} $else $if f.typ is $array_fixed {
			fixeda++
		} $else $if f.typ is $array {
			a++
		} $else $if f.typ is $map {
			m++
		} $else $if f.typ is $enum {
			e++
		} $else $if f.typ is $int {
			c_int++
		} $else $if f.typ is $float {
			c_float++
		} $else $if f.typ is $struct {
			c_struct++
		} $else $if f.typ is $function {
			c_fn++
		} $else $if f.typ is $sumtype {
			c_sum++
		} $else $if f.typ is $option {
			c_option++
		}
	}
	assert i == 1
	assert a == 1
	assert m == 1
	assert e == 1
	assert fixeda == 0
	assert c_int == 1
	assert c_float == 1
	assert c_alias == 1
	assert c_struct == 1
	assert c_fn == 1
	assert c_sum == 1
	assert c_option == 1
}
