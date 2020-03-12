struct A{
mut:
	val int
	nums []int
}

struct B{
mut:
	a A
}

struct C {
mut:
	b B
	nums []int
	aarr []A
	num int
}

struct User {
	name string
	age int
}

struct Foo {
	@type string
}

//We need to make sure that this compiles with all the reserved names.
struct ReservedKeywords {
	delete   int
	exit     int
	unix     int
	error    int
	malloc   int
	calloc   int
	free     int
	panic    int
	auto     int
	char     int
	do       int
	double   int
	extern   int
	float    int
	inline   int
	long     int
	register int
	restrict int
	short    int
	signed   int
	typedef  int
	unsigned int
	void     int
	volatile int
	while    int
}

fn test_struct_levels() {
	mut c := C{}
	assert c.nums.len == 0
	c.nums << 3
	assert c.nums.len == 1
	assert c.nums[0] == 3
	c.nums[0] = 4
	assert c.nums[0] == 4
	c.b.a.val = 34
	assert c.b.a.val == 34
	c.b.a.nums = [0].repeat(0)
	c.b.a.nums << 0
	c.b.a.nums << 2
	assert c.b.a.nums.len == 2
	assert c.b.a.nums[0] == 0
	assert c.b.a.nums[1] == 2
	c.b.a.nums [0] = 7
	assert c.b.a.nums[0] == 7
	c.aarr << A{val:8}
	assert c.aarr.len == 1
	assert c.aarr[0].val == 8
	c.num = 20
	assert c.num == 20
	c.aarr[0].val  = 10
	assert c.aarr[0].val == 10
}

fn test_struct_str() {
	u := User{'Bob', 30}
	println(u)  // make sure the struct is printable
	// assert u.str() == '{name:"Bob", age:30}'  // TODO
}

fn test_at() {
	foo := Foo{ @type: 'test' }
	println(foo.@type)
}

fn test_reserved_keywords() {
	//Make sure we can initialize them correctly using full syntax.
	rk_holder := ReservedKeywords{0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3}
	//Test a few as it'll take too long to test all. If it's initialized
	//correctly, other fields are also probably valid.
	assert rk_holder.unix == 5
	assert rk_holder.while == 3

	rk_holder2 := ReservedKeywords{inline: 9, volatile: 11}
	//Make sure partial initialization works too.
	assert rk_holder2.inline == 9
	assert rk_holder2.volatile == 11
	assert rk_holder2.while == 0 //Zero value as not specified.
}

struct User2 {
mut:
	name string
}

fn test_mutable_fields() {
	mut u := User2{}
	u.name = 'Peter'
	assert u.name == 'Peter'
}


struct Def {
	a int
	b int = 7
}

fn test_default_vals() {
	d := Def{}
	assert d.a == 0
	assert d.b == 7
	d2 := Def{10, 20}
	assert d2.a == 10
	assert d2.b == 20
}

fn test_assoc_with_vars() {
	def2 := Def { a: 12 }
	merged := { def2 | a: 42 }
	assert merged.a == 42
	assert merged.b == 7
}

const (
	const_def = Def { a: 100 }
)
fn test_assoc_with_constants() {
	merged := { const_def | a: 42 }
	assert merged.a == 42
	assert merged.b == 7

	again := { const_def | b: 22 }
	assert again.a == 100
	assert again.b == 22
}

struct AttrTest{
	a int     // private immutable (default)
mut:
	b int     // private mutable
	c int     // (you can list multiple fields with the same access modifier)
pub:
	d int     // public immmutable (readonly)
pub mut:
	e int     // public, but mutable only in parent module
__global:
	f int 	  // public and mutable both inside and outside parent module
}

fn fooo(){
	a:=AttrTest{1,2,3,4,5,6}
}

/*
[typedef]
struct C.fixed {
	points [10]C.point
}

[typedef]
struct C.point {
	x int
	y int
}

fn test_fixed_field() {
	f := &C.fixed{}
	p := f.points[0]
	//f.nums[0] = 10
	//println(f.nums[0])
	println(p.x)
		//nums: [10]int
	//}
}
*/


struct Config {
	n int
	def int = 10
}

fn foo_config(c Config) {
}

fn foo2(u User) {

}

fn test_config() {
	foo_config({n: 10, def: 20})
	foo_config({})
	foo2({name:'Peter'})
}


