struct Aa {
mut:
	val  int
	nums []int
}

struct Bb {
mut:
	a Aa
}

struct Cu {
mut:
	b    Bb
	nums []int
	aarr []Aa
	num  int
}

struct Lol {
	b []string [json: lol]
	c string   [json: cc]
	d int
}

struct User {
	name string
	age  int
}

struct Foo {
	typ string
}

struct Empty {
}

// We need to make sure that this compiles with all the reserved names.
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
	while    int
}

fn test_empty_struct() {
	d := &Empty{}
	d2 := Empty{}
	println('&empty:')
	println(d) // != voidptr(0)
	println('empty:')
	println(d2) // empty struct print
	println(sizeof(Empty)) // == 0
}

fn test_struct_levels() {
	mut c := Cu{}
	println(c.nums.len)
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
	c.b.a.nums[0] = 7
	assert c.b.a.nums[0] == 7
	c.aarr << Aa{
		val: 8
	}
	assert c.aarr.len == 1
	assert c.aarr[0].val == 8
	c.num = 20
	assert c.num == 20
	c.aarr[0].val = 10
	assert c.aarr[0].val == 10
}

fn test_struct_str() {
	u := User{'Bob', 30}
	println(u) // make sure the struct is printable
	// assert u.str() == '{name:"Bob", age:30}'  // QTODO
}

fn test_at() {
	foo := Foo{
		typ: 'test'
	}
	// type: 'test'
	println(foo.typ)
}

fn test_reserved_keywords() {
	// Make sure we can initialize them correctly using full syntax.
	rk_holder := ReservedKeywords{0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3}
	// Test a few as it'll take too long to test all. If it's initialized
	// correctly, other fields are also probably valid.
	assert rk_holder.unix == 5
	assert rk_holder.while == 3
	rk_holder2 := ReservedKeywords{
		inline: 9
	}
	// Make sure partial initialization works too.
	assert rk_holder2.inline == 9
	assert rk_holder2.while == 0 // Zero value as not specified.
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
	def2 := Def{
		a: 12
	}
	mut merged := Def{
		...def2
		a: 42
	}
	assert merged.a == 42
	assert merged.b == 7
	merged = Def{
		...def2
		b: 9
	}
	assert merged == Def{12, 9}

	def3 := &Def{100, 200}
	merged1 := Def{
		...(*def3)
	}
	merged2 := &Def{
		...(*def3)
	}
	assert merged1.a == 100
	assert merged1.b == 200
	assert merged2.a == 100
	assert merged2.b == 200
}

const (
	const_def = Def{
		a: 100
	}
)

fn test_assoc_with_constants() {
	println(1)
	/*
	TODO:
	merged := { const_def | a: 42 }
	assert merged.a == 42
	assert merged.b == 7

	again := { const_def | b: 22 }
	assert again.a == 100
	assert again.b == 22
	*/
}

struct AttrTest {
	a int // private immutable (default)
mut:
	b int // private mutable
	c int // (you can list multiple fields with the same access modifier)
pub:
	d int // public immmutable (readonly)
pub mut:
	e int // public, but mutable only in parent module
	f int // public and mutable both inside and outside parent module
}

fn fooo() {
	_ := AttrTest{1, 2, 3, 4, 5, 6}
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
[params]
struct Config {
mut:
	n   int
	def int = 10
}

fn foo_config(def int, c Config) {
	assert c.def == def
}

fn bar_config(c Config, def int) {
	assert c.def == def
}

fn mut_bar_config(mut c Config, def int) &Config {
	c.n = c.def
	assert c.n == def
	return unsafe { c }
}

fn foo_user(u User) {}

fn test_struct_literal_args() {
	foo_config(20,
		n: 10
		def: 20
	)
	foo_config(10)
	foo_config(10, n: 40)
	foo_config(40, n: 30, def: 40)

	bar_config(Config{}, 10)
	bar_config(Config{ def: 4 }, 4)

	mut c_ := Config{
		def: 10
	}
	c := mut_bar_config(mut c_, 10)
	assert c.n == 10
	assert c.def == 10

	foo_user(name: 'Peter')
	foo_user(name: 'Peter')
	foo_user(age: 7)
	foo_user(name: 'Stew', age: 50)
}

struct City {
	name       string
	population int
}

struct Country {
	name    string
	capital City
}

fn test_levels() {
	_ := Country{
		name: 'UK'
		capital: City{
			name: 'London'
			population: 10
		}
	}
}

// Struct where an inizialized field is after a non-initilized field.
struct StructWithDefaultValues1 {
	field_required int
	field_optional int = 5
}

// Struct where an inizialized field is before a non-initilized field.
struct StructWithDefaultValues2 {
	field_optional int = 3
	field_required int
}

// Struct where an inizialized field is before several non-initilized fields.
struct StructWithDefaultValues3 {
	field_optional     int = 2
	field_required     int
	field_required_too int
}

fn test_struct_with_default_values_init() {
	s1 := StructWithDefaultValues1{
		field_required: 5
	}
	s2 := StructWithDefaultValues2{
		field_required: 5
	}
	// Partially initialized
	s3 := StructWithDefaultValues3{
		field_required: 5
	}

	assert s1.field_optional == 5
	assert s2.field_optional == 3
	assert s3.field_optional == 2
}

fn test_struct_with_default_values_no_init() {
	// Don't inititialize
	s1 := StructWithDefaultValues1{}
	s2 := StructWithDefaultValues2{}
	s3 := StructWithDefaultValues3{}

	assert s1.field_optional == 5
	assert s2.field_optional == 3
	assert s3.field_optional == 2
}

struct FieldsWithOptionalVoidReturnType {
	f fn () ?
	g fn () ?
}

fn test_fields_anon_fn_with_optional_void_return_type() {
	foo := FieldsWithOptionalVoidReturnType{
		f: fn () ? {
			return error('oops')
		}
		g: fn () ? {
			return
		}
	}

	foo.f() or { assert err.msg() == 'oops' }

	foo.g() or { assert false }
}

struct Commands {
	show []fn () string
}

fn a() string {
	return 'HELLOW'
}

fn b() string {
	return 'WOLLEH'
}

fn test_fields_array_of_fn() {
	commands := Commands{
		show: [a, b]
	}
	println(commands.show)
	assert '${commands.show}' == '[fn () string, fn () string]'
}

fn test_struct_update() {
	c := Country{
		name: 'test'
	}
	c2 := Country{
		...c
		capital: City{
			name: 'city'
		}
	}
	assert c2.capital.name == 'city'
	assert c2.name == 'test'
}

// Test anon structs
struct Book {
	x      Foo
	author struct {
		name string
		age  int
	}

	title string
}

fn test_anon() {
	empty_book := Book{}
	assert empty_book.author.age == 0
	assert empty_book.author.name == ''
	println(empty_book.author.age)

	book := Book{
		author: struct {'Peter Brown', 23}
	}
	assert book.author.name == 'Peter Brown'
	assert book.author.age == 23
	println(book.author.name)

	book2 := Book{
		author: struct {
			name: 'Samantha Black'
			age: 24
		}
	}
	assert book2.author.name == 'Samantha Black'
	assert book2.author.age == 24
	println(book2.author.name)
}

fn test_anon_auto_stringify() {
	b := Book{}
	s := b.str()
	assert s.contains('author: ')
	assert s.contains('name: ')
	assert s.contains('age: 0')
}
