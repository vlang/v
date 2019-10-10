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
