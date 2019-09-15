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
	c.b.a.nums = [0]
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
