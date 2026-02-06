// for vls module test
module sample_mod1

// public

// This line is not the PublicStruct1's comment
// PublicStruct1 is a public struct
pub struct PublicStruct1 { // And PublicStruct1 should init here
	// data_int is a int data
	data_int int
	// this is a string
	data_string string // note: init = ''
	// and this is a u8
	data_u8 u8
}

// This line is not the public_const1's comment
// public_const1 is a public const
pub const public_const1 = 'a public const' // public_const1 is a string

// This line is not the PublicEnum1's comment
// PublicEnum1 contains `a`,`b` and `c`
pub enum PublicEnum1 {
	// a is a PublicEnum1 data
	a
	// b is a PublicEnum1 data
	b
	// c is a PublicEnum1 data
	c
}

// PublicInterface1 is a public interface, has fields and methods
pub interface PublicInterface1 {
	// val is a int type data field
	val int
	// method is a void func return string
	method() string
}

// PublicAlias1_1 is a sumtype of `u8`,`u16` and `int`
pub type PublicAlias1_1 = u8 | u16 | int

// PublicAlias1_2 is a alias of `PublicStruct1`
pub type PublicAlias1_2 = PublicStruct1

// PublicCB is a funtion type
pub type PublicCB = fn (msg &char, arg usize)

// public_fn1 is a public function, return string
pub fn public_fn1(val int) string {
	return '${val}'
}

// public is a static method of `PublicStruct1`
pub fn PublicStruct1.public() PublicStruct1 {
	return PublicStruct1{}
}

// new is a method of `PublicStruct1`
pub fn (mut p PublicStruct1) new() PublicStruct1 {
	return PublicStruct1{}
}

// private

struct PrivateStruct1 {
	data        int
	data_string string
}

const private_const1 = 'a private const'

enum PrivateEnum1 {
	a
	b
	c
}

interface PrivateInterface1 {
	val int
	method() string
}

type PrivateAlias1_1 = u8 | u16 | int

type PrivateAlias1_2 = PublicStruct1

type PrivateCB = fn (msg &char, arg usize)

fn private_fn1(val int) string {
	return '${val}'
}

fn PrivateStruct1.new() PrivateStruct1 {
	return PrivateStruct1{}
}

fn PublicStruct1.private() PublicStruct1 {
	return PublicStruct1{}
}
